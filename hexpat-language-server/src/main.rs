use std::collections::HashMap;
use std::str::FromStr;

use dashmap::DashMap;
use hexparser::recovery_err::RecoveredError;
use hexparser::token::Spanned;
use hexparser::{parse, type_inference, ImCompleteSemanticToken, Value, Expr};
use hexpat_language_server::completion::{completion, ImCompleteCompletionItem, add_completion};
use hexpat_language_server::jump_definition::get_definition;
use hexpat_language_server::reference::get_reference;
use hexpat_language_server::semantic_token::semantic_token_from_ast;
use parserlib::LEGEND_TYPE;
use ropey::Rope;
use serde::{Deserialize, Serialize};
use serde_json::{Value as jValue};
use tower_lsp::jsonrpc::{Result, Error as LSPError, ErrorCode};
use tower_lsp::lsp_types::notification::Notification;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

const LSP_NAME: &str = "hexpat-language-server";
const COMMAND_RUN_ON_IMHEX: &str = "hexpat-language-server.runOnImHex";
const DEFAULT_IMHEX_PORT: u16 = 31337;

const CONFIG_IMHEX_BASE_FOLDERS: &str = "imhexBaseFolders";
const CONFIG_IMHEX_PORT: &str = "imhexPort";

#[derive(Debug)]
enum ConfigurationEntry {
    ImHexPaths(Vec<String>),
    Port(u16),
}

#[derive(Debug)]
struct Backend {
    client: Client,
    ast_map: DashMap<String, Spanned<Expr>>,
    document_map: DashMap<String, Rope>,
    semantic_token_map: DashMap<String, Vec<ImCompleteSemanticToken>>,
    configuration: DashMap<String, ConfigurationEntry>
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            server_info: None,
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(false),
                    trigger_characters: Some(vec![".".to_string()]),
                    work_done_progress_options: Default::default(),
                    all_commit_characters: None,
                    completion_item: None, // TODO
                }),
                execute_command_provider: Some(ExecuteCommandOptions {
                    commands: vec![
                        COMMAND_RUN_ON_IMHEX.to_string()
                    ],
                    work_done_progress_options: Default::default(),
                }),
                workspace: Some(WorkspaceServerCapabilities {
                    workspace_folders: Some(WorkspaceFoldersServerCapabilities {
                        supported: Some(true),
                        change_notifications: Some(OneOf::Left(true)),
                    }),
                    file_operations: None,
                }),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensRegistrationOptions(
                        SemanticTokensRegistrationOptions {
                            text_document_registration_options: {
                                TextDocumentRegistrationOptions {
                                    document_selector: Some(vec![DocumentFilter {
                                        language: Some("hexpat".to_string()),
                                        scheme: Some("file".to_string()),
                                        pattern: None,
                                    }]),
                                }
                            },
                            semantic_tokens_options: SemanticTokensOptions {
                                work_done_progress_options: WorkDoneProgressOptions::default(),
                                legend: SemanticTokensLegend {
                                    token_types: LEGEND_TYPE.clone().into(),
                                    token_modifiers: vec![],
                                },
                                range: Some(true),
                                full: Some(SemanticTokensFullOptions::Bool(true)),
                            },
                            static_registration_options: StaticRegistrationOptions::default(),
                        },
                    ),
                ),
                // definition: Some(GotoCapability::default()),
                definition_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
                rename_provider: Some(OneOf::Left(true)),
                ..ServerCapabilities::default()
            },
        })
    }
    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = params.text_document.uri.to_string();
        self.client
            .log_message(MessageType::LOG, "semantic_token_full")
            .await;
        let semantic_tokens = || -> Option<Vec<SemanticToken>> {
            let mut im_complete_tokens = self.semantic_token_map.get_mut(&uri)?;
            let rope = self.document_map.get(&uri)?;
            let ast = self.ast_map.get(&uri)?;
            let extends_tokens = semantic_token_from_ast(&ast);
            im_complete_tokens.extend(extends_tokens);
            im_complete_tokens.sort_by(|a, b| a.start.cmp(&b.start));
            let mut pre_line = 0;
            let mut pre_start = 0;
            let semantic_tokens = im_complete_tokens
                .iter()
                .filter_map(|token| {
                    let line = rope.try_byte_to_line(token.start as usize).ok()? as u32;
                    let first = rope.try_line_to_char(line as usize).ok()? as u32;
                    let start = rope.try_byte_to_char(token.start as usize).ok()? as u32 - first;
                    let delta_line = line - pre_line;
                    let delta_start = if delta_line == 0 {
                        start - pre_start
                    } else {
                        start
                    };
                    let ret = Some(SemanticToken {
                        delta_line,
                        delta_start,
                        length: token.length as u32,
                        token_type: token.token_type as u32,
                        token_modifiers_bitset: 0,
                    });
                    pre_line = line;
                    pre_start = start;
                    ret
                })
                .collect::<Vec<_>>();
            Some(semantic_tokens)
        }();
        if let Some(semantic_token) = semantic_tokens {
            return Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
                result_id: None,
                data: semantic_token,
            })));
        }
        Ok(None)
    }

    async fn semantic_tokens_range(
        &self,
        params: SemanticTokensRangeParams,
    ) -> Result<Option<SemanticTokensRangeResult>> {
        let uri = params.text_document.uri.to_string();
        let semantic_tokens = || -> Option<Vec<SemanticToken>> {
            let im_complete_tokens = self.semantic_token_map.get(&uri)?;
            let rope = self.document_map.get(&uri)?;
            let mut pre_line = 0;
            let mut pre_start = 0;
            let semantic_tokens = im_complete_tokens
                .iter()
                .filter_map(|token| {
                    let line = rope.try_byte_to_line(token.start as usize).ok()? as u32;
                    let first = rope.try_line_to_char(line as usize).ok()? as u32;
                    let start = rope.try_byte_to_char(token.start as usize).ok()? as u32 - first;
                    let ret = Some(SemanticToken {
                        delta_line: line - pre_line,
                        delta_start: if start >= pre_start {
                            start - pre_start
                        } else {
                            start
                        },
                        length: token.length as u32,
                        token_type: token.token_type as u32,
                        token_modifiers_bitset: 0,
                    });
                    pre_line = line;
                    pre_start = start;
                    ret
                })
                .collect::<Vec<_>>();
            Some(semantic_tokens)
        }();
        if let Some(semantic_token) = semantic_tokens {
            return Ok(Some(SemanticTokensRangeResult::Tokens(SemanticTokens {
                result_id: None,
                data: semantic_token,
            })));
        }
        Ok(None)
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client.register_capability(vec![
            Registration {
                id: "configurationcapability".to_string(),
                method: "workspace/didChangeConfiguration".to_string(),
                register_options: None
            }
        ]).await.unwrap();
        self.update_configuration().await;
        self.client
            .log_message(MessageType::INFO, "initialized!")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let reference_list = || -> Option<Vec<Location>> {
            let uri = params.text_document_position.text_document.uri;
            let ast = self.ast_map.get(&uri.to_string())?;
            let rope = self.document_map.get(&uri.to_string())?;

            let position = params.text_document_position.position;
            let char = rope.try_line_to_char(position.line as usize).ok()?;
            let offset = char + position.character as usize;
            let reference_list = get_reference(&ast, offset, false);
            let ret = reference_list
                .into_iter()
                .filter_map(|(_, range)| {
                    let start_position = offset_to_position(range.start, &rope)?;
                    let end_position = offset_to_position(range.end, &rope)?;

                    let range = Range::new(start_position, end_position);

                    Some(Location::new(uri.clone(), range))
                })
                .collect::<Vec<_>>();
            Some(ret)
        }();
        Ok(reference_list)
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let definition = || -> Option<GotoDefinitionResponse> {
            let uri = params.text_document_position_params.text_document.uri;
            let ast = self.ast_map.get(&uri.to_string())?;
            let rope = self.document_map.get(&uri.to_string())?;

            let position = params.text_document_position_params.position;
            let char = rope.try_line_to_char(position.line as usize).ok()?;
            let offset = char + position.character as usize;
            let span = get_definition(&ast, offset);
            span.and_then(|(_, range)| {
                let start_position = offset_to_position(range.start, &rope)?;
                let end_position = offset_to_position(range.end, &rope)?;

                let range = Range::new(start_position, end_position);

                Some(GotoDefinitionResponse::Scalar(Location::new(uri, range)))
            })
        }();
        Ok(definition)
    }

    async fn did_change_workspace_folders(&self, _: DidChangeWorkspaceFoldersParams) {
        self.update_configuration().await;
        self.client
            .log_message(MessageType::INFO, "workspace folders changed!")
            .await;
    }

    async fn did_change_configuration(&self, params: DidChangeConfigurationParams) {
        self.update_configuration().await;
        self.client
            .log_message(MessageType::INFO, format!("configuration changed!"))
            .await;
    }

    async fn did_change_watched_files(&self, _: DidChangeWatchedFilesParams) {
        self.client
            .log_message(MessageType::INFO, "watched files have changed!")
            .await;
    }

    async fn execute_command(&self, command: ExecuteCommandParams) -> Result<Option<jValue>> {
        self.client
            .log_message(MessageType::INFO, "command executed!")
            .await;

        match command.command.as_str() {
            COMMAND_RUN_ON_IMHEX => self.execute_run_on_imhex(command),
            command => Err(LSPError {
                code: ErrorCode::MethodNotFound,
                message: format!("Command {command} does not exist"),
                data: None
            })
        }
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "file opened!")
            .await;
        self.on_change(TextDocumentItem {
            uri: params.text_document.uri,
            text: params.text_document.text,
            version: Some(params.text_document.version),
        })
        .await
    }

    async fn did_change(&self, mut params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri.clone();
        self.client
            .log_message(MessageType::INFO, format!("Changed {uri}!"))
            .await;
        self.on_change(TextDocumentItem {
            uri: params.text_document.uri,
            text: std::mem::take(&mut params.content_changes[0].text),
            version: Some(params.text_document.version),
        })
        .await
    }

    async fn did_save(&self, _: DidSaveTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "file saved!")
            .await;
    }

    async fn did_close(&self, _: DidCloseTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "file closed!")
            .await;
    }

    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        let workspace_edit = || -> Option<WorkspaceEdit> {
            let uri = params.text_document_position.text_document.uri;
            let ast = self.ast_map.get(&uri.to_string())?;
            let rope = self.document_map.get(&uri.to_string())?;

            let position = params.text_document_position.position;
            let char = rope.try_line_to_char(position.line as usize).ok()?;
            let offset = char + position.character as usize;
            let reference_list = get_reference(&ast, offset, true);
            let new_name = params.new_name;
            if reference_list.len() > 0 {
                let edit_list = reference_list
                    .into_iter()
                    .filter_map(|(_, range)| {
                        let start_position = offset_to_position(range.start, &rope)?;
                        let end_position = offset_to_position(range.end, &rope)?;
                        Some(TextEdit::new(
                            Range::new(start_position, end_position),
                            new_name.clone(),
                        ))
                    })
                    .collect::<Vec<_>>();
                let mut map = HashMap::new();
                map.insert(uri, edit_list);
                let workspace_edit = WorkspaceEdit::new(map);
                Some(workspace_edit)
            } else {
                None
            }
        }();
        Ok(workspace_edit)
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        self.client
            .log_message(MessageType::INFO, format!("COMPLETION"))
            .await;
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        let mut comps = Vec::new();
        let completions = || -> Option<Vec<CompletionItem>> {
            let rope = self.document_map.get(&uri.to_string())?;
            let ast = self.ast_map.get(&uri.to_string())?;
            let char = rope.try_line_to_char(position.line as usize).ok()?;
            let offset = char + position.character as usize;
            let completions = completion(&ast, offset);
            comps = completions.keys().map(|a| a.clone()).collect();
            let mut ret = Vec::with_capacity(completions.len());
            for (_, item) in completions {
                add_completion(item, &mut ret, None);
            }
            Some(ret)
        }();

        self.client
            .log_message(MessageType::INFO, format!("{:?}", comps))
            .await;
        Ok(completions.map(CompletionResponse::Array))
    }
}

#[derive(Debug, Deserialize, Serialize)]
struct InlayHintParams {
    path: String,
}

enum CustomNotification {}
impl Notification for CustomNotification {
    type Params = InlayHintParams;
    const METHOD: &'static str = "custom/notification";
}
struct TextDocumentItem {
    uri: Url,
    text: String,
    version: Option<i32>,
}
impl Backend {
    async fn inlay_hint(&self, params: InlayHintParams) -> Result<Vec<(usize, usize, String)>> {
        let mut hashmap = HashMap::new();
        if let Some(ast) = self.ast_map.get(&params.path) {
            type_inference(&ast, &mut hashmap);
        }

        let inlay_hint_list = hashmap
            .into_iter()
            .map(|(k, v)| {
                (
                    k.start,
                    k.end,
                    match v {
                        Value::Null => "null".to_string(),
                        Value::Bool(_) => "bool".to_string(),
                        Value::Num(_) => "number".to_string(),
                        Value::Str(_) => "string".to_string(),
                        Value::Char(_) => "char".to_string(),
                        Value::Func(_) => v.to_string(),
                    },
                )
            })
            .collect::<Vec<_>>();
        Ok(inlay_hint_list)
    }

    async fn on_change(&self, params: TextDocumentItem) {
        let rope = ropey::Rope::from_str(&params.text);
        self.document_map
            .insert(params.uri.to_string(), rope.clone());
        self.update_parse(params.uri, rope, &params.text, params.version).await
    }

    async fn update_parse(&self, uri: Url, rope: Rope, text: &str, version: Option<i32>) {
        let mut paths_v = vec![];
        let paths = self.configuration.get(CONFIG_IMHEX_BASE_FOLDERS);
        match paths {
            Some(p) => match p.value() {
                ConfigurationEntry::ImHexPaths(p) => for path in p {
                    paths_v.push(path.clone())
                },
                _ => unreachable!(),
            },
            None => (),
        };
        let (ast, errors, semantic_tokens) = parse(text, &paths_v);
        self.client
            .log_message(MessageType::INFO, format!("{:?}", errors))
            .await;
        let diagnostics = errors
            .into_iter()
            .filter_map(|RecoveredError(span, message)| {
                let diagnostic = || -> Option<Diagnostic> {
                    // let start_line = rope.try_char_to_line(span.start)?;
                    // let first_char = rope.try_line_to_char(start_line)?;
                    // let start_column = span.start - first_char;
                    let start_position = offset_to_position(span.start, &rope)?;
                    let end_position = offset_to_position(span.end, &rope)?;
                    // let end_line = rope.try_char_to_line(span.end)?;
                    // let first_char = rope.try_line_to_char(end_line)?;
                    // let end_column = span.end - first_char;
                    Some(Diagnostic::new_simple(
                        Range::new(start_position, end_position),
                        message,
                    ))
                }();
                diagnostic
            })
            .collect::<Vec<_>>();
        self.client
            .publish_diagnostics(uri.clone(), diagnostics, version)
            .await;

        self.ast_map.insert(uri.to_string(), ast);
        self.client
            .log_message(MessageType::INFO, &format!("{:?}", semantic_tokens))
            .await;
        self.semantic_token_map
            .insert(uri.to_string(), semantic_tokens);
    }

    async fn update_configuration(&self) {
        let mut config = self.client.configuration(vec![
            ConfigurationItem {
                scope_uri: None,
                section: Some(format!("{LSP_NAME}.{CONFIG_IMHEX_BASE_FOLDERS}"))
            },
            ConfigurationItem {
                scope_uri: None,
                section: Some(format!("{LSP_NAME}.{CONFIG_IMHEX_PORT}"))
            }
        ]).await.unwrap();

        let port = match config.pop().unwrap() {
            jValue::Number(port) => {
                let port = port.as_u64().unwrap() as u16;

                ConfigurationEntry::Port(port)
            },
            _ => unreachable!()
        };

        let paths = match config.pop().unwrap() {
            jValue::Array(v) => {
                let paths = v.into_iter()
                    .map(|path| match path {
                        jValue::String(s) => s,
                        _ => unreachable!(),
                    }).collect();
                
                ConfigurationEntry::ImHexPaths(paths)
            },
            _ => unreachable!(),
        };

        self.configuration.insert(CONFIG_IMHEX_BASE_FOLDERS.to_string(), paths);
        self.configuration.insert(CONFIG_IMHEX_PORT.to_string(), port);

        for document in self.document_map.iter() {
            let uri = Url::from_str(document.key()).unwrap();
            let rope = document.value();
            self.update_parse(uri, rope.clone(), &rope.to_string(), None).await
        }
    }

    fn execute_run_on_imhex(&self, command: ExecuteCommandParams) -> Result<Option<jValue>> {
        let current_file = command.arguments.get(0).ok_or(tower_lsp::jsonrpc::Error{
            code: ErrorCode::InvalidParams,
            message: "Command needs current file path as argument".to_string(),
            data: None
        })?;
        let current_file = current_file.as_array()
            .ok_or(tower_lsp::jsonrpc::Error{
                code: ErrorCode::InvalidParams,
                message: "Argument must ba an array".to_string(),
                data: None
            })?
            .get(1)
            .ok_or(tower_lsp::jsonrpc::Error{
                code: ErrorCode::InvalidParams,
                message: "Must have at least 2 arguments".to_string(),
                data: None
            })?
            .as_str()
            .ok_or(tower_lsp::jsonrpc::Error{
                code: ErrorCode::InvalidParams,
                message: "Second argument must be a string".to_string(),
                data: None
            })?;

        let mut current_document = None;
        for item in self.document_map.iter() {
            let (uri, document) = item.pair();
            let document = document.to_string();

            let mut decoded_uri = match urlencoding::decode(&uri) {
                Ok(decoded_url) => decoded_url.to_string(),
                _ => continue
            };

            let mut current_uri = Url::from_file_path(current_file)
                .map_err(|()| tower_lsp::jsonrpc::Error{
                    code: ErrorCode::ServerError(-8001),
                    message: "Unable to find current file".to_string(),
                    data: None
                })?
                .to_string();
            if decoded_uri == current_uri {
                current_document = Some(document);
                break;
            } else {
                #[cfg(target_os="windows")]
                {
                    // Check if changing the drive letter to lowercase they are the same
                    decoded_uri.replace_range(8..9, &decoded_uri[8..9].to_lowercase());
                    current_uri.replace_range(8..9, &current_uri[8..9].to_lowercase());
                    if current_uri == decoded_uri {
                        current_document = Some(document);
                        break;
                    }
                }
            }
        }
        match current_document {
            Some(current_document) => {
                let port = self.configuration.get(CONFIG_IMHEX_PORT);
                let port = port.map(|port| match port.value() {
                    ConfigurationEntry::Port(port) => *port,
                    _ => unreachable!()
                }).unwrap_or(DEFAULT_IMHEX_PORT);
                hexpat_language_server::imhex_connection::send_file(&current_document, port).map(|_| None)
            },
            None => Err(tower_lsp::jsonrpc::Error {
                code: ErrorCode::ServerError(-8000),
                message: "Unable to find current file".to_string(),
                data: None
            })
        }
    }
}

#[tokio::main]
async fn main() {
    eprintln!("{LSP_NAME} v0.2.4 (pre-release)");
    env_logger::init();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::build(|client| Backend {
        client,
        ast_map: DashMap::new(),
        document_map: DashMap::new(),
        semantic_token_map: DashMap::new(),
        configuration: DashMap::new()
    })
    .custom_method("custom/inlay_hint", Backend::inlay_hint)
    .finish();
    Server::new(stdin, stdout, socket).serve(service).await;
}

fn offset_to_position(offset: usize, rope: &Rope) -> Option<Position> {
    let line = rope.try_char_to_line(offset).ok()?;
    let first_char = rope.try_line_to_char(line).ok()?;
    let column = offset - first_char;
    Some(Position::new(line as u32, column as u32))
}
