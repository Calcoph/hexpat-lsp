use tower_lsp::lsp_types::SemanticTokenType;

pub const LEGEND_TYPE: &[SemanticTokenType] = &[
    SemanticTokenType::FUNCTION, // 0
    SemanticTokenType::VARIABLE, // 1
    SemanticTokenType::STRING, // 2
    SemanticTokenType::COMMENT, // 3
    SemanticTokenType::NUMBER, // 4
    SemanticTokenType::KEYWORD, // 5
    SemanticTokenType::OPERATOR, // 6
    SemanticTokenType::PARAMETER, // 7
    SemanticTokenType::STRUCT, // 8
    SemanticTokenType::new("bitfield"), // 9
    SemanticTokenType::ENUM, // 10
    SemanticTokenType::NAMESPACE, // 11
    SemanticTokenType::TYPE, // 12
    SemanticTokenType::new("dollar"), // 13
];