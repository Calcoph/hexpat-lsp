use std::{net::TcpStream, io::Write};

pub fn send_file(file: &str) -> Result<(), tower_lsp::jsonrpc::Error> {
    let mut connection = TcpStream::connect("localhost:51337").map_err(|err| tower_lsp::jsonrpc::Error {
        code: tower_lsp::jsonrpc::ErrorCode::ServerError(-8004),
        message: format!("Could not connect to ImHex, make sure you are using the most recent version and the firewall is not blocking it. {err}"),
        data: None,
    })?;

    let data = json::object! {
        "endpoint": "pattern_editor/set_code",
        "data": {
            "code": file
        }
    };

    data.write(&mut connection).map_err(|err| tower_lsp::jsonrpc::Error {
        code: tower_lsp::jsonrpc::ErrorCode::ServerError(-8004),
        message: format!("Could not communicate with ImHex. {err}"),
        data: None,
    })?;
    connection.write(&[b'\0']).map_err(|err| tower_lsp::jsonrpc::Error {
        code: tower_lsp::jsonrpc::ErrorCode::ServerError(-8004),
        message: format!("Could not communicate with ImHex. {err}"),
        data: None,
    })?;

    Ok(())
}
