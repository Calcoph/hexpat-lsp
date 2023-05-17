use json::JsonValue;

const ENDPOINT_PATH_NAME: &str = "pattern_editor";

pub fn set_code(code: &str) -> JsonValue {
    const ENDPOINT_NAME: &str = "set_code";

    json::object! {
        "endpoint": format!("{ENDPOINT_PATH_NAME}/{ENDPOINT_NAME}"),
        "data": {
            "code": code
        }
    }
}