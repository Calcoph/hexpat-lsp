The parser in this repository is the same as in commit **7362827** of the official pattern language in *(August 5th, 2022 2:41 PM)*


Non implemented features:
 * ~~sizeof(type) (example: sizeof(u8)) (praseFactor)~~
 * templates (example: Struct<T> mystruct {}) including whatever the fuck is line 765 onward. New parser parseTemplateList
 * ~~returns (parseFunctionControlFlowStatement) (line 612)~~
 * ~~Reference (parseType)~~
 * parseUsingDeclaration modification
 * dont allow placement in parsemembervariable sometimes (line 888)
 * dont allow placement in parsememberarrayvariable sometimes (line 915)
 * Accept namespaced types in sizeof() (example: sizeof(types::MyType))
 * enum ranges (line 1141)
 * ~~declare+assign in same line (line 1252)~~
 * ~~Global array (no @ required) (line 1294)~~
 