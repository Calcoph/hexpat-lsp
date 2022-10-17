The parser in this repository is the same as in commit **7362827** of the official pattern language in *(August 5th, 2022 2:41 PM)*


Non implemented features:
 * -sizeof(type) (example: sizeof(u8)) (praseFactor)-
 * templates (example: Struct<T> mystruct {}) including whatever the fuck is line 765 onward. New parser parseTemplateList
 * -returns (parseFunctionControlFlowStatement)- (line 612)
 * -Reference (parseType)-
 * new parser: parseTemplateList
 * parseUsingDeclaration modification
 * etc. . .
 * Accept namespaced types in sizeof() (example: sizeof(types::MyType))
