# This document is absolutely not interesting, I just use it to take notes

The parser in this repository was originally the same as in commit **7362827** of the official pattern language in *(August 5th, 2022 2:41 PM)* (currently **3d192fb**)

Non implemented features: (comparing with **3d192fb**, *(October 10th, 2022 7:09 PM)*)
 * ~~sizeof(type) (example: sizeof(u8)) (praseFactor)~~
 * ~~templates (example: Struct<T> mystruct {}) including whatever the fuck is line 765 onward. New parser parseTemplateList~~
 * ~~returns (parseFunctionControlFlowStatement) (line 612)~~
 * ~~Reference (parseType)~~
 * ~~parseUsingDeclaration modification~~
 * ~~dont allow placement in parsemembervariable sometimes (line 888)~~ (i think this one was already done)
 * ~~dont allow placement in parsememberarrayvariable sometimes (line 915)~~ (i think this one was already done)
 * ~~Accept namespaced types in sizeof() (example: sizeof(types::MyType))~~
 * ~~enum ranges (line 1141)~~
 * ~~declare+assign in same line (line 1252)~~
 * ~~Global array (no @ required) (line 1294)~~

Non implemented features: (**3d192fb** <-> **951944d**, *(November 6th, 2022 6:30 PM)*)
 * ~~line 1250~~
 * ~~line 1303~~
 * ~~line 1321~~
 * ~~line 1355~~
 
Non implemented features:
 * try-catch
 * match statements
 * constants
 