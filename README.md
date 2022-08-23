Language Server Provider for ImHex's pattern language.

It is still buggy. For more stable syntax highlighting, see here: [(only for vscode)](https://github.com/Calcoph/vscode-hexpat)

## Installation guide
1. Get [rust](https://www.rust-lang.org/tools/install)
2. Compile  using `cargo build --release`
3. Take the binary at `target/release` (will be called __hexpat-language-server__ or __hexpat-language-server.exe__)
4. Move it somewhere in $PATH

## Extension guide
Available editors:
 * [VSCode](https://github.com/Calcoph/vscode-hexpat-lsp)

If you want to support this LSP in other editors here is what you have to take into account:

* Configuration called `hexpat-language-server.imhexBaseFolders`. It is an array of strings.
* New semantic token `bitfield`. It's recommended that it inherits from "struct".