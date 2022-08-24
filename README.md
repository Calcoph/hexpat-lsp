Language Server Provider for ImHex's pattern language.

It is still buggy. For more stable syntax highlighting, see here: [(only for vscode)](https://github.com/Calcoph/vscode-hexpat)

## Installation
1. Get the server's binary.
    * See [releases](https://github.com/Calcoph/hexpat-lsp/releases).
    * [Compile](#compilation) it.
2. Move it somewhere in $PATH
### Compilation
1. Get [rust](https://www.rust-lang.org/tools/install)
2. Clone this repo `git clone https://github.com/Calcoph/hexpat-lsp.git`
    * Prefer to use a release tagged commit, since latest commit might not even compile.
3. `cd hexpat-lsp`
4. Compile  using `cargo build --release`
5. Take the binary at `target/release` (will be called __hexpat-language-server__ or __hexpat-language-server.exe__)
6. Move it somewhere in $PATH

## Extension guide
Available editors:
 * [VSCode](https://github.com/Calcoph/vscode-hexpat-lsp)

If you want to support this LSP in other editors here is what you have to take into account:

* Configuration called `hexpat-language-server.imhexBaseFolders`. It is an array of strings.
* New semantic token `bitfield`. It's recommended that it inherits from "struct".
* New semantic token `dollar`. It's recommended that it inherits from "variable".
