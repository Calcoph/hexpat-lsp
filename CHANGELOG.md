# v0.2.4 (unreleased)
* Improved namespace completion
* Improved error recovery
* Multiline comments stop throwing errors
* Return statements get colored correctly

# v0.2.3
* Sections
* better support inside:
    * Template parameters
    * Array indices
    * Array sizes
* Fix `for` loops
* Fix char literals throwing errors
* Debugging a script makes it run on imhex (only on latest imhex builds)
* Settings change take effect without reloading server

# v0.2.2
* Go to definition
* Find references
* Should work with imhex v1.24.2

# v0.2.2-alpha2
* Fix the error when importing std/mem

# v0.2.2-alpha1
* Go to definition
* Find references

These features are experimental and don't work in all scenarios, specifically when namespaces are involved.

# v0.2.1
Brings most of the new features imhex has released since Version 0.2.0

* sizeof(u8) works, but addressof(u8) doesn't
* sizeof(myNamespace::type) works
* Templates!

```cpp
struct A<T> {
    T a;
};

A<u8> b @ 0x00;
```

* references (with the ref keyword)
* declare and assign in the same line (instead of place)
* global arrays

# v0.2.0
* The entire parser has been rewritten using another library (nom, instead of chumsky). Which means that it should be faster and faster releases might come in the future. But error messages and error recovery might be worse than before.

* The parser now more closely resembles the one of ImHex, so it should have way less false positives.

* Unions now have their own semantic token.

# v0.1.0
* Syntax highlighting
