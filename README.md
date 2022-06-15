# Dhall ASM

A Dhall DSL for writing assembly programs. The DSL was designed to support
multiple backends, specifically x86, ARM, and RISC-V. The only one currently
implemented is x86.

## Testing

```
dhall text --file ./test/x86_64.dhall | ./test-copile.dhall
```
