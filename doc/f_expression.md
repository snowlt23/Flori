# F Expression

F Expression is new flexible meta syntax, it's looks like C or Go. (like Lisp and REBOL with arrangement)

## All kind

- comment `# ...`
- identifiers
  - FIdent : `name`
  - FOperator : `+`, apply priority by parsing rules.
  - FSymbol : generate by compiler internal, can use from typed macro.
- atoms
  - FIntLit : `9`
  - FFloatLit : `1.0`
  - FStrLit : `"Yukari"`
- containers
  - FSeq : `if (true) {}`
  - FArray : `[a, b, c]`
  - FList : `(a, b, c)`
  - FBlock : `{}`, with separated by newline("\n") or semicolon(";")
- reader macros `^`
