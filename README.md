# Programming Languages Project Collection

This is a collection of projects and other small programs, all written in OCaml and supporting languages. 

## Folder Structure

`assigns`: a folder that contains all code files for this project
- `assign1` - assign6: contain small coding exercises in the OCaml language, geared toward understanding functional programming and getting practice with special concepts such as folding, mapping, etc.
- `interp1` - interp3: contain implementations of self-defined programming languages of increasing complexity. Each interp folder has a structure similar to the following:
  - `spec.pdf`: contains the description for the project being done (pdf file)
  - `lib/interp1.ml`, `lib/interp2.ml`, `lib/interp3.ml`: contain the main code for the respective projects (OCaml file)
  - `lib/utils.ml`: contains type definitions and basic helper functions (OCaml file)
  - `lib/lexer.mll`: contains the lexing for each of the projects (ocamllex file)
  - `lib/parser.mly`: contains the parsing for each of the projects (Menhir file)
  - `test/test_interp1.ml`, `test/test_interp2.ml`, `test/test_interp3.ml`: contain testers of the code in the corresponding interp files (OCaml file)
  - `examples`: only in interp1 - contains example programs in the language defined in interp1 (folder)
  - `examples.ml`: only in interp3 - contains example programs in the language defined in interp3 (OCaml file)
