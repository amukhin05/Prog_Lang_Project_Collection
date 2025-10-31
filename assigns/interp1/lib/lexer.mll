{
open Parser
}

let num = '-'? ['0'-'9']+
let var = ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']* 
let whitespace = [' ' '\t' '\n' '\r']+

rule read =
  parse
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "let" { LET }
  | "in" { IN }
  | "fun" { FUN }
  | "true" { TRUE }
  | "false" { FALSE }
  | "mod" { MOD }
  | "->" { APP }
  | ">=" { GTE }
  | "<=" { LTE }
  | "<>" { NEQ }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { MUL }
  | "/" { DIV }
  | "=" { EQ }
  | "<" { LT }
  | ">" { GT }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | "&&" { AND }
  | "||" { OR }
  | var { IDENT (Lexing.lexeme lexbuf) }
  | num { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | whitespace { read lexbuf }
  | eof { EOF }
