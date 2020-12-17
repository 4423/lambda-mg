{
exception Lexical_error of string
open Parser
}

let whitespace = [' ' '\t' '\n' '\r']
let digit      = ['0'-'9']
let char       = ['\x20'-'\x7e']
let lower      = ['a'-'z']
let upper      = ['A'-'Z']
let alpha      = (lower | upper)
let var        = (lower | "_") (alpha | "_" | digit)*
let con        = (upper) (alpha | "_" | digit)*

rule token = parse
| whitespace+
    { token lexbuf }
| "(*"
    { comment lexbuf;
      token lexbuf }
| eof
    { EOF  }
| "+"
    { ADD }
| "-"
    { SUB }
| "*"
    { MUL }
| "/"
    { DIV }
| "="
    { EQ }
| "<>"
    { NE }
| "<"
    { LE }
| "<="
    { LE_EQ }
| ">"
    { GT }
| ">="
    { GT_EQ }
| "$"
    { DOLLAR }
| "("
    { LPAREN }
| ")"
    { RPAREN }
| ".<"
    { LCOD }
| ">."
    { RCOD }
| ".~"
    { ESC }
| "Runcode.run"
    { RUN }
| "run_module"
    { RUNMOD }
| "->"
    { SINGLE_ARROW }
| "=>"
    { DOUBLE_ARROW }
| ","
    { COMMA }
| ":"
    { COL }
| "::"
    { COL_COL }
| "."
    { DOT }
| ";"
    { SEM }
| ";;"
    { SEM_SEM }
| "()"
    { UNIT }
| "&&"
    { CONJ }
| "|"
    { BAR }
| "||"
    { DISJ }
| "true"
    { TRUE }
| "false"
    { FALSE }
| "not"
    { NOT }
| "fun"
    { FUN }
| "let"
    { LET }
| "rec"
    { REC }
| "in"
    { IN }
| "if"
    { IF }
| "then"
    { THEN }
| "else"
    { ELSE }
| "module"
    { MODULE }
| "struct"
    { STRUCTURE }
| "sig"
    { SIGNATURE }
| "end"
    { END }
| "val"
    { VAL }
| "type"
    { TYPE }
| "and"
    { AND }
| "match"
    { MATCH }
| "with"
    { WITH }
| "code"
    { CODE }
| "mcod"
    { MCOD }
| ".<<"
    { LMCOD }
| ">>."
    { RMCOD }
| digit+
    { INT (int_of_string @@ Lexing.lexeme lexbuf) }
| var
    { VAR (Lexing.lexeme lexbuf) }
| con
    { CON (Lexing.lexeme lexbuf) }
| "\"" (char+ as lexeme) "\""
    { STR (lexeme) }
| _
    { failwith (Printf.sprintf "unknown token %s" (Lexing.lexeme lexbuf)) }

and comment = parse
| eof
    { Printf.eprintf "warning: unterminated comment" }
| "(*"
    { comment lexbuf;
      comment lexbuf }
| "*)"
    { () }
| _
    { comment lexbuf }
