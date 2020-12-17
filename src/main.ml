exception Quit of int
exception Error of string

let parse lexbuf =
  Parser.main Lexer.token lexbuf

let parse_file filepath =
  let ichannel = open_in filepath in
  let lexbuf = Lexing.from_channel ichannel in
  try parse lexbuf with
  | Parsing.Parse_error ->
      close_in ichannel;
      let s = Printf.sprintf "Syntax error at char %d" (Lexing.lexeme_start lexbuf) in
      raise(Error s)
  | Lexer.Lexical_error msg ->
      close_in ichannel;
      let s = Printf.sprintf "Lexical error: %s, around character %d" msg (Lexing.lexeme_start lexbuf) in
      raise(Error s)

let usage () =
  Printf.sprintf "usage: %s [<option>] <file0> <file1> ...\n" (Sys.argv.(0))

let _ =
  let open Sys in
  set_signal sigint (Signal_handle (fun n -> raise (Quit n)));
  set_signal sigusr1 (Signal_handle (fun n -> raise (Quit n)));
  let filepaths = ref [] in
  Arg.parse
    []
    (fun path -> filepaths := !filepaths @ path :: [])
    (usage ());
  try
    match !filepaths with
    | [] -> print_string @@ usage ()
    | _  ->
      List.iter begin fun path ->
        print_endline @@ Pretty.f @@ Norm.g @@ Elab.f @@ Norm.f @@ parse_file path
      end !filepaths
  with
  | Quit n -> exit n
;;
