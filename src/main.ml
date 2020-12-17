exception Quit of int

let parse lexbuf =
  Parser.main Lexer.token lexbuf

let parse_file filepath =
  let ichannel = open_in filepath in
  try parse (Lexing.from_channel ichannel) with
  | e -> close_in ichannel; raise e

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
