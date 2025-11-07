let rec loop =
 fun () ->
  try
    let s = Readline.readline ~prompt:"clispy> " () in
    match s with
    | Some s ->
        Lexer.lex_line s |> Lexer.display_tokens;
        Readline.add_history s;
        loop ()
    | None -> ()
  with Sys.Break -> loop ()

let () =
  Readline.init ~catch_break:true ~program_name:"clispy" ();
  loop ()
