let rec loop =
 fun () ->
  try
    let s = Readline.readline ~prompt:"clispy> " () in
    match s with
    | Some s ->
        let expr, rest = Lexer.lex_line s |> Parser.parse_tokens in
        Parser.display_expr expr;
        if List.length rest > 0 then failwith "must";
        print_newline ();
        Readline.add_history s;
        loop ()
    | None -> ()
  with Sys.Break -> loop ()

let () =
  Readline.init ~catch_break:true ~program_name:"clispy" ();
  loop ()
