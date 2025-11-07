let rec eval env ast =
  match ast with
  | Parser.SymbolNode s -> (
      match Parser.Env.find_opt s env with
      | Some v -> v
      | None -> failwith "unknown symbol")
  | Parser.ListNode (arg0 :: args) -> (
      match eval env arg0 with
      | Parser.FunctionNode (fn, lazy_eval) ->
          fn (env, List.map (fun x -> if lazy_eval then x else eval env x) args)
      | _ -> failwith "not a function")
  | v -> v

let num_fn (int_op : int -> int -> int) (float_op : float -> float -> float) =
  Parser.FunctionNode
    ( (function
      | _, [ Parser.IntNode a; Parser.IntNode b ] -> Parser.IntNode (int_op a b)
      | _, [ Parser.FloatNode a; Parser.FloatNode b ] ->
          Parser.FloatNode (float_op a b)
      | _, [ Parser.IntNode a; Parser.FloatNode b ] ->
          Parser.FloatNode (float_op (float_of_int a) b)
      | _, [ Parser.FloatNode a; Parser.IntNode b ] ->
          Parser.FloatNode (float_op a (float_of_int b))
      | _ -> failwith "expected two numeric arguments"),
      false )

let cmp_fn (int_op : int -> int -> bool) (float_op : float -> float -> bool) =
  Parser.FunctionNode
    ( (function
      | _, [ Parser.IntNode a; Parser.IntNode b ] ->
          Parser.BoolNode (int_op a b)
      | _, [ Parser.FloatNode a; Parser.FloatNode b ] ->
          Parser.BoolNode (float_op a b)
      | _, [ Parser.IntNode a; Parser.FloatNode b ] ->
          Parser.BoolNode (float_op (float_of_int a) b)
      | _, [ Parser.FloatNode a; Parser.IntNode b ] ->
          Parser.BoolNode (float_op a (float_of_int b))
      | _ -> failwith "expected two numeric arguments"),
      false )

let not_fn =
  Parser.FunctionNode
    ( (function
      | _, [ BoolNode b ] -> BoolNode (not b)
      | _ -> failwith "invalid not statement"),
      false )

let if_fn =
  Parser.FunctionNode
    ( (function
      | env, [ cond; truthy; falsy ] -> (
          match eval env cond with
          | BoolNode b -> eval env (if b then truthy else falsy)
          | _ -> failwith "expected boolean in cond")
      | _ -> failwith "invalid if statement"),
      true )

let and_fn =
  Parser.FunctionNode
    ( (function
      | env, [ left; right ] -> (
          match eval env left with
          | BoolNode false -> BoolNode false
          | BoolNode true -> (
              match eval env right with
              | BoolNode b -> BoolNode b
              | _ -> failwith "expected boolean in and")
          | _ -> failwith "expected boolean in and")
      | _ -> failwith "invalid if statement"),
      true )

let or_fn =
  Parser.FunctionNode
    ( (function
      | env, [ left; right ] -> (
          match eval env left with
          | BoolNode true -> BoolNode true
          | BoolNode false -> (
              match eval env right with
              | BoolNode b -> BoolNode b
              | _ -> failwith "expected boolean in or")
          | _ -> failwith "expected boolean in or")
      | _ -> failwith "invalid or statement"),
      true )

let env =
  Parser.Env.of_list
    [
      ("add", num_fn ( + ) ( +. ));
      ("sub", num_fn ( - ) ( -. ));
      ("mult", num_fn ( * ) ( *. ));
      ("div", num_fn ( / ) ( /. ));
      ("mod", num_fn ( mod ) mod_float);
      ("gt", cmp_fn ( > ) ( > ));
      ("lt", cmp_fn ( < ) ( < ));
      ("eq", cmp_fn ( = ) ( = ));
      ("neq", cmp_fn ( <> ) ( <> ));
      ("ge", cmp_fn ( >= ) ( >= ));
      ("le", cmp_fn ( <= ) ( <= ));
      ("if", if_fn);
      ("not", not_fn);
      ("and", and_fn);
      ("or", or_fn);
    ]

let eval_code line =
  let rec eval_tokens tokens =
    let expr, rest = Parser.parse_tokens tokens in
    Parser.display_expr (eval env expr);
    print_newline ();
    if List.length rest > 0 then eval_tokens rest
  in
  Lexer.lex_line line |> eval_tokens

let rec repl =
 fun () ->
  try
    match Readline.readline ~prompt:"clispy> " () with
    | Some line ->
        eval_code line;
        Readline.add_history line;
        repl ()
    | None -> ()
  with Sys.Break -> repl ()

let read_file filename =
  let chan = open_in filename in
  let len = in_channel_length chan in
  let content = really_input_string chan len in
  close_in chan;
  content

let () =
  if Array.length Sys.argv > 1 then eval_code (read_file Sys.argv.(1))
  else (
    Readline.init ~catch_break:true ~program_name:"clispy"
      ~history_file:(Sys.getenv "HOME" ^ "/.clispy-history")
      ();
    repl ())
