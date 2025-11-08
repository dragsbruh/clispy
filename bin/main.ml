let rec eval env ast =
  match ast with
  | Parser.SymbolNode s -> (
      match Parser.Env.find_opt s env with
      | Some v -> v
      | None -> failwith (Printf.sprintf "unknown symbol %s" s))
  | Parser.ListNode (arg0 :: args) -> (
      match eval env arg0 with
      | Parser.FunctionNode (fn, lazy_eval) ->
          fn (env, List.map (fun x -> if lazy_eval then x else eval env x) args)
      | _ -> failwith "not a function")
  | v -> v

let num_fn int_op float_op =
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

let cmp_fn int_op float_op =
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

let num_conv_fn use_float_of_int =
  Parser.FunctionNode
    ( (function
      | _, [ Parser.IntNode i ] when use_float_of_int ->
          Parser.FloatNode (float_of_int i)
      | _, [ Parser.FloatNode f ] when not use_float_of_int ->
          Parser.IntNode (int_of_float f)
      | _ -> failwith "invalid num conversion"),
      false )

let fun_fn =
  Parser.FunctionNode
    ( (function
      | closure_env, [ args; body ] -> (
          match (args, body) with
          | Parser.ListNode args_names, anyexpr
            when List.for_all
                   (function Parser.SymbolNode _ -> true | _ -> false)
                   args_names ->
              Parser.FunctionNode
                ( (fun (args_env, args_vals) ->
                    if List.length args_names <> List.length args_vals then
                      failwith "invalid number of arguments"
                    else
                      let args_tups =
                        List.combine args_names
                          (List.map (eval args_env) args_vals)
                      in
                      let combined_env =
                        List.fold_left
                          (fun acc (k, v) ->
                            match k with
                            | Parser.SymbolNode s -> Parser.Env.add s v acc
                            | _ -> failwith "")
                          closure_env args_tups
                      in
                      eval combined_env anyexpr),
                  false )
          | _ -> failwith "invalid function arguments")
      | _ -> failwith "invalid function definition"),
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
      ("int", num_conv_fn false);
      ("float", num_conv_fn true);
      ("fun", fun_fn);
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
