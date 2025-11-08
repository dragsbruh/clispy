let rec eval env ast =
  match ast with
  | Parser.SymbolNode s -> (
      match Parser.Env.find_opt s env with
      | Some v -> v
      | None -> failwith (Printf.sprintf "unknown symbol %s" s))
  | Parser.ExprNode (fn_expr, args) -> (
      match eval env fn_expr with
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

let str_op_fn str_op =
  Parser.FunctionNode
    ( (function
      | _, [ Parser.StringNode a; Parser.StringNode b ] ->
          StringNode (str_op a b)
      | _ -> failwith "invalid str operation"),
      false )

let cmp_fn int_op float_op str_op =
  Parser.FunctionNode
    ( (fun (env, args) ->
        match (env, str_op, args) with
        | _, _, [ Parser.IntNode a; Parser.IntNode b ] ->
            Parser.BoolNode (int_op a b)
        | _, _, [ Parser.FloatNode a; Parser.FloatNode b ] ->
            Parser.BoolNode (float_op a b)
        | _, _, [ Parser.IntNode a; Parser.FloatNode b ] ->
            Parser.BoolNode (float_op (float_of_int a) b)
        | _, _, [ Parser.FloatNode a; Parser.IntNode b ] ->
            Parser.BoolNode (float_op a (float_of_int b))
        | _, Some str_op, [ Parser.StringNode a; Parser.StringNode b ] ->
            Parser.BoolNode (str_op a b)
        | _, _, [ _; _ ] -> Parser.BoolNode false
        | _ -> failwith "expected two comparable arguments"),
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

let num_conv_fn to_float =
  Parser.FunctionNode
    ( (function
      | _, [ Parser.IntNode i ] when to_float ->
          Parser.FloatNode (float_of_int i)
      | _, [ Parser.FloatNode f ] when to_float -> Parser.FloatNode f
      | _, [ Parser.IntNode i ] when not to_float -> Parser.IntNode i
      | _, [ Parser.FloatNode f ] when not to_float ->
          Parser.IntNode (int_of_float f)
      | _, [ Parser.StringNode s ] when to_float ->
          Parser.FloatNode (float_of_string s)
      | _, [ Parser.StringNode s ] when not to_float ->
          Parser.IntNode (int_of_string s)
      | _ -> failwith "invalid num conversion"),
      false )

let str_conv_fn =
  Parser.FunctionNode
    ( (fun v ->
        Parser.StringNode
          (match v with
          | _, [ Parser.IntNode i ] -> string_of_int i
          | _, [ Parser.FloatNode f ] -> string_of_float f
          | _, [ Parser.BoolNode b ] -> string_of_bool b
          | _, [ Parser.StringNode s ] -> s
          | _, [ Parser.NilNode ] -> "()"
          | _ -> failwith "invalid str conversion")),
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

let let_fn =
  Parser.FunctionNode
    ( (function
      | env, [ name; expr; nest ] -> (
          match name with
          | Parser.SymbolNode s ->
              eval (Parser.Env.add s (eval env expr) env) nest
          | _ -> failwith "expected symbol for let binding name")
      | _ -> failwith "invalid let expression"),
      true )

let print_fn =
  Parser.FunctionNode
    ( (function
      | _, [ arg ] ->
          let rec print_val = function
            | Parser.IntNode i -> print_int i
            | Parser.FloatNode i -> print_float i
            | Parser.StringNode i -> print_string i
            | Parser.NilNode -> print_string "()"
            | Parser.ListNode l ->
                (* FIXME: this is impossible, add explicit lists *)
                print_string "( ";
                List.iter print_val l;
                print_string ")"
            | Parser.BoolNode b -> print_string (if b then "true" else "false")
            | _ -> failwith "cannot print type"
          in
          print_val arg;
          Parser.NilNode
      | _ -> failwith "invalid print expression"),
      false )

let list_fn = Parser.FunctionNode ((function _, args -> ListNode args), false)

let readline_fn =
  Parser.FunctionNode
    ( (function
      | _, [ Parser.StringNode s ] -> (
          match Readline.readline ~prompt:s () with
          | Some s -> StringNode s
          | None -> NilNode)
      | _ -> failwith "invalid readline expression"),
      false )

let float_op_fn op_fn =
  Parser.FunctionNode
    ( (function
      | _, [ Parser.FloatNode x ] -> Parser.FloatNode (op_fn x)
      | _, [ Parser.IntNode x ] -> Parser.FloatNode (op_fn (float_of_int x))
      | _ -> failwith "expected one numeric argument"),
      false )

let two_float_fn op_fn =
  Parser.FunctionNode
    ( (function
      | _, [ Parser.FloatNode a; Parser.FloatNode b ] ->
          Parser.FloatNode (op_fn a b)
      | _ -> failwith "expected two float arguments"),
      false )

let env =
  Parser.Env.of_list
    [
      ("add", num_fn ( + ) ( +. ));
      ("sub", num_fn ( - ) ( -. ));
      ("mult", num_fn ( * ) ( *. ));
      ("div", num_fn ( / ) ( /. ));
      ("mod", num_fn ( mod ) mod_float);
      ("gt", cmp_fn ( > ) ( > ) None);
      ("lt", cmp_fn ( < ) ( < ) None);
      ("eq", cmp_fn ( = ) ( = ) (Some ( = )));
      ("neq", cmp_fn ( <> ) ( <> ) (Some ( <> )));
      ("ge", cmp_fn ( >= ) ( >= ) None);
      ("le", cmp_fn ( <= ) ( <= ) None);
      ("if", if_fn);
      ("not", not_fn);
      ("and", and_fn);
      ("or", or_fn);
      ("str", str_conv_fn);
      ("int", num_conv_fn false);
      ("float", num_conv_fn true);
      ("cat", str_op_fn ( ^ ));
      ("fun", fun_fn);
      ("let", let_fn);
      ("print", print_fn);
      ("readline", readline_fn);
      ("list", list_fn);
      ("sqrt", float_op_fn sqrt);
      ("sin", float_op_fn sin);
      ("cos", float_op_fn cos);
      ("tan", float_op_fn tan);
      ("asin", float_op_fn asin);
      ("acos", float_op_fn acos);
      ("atan", float_op_fn atan);
      ("atan2", two_float_fn atan2);
      ("sinh", float_op_fn sinh);
      ("cosh", float_op_fn cosh);
      ("tanh", float_op_fn tanh);
      ("pow", two_float_fn ( ** ));
      ("exp", float_op_fn exp);
      ("deg", float_op_fn (fun x -> x *. 180.0 /. Float.pi));
      ("rad", float_op_fn (fun x -> x *. Float.pi /. 180.0));
      ("pi", Parser.FloatNode Float.pi);
      ("epsilon", Parser.FloatNode Float.epsilon);
    ]

let eval_code line =
  let rec eval_tokens tokens =
    let expr, rest = Parser.parse_tokens tokens in
    Parser.display_expr (eval env expr);
    print_newline ();
    if List.length rest > 0 then eval_tokens rest
  in
  try Lexer.lex_line line |> eval_tokens with End_of_file -> ()

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
  Readline.init ~catch_break:true ~program_name:"clispy"
    ~history_file:(Sys.getenv "HOME" ^ "/.clispy-history")
    ();

  if Array.length Sys.argv > 1 then eval_code (read_file Sys.argv.(1))
  else repl ()
