type expr =
  | NilNode
  | IntNode of int
  | FloatNode of float
  | StringNode of string
  | SymbolNode of string
  | ListNode of expr list
  | CommentNode of string

exception UnclosedList of int

let rec display_expr expr =
  match expr with
  | NilNode -> Printf.printf "nil'nil; "
  | IntNode i -> Printf.printf "int'%d; " i
  | FloatNode f -> Printf.printf "float'%f; " f
  | StringNode s -> Printf.printf "string'{|%s|}; " s
  | SymbolNode s -> Printf.printf "symbol'%s; " s
  | ListNode xs ->
      Printf.printf "list[ ";
      List.iter display_expr xs;
      Printf.printf "]; ";
      flush stdout
  | CommentNode _ -> ()

let rec parse_tokens tokens =
  match tokens with
  | [] -> failwith "EOF"
  | Lexer.Int i :: rest -> (IntNode i, rest)
  | Lexer.Float f :: rest -> (FloatNode f, rest)
  | Lexer.String s :: rest -> (StringNode s, rest)
  | Lexer.Symbol s :: rest -> (SymbolNode s, rest)
  | Lexer.LParen :: rest ->
      let rec parse_list acc tokens =
        match tokens with
        | [] -> failwith "Unclosed paren"
        | Lexer.RParen :: rest -> (ListNode (List.rev acc), rest)
        | _ ->
            let expr, expr_rest = parse_tokens tokens in
            parse_list (expr :: acc) expr_rest
      in
      parse_list [] rest
  | Lexer.RParen :: _ -> failwith "Unbound closing paren"
  | Lexer.Comment c :: rest -> (CommentNode c, rest)
