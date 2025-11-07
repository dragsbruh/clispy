module Env = Map.Make (String)

type expr =
  | NilNode
  | IntNode of int
  | FloatNode of float
  | StringNode of string
  | SymbolNode of string
  | ListNode of expr list
  | CommentNode of string
  | FunctionNode of (expr Env.t * expr list -> expr) * bool
  | BoolNode of bool

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
  | FunctionNode _ -> Printf.printf "fn; "
  | BoolNode b -> Printf.printf "bool'%b; " b

let rec parse_tokens tokens =
  match tokens with
  | [] -> failwith "EOF"
  | Lexer.Int i :: rest -> (IntNode i, rest)
  | Lexer.Float f :: rest -> (FloatNode f, rest)
  | Lexer.String s :: rest -> (StringNode s, rest)
  | Lexer.Symbol s :: rest ->
      ( (match s with
        | "true" -> BoolNode true
        | "false" -> BoolNode false
        | _ -> SymbolNode s),
        rest )
  | Lexer.LParen :: rest ->
      let rec parse_list acc tokens =
        match tokens with
        | [] -> failwith "unclosed paren"
        | Lexer.RParen :: rest -> (ListNode (List.rev acc), rest)
        | _ ->
            let expr, expr_rest = parse_tokens tokens in
            parse_list (expr :: acc) expr_rest
      in
      parse_list [] rest
  | Lexer.RParen :: _ -> failwith "unbound closing paren"
  | Lexer.Comment c :: rest -> (CommentNode c, rest)
