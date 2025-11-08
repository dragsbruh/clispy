type token =
  | LParen
  | RParen
  | String of string
  | Int of int
  | Float of float
  | Symbol of string
  | Comment of string

let escape_symbol = function
  | 'n' -> '\n'
  | 't' -> '\t'
  | '\\' -> '\\'
  | '"' -> '"'
  | c -> c

let lex_line s =
  let rec aux pos =
    if pos >= String.length s then []
    else
      match s.[pos] with
      | ' ' | '\t' | '\n' -> aux (pos + 1)
      | '(' -> LParen :: aux (pos + 1)
      | ')' -> RParen :: aux (pos + 1)
      | ';' ->
          let resolve_comment start upto =
            Comment (String.sub s start (upto - start))
          in
          let rec iloop start pos =
            if pos >= String.length s then resolve_comment start pos :: []
            else
              match s.[pos] with
              | '\n' -> resolve_comment start pos :: aux (pos + 1)
              | _ -> iloop start (pos + 1)
          in
          iloop pos (pos + 1)
      | '"' ->
          let rec iloop start pos acc =
            if pos >= String.length s then
              failwith
                (Printf.sprintf
                   "Unclosed string literal (from position %d to %d)"
                   (start - 1) pos)
            else
              match s.[pos] with
              | '"' -> String acc :: aux (pos + 1)
              | '\\' ->
                  if pos + 1 >= String.length s then
                    failwith
                      (Printf.sprintf
                         "Unclosed string literal (from position %d to %d)"
                         (start - 1) pos)
                  else
                    iloop start (pos + 2)
                      (acc ^ String.make 1 (escape_symbol s.[pos + 1]))
              | c -> iloop start (pos + 1) (acc ^ String.make 1 c)
          in
          iloop (pos + 1) (pos + 1) ""
      | _ ->
          let resolve_symbol start upto =
            let s = String.sub s start (upto - start) in
            let numeric =
              if String.contains s '.' then
                match float_of_string_opt s with
                | Some f -> Some (Float f)
                | None -> None
              else
                match int_of_string_opt s with
                | Some i -> Some (Int i)
                | None -> None
            in
            match numeric with Some n -> n | None -> Symbol s
          in
          let rec iloop start pos =
            if pos >= String.length s then resolve_symbol start pos :: []
            else
              match s.[pos] with
              | ' ' | '\t' | '\n' | '(' | ')' | '"' ->
                  resolve_symbol start pos :: aux pos
              | _ -> iloop start (pos + 1)
          in
          iloop pos (pos + 1)
  in
  aux 0

let rec display_tokens = function
  | [] -> ()
  | x :: xs ->
      (match x with
      | Int i -> Printf.printf "Int %d\n" i
      | Float f -> Printf.printf "Float %f\n" f
      | LParen -> Printf.printf "LParen\n"
      | RParen -> Printf.printf "RParen\n"
      | String s -> Printf.printf "String len=%d %s\n" (String.length s) s
      | Symbol s -> Printf.printf "Symbol len=%d %s\n" (String.length s) s
      | Comment _ -> ());
      display_tokens xs;
      flush stdout
