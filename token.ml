module Token = struct
  type t = | Illegal
           | Eof
           | Ident of string
           | Int of int
           | Assign
           | Plus
           | Comma
           | Semicolon
           | LParen
           | RParen
           | LBrace
           | RBrace
           | Function
           | Let

  let to_string token = match token with
    | Illegal -> "ILLEGAL"
    | Eof -> "EOF"
    | Ident str -> Printf.sprintf "IDENT(%s)" str
    | Int num -> Printf.sprintf "INT(%d)" num
    | Assign -> "ASSIGN"
    | Plus -> "PLUS"
    | Comma -> "COMMA"
    | Semicolon -> "SEMICOLON"
    | LParen -> "LPAREN"
    | RParen -> "RPAREN"
    | LBrace -> "LBRACE"
    | RBrace -> "RBRACE"
    | Function -> "FUNCTION"
    | Let -> "LET"
end
