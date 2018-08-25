open Printf

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
    | Ident str -> sprintf "IDENT(%s)" str
    | Int num -> sprintf "INT(%d)" num
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

module Lexer = struct
  type t = {
    input : string;
    pos : int;
    read : int;
    ch : char;
  }

  let to_string l = sprintf "pos=%d, read=%d, ch=%c" l.pos l.read l.ch

  let null = '\x00'

  let read l =
    let ch = if l.read >= String.length l.input
      then null
      else String.get l.input l.read in
    { input=l.input; pos=l.read; read=l.read + 1; ch=ch }

  let create prog = read { input=prog; pos=0; read=0; ch=(String.get prog 0) }

  let next l = match l.ch with
    | '=' -> (read l, Token.Assign)
    | ';' -> (read l, Token.Semicolon)
    | '(' -> (read l, Token.LParen)
    | ')' -> (read l, Token.RParen)
    | ',' -> (read l, Token.Comma)
    | '+' -> (read l, Token.Plus)
    | '{' -> (read l, Token.LBrace)
    | '}' -> (read l, Token.RBrace)
    | '\x00' -> (l, Token.Eof)
    | c -> (read l, Token.Ident (sprintf "%c" c))
end

let _ =
  printf "Monkey Programming Language :=)\n";
  let lex = Lexer.create "+()=;" in
  printf "lexer[0]=%s\n" (Lexer.to_string lex);
  let (lex, tok) = Lexer.next lex in
  printf "token[1]=%s\n" (Token.to_string tok);
  printf "lexer[1]=%s\n" (Lexer.to_string lex);
  let (lex, tok) = Lexer.next lex in
  printf "token[2]=%s\n" (Token.to_string tok);
  printf "lexer[2]=%s\n" (Lexer.to_string lex)
