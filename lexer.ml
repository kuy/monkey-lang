open Token

module Lexer = struct
  type t = {
    input : string;
    pos : int;
    read : int;
    ch : char;
  }

  let to_string l = Printf.sprintf "pos=%d, read=%d, ch=%c" l.pos l.read l.ch

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
    | c -> (read l, Token.Ident (Printf.sprintf "%c" c))
end
