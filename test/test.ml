open Printf
open Monkey
open Lexer
open Token

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
