
{
  open Parser;;
  exception Lexical_error;;
}

rule token = parse
    [' ' '\t']  { token lexbuf }
  | "lambda"    { LAMBDA }
  | "L"         { LAMBDA }
  | "true"      { TRUE }
  | "false"     { FALSE }
  | "if"        { IF }
  | "then"      { THEN }
  | "else"      { ELSE }
  | "succ"      { SUCC }
  | "pred"      { PRED }
  | "iszero"    { ISZERO }
  | "let"       { LET }
  | "in"        { IN }
  | "Bool"      { BOOL }
  | "Nat"       { NAT }
  | "letrec"    { LETREC }
  | "concat"    { CONCAT }
  | "String"    { STRING }
  | '('         { LPAREN }
  | ')'         { RPAREN }
  | '{'         { LBRACE }
  | '}'         { RBRACE } 
  | ','         { COMMA }
  | '.'         { DOT }
  | '='         { EQ }
  | ':'         { COLON }
  | "->"        { ARROW }
  | ";;"        { SEMICOLON }

  | ['0'-'9']+  { INTV (int_of_string (Lexing.lexeme lexbuf)) }
  | ['a'-'z']['a'-'z' '_' '0'-'9']*
                { IDV (Lexing.lexeme lexbuf) }
  | ['A'-'Z']['A'-'Z' 'a'-'z' '_' '0'-'9']*
                { IDT (Lexing.lexeme lexbuf) }
  | '"' [^ '"' ';' '\n']*'"'
                {let s = Lexing.lexeme lexbuf in 
                STRINGV (String.sub s 1 (String.length s - 2))}
  | eof         { EOF }
  | _           { raise Lexical_error }

