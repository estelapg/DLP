
%{
  open Lambda;;
%}

%token LAMBDA
%token TRUE
%token FALSE
%token IF
%token THEN
%token ELSE
%token SUCC
%token PRED
%token ISZERO
%token LET
%token IN
%token CONCAT 
%token BOOL
%token NAT
%token LETREC
%token STRING

%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token LBRACKET
%token RBRACKET
%token COMMA
%token DOT
%token EQ
%token COLON
%token ARROW
%token SEMICOLON
%token EOF
%token HEAD
%token TAIL
%token LIST
%token CONS
%token UNIT
%token NIL
%token ISEMPTY

%token <int> INTV
%token <string> IDV
%token <string> IDT
%token <string> STRINGV

%start s
%type <Lambda.command> s

%%

s :
   IDT EQ ty EOF
      { TBind ($1, $3) }
    |IDV EQ term EOF
      { Bind ($1, $3) }
    |term EOF
      { Eval $1 }
term :
    appTerm
      { $1 }
  | IF term THEN term ELSE term
      { TmIf ($2, $4, $6) }
  | LAMBDA IDV COLON ty DOT term
      { TmAbs ($2, $4, $6) }
  | LET IDV EQ term IN term
      { TmLetIn ($2, $4, $6) }
  | LETREC IDV COLON ty EQ term IN term
      { TmLetIn ($2, TmFix (TmAbs ($2, $4, $6)), $8) }

appTerm :
    pathTerm
      { $1 }
  | SUCC pathTerm
      { TmSucc $2 }
  | PRED pathTerm
      { TmPred $2 }
  | ISZERO pathTerm
      { TmIsZero $2 }
  | CONCAT pathTerm pathTerm
      {TmConcat ($2, $3) }   
   | CONS LBRACKET ty COLON pathTerm COMMA pathTerm RBRACKET 
    { TmCons($3,$5,$7)}
  | HEAD LBRACKET ty RBRACKET pathTerm 
      {TmHead($3,$5)}
  | TAIL LBRACKET ty RBRACKET pathTerm
      {TmTail($3,$5)}  
  | NIL LBRACKET ty RBRACKET
      {TmNil ($3)}
  | ISEMPTY LBRACKET ty RBRACKET pathTerm
       {TmisEmpty ($3,$5)}
  | appTerm pathTerm
      { TmApp ($1, $2) }

pathTerm :
    | pathTerm DOT INTV
        { TmProjection ($1, (string_of_int $3))}
    | pathTerm DOT IDV
        { TmProjection ($1, $3)}
    | atomicTerm
        { $1 }

atomicTerm :
    LPAREN term RPAREN
      { $2 }
  | TRUE
      { TmTrue }
  | FALSE
      { TmFalse }
  | IDV
      { TmVar $1 }
  | INTV
      { let rec f = function
            0 -> TmZero
          | n -> TmSucc (f (n-1))
        in f $1 }
 | STRINGV 
    {TmString $1}
| LBRACE tupleTerm RBRACE
    { TmTuple $2 }
| LBRACE recordTerm RBRACE
    { TmRecord $2}


ty :
    atomicTy
      { $1 }
  | atomicTy ARROW ty
      { TyArr ($1, $3) }

atomicTy :
    LPAREN ty RPAREN
      { $2 }
  | BOOL
      { TyBool }
  | NAT
      { TyNat }
  | STRING 
    { TyString}
  | LBRACE recty RBRACE
      { TyRecord $2 }
  | LIST LBRACKET ty RBRACKET
      { TyList $3 }

tupleTerm:
    | term 
        {[$1]}
    | term COMMA tupleTerm 
        {$1::$3}

recordTerm :
    | {[]}
    | nonemptyrec {$1}

nonemptyrec:
    | IDV EQ term {[$1,$3]}
    | IDV EQ term COMMA nonemptyrec {($1,$3)::$5}


recty: 
    | {[]}
    |nonemptyrecTY {$1}

nonemptyrecTY:
    | IDV COLON ty {[$1,$3]}
    | IDV COLON ty COMMA nonemptyrecTY {($1,$3)::$5}

