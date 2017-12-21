/* Ocamlyacc parser for miniMap */
/*
@Charis Lam
@Jamie Song
@Ryan DeCosmo
@Olessya Medvedeva
*/

%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK COMMA COLON
%token PLUS MINUS TIMES DIVIDE ASSIGN NOT
%token EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR
%token FILE INT BOOL STRING FLOAT VOID

%token RETURN IF ELSE FOR WHILE

/* Array related: Reference and Dereference and len */
%token OCTOTHORP PERCENT LEN

%token <float> FLOATLITERAL
%token <int> LITERAL
%token <string> STRING_SEQ
%token <string> ID
%token EOF

/* the presedence of the tokens, bottom is the highest */
/* associativity of the token, left, right or no assoc */
%nonassoc NOELSE
%nonassoc ELSE
%nonassoc NOLBRACK
%nonassoc LBRACK

%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%right NOT NEG

/* specifies where your program starts */
%start program

/* defines what the program is, token program with the value of <Ast.program>*/
%type <Ast.program> program

%%


/*the syntax : symbol … symbol { semantic-action }*/

program:
  decls EOF { $1 }

decls:
   /* nothing */ { [], [] }
 | decls vdecl { ($2 :: fst $1), snd $1 }
 | decls fdecl { fst $1, ($2 :: snd $1) }

fdecl:
   typ ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
     { { typ = $1;
	 fname = $2;
	 formals = $4;
	 locals = List.rev $7;
	 body = List.rev $8 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    typ ID                   { [($1,$2)] }
  | formal_list COMMA typ ID { ($3,$4) :: $1 }

typ:
    INT { Int }
  | BOOL { Bool }
  | FLOAT { Float }
  | VOID { Void }
  | STRING { String }
  | array_typ { $1 }
  | array_pointer_typ { $1 }
  | FILE { File }

array_typ:
    typ LBRACK LITERAL RBRACK %prec NOLBRACK  { ArrayType($1, $3) }

array_pointer_typ:
    typ LBRACK RBRACK %prec NOLBRACK { ArrayPointer($1)}

vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

vdecl:
   typ ID SEMI { ($1, $2) }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI { Expr $1 }
  | RETURN SEMI { Return Noexpr }
  | RETURN expr SEMI { Return $2 }
  | LBRACE stmt_list RBRACE { Block(List.rev $2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt
     { For($3, $5, $7, $9) }
  | WHILE LPAREN expr RPAREN stmt { While($3, $5) }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
  primitives       { $1 }
  | STRING_SEQ       { StringSeq($1) }
  | TRUE             { BoolLit(true) }
  | FALSE            { BoolLit(false) }
  | ID               { Id($1) }
  | expr PLUS   expr { Binop($1, Add,   $3) }
  | expr MINUS  expr { Binop($1, Sub,   $3) }
  | expr TIMES  expr { Binop($1, Mult,  $3) }
  | expr DIVIDE expr { Binop($1, Div,   $3) }
  | expr EQ     expr { Binop($1, Equal, $3) }
  | expr NEQ    expr { Binop($1, Neq,   $3) }
  | expr LT     expr { Binop($1, Less,  $3) }
  | expr LEQ    expr { Binop($1, Leq,   $3) }
  | expr GT     expr { Binop($1, Greater, $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3) }
  | expr AND    expr { Binop($1, And,   $3) }
  | expr OR     expr { Binop($1, Or,    $3) }
  | MINUS expr %prec NEG { Unop(Neg, $2) }
  | NOT expr         { Unop(Not, $2) }
  | expr ASSIGN expr                              { Assign($1, $3)  }
  | LPAREN expr RPAREN { $2 }
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
  | LBRACK array_literal RBRACK  { ArrayLiteral(List.rev $2) }
  | LEN LPAREN ID RPAREN                          { Len($3) }
  | ID LBRACK expr  RBRACK %prec NOLBRACK         { ArrayAccess($1, $3)}
  | PERCENT ID                                    { ArrayReference($2)}
  | OCTOTHORP ID                                  { Dereference($2)}
  | PLUS PLUS ID                                  { PointerIncrement($3) }

primitives:
    LITERAL                                    { Literal($1)   }
  | FLOATLITERAL                               { FloatLiteral($1) }

array_literal:
    primitives                      { [$1] }
  | array_literal COMMA primitives { $3 :: $1 }


actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }
