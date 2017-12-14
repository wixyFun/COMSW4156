/* Ocamlyacc parser for MicroC */

%{
open Ast
%}
/*Olesya:just listing all the tokens without any value attached to them */
%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK COMMA COLON
%token PLUS MINUS TIMES DIVIDE ASSIGN NOT
%token EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR

%token RETURN IF ELSE FOR WHILE INT BOOL STRING VOID FILE

/* Reference and Dereference */
%token OCTOTHORP PERCENT

/*Olesya:this tokens must have value attached to them*/

%token <float> FLOATLITERAL
%token <int> LITERAL
%token <string> STRING_SEQ
%token <string> ID
%token EOF


/*Olesya: the presedence of the tokens, bottom is the highest */
/*associativity of the token, left, right or no assoc*/
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

/*Olesya: specifies where your program starts */
%start program

/*Olesya: defines what the program is, token program with the value of <Ast.program>*/
%type <Ast.program> program

%%


/*rules that mirow the ast*/
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
  | VOID { Void }
  | STRING { String }
  | matrix1D_typ { $1 }
  | matrix1D_pointer_typ { $1 }
  | FILE { File }

matrix1D_typ:
    typ LBRACK LITERAL RBRACK %prec NOLBRACK  { Matrix1DType($1, $3) }

matrix1D_pointer_typ:
    typ LBRACK RBRACK %prec NOLBRACK { Matrix1DPointer($1)}

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
  /*| ID ASSIGN expr   { Assign($1, $3) }*/
  | expr ASSIGN expr                              { Assign($1, $3)  }
  | LPAREN expr RPAREN { $2 }
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
  | LBRACK matrix_literal RBRACK  { MatrixLiteral(List.rev $2) }
  | ID LBRACK expr  RBRACK %prec NOLBRACK         { Matrix1DAccess($1, $3)}
  | PERCENT ID                                    { Matrix1DReference($2)}
  | OCTOTHORP ID                                  { Dereference($2)}
  | PLUS PLUS ID                                  { PointerIncrement($3) }

primitives:
    LITERAL                                    { Literal($1)   }
  | FLOATLITERAL                               { FloatLiteral($1) }

matrix_literal:
    primitives                      { [$1] }
  | matrix_literal COMMA primitives { $3 :: $1 }


actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }
