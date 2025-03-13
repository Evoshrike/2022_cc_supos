/* File parser.mly */

%{

let get_loc = Parsing.symbol_start_pos 

%}

%token <int> INT
%token ADD SUB MUL DIV SEMICOLON SKIP GTEQ ASSIGN DEREF
%token TRUE FALSE IF THEN ELSE WHILE DO SKIP
%token <string> VAR
%token LPAREN RPAREN
%token BEGIN END
%token EOF
%left ADD SUB ASSIGN GTEQ       /* lowest precedence */
%left MUL DIV         /* medium precedence */
%nonassoc UMINUS DEREF     /* highest precedence */


%start main
%type <Past.expr> simple_expr 
%type <Past.expr> expr 
%type <Past.expr list> exprlist
%type <Past.expr> main

%%
main:
	expr EOF                { $1 }
;
simple_expr:
| INT                                { Past.Integer (get_loc(), $1) }
| LPAREN expr RPAREN                 { $2 }
| UMINUS simple_expr                     { Past.UnaryOp(get_loc(), Past.NEG, $2) }
| SKIP                               { Past.Skip(get_loc()) }
| DEREF var				  			 { Past.UnaryOp(get_loc(),Past.DEREF $2) }

loc: 
| VAR                                { Past.Var (get_loc(), $1) }

var:
| VAR                                { Past.Var (get_loc(), $1) }

expr:
| simple_expr                        {  $1 }
| expr ADD expr                      { Past.Op(get_loc(), $1, Past.ADD, $3) }
| expr SUB expr                      { Past.Op(get_loc(), $1, Past.SUB, $3) }
| expr MUL expr                      { Past.Op(get_loc(), $1, Past.MUL, $3) }
| expr DIV expr                      { Past.Op(get_loc(), $1, Past.DIV, $3) }
| expr GTEQ expr                     { Past.Op(get_loc(), $1, Past.GTEQ, $3) }
| loc ASSIGN expr                    { Past.Op(get_loc(), $1, Past.ASSIGN, $3) }
| IF expr THEN expr ELSE expr        { Past.If(get_loc(), $2, $4, $6) }
| WHILE expr DO expr                 { Past.While(get_loc(), $2, $4) }
| BEGIN exprlist END                 { Past.Seq(get_loc(), $2) }
| DEREF var				  			 { Past.UnaryOp(get_loc(),Past.DEREF $2) }


exprlist:
|   expr                             { [$1] }
|   expr  SEMICOLON exprlist         { $1 :: $3  }


