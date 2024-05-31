grammar ProyectoFinal_Compiladores2024;
program
	: codigojava {System.out.println("Javascript");}
	| codigopascal {System.out.println("Pascal");}
	| codigopython {System.out.println("Python");}
	| codigoCPP {System.out.println("C++");}
	| codigohtml {System.out.println("HTML");}
	| codigosql {System.out.println("SQL");}
	;

codigojava
    : jsstatement*
    ;

jsstatement
    : jsvariableDeclaration
    | jsfunctionDeclaration
    | jsprintData
    | jsespecialfunctions
    | jsexpressionStatement
    | jsblock
    | GLOBALWHITESPACE
    | GLOBALCOMMENT
    | GLOBALMULTILINE_COMMENT
    ;

jsvariableDeclaration
    : ('var' | 'let' | 'const') GLOBALIDENTIFIER ('=' jsexpression)? ';'
    ;

jsfunctionDeclaration
    : 'function' GLOBALIDENTIFIER OPEN_PAREN jsparameterList? CLOSE_PAREN jsblock
    ;

jsparameterList
    : jsvalues (',' jsvalues)*
    ;

jsvalues
	: GLOBALIDENTIFIER
	| GLOBALSTRING
	| GLOBALNUMBER
	;

jsprintData
	: 'console.log' OPEN_PAREN jsexpression CLOSE_PAREN ';'
	;

jsespecialfunctions
	: GLOBALIDENTIFIER OPEN_PAREN jsparameterList? CLOSE_PAREN ';'
	;

jsexpressionStatement
    : jsexpression ';'
    ;

jsblock
	: OPEN_KEY ('return')? ( jsstatement )* CLOSE_KEY
	;

jsexpression
    : jsprimary (jsoperator jsprimary)*
    ;

jsprimary
    : GLOBALIDENTIFIER 
    | GLOBALNUMBER 
    | GLOBALSTRING
    | OPEN_PAREN jsexpression CLOSE_PAREN
    ;

jsoperator
    : '+' | '-' | '*' | '/' | '==' | '!=' | '<' | '>' | '<=' | '>=' | '&&' | '||' | '!'
    ;

//GRAMATICA PARA PASCAL------------------------------------------

codigopascal
	: 'program' GLOBALIDENTIFIER ';' pcblock '.'
	;

pcblock
    : pcdeclarations pccompoundStatement
    ;

pcdeclarations
    : (pcvariableDeclaration ';')*
    ;

pcvariableDeclaration
    : 'var' (GLOBALIDENTIFIER (',' GLOBALIDENTIFIER)*) ':' pctypeSpecifier
    | GLOBALIDENTIFIER (',' GLOBALIDENTIFIER)* ':'  pctypeSpecifier
    ;

pctypeSpecifier
    : 'integer' | 'real' | 'boolean'
    ;

pccompoundStatement
    : 'begin' pcstatementList 'end'
    ;

pcstatementList
    : pcstatement (';' pcstatement?)*
    ;

pcstatement
    : pcassignmentStatement
    | pcprocedureCall
    | pcifStatement
    | pcwhileStatement
    | pcwritelnStatement
    | pccompoundStatement
    ;

pcassignmentStatement
    : GLOBALIDENTIFIER ':=' pcexpression
    ;

pcprocedureCall
    : GLOBALIDENTIFIER OPEN_PAREN (pcexpression (',' pcexpression)*)? CLOSE_PAREN
    ;

pcifStatement
    : 'if' pcexpression 'then' pcstatement pcelseifStatement* pcelseStatement?
    ;
    
pcelseifStatement
	: 'else if' pcexpression 'then' pcstatement (pcelseifStatement)* pcelseStatement?
	;
	
pcelseStatement
	: 'else' pcstatement
	;

pcwhileStatement
    : 'while' pcexpression 'do' pcstatement
    ;

pcwritelnStatement
    : 'writeln' OPEN_PAREN pcexpression(',' pcexpression)* CLOSE_PAREN
    ;

pcexpression
    : pcsimpleExpression (pcrelationalOperator pcsimpleExpression)?
    ;

pcsimpleExpression
    : pcterm (pcadditiveOperator pcterm)*
    ;

pcterm
    : pcfactor (pcmultiplicativeOperator pcfactor)*
    ;

pcfactor
    : GLOBALIDENTIFIER
    | GLOBALNUMBER
    | GLOBALSIMPLESTRING
    | OPEN_PAREN pcexpression CLOSE_PAREN
    ;

pcrelationalOperator
    : '=' | '<>' | '<' | '<=' | '>' | '>='
    ;

pcadditiveOperator
    : '+' | '-'
    ;

pcmultiplicativeOperator
    : '*' | '/'
    ;

//GRAMATICA EN PYTHON----------------------------------------
codigopython
    : pyStatement* EOF
    ;

pyStatement
    : pySimpleStatement
    | pyCompoundStatement
    | GLOBALWHITESPACE
    | PYCOMMENT
    ;

pySimpleStatement
    : (pyExpressionStatement | pyAssignment | pyPrintln)
    ;

pyCompoundStatement
    : pyIfStatement
    | pyWhileStatement
    | pyFunctionDefinition
    ;

pyAssignment
    : GLOBALIDENTIFIER '=' pyExpression
    ;
    
pyPrintln
	: 'print' OPEN_PAREN pyPrintParameters CLOSE_PAREN
	| 'println' OPEN_PAREN pyPrintParameters CLOSE_PAREN
	;
	
pyPrintParameters
	: pyPrimary (',' pyPrimary)*
	;

pyExpressionStatement
    : pyExpression
    | pyAssignment
    ;

pyFunctionDefinition
    : 'def' GLOBALIDENTIFIER OPEN_PAREN pyParameterList? CLOSE_PAREN ':' pyBlock
    ;

pyParameterList
    : GLOBALIDENTIFIER (',' GLOBALIDENTIFIER)*
    ;

pyIfStatement
    : 'if' pyExpression ':' pyBlock (pyElifStatement)? (pyElseStatement)?
    ;
    
pyElifStatement
	: 'elif' pyExpression ':' pyBlock (pyElifStatement*)?
	;

pyElseStatement
	: 'else:' pyBlock
	;

pyWhileStatement
    : 'while' pyExpression ':' pyBlock 
    ;

pyBlock
    : pyStatement+
    ;

pyExpression
    : 'return'? pyPrimary (pyOperator pyPrimary)*
    ;

pyPrimary
    : GLOBALIDENTIFIER ( OPEN_PAREN pyParameterList? CLOSE_PAREN )?
    | GLOBALNUMBER
    | GLOBALSTRING
    | OPEN_PAREN pyExpression CLOSE_PAREN
    ;

pyOperator
    : '+' | '-' | '*' | '/' | '==' | '!=' | '<' | '>' | '<=' | '>=' | 'and' | 'or' | 'not'
    ;

//GRAMATICA C++

// Parser rules
codigoCPP
    : cppprocessincludes* (cppfunctionDeclaration)+
    ;
    
cppTypeContainer
	: 'class'
	| cpptypeSpecifier
	;
	
cppTypePermission
	: 'public'
	| 'private'
	| 'protected'
	;
cppprocessincludes
    : '#include' OPEN_ANGULAR GLOBALIDENTIFIER CLOSE_ANGULAR
    //| 'include<' GLOBALIDENTIFIER ('/'? GLOBALIDENTIFIER)* '>'
    ;

cppstatement
    : cppvariableDeclaration
    | cppPrintData
    | cppAssignment
    | cppfunctionDeclaration
    | cppifStatement
    | cppelseIfStatement
    | cppElseStatement
    | cppexpressionStatement
    | cppblock
    | cppreturnvalues
    | ';'
    ;

cppvariableDeclaration
    : cpptypeSpecifier GLOBALIDENTIFIER (',' GLOBALIDENTIFIER)*('=' cppexpression)? ';'
    ;

cppPrintData
	: 'std::cout <<' cppprimary ('<<' (cppendline | cppprimary))* ';'
	;
cppendline
	: 'std::endl'
	;

cppAssignment
	: 'std::cin >>' GLOBALIDENTIFIER ('>>' GLOBALIDENTIFIER)* ';'
	| GLOBALIDENTIFIER '=' cppexpression ';'
	;

cppfunctionDeclaration
    : cppTypeContainer GLOBALIDENTIFIER OPEN_PAREN cppparameterList? CLOSE_PAREN cppblock
    ;

cppparameterList
    : cppparameter (',' cppparameter)*
    ;

cppparameter
    : cpptypeSpecifier GLOBALIDENTIFIER
    ;
    
cppifStatement
	: 'if' OPEN_PAREN cppexpression CLOSE_PAREN cppblock (cppelseIfStatement)* (cppElseStatement)?
	; 
	
cppelseIfStatement
	: 'else if' OPEN_PAREN cppexpression CLOSE_PAREN cppblock
	;

cppElseStatement
	: 'else' cppblock
	;

cppexpressionStatement
    : cppexpression ';'
    ;

cppblock
    : OPEN_KEY cppstatement* CLOSE_KEY
    ;
    
cppreturnvalues
	: 'return' cppexpression ';'
	;

cppexpression
    : cppprimary (cppoperator cppprimary)*
    ;

cppprimary
    : GLOBALIDENTIFIER
    | GLOBALNUMBER
    | GLOBALSTRING
    | OPEN_PAREN cppexpression CLOSE_PAREN
    ;

cppoperator
    : '+' | '-' | '*' | '/' | '==' | '!=' | '<' | '>' | '<=' | '>=' | '&&' | '||' | '!'
    ;

cpptypeSpecifier
    : 'int' | 'float' | 'double' | 'char' | 'void'
    ;

//GRAMATICA EN HTML----------------------------------------
codigohtml
    : htmlDocType htmllabel EOF
    ;
    
htmlDocType
    : '<!DOCTYPE html>'
    ;
    
htmllabel
	: '<html>' htmlheaderlabel htmlbodylabel '</html>'
	;
    
    
htmlheaderlabel
	: '<head>' htmlelement* '</head>'
	;
	
htmlbodylabel
	: '<body>' htmlelement* '</body>'
	;  
    
htmlelement
    : (htmlopenTag | htmlcontent | htmlcloseTag)+
    //| htmlopenTag+ htmlcontent? htmlcloseTag+
    ;

htmlopenTag
    : OPEN_ANGULAR GLOBALIDENTIFIER htmlattribute* CLOSE_ANGULAR
    ;

htmlcloseTag
    : OPEN_ANGULAR '/' GLOBALIDENTIFIER CLOSE_ANGULAR
    ;

htmlcontent
    : GLOBALSTRING
    ;

htmlattribute
    : GLOBALIDENTIFIER '=' GLOBALSTRING
    ;

//GRAMATICA SQL-----------------------------------------------

codigosql
    : sqlstatement* EOF
    ;

sqlstatement
    : insertStatement
    | updateStatement
    | selectStatement
    | deleteStatement
    | sqldeclareStatement
    | sqlBeginEndBlock
    ;

insertStatement
    : 'INSERT' 'INTO' sqltableName '(' sqlcolumnName (',' sqlcolumnName)* ')' 'VALUES' '(' sqlvalue (',' sqlvalue)* ')' ';'
    ;

updateStatement
    : 'UPDATE' sqltableName 'SET' sqlcolumnName '=' sqlvalues ('WHERE' sqlcondition)? ';'
    ;

selectStatement
    : 'SELECT' sqlcolumnName (',' sqlcolumnName)* 'FROM' sqltableName ('WHERE' sqlcondition)? ';'
    ;

deleteStatement
    : 'DELETE' 'FROM' sqltableName ('WHERE' sqlcondition)? ';'
    ;

sqldeclareStatement
    : (plSqlDeclare {System.out.println("PL/SQL");}
    | tSqlDeclare) {System.out.println("T-SQL");}
    ;

sqlBeginEndBlock
    : 'BEGIN' sqlstatement* 'END' ';'
    ;

sqlcondition
    : sqlcolumnName sqlcompoundOperator sqlvalue
    ;

sqltableName
    : GLOBALIDENTIFIER
    ;

sqlcolumnName
    : GLOBALIDENTIFIER
    ;

sqlvalues
	: sqlvalueSet (sqlsimpleOperator sqlvalueSet)*
	;
	
sqlvalueSet
	: GLOBALSIMPLESTRING
	| GLOBALNUMBER
	| GLOBALIDENTIFIER
	;
	
sqlsimpleOperator
	: '+' | '-' | '*' | '/'
	;	

sqlcompoundOperator
	: '=' | '<' | '<=' | '>' | '>=' | '!='
	;	

sqlvalue
    : GLOBALSIMPLESTRING 
    | GLOBALNUMBER
    ;

plSqlDeclare
    : 'DECLARE' plSqlvariableDeclaration+ 
    ;

tSqlDeclare
    : 'DECLARE @' variableName sqlvariableType ('=' sqlvalue)? ';'
    ;

plSqlvariableDeclaration
    : variableName sqlvariableType ':=' sqlvalue ';'
    | variableName sqlvariableType ';'
    ;

variableName
	: GLOBALIDENTIFIER
	;

sqlvariableType
    : 'NUMBER' | 'VARCHAR2' '(' GLOBALNUMBER ')' | 'DATE'
    | 'INT' | 'VARCHAR' '(' GLOBALNUMBER ')' | 'DATETIME'
    | 'NVARCHAR' '(' GLOBALNUMBER ')'
    | 'DECIMAL' '(' GLOBALNUMBER ',' GLOBALNUMBER ')'
    ;

//IDENTIFIER
//    : [a-zA-Z_][a-zA-Z_0-9]*
//    ;

//STRING
//    : '\'' ( ~[\'\\] | '\\' . )* '\''
//    ;

//NUMBER
//    : [0-9]+ ('.' [0-9]+)?
//    ;

//WS
//    : [ \t\r\n]+ -> skip
//    ;


//simbolos globales
OPEN_PAREN
	: '('
	;
	
CLOSE_PAREN
	: ')'
	;
	
OPEN_KEY
	: '{'
	;
	
CLOSE_KEY
	: '}'
	;	
	
OPEN_ANGULAR
	: '<'
	;
	
CLOSE_ANGULAR
	: '>'
	;
	
OPEN_CORCHETE
	: '['
	;
	
CLOSE_CORCHETE
	: ']'
	;
	
GLOBALIDENTIFIER
    : [a-zA-Z_][a-zA-Z_0-9]*
    ;

GLOBALNUMBER
    : [0-9]+ ('.' [0-9]+)?
    ;

GLOBALSTRING
    : '"' (~["\\] | '\\' .)* '"'
    ;
    
GLOBALSIMPLESTRING
    : '\'' (~[\'\\] | '\\' .)* '\''
    ;
	
GLOBALWHITESPACE
    : [ \t\r\n]+ -> skip
    ;
//comentarios 
GLOBALCOMMENT
    : '//' ~[\r\n]* -> skip
    ;

GLOBALMULTILINE_COMMENT
    : '/*' .*? '*/' -> skip
    ;

PCCOMMENT
    : '{ ' .*? ' }' -> skip
    ;

//PYNEWLINE
//    : '\r'? '\n'
//    ;

//PYINDENT
//    : '    ' // Assuming 4 spaces for indentation
//    ;

//PYDEDENT
//    : '' // DEDENT is usually handled by ANTLR's indentation handling
//    ;

//PYWHITESPACE
//    : [ \t]+ -> skip
//    ;

PYCOMMENT
    : '# ' ~[\r\n]* -> skip
    ;
    
HTMLCOMMENT
	: '<!-- ' ~[\r\n]* '-->' -> skip
	;