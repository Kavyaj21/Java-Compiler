%{
#include "parser.tab.h"
#include<iostream>
using namespace std;
#define yyterminate() return END;
%}
%option yylineno

DECIMAL [0-9]
BINARY [0-1]
HEXA [0-9A-Fa-f]
OCTA [0-7]
IDENTIFIER [a-zA-Z$_][a-zA-Z0-9$_]*
TEXT [^\\\"]
ESCAPE "\\b"|"\\s"|"\\t"|"\\n"|"\\f"|"\\r"|"\\\""|"\\\'"|"\\\\"|"\\0"|"\\1"|"\\2"|"\\3"|"\\4"|"\\5"|"\\6"|"\\7"
%%

[/][*][^*]*[*]+([^*/][^*]*[*]+)*[/]
"//"[^\n\r]*

"abstract" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="ABSTRACT";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return ABSTRACT; }

"continue" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="CONTINUE";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return CONTINUE; }

"for" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="FOR";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return FOR; }

"new" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="NEW";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return NEW; }

"switch" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="SWITCH";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return SWITCH; }

"assert" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="ASSERT";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return ASSERT; }

"default" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="DEFAULT";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return DEFAULT; }

"if" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="IF";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return IF; }

"package" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="PACKAGE";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return PACKAGE; }

"synchronized" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="SYNCHRONIZED";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return SYNCHRONIZED; }

"boolean" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="BOOLEAN";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return BOOLEAN; }

"do" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="DO";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return DO; }

"goto" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="GOTO";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return GOTO; }

"private" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="PRIVATE";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return PRIVATE; }

"this" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="THIS";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return THIS; }

"break" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="BREAK";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return BREAK; }

"double" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="DOUBLE";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return DOUBLE; }

"implements" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="IMPLEMENTS";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return IMPLEMENTS; }

"protected" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="PROTECTED";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return PROTECTED; }

"throw" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="THROW";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return THROW; }

"byte" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="BYTE";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return BYTE; }

"else" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="ELSE";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return ELSE; }

"import" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="IMPORT";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return IMPORT; }

"public" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="PUBLIC";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return PUBLIC; }

"throws" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="THROWS";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return THROWS; }

"case" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="CASE";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return CASE; }

"enum" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="ENUM";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return ENUM; }

"instanceof" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="INSTANCEOF";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return INSTANCEOF; }

"return" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="RETURN";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return RETURN; }

"transient" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="TRANSIENT";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return TRANSIENT; }

"catch" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="CATCH";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return CATCH; }

"extends" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="EXTENDS";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return EXTENDS; }

"int" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="INT";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return INT; }

"String" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="String";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return STRING; }


"short" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="SHORT";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return SHORT; }

"try" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="TRY";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return TRY; }

"char" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="CHAR";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return CHAR; }

"final" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="FINAL";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return FINAL; }

"interface" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="INTERFACE";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return INTERFACE; }

"static" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="STATIC";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return STATIC; }

"void" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="VOID";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return VOID; }

"class" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="CLASS";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return CLASS; }

"finally" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="FINALLY";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return FINALLY; }

"long" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="LONG";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return LONG; }

"strictfp" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="STRICTFP";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return STRICTFP; }

"volatile" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="VOLATILE";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return VOLATILE; }

"const" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="CONST";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return CONST; }

"float" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="FLOAT";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return FLOAT; }

"native" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="NATIVE";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return NATIVE; }

"super" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="SUPER";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return SUPER; }

"while" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="WHILE";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return WHILE; }

"_" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="_";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return _; }

"exports" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="EXPORTS";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return EXPORTS; }

"opens" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="OPENS";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return OPENS; }

"requires" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="REQUIRES";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return REQUIRES; }

"uses" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="USES";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return USES; }

"module" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="MODULE";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return MODULE; }

"permits" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="PERMITS";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return PERMITS; }

"sealed" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="SEALED";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return SEALED; }

"var" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="VAR";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return VAR; }

"provides" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="PROVIDES";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return PROVIDES; }

"to" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="TO";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return TO; }

"with" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="WITH";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return WITH; }

"open" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="OPEN";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return OPEN; }

"yield" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="YIELD";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return YIELD; }

"=" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="EQ";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return EQ; }

">" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="GT";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return GT; }

"<" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="LT";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return LT; }

"!" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="N";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return N; }

"~" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="TIL";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return TIL; }

"?" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="QM";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return QM; }

":" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="COLON";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return COLON; }

"->" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="RA";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return RA; }

"==" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="EE";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return EE; }

">=" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="GE";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return GE; }

"<=" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="LE";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return LE; }

"!=" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="NE";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return NE; }

"&&" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="AA";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return AA; }

"||" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="OO";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return OO; }

"++" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="PP";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return PP; }

"--" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="MM";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return MM; }

"+" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="P";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return P; }

"-" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="M";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return M; }

"*" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="MUL";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return MUL; }

"/" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="DIV";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return DIV; }

"&" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="A";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return A; }

"|" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="O";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return O; }

"^" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="X";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return X; }

"%" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="MOD";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return MOD; }

"<<" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="LL";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return LL; }

">>" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="GG";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return GG; }

">>>" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="GGG";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return GGG; }

"+=" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="PE";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return PE; }

"-=" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="ME";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return ME; }

"*=" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="MULE";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return MULE; }

"/=" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="DIVE";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return DIVE; }

"&=" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="AE";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return AE; }

"|=" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="OE";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return OE; }

"^=" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="XE";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return XE; }

"%=" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="MODE";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return MODE; }

"<<=" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="LLE";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return LLE; }

">>=" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="GGE";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return GGE; }

">>>=" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="GGGE";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return GGGE; }


"(" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="BO";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return BO; }

")" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="BC";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return BC; }

"{" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="CBO";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return CBO; }

"}" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="CBC";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return CBC; }

"[" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="SBO";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return SBO; }

"]" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="SBC";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return SBC; }

";" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="SC";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return SC; }

"," {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="C";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return C; }

"." {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="D";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return D; }

"..." {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="DDD";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return DDD; }

"@" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="ATR";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return ATR; }

"::" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="CC";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return CC; }


"0"[lL]?|[1-9]{DECIMAL}*([0-9_]*{DECIMAL}+)?[lL]? {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="IntegerLiteral";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return IntegerLiteral; }

("0x"|"0X"){HEXA}+([0-9a-fA-F_]*{HEXA}+)?[lL]? {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="IntegerLiteral";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return IntegerLiteral; }

("0b"|"0B"){BINARY}+([0-1_]*{BINARY}+)?[lL]? {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="IntegerLiteral";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return IntegerLiteral; }

"0"{OCTA}+([0-7_]*{OCTA}+)?[lL]? {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="IntegerLiteral";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return IntegerLiteral; }

({DECIMAL}+([0-9_]*{DECIMAL}+)?)"."({DECIMAL}+([0-9_]*{DECIMAL}+)?)?([eE][+-]?{DECIMAL}+([0-9_]*{DECIMAL}+)?)?[fFdD]? {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="FloatingPointLiteral";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return FloatingPointLiteral; }

({DECIMAL}+([0-9_]*{DECIMAL}+)?)?"."({DECIMAL}+([0-9_]*{DECIMAL}+)?)([eE][+-]?{DECIMAL}+([0-9_]*{DECIMAL}+)?)?[fFdD]? {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="FloatingPointLiteral";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return FloatingPointLiteral; }

("0x"|"0X")({HEXA}+([0-9a-fA-F_]*{HEXA}+)?)"."({HEXA}+([0-9a-fA-F_]*{HEXA}+)?)?([pP][+-]?{HEXA}+([0-9a-fA-F_]*{HEXA}+)?)?[fFdD]? {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="FloatingPointLiteral";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return FloatingPointLiteral; }

("0x"|"0X")({HEXA}+([0-9a-fA-F_]*{HEXA}+)?)?"."({HEXA}+([0-9a-fA-F_]*{HEXA}+)?)([pP][+-]?{HEXA}+([0-9a-fA-F_]*{HEXA}+)?)?[fFdD]? {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="FloatingPointLiteral";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return FloatingPointLiteral; }

"\'"[^\n\'\\]"\'" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="CharacterLiteral";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return CharacterLiteral; }

"\""[^\n\\\"]*({ESCAPE}[^\n\\\"]*)*[^\n\\\"]*"\"" {
        yylval.node=new astnode;
        string s=strdup(yytext);
        (yylval.node)->lexeme="\\"+s.substr(0, s.length() - 1)+"\\"+"\"";
        (yylval.node)->token="StringLiteral";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return StringLiteral; }

"\'""\\"{ESCAPE}"\'" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="CharacterLiteral";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return CharacterLiteral; }

"true"|"false" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="BooleanLiteral";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return BooleanLiteral; }

"null" {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="NullLiteral";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return NullLiteral; }

{IDENTIFIER}+ {
        yylval.node=new astnode;
        (yylval.node)->lexeme=strdup(yytext);
        (yylval.node)->token="Identifier";
        vector<astnode*>v;
        v.push_back(NULL);
        (yylval.node)->children=v;
        return Identifier; }

[ \t\f\n\r]+

"<>"

. {
    cout<<"ERROR in Line Number: "<<yylineno<<" in sequence: "<<yytext<<'\n';
}

<<EOF>> {return 0;}


%%

// yywrap() {}

int yywrap(){
    return 1;
}