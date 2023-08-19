%{
#include <bits/stdc++.h>
#include "parser.tab.h"
#include "src/symtab.h"
using namespace std;

int yylex(void);
int yyerror (const char *s) ;
vector<pair<string,string> > buffer;
extern int yylineno;
extern char* yytext;
vector <string> prog;
int lineno = 1;
int tempno = 0;
int temp_str_no = 0;
int labelno = 0;
astnode *program;
ofstream symbolTable;			
ofstream tac;		
ofstream* fp ;

map< string, symtab_t* > symtab_top;
symtab_t *symtab =  init_symtab_top();	
int currscope = 0;
string fullscope = "0";
int curr_fcnscope = 0;
string cname="class";
string get_tempvar(){
	string str_t = "t";
	string ans =  str_t.append(to_string(tempno));
	tempno++;
	return ans;
}
string get_temp_strvar(){
	string str_t = "t_temporary_string";
	string ans =  str_t.append(to_string(temp_str_no));
	temp_str_no++;
	return ans;
}

string get_label(){
	string str_l = "label";
	string ans =  str_l.append(to_string(labelno));
	labelno++;
	return ans;
}

stack <string> if_endlabels;
stack <string> if_bwlabels;
stack<string>while_bwlabels;
stack<string>while_endlabels;
stack<string>do_bwlabels;
stack <string> for_initlabels;
stack <string> for_endlabels;
stack <string> for_blocklabels;
stack <string> for_postlabels;
stack <string> for_breaklabels;	//not used
stack <string> for_continuelabels;

string argumentlist_str;
stack <int> is_argument_list_empty;
int temp_var_counter = 0;

//splitting strings separated by comma
vector<string> split(const string &s, char delim) {
    stringstream ss(s);
    string item;
    vector<string> tokens;
    while (getline(ss, item, delim)) {
        tokens.push_back(item);
    }
    return tokens;
}


%}
%code requires {
	#include <bits/stdc++.h>
    #include "src/ast.h"
	// #include "src/symtab.h"
	using namespace std;
}
%union{
    // int i;
    // float d;
    // char s[1000];
    // char *c;
	// Data *data ;
	astnode *node;
}
%token<node> ABSTRACT CONTINUE FOR NEW SWITCH ASSERT DEFAULT IF PACKAGE SYNCHRONIZED STRING BOOLEAN DO GOTO PRIVATE THIS BREAK DOUBLE IMPLEMENTS PROTECTED THROW BYTE ELSE IMPORT PUBLIC THROWS CASE ENUM INSTANCEOF RETURN TRANSIENT CATCH EXTENDS INT SHORT TRY CHAR FINAL INTERFACE STATIC VOID CLASS FINALLY LONG STRICTFP VOLATILE CONST FLOAT NATIVE SUPER WHILE EXPORTS OPENS REQUIRES USES MODULE PERMITS SEALED VAR PROVIDES TO WITH OPEN RECORD TRANSITIVE YIELD _
%token<node> EQ GT LT N TIL QM COLON RA EE GE LE NE AA OO PP MM P M MUL DIV A O X MOD LL GG GGG PE ME MULE DIVE AE OE XE MODE LLE GGE GGGE BO BC CBO CBC SBO SBC SC C D DDD ATR CC Identifier
%token<node> IntegerLiteral FloatingPointLiteral BooleanLiteral CharacterLiteral StringLiteral NullLiteral 

%type<node>  FormalParameterLists Fname DimExprs Literal Type PrimitiveType NumericType IntegralType FloatingPointType ReferenceType ClassOrInterfaceType ClassType InterfaceType ArrayType Name SimpleName QualifiedName CompilationUnit ImportDeclarations 
%type<node> TypeDeclarations PackageDeclaration ImportDeclaration SingleTypeImportDeclaration TypeImportOnDemandDeclaration TypeDeclaration Modifiers Modifier ClassDeclaration Super Interfaces IfThenElseStatementNoShortIf
%type<node> InterfaceMemberDeclarations InterfaceTypeList ClassBody ClassBodyDeclarations ClassBodyDeclaration ClassMemberDeclaration FieldDeclaration VariableDeclarators VariableDeclarator VariableDeclaratorId  
%type<node> VariableInitializer MethodDeclaration MethodHeader MethodDeclarator FormalParameterList FormalParameter Throws ClassTypeList MethodBody StaticInitializer ConstructorDeclaration
%type<node> ConstructorDeclarator ConstructorBody ExplicitConstructorInvocation InterfaceDeclaration ExtendsInterfaces InterfaceBody InterfaceMemberDeclaration ConstantDeclaration AbstractMethodDeclaration
%type<node> ArrayInitializer VariableInitializerList Block BlockStatements BlockStatement LocalVariableDeclarationStatement LocalVariableDeclaration Statement StatementNoShortIf StatementWithoutTrailingSubstatement
%type<node> EmptyStatement LabeledStatement LabeledStatementNoShortIf ExpressionStatement StatementExpression IfThenStatement IfThenElseStatement SwitchBlock SwitchBlockStatementGroups
%type<node> SwitchBlockStatementGroup SwitchLabels SwitchLabel WhileStatement WhileStatementNoShortIf DoStatement ForStatement ForStatementNoShortIf ForInit ForUpdate StatementExpressionList
%type<node> BreakStatement ContinueStatement ReturnStatement ThrowStatement SynchronizedStatement TryStatement Catches CatchClause Finally Primary PrimaryNoNewArray ClassInstanceCreationExpression
%type<node> ArgumentList ArrayCreationExpression DimExpr Dims FieldAccess MethodInvocation ArrayAccess PostfixExpression PostIncrementExpression PostDecrementExpression UnaryExpression
%type<node> PreIncrementExpression PreDecrementExpression UnaryExpressionNotPlusMinus CastExpression MultiplicativeExpression AdditiveExpression ShiftExpression RelationalExpression
%type<node> EqualityExpression AndExpression ExclusiveOrExpression InclusiveOrExpression ConditionalAndExpression ConditionalOrExpression ConditionalExpression AssignmentExpression Assignment
%type<node> LeftHandSide AssignmentOperator Expression ConstantExpression SwitchStatement SYMTABE SYMTABS
%type<node> ClassName
%define parse.error verbose

%start Goal

%%
Goal : 
CompilationUnit{
	program = new astnode;
	program->token = "Goal";
	vector<astnode*> v;
	v.push_back($1);
	program->children = v; 
} ;

SYMTABS:
	{
		currscope++;
		fullscope = fullscope + "/" + to_string(currscope);
		curr_fcnscope = currscope;
		if(symtab_top.find(fullscope)==symtab_top.end())
		{
			symtab = new symtab_t ;
			symtab_top[fullscope]=symtab;
		}
		for(auto i:buffer)
		{
			symadd(i.first,i.second);
		}
		buffer.clear();
	}
	;
	
SYMTABE:
	{
		
		fullscope = fullscope.substr(0, fullscope.find_last_of("/") );
		
		if(symtab_top.find(fullscope)!=symtab_top.end())
		{
			
			symtab = symtab_top[fullscope];
		}
		else{
			symtab  =symtab_top["0"];
			fullscope = "0";
		}
	}
IFMARK1:
    {
        string tac,togo_label;
        togo_label=get_label();
        tac="if (t"+to_string(tempno-1)+"=1) goto "+togo_label;
        prog.push_back(tac);

        string topush_label=get_label();
        tac="goto "+topush_label;
        prog.push_back(tac);
        if_bwlabels.push(topush_label);

        tac=togo_label+":";
        prog.push_back(tac);


    }
;
IFEND1:
    {
        string tac,toget_label;
        toget_label=if_bwlabels.top();
        if_bwlabels.pop();
	    tac = toget_label+":";
	    prog.push_back(tac);
    }
;
IFEND2:
    {
        string tac,topush_label;
        topush_label=get_label();
        tac="goto "+topush_label;
        prog.push_back(tac);
        if_endlabels.push(topush_label);
    }
;
ELSEMARK1:
    {
        string tac,toget_label;
        toget_label=if_bwlabels.top();
        if_bwlabels.pop();
	    tac = toget_label+":";
	    prog.push_back(tac);
    }
;
ELSEEND1:
    {
        string tac,toget_label;
        toget_label=if_endlabels.top();
        if_endlabels.pop();
	    tac = toget_label+":";
	    prog.push_back(tac);
    }
;

WHILEMARK2:
    {
        string tac,topush_label;
        topush_label=get_label();
        tac=topush_label+":";
        prog.push_back(tac);
        while_bwlabels.push(topush_label);
   }
;

WHILEMARK1:
    {
        string tac,topush_label;
        topush_label=get_label();
        tac="if (t"+to_string(tempno-1)+"=0) goto "+topush_label;
        // while_exp.push("t"+to_string(tempno-1));
        prog.push_back(tac);
        while_endlabels.push(topush_label);
        // topush_label=get_label();
        // tac=topush_label+":";
        // prog.push_back(tac);
        // while_endlabels.push(topush_label);
    }
;
WHILEEND1:
    {
        string tac,toget_label,exp;
        toget_label=while_bwlabels.top();
        while_bwlabels.pop();
        // exp=while_exp.top();
        // while_exp.pop();
        // tac="if ("+exp+"=1) goto "+toget_label;
        tac="goto "+toget_label;
        prog.push_back(tac);
        toget_label=while_endlabels.top();
        while_endlabels.pop();
        tac=toget_label+":";
        prog.push_back(tac);
    }
;
DOMARK1:
    {
        string tac,topush_label;
        topush_label=get_label();
        do_bwlabels.push(topush_label);
        tac=topush_label+":";
        prog.push_back(tac);
    }
;
DOEND1:
    {
        string tac,toget_label;
        toget_label=do_bwlabels.top();
        do_bwlabels.pop();
        tac="if (t"+to_string(tempno-1)+"=1) goto "+toget_label;
        prog.push_back(tac);
    }
;

	/*
SYMTABS:
	{
		currscope++;
		fullscope = fullscope + "/" + to_string(currscope);
		curr_fcnscope = currscope;
        if(symtab_top.find(fullscope)==symtab_top.end())
		{
			symtab = new symtab_t ;
			symtab_top[fullscope]=symtab;
		}
        for(auto i:buffer){
            symadd(i.first,i.second);
        }
        buffer.clear();
	};

SYMTABE:
	{
        fullscope = fullscope.substr(0, fullscope.find_last_of("/") );
        if(symtab_top.find(fullscope)!=symtab_top.end()) {
            symtab = symtab_top[fullscope];
        }
        else {
            symtab=symtab_top["0"];
            fullscope="0";
        }
}; */
Literal  :
IntegerLiteral{
		$$=new astnode;
		$$->token="Literal";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->value=$1->lexeme;
		$$->tac = $1->lexeme;
		$$->flag=0;
	} |
FloatingPointLiteral{
		$$=new astnode;
		$$->token="Literal";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->value=$1->lexeme;
		$$->tac = $1->lexeme;
	} |
BooleanLiteral{
		$$=new astnode;
		$$->token="Literal";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->value=$1->lexeme;
		$$->tac = $1->lexeme;
	} | 
CharacterLiteral{
		$$=new astnode;
		$$->token="Literal";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->value=$1->lexeme;
		$$->tac = $1->lexeme;
	} | 
StringLiteral{
		$$=new astnode;
		$$->token="Literal";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->value=$1->lexeme;
		$$->tac = $1->lexeme;
	} | 
NullLiteral{
		$$=new astnode;
		$$->token="Literal";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->value=$1->lexeme;
		$$->tac = $1->lexeme;
	} ;

Type:

	PrimitiveType{
		$$=new astnode;
		$$->token="Type";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->type=$1->type;
		$$->tac = $1->tac;
	} | 
	ReferenceType{
		{
		$$=new astnode;
		$$->token="Type";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->type=$1->type;
		$$->tac = $1->tac;
	}
	} ;

PrimitiveType:

	NumericType{
		$$=new astnode;
		$$->token="PrimitiveType";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->type=$1->type;
		$$->tac = $1->tac;
	}
		|
	BOOLEAN{
		$$=new astnode;
		$$->token="PrimitiveType";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->type=$1->lexeme;
		$$->tac = $1->lexeme;
	} ;
	|
	STRING{
		$$=new astnode;
		$$->token="PrimitiveType";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->type=$1->lexeme;
		$$->tac = $1->lexeme;
	} ;

NumericType:

	IntegralType{
		$$=new astnode;
		$$->token="NumericType";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->type=$1->type;
		$$->tac = $1->tac;
	} | 
	FloatingPointType {
		$$=new astnode;
		$$->token="NumericType";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->type=$1->type;
		$$->tac = $1->tac;
	};

IntegralType : 
    BYTE{
		$$=new astnode;
		$$->token="IntegralType";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->type=$1->lexeme;
		$$->tac = $1->lexeme;
	} | 
    SHORT{
		$$=new astnode;
		$$->token="IntegralType";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->type=$1->lexeme;
		$$->tac = $1->lexeme;

	} |
    INT{
		$$=new astnode;
		$$->token="IntegralType";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->type=$1->lexeme;
		$$->tac = $1->lexeme;

	} | 
    LONG{
		$$=new astnode;
		$$->token="IntegralType";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->type=$1->lexeme;
		$$->tac = $1->lexeme;

	} | 
    CHAR {
		$$=new astnode;
		$$->token="IntegralType";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->type=$1->lexeme;
		$$->tac = $1->lexeme;

	};

FloatingPointType  :
    FLOAT{
		$$=new astnode;
		$$->token="FloatingPointType";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->type=$1->lexeme;
		$$->tac = $1->lexeme;

	} |
    DOUBLE{
		$$=new astnode;
		$$->token="FloatingPointType";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->type=$1->lexeme;
		$$->tac = $1->lexeme;

	} ;


ReferenceType:

	ClassOrInterfaceType{
		$$=new astnode;
		$$->token="ReferenceType";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->type=$1->value;
		$$->tac = $1->tac;

	} |
	ArrayType{
		$$=new astnode;
		$$->token="ReferenceType";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->type=$1->value;
	
	} ;

ClassOrInterfaceType:

	Name{
		$$=new astnode;
		$$->token="ClassOrInterfaceType";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->value=$1->value;
		$$->tac=$1->tac;
	} ;

ClassType:

	ClassOrInterfaceType{
		$$=new astnode;
		$$->token="ClassType";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->value=$1->value;
		$$->tac = $1->tac;

	} ;

InterfaceType:

	ClassOrInterfaceType{
		$$=new astnode;
		$$->token="InterfaceType";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->value=$1->value;
		$$->tac = $1->tac;

	} ;

ArrayType:

	PrimitiveType SBO SBC{
		$$=new astnode;
		$$->token="ArrayType";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		$$->children=v;
		$$->value=$1->value;
		$$->tac = $1->tac;

	} |
	Name SBO SBC{
		$$=new astnode;
		$$->token="ArrayType";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		$$->children=v;
		$$->value=$1->value;
		$$->tac = $1->tac;

	} |
	ArrayType SBO SBC{
		$$=new astnode;
		$$->token="ArrayType";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		$$->children=v;
		$$->value=$1->value;
		$$->tac = $1->tac;

	} ;


Name:

	SimpleName{
		$$=new astnode;
		$$->token="Name";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->value=$1->value;
		$$->tac = $1->tac;

	}  |
	QualifiedName{
		$$=new astnode;
		$$->token="Name";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->value=$1->value;
		$$->tac = $1->tac;

	}  ;

SimpleName:

	Identifier{
		$$=new astnode;
		$$->token="SimpleName";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->value=$1->lexeme;
		$$->tac = $1->lexeme;

	}  ; 

QualifiedName:

	Name D Identifier{
		$$=new astnode;
		$$->token="QualifiedName";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		$$->children=v;
		$$->value=$1->value+$2->lexeme+$3->lexeme;
		$$->tac = $1->tac;

	} ;

CompilationUnit : 

    PackageDeclaration ImportDeclarations TypeDeclarations{
		$$=new astnode;
		$$->token="CompilationUnit";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		$$->children=v;
		$$->tac = $1->tac;

	} | 
    ImportDeclarations TypeDeclarations{
		$$=new astnode;
		$$->token="CompilationUnit";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		$$->children=v;
		$$->tac = $1->tac;

	} | 
    PackageDeclaration TypeDeclarations{
		$$=new astnode;
		$$->token="CompilationUnit";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		$$->children=v;
		$$->tac = $1->tac;

	} | 
    PackageDeclaration ImportDeclarations{
		$$=new astnode;
		$$->token="CompilationUnit";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		$$->children=v;
		$$->tac = $1->tac;

	} |
    PackageDeclaration{
		$$=new astnode;
		$$->token="CompilationUnit";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->value=$1->value;
		$$->tac = $1->tac;

	} | 
    ImportDeclarations{
		$$=new astnode;
		$$->token="CompilationUnit";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->value=$1->value;
		$$->tac = $1->tac;

	} | 
    TypeDeclarations {
		$$=new astnode;
		$$->token="CompilationUnit";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->value=$1->value;
		$$->tac = $1->tac;

	} ;

ImportDeclarations:

	ImportDeclaration {
		$$=new astnode;
		$$->token="ImportDeclarations";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->value=$1->value;
		$$->tac = $1->tac;

	}|
	ImportDeclarations ImportDeclaration{
		$$=new astnode;
		$$->token="ImportDeclarations";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		$$->children=v;
		$$->tac = $2->tac;

	};

TypeDeclarations:

	TypeDeclaration{
		$$=new astnode;
		$$->token="TypeDeclarations";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->value=$1->value;
		$$->tac = $1->tac;

	} |
	TypeDeclarations TypeDeclaration{
		$$=new astnode;
		$$->token="TypeDeclarations";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		$$->children=v;
		$$->value=$1->value + $2->value;
		$$->tac = $2->tac;

	} ;

PackageDeclaration:

	PACKAGE Name SC{
		$$=new astnode;
		$$->token="PackageDeclaration";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		$$->children=v;
		$$->tac = $1->lexeme;

	}  ;

ImportDeclaration:

	SingleTypeImportDeclaration{
		$$=new astnode;
		$$->token="ImportDeclaration";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->value=$1->value;
		$$->tac = $1->tac;
	}  |
	TypeImportOnDemandDeclaration{
		$$=new astnode;
		$$->token="ImportDeclaration";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->value=$1->value;
		$$->tac = $1->tac;

	} ;

SingleTypeImportDeclaration:

	IMPORT Name SC {
		$$=new astnode;
		$$->token="SingleTypeImportDeclaration";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		$$->children=v;
		$$->tac = $1->lexeme;
	};
    
TypeImportOnDemandDeclaration:

	IMPORT Name D MUL SC{
		$$=new astnode;
		$$->token="TypeImportOnDemandDeclaration";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		v.push_back($4);
		v.push_back($5);
		$$->children=v;
		$$->tac = $1->lexeme;

	} ; 

TypeDeclaration:

	ClassDeclaration{
		$$=new astnode;
		$$->token="TypeDeclaration";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->value=$1->value;
		$$->tac = $1->tac;

	} |
	InterfaceDeclaration{
		$$=new astnode;
		$$->token="TypeDeclaration";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->value=$1->value;
		$$->tac = $1->tac;

	} |
	SC{
		$$=new astnode;
		$$->token="TypeDeclaration";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->value=$1->lexeme;
		$$->tac = $1->tac;

	} ;

Modifiers:

	Modifier{
		$$=new astnode;
		$$->token="Modifiers";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->value=$1->value;
		$$->tac = $1->tac;

	} | 
	Modifiers Modifier{
		$$=new astnode;
		$$->token="Modifiers";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		$$->children=v;
		$$->tac = $1->tac;

	} ;

Modifier : 

    PUBLIC{
		$$=new astnode;
		$$->token="Modifier";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->value=$1->lexeme;
		$$->tac = $1->lexeme;
	} | 
    PROTECTED {
		$$=new astnode;
		$$->token="Modifier";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->value=$1->lexeme;
		$$->tac = $1->lexeme;
	}| 
    PRIVATE {
		$$=new astnode;
		$$->token="Modifier";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->value=$1->lexeme;
		$$->tac = $1->lexeme;
	}| 
    STATIC {
		$$=new astnode;
		$$->token="Modifier";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->value=$1->lexeme;
		$$->tac = $1->lexeme;
	}| 
    ABSTRACT{
		$$=new astnode;
		$$->token="Modifier";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->value=$1->lexeme;
		$$->tac = $1->lexeme;
	} | 
    FINAL{
		$$=new astnode;
		$$->token="Modifier";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->value=$1->lexeme;
		$$->tac = $1->lexeme;
	} | 
    NATIVE{
		$$=new astnode;
		$$->token="Modifier";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->value=$1->lexeme;
		$$->tac = $1->lexeme;
	} |
    SYNCHRONIZED{
		$$=new astnode;
		$$->token="Modifier";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->value=$1->lexeme;
		$$->tac = $1->lexeme;
	} | 
    TRANSIENT{
		$$=new astnode;
		$$->token="Modifier";
		vector<astnode*>v;
		v.push_back($1);
		$$->value=$1->lexeme;
		$$->children=v;
		$$->tac = $1->lexeme;
	} | 
    VOLATILE{
		$$=new astnode;
		$$->token="Modifier";
		vector<astnode*>v;
		v.push_back($1);
		$$->value=$1->lexeme;
		$$->children=v;
		$$->tac = $1->lexeme;
	} ; 

ClassName:
Identifier {
	$$=new astnode;
		$$->token="ClassName";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->lexeme=$1->lexeme;
		cname=$1->lexeme;
}

ClassDeclaration :  

    Modifiers CLASS ClassName Super Interfaces ClassBody{
		$$=new astnode;
		$$->token="ClassDeclaration";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		v.push_back($4);
		v.push_back($5);
		v.push_back($6);
		$$->children=v;
		symadd($3->lexeme,$2->lexeme);
		$$->tac = $1->tac;
	} |
    CLASS ClassName Super Interfaces ClassBody{
		$$=new astnode;
		$$->token="ClassDeclaration";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		v.push_back($4);
		v.push_back($5);
		$$->children=v;
		symadd($2->lexeme,$1->lexeme);
		$$->tac = $1->lexeme;
	}  | 
    Modifiers CLASS ClassName Interfaces ClassBody {
		$$=new astnode;
		$$->token="ClassDeclaration";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		v.push_back($4);
		v.push_back($5);
		$$->children=v;
		symadd($3->lexeme,$2->lexeme);
		$$->tac = $1->tac;
	} | 
    Modifiers CLASS ClassName Super ClassBody {
		$$=new astnode;
		$$->token="ClassDeclaration";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		v.push_back($4);
		v.push_back($5);
		$$->children=v;
		symadd($3->lexeme,$2->lexeme);
		$$->tac = $1->tac;
	} | 
    CLASS ClassName Interfaces ClassBody {
		$$=new astnode;
		$$->token="ClassDeclaration";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		v.push_back($4);
		$$->children=v;
		symadd($2->lexeme,$1->lexeme);
		$$->tac = $1->lexeme;
	} | 
    CLASS ClassName Super ClassBody {
		$$=new astnode;
		$$->token="ClassDeclaration";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		v.push_back($4);
		$$->children=v;
		symadd($2->lexeme,$1->lexeme);
		$$->tac = $1->lexeme;
	} | 
    Modifiers CLASS ClassName ClassBody {
		$$=new astnode;
		$$->token="ClassDeclaration";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		v.push_back($4);
		$$->children=v;
		symadd($3->lexeme,$2->lexeme);
		$$->tac = $1->tac;
	} | 
    CLASS ClassName ClassBody {
		$$=new astnode;
		$$->token="ClassDeclaration";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		$$->children=v;
		symadd($2->lexeme,$1->lexeme);
		$$->tac = $1->lexeme;
	} ;

Super:

	EXTENDS ClassType {
		$$=new astnode;
		$$->token="Super";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		$$->children=v;
		$$->tac = $1->lexeme;
	} ;

Interfaces:

	IMPLEMENTS InterfaceTypeList{
		$$=new astnode;
		$$->token="Interfaces";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		$$->children=v;
		$$->tac = $1->lexeme;

	}  ;

InterfaceTypeList:

	InterfaceType{
		$$=new astnode;
		$$->token="InterfaceTypeList";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->value=$1->value;
		$$->tac = $1->tac;
	} | 
	InterfaceTypeList C InterfaceType{
		$$=new astnode;
		$$->token="InterfaceTypeList";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		$$->children=v;
		$$->tac = $1->tac;
	} ;

ClassBody:

	CBO SYMTABS ClassBodyDeclarations CBC SYMTABE {
		$$=new astnode;
		$$->token="ClassBody";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($3);
		v.push_back($4);
		$$->children=v;
		$$->tac = $1->tac;
	}| 
    CBO CBC{
		$$=new astnode;
		$$->token="ClassBody";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		$$->children=v;
		$$->tac = $1->tac;
	} ;

ClassBodyDeclarations:

	ClassBodyDeclaration {
		$$=new astnode;
		$$->token="ClassBodyDeclarations";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->value=$1->value;
		$$->tac = $1->tac;
	}| 
	ClassBodyDeclarations ClassBodyDeclaration {
		$$=new astnode;
		$$->token="ClassBodyDeclarations";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		$$->children=v;
		$$->value=$1->value+$2->value;
		$$->tac = $1->tac;
	};

ClassBodyDeclaration:

	ClassMemberDeclaration {
		$$=new astnode;
		$$->token="ClassBodyDeclaration";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->value=$1->value;
		$$->tac = $1->tac;
	}| 
	StaticInitializer {
		$$=new astnode;
		$$->token="ClassBodyDeclaration";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->value=$1->value;
		$$->tac = $1->tac;
	}| 
	ConstructorDeclaration {
		$$=new astnode;
		$$->token="ClassBodyDeclaration";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->value=$1->value;
		$$->tac = $1->tac;
	};

ClassMemberDeclaration:

	FieldDeclaration{
		$$=new astnode;
		$$->token="ClassMemberDeclaration";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->value=$1->value;
		$$->tac = $1->tac;
	} |
	MethodDeclaration{
		$$=new astnode;
		$$->token="ClassMemberDeclaration";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->value=$1->value;
		$$->tac = $1->tac;
	} ;

FieldDeclaration:

	Modifiers Type VariableDeclarators SC{
		$$=new astnode;
		$$->token="FieldDeclaration";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		v.push_back($4);
		$$->children=v;
		string s=$3->value;
		vector <string> tokens=split(s,',');
		symadd_list(tokens,$2->type);
		$$->tac = $1->tac;
	} | 
    Type VariableDeclarators SC{
		$$=new astnode;
		$$->token="FieldDeclaration";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		$$->children=v;
		string s=$2->value;
		vector <string> tokens=split(s,',');
		symadd_list(tokens,$1->type);
		$$->tac = $1->tac;
	} ;

VariableDeclarators:

	VariableDeclarator{
		$$=new astnode;
		$$->token="VariableDeclarators";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->value=$1->value;
		$$->tac = $1->tac;
	} |
	VariableDeclarators C VariableDeclarator{
		$$=new astnode;
		$$->token="VariableDeclarators";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		$$->children=v;
		$$->value=$1->value + $2->lexeme + $3->value;
		$$->tac = $1->tac;
	} ;

VariableDeclarator:

	VariableDeclaratorId{
		$$=new astnode;
		$$->token="VariableDeclarator";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->value=$1->value;
		$$->tac = $1->tac;
	} |
	VariableDeclaratorId EQ VariableInitializer{
		$$=new astnode;
		$$->token="VariableDeclarator";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		$$->children=v;
		$$->value=$1->value;
		$$->tac = $1->tac;
		string tac;
		tac=$1->tac+" = "+$3->tac;
		prog.push_back(tac);
	} ;

VariableDeclaratorId:

	Identifier{
		$$=new astnode;
		$$->token="VariableDeclaratorsId";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->value=$1->lexeme;
		$$->tac = $1->lexeme;
	} | 
	VariableDeclaratorId SBO SBC{
		$$=new astnode;
		$$->token="VariableDeclaratorId";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		$$->children=v;
		$$->value=$1->value;
		$$->tac = $1->tac;
	} ;

VariableInitializer:

	Expression{
		$$=new astnode;
		$$->token="VariableInitializer";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
	} | 
	ArrayInitializer{
		$$=new astnode;
		$$->token="VariableInitializer";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
	} ;


MethodDeclaration:

	MethodHeader MethodBody{
		$$=new astnode;
		$$->token="MethodDeclaration";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		$$->children=v;
		$$->tac = $1->tac;
	} ;

MethodHeader:

	Modifiers Type MethodDeclarator Throws{
		$$=new astnode;
		$$->token="MethodHeader";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		v.push_back($4);
		$$->children=v;
		symadd($3->name,$2->type,$3->value);
		$$->tac = $1->tac;

	} |
    Type MethodDeclarator Throws{
		$$=new astnode;
		$$->token="MethodHeader";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		$$->children=v;
		symadd($2->name,$1->type,$2->value);
		$$->tac = $1->tac;
	} |
    Modifiers Type MethodDeclarator{
		$$=new astnode;
		$$->token="MethodHeader";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		$$->children=v;
		symadd($3->name,$2->type,$3->value);
		$$->tac = $1->tac;
	}  | 
    Type MethodDeclarator{
		$$=new astnode;
		$$->token="MethodHeader";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		$$->children=v;
		symadd($2->name,$1->type,$2->value);
		$$->tac = $1->tac;
	} | 
	Modifiers VOID MethodDeclarator Throws{
		$$=new astnode;
		$$->token="MethodHeader";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		v.push_back($4);
		$$->children=v;
		symadd($3->name,$2->lexeme,$3->value);
		$$->tac = $1->tac;
	} |
    VOID MethodDeclarator Throws {
		$$=new astnode;
		$$->token="MethodHeader";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		$$->children=v;
		symadd($2->name,$1->lexeme,$2->value);
		$$->tac = $1->tac;
	}| 
    Modifiers VOID MethodDeclarator{
		$$=new astnode;
		$$->token="MethodHeader";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		$$->children=v;
		symadd($3->name,$2->lexeme,$3->value);
		$$->tac = $1->tac;
	} |
    VOID MethodDeclarator {
		$$=new astnode;
		$$->token="MethodHeader";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		$$->children=v;
		symadd($2->name,$1->lexeme,$2->value);
		$$->tac = $1->lexeme;
	};  

MethodDeclarator :

    Fname BO FormalParameterLists BC{
		$$=new astnode;
		$$->token="MethodDeclarator";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		v.push_back($4);
		$$->children=v;
		$$->name=$1->name;
		$$->value=$3->value;
		$$->tac = $1->lexeme;
	} | 
    Fname BO BC{
		$$=new astnode;
		$$->token="MethodDeclarator";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		$$->children=v;
		$$->name=$1->name;
		$$->value="args";
		$$->tac = $1->lexeme;
	} | 
    MethodDeclarator SBO SBC{
		$$=new astnode;
		$$->token="MethodDeclarator";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		$$->children=v;
		$$->name=$1->name;
		$$->value=$1->value;
		$$->tac = $1->lexeme;
	} ;
Fname:
    Identifier{
        $$=new astnode;
        $$->token="FunctionName";
        vector<astnode*>v;
        v.push_back($1);
        $$->children=v;
        $$->name=$1->lexeme;
        // $$->value=$1->type;
        // //cout<<"in"<<$$->token;
        string tac;
        tac="define "+cname+"."+$1->lexeme+":";
        prog.push_back(tac);
		tac="beginfunc ";
		prog.push_back(tac);
tac="REF = popparam";

        prog.push_back(tac);
tac = "return addr = SP+4";
        prog.push_back(tac);
tac = "push BP ";
        prog.push_back(tac);
tac = "BP = SP";
        prog.push_back(tac);
    }
;
FormalParameterLists:
    FormalParameterList{
        $$=new astnode;
        $$->token="FormalParameterLists";
        vector<astnode*>v;
        v.push_back($1);
        $$->children=v;
		$$->value=$1->value;
        // //cout<<"in"<<$$->token;
        string s=$1->tac,tac;
        vector<string>token=split(s,',');
        tac="args_num "+to_string(token.size());
        prog.push_back(tac);
        for(int i=token.size()-1;i>=0;i--) 
		{
            tac="pop "+token[i];
            prog.push_back(tac);
        }
    }
;
FormalParameterList:

	FormalParameter{
		$$=new astnode;
		$$->token="FormalParameterList";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->value=$1->value;
		$$->tac = $1->tac;
	} |
	FormalParameterList C FormalParameter{
		$$=new astnode;
		$$->token="FormalParameterList";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		$$->children=v;
		$$->value=$1->value+$2->lexeme+$3->value;
		$$->tac = $1->tac + "," + $3->tac;
	} ;

FormalParameter:

	Type VariableDeclaratorId{
		$$=new astnode;
		$$->token="FormalParameter";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		$$->children=v;
		$$->value=$2->value;
		$$->type=$1->type;
		$$->tac = $1->tac;
		
	} ;

Throws:

	THROWS ClassTypeList{
		$$=new astnode;
		$$->token="Throws";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		$$->children=v;
		$$->tac = $1->lexeme;
	} ;

ClassTypeList:

	ClassType{
		$$=new astnode;
		$$->token="ClassTypeList";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
	} | 
	ClassTypeList C ClassType{
		$$=new astnode;
		$$->token="ClassTypeList";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		$$->children=v;
		$$->tac = $1->tac;
	} ;

MethodBody:

	Block{
		$$=new astnode;
		$$->token="MethodBody";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
	} |
	SC{
		$$=new astnode;
		$$->token="MethodBody";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->lexeme;
	} ; 

StaticInitializer:

	STATIC Block{
		$$=new astnode;
		$$->token="StaticInitializer";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		$$->children=v;
		$$->type=$1->lexeme;
		$$->tac = $1->lexeme;
	} ;

ConstructorDeclaration:

	Modifiers ConstructorDeclarator Throws ConstructorBody{
		$$=new astnode;
		$$->token="ConstructorDeclaration";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		v.push_back($4);
		$$->children=v;
		$$->tac = $1->tac;
	} | 
    ConstructorDeclarator Throws ConstructorBody{
		$$=new astnode;
		$$->token="ConstructorDeclaration";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		$$->children=v;
		$$->tac = $1->tac;
	} | 
    Modifiers ConstructorDeclarator ConstructorBody{
		$$=new astnode;
		$$->token="ConstructorDeclaration";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		$$->children=v;
		$$->tac = $1->tac;
	} | 
    ConstructorDeclarator ConstructorBody{
		$$=new astnode;
		$$->token="ConstructorDeclaration";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		$$->children=v;
		$$->tac = $1->tac;
	} ; 

ConstructorDeclarator:

	SimpleName BO FormalParameterList BC{
		$$=new astnode;
		$$->token="ConstructorDeclarator";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		v.push_back($4);
		$$->children=v;
		$$->tac = $1->tac;
	} | 
    SimpleName BO BC{
		$$=new astnode;
		$$->token="ConstructorDeclarator";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		$$->children=v;
		$$->tac = $1->tac;
	} ;

ConstructorBody:

	CBO SYMTABS ExplicitConstructorInvocation BlockStatements CBC SYMTABE{
		$$=new astnode;
		$$->token="ConstructorBody";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($3);
		v.push_back($4);
		v.push_back($5);
		$$->children=v;
		$$->tac = $3->tac;
	} | 
    CBO SYMTABS BlockStatements CBC SYMTABE{
		$$=new astnode;
		$$->token="ConstructorBody";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($3);
		v.push_back($4);
		$$->children=v;
		$$->tac = $3->tac;
	}  | 
    CBO SYMTABS ExplicitConstructorInvocation CBC SYMTABE{
		$$=new astnode;
		$$->token="ConstructorBody";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($3);
		v.push_back($4);
		$$->children=v;
		$$->tac = $3->tac;
	}  | 
    CBO CBC{
		$$=new astnode;
		$$->token="ConstructorBody";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		$$->children=v;
		$$->tac = $1->tac;
	}  ;

ExplicitConstructorInvocation:

	THIS BO ArgumentList BC SC{
		$$=new astnode;
		$$->token="ExplicitConstructorInvocation";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		v.push_back($4);
		v.push_back($5);
		$$->children=v;
		$$->tac = $1->tac;
	}  | 
    THIS BO BC SC{
		$$=new astnode;
		$$->token="ExplicitConstructorInvocation";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		v.push_back($4);
		$$->children=v;
		$$->tac = $1->tac;
	} |
	SUPER BO ArgumentList BC SC{
		$$=new astnode;
		$$->token="ExplicitConstructorInvocation";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		v.push_back($4);
		v.push_back($5);
		$$->children=v;
		$$->tac = $1->lexeme;
	} | 
    SUPER BO BC SC{
		$$=new astnode;
		$$->token="ExplicitConstructorInvocation";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		v.push_back($4);
		$$->children=v;
		$$->tac = $1->lexeme;
	} ; 

InterfaceDeclaration:

	Modifiers INTERFACE Identifier ExtendsInterfaces InterfaceBody{
		$$=new astnode;
		$$->token="InterfaceDeclaration";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		v.push_back($4);
		v.push_back($5);
		$$->children=v;
		$$->tac = $1->tac;
	} |
    INTERFACE Identifier ExtendsInterfaces InterfaceBody{
		$$=new astnode;
		$$->token="InterfaceDeclaration";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		v.push_back($4);
		$$->children=v;
		$$->tac = $1->lexeme;
	} | 
    Modifiers INTERFACE Identifier InterfaceBody{
		$$=new astnode;
		$$->token="InterfaceDeclaration";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		v.push_back($4);
		$$->children=v;
		$$->tac = $1->tac;
	} |
    INTERFACE Identifier InterfaceBody{
		$$=new astnode;
		$$->token="InterfaceDeclaration";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		$$->children=v;
		$$->tac = $1->lexeme;
	} ;

ExtendsInterfaces:

	EXTENDS InterfaceType{
		$$=new astnode;
		$$->token="ExtendsInterfaces";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		$$->children=v;
		$$->tac = $1->lexeme;
	} |
	ExtendsInterfaces C InterfaceType{
		$$=new astnode;
		$$->token="ExtendsInterfaces";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		$$->children=v;
		$$->tac = $1->lexeme;
	} ;

InterfaceBody:

	CBO SYMTABS InterfaceMemberDeclarations CBC SYMTABE{
		$$=new astnode;
		$$->token="InterfaceBody";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($3);
		v.push_back($4);
		$$->children=v;
		$$->tac = $3->tac;
	} |
    CBO CBC{
		$$=new astnode;
		$$->token="InterfaceBody";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		$$->children=v;
		$$->tac = $1->tac;
	} ;

InterfaceMemberDeclarations:

	InterfaceMemberDeclaration {
		$$=new astnode;
		$$->token="InterfaceMemberDeclarations";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
	} |
	InterfaceMemberDeclarations InterfaceMemberDeclaration{
		$$=new astnode;
		$$->token="InterfaceMemberDeclarations";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		$$->children=v;
		$$->tac = $1->tac;
	}  ;

InterfaceMemberDeclaration:

	ConstantDeclaration{
		$$=new astnode;
		$$->token="InterfaceMemberDeclaration";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
	}  |
	AbstractMethodDeclaration{
		$$=new astnode;
		$$->token="InterfaceMemberDeclaration";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
	}  ;

ConstantDeclaration:

	FieldDeclaration {
		$$=new astnode;
		$$->token="ConstantDeclaration";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
	} ;

AbstractMethodDeclaration:

	MethodHeader SC{
		$$=new astnode;
		$$->token="AbstractMethodDeclaration";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		$$->children=v;
		$$->tac = $1->tac;
	}  ;


ArrayInitializer:

	CBO SYMTABS VariableInitializerList C CBC SYMTABE{
		$$=new astnode;
		$$->token="ArrayInitializer";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($3);
		v.push_back($4);
		v.push_back($5);
		$$->children=v;
		$$->tac = $3->tac;
	} | 
    CBO SYMTABS C CBC SYMTABE{
		$$=new astnode;
		$$->token="ArrayInitializer";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($3);
		v.push_back($4);
		$$->children=v;
		$$->tac = $1->tac;
	} | 
    CBO SYMTABS VariableInitializerList CBC SYMTABE{
		$$=new astnode;
		$$->token="ArrayInitializer";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($3);
		v.push_back($4);
		$$->children=v;
		$$->tac = $1->tac;
	} | 
    CBO CBC{
		$$=new astnode;
		$$->token="ArrayInitializer";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		$$->children=v;
		$$->tac = $1->tac;
	} ;

VariableInitializerList:

	VariableInitializer{
		$$=new astnode;
		$$->token="VariableInitializerList";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
	} |
	VariableInitializerList C VariableInitializer{
		$$=new astnode;
		$$->token="VariableInitializerList";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		$$->children=v;
		$$->tac = $1->tac;
	} ;

Block:

	CBO SYMTABS BlockStatements CBC SYMTABE{
		$$=new astnode;
		$$->token="Block";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($3);
		v.push_back($4);
		$$->children=v;
		$$->tac = $1->tac;
	} | 
    CBO CBC{
		$$=new astnode;
		$$->token="Block";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		$$->children=v;
		$$->tac = $1->tac;
	} ;

BlockStatements:

	BlockStatement{
		$$=new astnode;
		$$->token="BlockStatements";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
	} |
	BlockStatements BlockStatement{
		$$=new astnode;
		$$->token="BlockStatements";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		$$->children=v;
		$$->tac = $2->tac;
	} ;

BlockStatement:

	LocalVariableDeclarationStatement{
		$$=new astnode;
		$$->token="BlockStatement";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
	} |
	Statement{
		$$=new astnode;
		$$->token="BlockStatement";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
	} ;

LocalVariableDeclarationStatement:

	LocalVariableDeclaration SC{
		$$=new astnode;
		$$->token="LocalVariableDeclarationStatement";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		$$->children=v;
		$$->tac = $1->tac;
	} ;

LocalVariableDeclaration:

	Type VariableDeclarators{
		$$=new astnode;
		$$->token="LocalVariableDeclarationStatement";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		$$->children=v;
		string s=$2->value;
		vector <string> tokens=split(s,',');
		symadd_list(tokens,$1->type);
		$$->tac = $1->tac;
	} ;

Statement:

	StatementWithoutTrailingSubstatement{
		$$=new astnode;
		$$->token="Statement";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
	} |
	LabeledStatement{
		$$=new astnode;
		$$->token="Statement";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
	} | 
	IfThenStatement{
		$$=new astnode;
		$$->token="Statement";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
	} |
	IfThenElseStatement{
		$$=new astnode;
		$$->token="Statement";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
	} | 
	WhileStatement{
		$$=new astnode;
		$$->token="Statement";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
	} |
	ForStatement{
		$$=new astnode;
		$$->token="Statement";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
	} ;

StatementNoShortIf:

	StatementWithoutTrailingSubstatement{
		$$=new astnode;
		$$->token="StatementNoShortIf";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
	} |
	LabeledStatementNoShortIf{
		$$=new astnode;
		$$->token="StatementNoShortIf";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
	} |
	IfThenElseStatementNoShortIf{
		$$=new astnode;
		$$->token="StatementNoShortIf";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
	} |
	WhileStatementNoShortIf{
		$$=new astnode;
		$$->token="StatementNoShortIf";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
	} |
	ForStatementNoShortIf{
		$$=new astnode;
		$$->token="StatementNoShortIf";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
	} ;

StatementWithoutTrailingSubstatement:

	Block{
		$$=new astnode;
		$$->token="StatementWithoutTrailingSubstatement";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
	} |
	EmptyStatement{
		$$=new astnode;
		$$->token="StatementWithoutTrailingSubstatement";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
	} | 
	ExpressionStatement{
		$$=new astnode;
		$$->token="StatementWithoutTrailingSubstatement";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
	} | 
	SwitchStatement{
		$$=new astnode;
		$$->token="StatementWithoutTrailingSubstatement";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
	} | 
	DoStatement{
		$$=new astnode;
		$$->token="StatementWithoutTrailingSubstatement";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
	} |
	BreakStatement{
		$$=new astnode;
		$$->token="StatementWithoutTrailingSubstatement";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
	} | 
	ContinueStatement{
		$$=new astnode;
		$$->token="StatementWithoutTrailingSubstatement";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
	} | 
	ReturnStatement{
		$$=new astnode;
		$$->token="StatementWithoutTrailingSubstatement";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
	} |
	SynchronizedStatement{
		$$=new astnode;
		$$->token="StatementWithoutTrailingSubstatement";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
	} |
	ThrowStatement{
		$$=new astnode;
		$$->token="StatementWithoutTrailingSubstatement";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
	} |
	TryStatement{
		$$=new astnode;
		$$->token="StatementWithoutTrailingSubstatement";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
	} ;

EmptyStatement:

	SC{
		$$=new astnode;
		$$->token="EmptyStatement";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->lexeme;
	} ;

LabeledStatement:

	Identifier COLON Statement{
		$$=new astnode;
		$$->token="LabeledStatement";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		$$->children=v;
		$$->tac = $1->lexeme;
	}  ; 

LabeledStatementNoShortIf:

	Identifier COLON StatementNoShortIf {
		$$=new astnode;
		$$->token="LabeledStatementNoShortIf";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		$$->children=v;
		$$->tac = $1->lexeme;
	} ;

ExpressionStatement:

	StatementExpression SC{
		$$=new astnode;
		$$->token="ExpressionStatement";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		$$->children=v;
		$$->tac = $1->lexeme;
	}  ;

StatementExpression:

	Assignment{
		$$=new astnode;
		$$->token="StatementExpression";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
	}  |
	PreIncrementExpression{
		$$=new astnode;
		$$->token="StatementExpression";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
	} |
	PreDecrementExpression{
		$$=new astnode;
		$$->token="StatementExpression";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
	} |
	PostIncrementExpression{
		$$=new astnode;
		$$->token="StatementExpression";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
	} |
	PostDecrementExpression{
		$$=new astnode;
		$$->token="StatementExpression";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
	} |
	MethodInvocation{
		$$=new astnode;
		$$->token="StatementExpression";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
	} |
	ClassInstanceCreationExpression {
		$$=new astnode;
		$$->token="StatementExpression";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
	};

IfThenStatement:
    IF BO Expression BC IFMARK1 Statement IFEND1 { 
        $$=new astnode;
        $$->token="IfThenStatement";
        vector<astnode*>v;
        v.push_back($1);
        v.push_back($2);
        v.push_back($3);
        v.push_back($4);
        v.push_back($6);
        $$->children=v;
        // //cout<<"in"<<$$->token;
    }
;
IfThenElseStatement:
    IF BO Expression BC IFMARK1 StatementNoShortIf IFEND2 ELSE ELSEMARK1 Statement ELSEEND1{
        $$=new astnode;
        $$->token="IfThenElseStatement";
        vector<astnode*>v;
        v.push_back($1);
        v.push_back($2);
        v.push_back($3);
        v.push_back($4);
        v.push_back($6);
        v.push_back($8);
        v.push_back($10);
        $$->children=v;
        // //cout<<"in"<<$$->token;
    }
;
IfThenElseStatementNoShortIf:
    IF BO Expression BC IFMARK1 StatementNoShortIf IFEND2 ELSE ELSEMARK1 StatementNoShortIf ELSEEND1{
        $$=new astnode;
        $$->token="IfThenElseStatementNoShortIf";
        vector<astnode*>v;
        v.push_back($1);
        v.push_back($2);
        v.push_back($3);
        v.push_back($4);
        v.push_back($6);
        v.push_back($8);
        v.push_back($10);
        $$->children=v;
        // //cout<<"in"<<$$->token;
    }
;
SwitchStatement:

	SWITCH BO Expression BC SwitchBlock{
		$$=new astnode;
		$$->token="SwitchStatement";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		v.push_back($4);
		v.push_back($5);
		$$->children=v;
		$$->tac = $1->lexeme;
	} ;

SwitchBlock:

	CBO SYMTABS SwitchBlockStatementGroups SwitchLabels CBC SYMTABE{
		$$=new astnode;
		$$->token="SwitchBlock";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($3);
		v.push_back($4);
		v.push_back($5);
		$$->children=v;
		$$->tac = $1->lexeme;
	} |
    CBO SYMTABS SwitchLabels CBC SYMTABE{
		$$=new astnode;
		$$->token="SwitchBlock";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($3);
		v.push_back($4);
		$$->children=v;
		$$->tac = $1->lexeme;
	} |
    CBO SYMTABS SwitchBlockStatementGroups CBC SYMTABE{
		$$=new astnode;
		$$->token="SwitchBlock";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($3);
		v.push_back($4);
		$$->children=v;
		$$->tac = $1->lexeme;
	} |
    CBO CBC{
		$$=new astnode;
		$$->token="SwitchBlock";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		$$->children=v;
		$$->tac = $1->lexeme;
	} ;

SwitchBlockStatementGroups:

	SwitchBlockStatementGroup{
		$$=new astnode;
		$$->token="SwitchBlockStatementGroups";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
	} |
	SwitchBlockStatementGroups SwitchBlockStatementGroup{
		$$=new astnode;
		$$->token="SwitchBlockStatementGroups";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		$$->children=v;
		$$->tac = $1->tac;
	} ;

SwitchBlockStatementGroup:

	SwitchLabels BlockStatements{
		$$=new astnode;
		$$->token="SwitchBlockStatementGroup";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		$$->children=v;
		$$->tac = $1->tac;
	} ;

SwitchLabels:

	SwitchLabel{
		$$=new astnode;
		$$->token="SwitchLabels";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
	} |
	SwitchLabels SwitchLabel{
		$$=new astnode;
		$$->token="SwitchLabels";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		$$->children=v;
		$$->tac = $1->tac;
	} ;

SwitchLabel:

	CASE ConstantExpression COLON{
		$$=new astnode;
		$$->token="SwitchLabel";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		$$->children=v;
		$$->tac = $1->lexeme;
	} |
	DEFAULT COLON{
		$$=new astnode;
		$$->token="SwitchLabel";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		$$->children=v;
		$$->tac = $1->lexeme;
	} ;

WhileStatement:
    WHILE WHILEMARK2 BO Expression BC WHILEMARK1 Statement WHILEEND1{
        $$=new astnode;
        $$->token="WhileStatement";
        vector<astnode*>v;
        v.push_back($1);
        v.push_back($3);
        v.push_back($4);
        v.push_back($5);
        v.push_back($7);
        $$->children=v;
        // //cout<<"in"<<$$->token;
    }
;
WhileStatementNoShortIf:
    WHILE WHILEMARK2 BO Expression BC WHILEMARK1 StatementNoShortIf WHILEEND1{
        $$=new astnode;
        $$->token="WhileStatementNoShortIf";
        vector<astnode*>v;
        v.push_back($1);
        v.push_back($3);
        v.push_back($4);
        v.push_back($5);
        v.push_back($7);
        $$->children=v;
        // //cout<<"in"<<$$->token;
    }
;
DoStatement:
    DO DOMARK1 Statement WHILE BO Expression BC SC DOEND1{
        $$=new astnode;
        $$->token="DoStatement";
        vector<astnode*>v;
        v.push_back($1);
        v.push_back($3);
        v.push_back($4);
        v.push_back($5);
        v.push_back($6);
        v.push_back($7);
        v.push_back($8);
        $$->children=v;
        // //cout<<"in"<<$$->token;
    }
;
ForStatement:

	FOR BO ForInit SC Expression SC ForUpdate BC Statement{
		$$=new astnode;
		$$->token="ForStatement";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		v.push_back($4);
		v.push_back($5);
		v.push_back($6);
		v.push_back($7);
		v.push_back($8);
		v.push_back($9);
		$$->children=v;
		$$->tac = $1->lexeme;
	} |
    FOR BO SC Expression SC ForUpdate BC Statement{
		$$=new astnode;
		$$->token="ForStatement";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		v.push_back($4);
		v.push_back($5);
		v.push_back($6);
		v.push_back($7);
		v.push_back($8);
		$$->children=v;
		$$->tac = $1->lexeme;
	} |
    FOR BO ForInit SC SC ForUpdate BC Statement{
		$$=new astnode;
		$$->token="ForStatement";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		v.push_back($4);
		v.push_back($5);
		v.push_back($6);
		v.push_back($7);
		v.push_back($8);
		$$->children=v;
		$$->tac = $1->lexeme;
	} |
    FOR BO ForInit SC Expression SC BC Statement{
		$$=new astnode;
		$$->token="ForStatement";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		v.push_back($4);
		v.push_back($5);
		v.push_back($6);
		v.push_back($7);
		v.push_back($8);
		$$->children=v;
		$$->tac = $1->lexeme;
	} |
    FOR BO SC SC ForUpdate BC Statement{
		$$=new astnode;
		$$->token="ForStatement";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		v.push_back($4);
		v.push_back($5);
		v.push_back($6);
		v.push_back($7);
		$$->children=v;
		$$->tac = $1->lexeme;
	} |
    FOR BO SC Expression SC BC Statement{
		$$=new astnode;
		$$->token="ForStatement";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		v.push_back($4);
		v.push_back($5);
		v.push_back($6);
		v.push_back($7);
		$$->children=v;
		$$->tac = $1->tac;
	} |
    FOR BO ForInit SC SC BC Statement {
		$$=new astnode;
		$$->token="ForStatement";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		v.push_back($4);
		v.push_back($5);
		v.push_back($6);
		v.push_back($7);
		$$->children=v;
		$$->tac = $1->tac;
	}| 
    FOR BO SC SC BC Statement{
		$$=new astnode;
		$$->token="ForStatement";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		v.push_back($4);
		v.push_back($5);
		v.push_back($6);
		$$->children=v;
		$$->tac = $1->tac;
	} ;

ForStatementNoShortIf:

	FOR BO ForInit SC Expression SC ForUpdate BC StatementNoShortIf{
		$$=new astnode;
		$$->token="ForStatementNoShortIf";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		v.push_back($4);
		v.push_back($5);
		v.push_back($6);
		v.push_back($7);
		v.push_back($8);
		v.push_back($9);
		$$->children=v;
		$$->tac = $1->tac;
	} |
    FOR BO SC Expression SC ForUpdate BC StatementNoShortIf{
		$$=new astnode;
		$$->token="ForStatementNoShortIf";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		v.push_back($4);
		v.push_back($5);
		v.push_back($6);
		v.push_back($7);
		v.push_back($8);
		$$->children=v;
		$$->tac = $1->tac;
	} |
    FOR BO ForInit SC SC ForUpdate BC StatementNoShortIf{
		$$=new astnode;
		$$->token="ForStatementNoShortIf";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		v.push_back($4);
		v.push_back($5);
		v.push_back($6);
		v.push_back($7);
		v.push_back($8);
		$$->children=v;
		$$->tac = $1->tac;
	} |
    FOR BO ForInit SC Expression SC BC StatementNoShortIf{
		$$=new astnode;
		$$->token="ForStatementNoShortIf";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		v.push_back($4);
		v.push_back($5);
		v.push_back($6);
		v.push_back($7);
		v.push_back($8);
		$$->children=v;
		$$->tac = $1->tac;
	} |
    FOR BO SC SC ForUpdate BC StatementNoShortIf{
		$$=new astnode;
		$$->token="ForStatementNoShortIf";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		v.push_back($4);
		v.push_back($5);
		v.push_back($6);
		v.push_back($7);
		$$->children=v;
		$$->tac = $1->tac;
	} |
    FOR BO SC Expression SC BC StatementNoShortIf{
		$$=new astnode;
		$$->token="ForStatementNoShortIf";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		v.push_back($4);
		v.push_back($5);
		v.push_back($6);
		v.push_back($7);
		$$->children=v;
		$$->tac = $1->tac;
	} |
    FOR BO ForInit SC SC BC StatementNoShortIf{
		$$=new astnode;
		$$->token="ForStatementNoShortIf";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		v.push_back($4);
		v.push_back($5);
		v.push_back($6);
		v.push_back($7);
		$$->children=v;
		$$->tac = $1->tac;
	} | 
    FOR BO SC SC BC StatementNoShortIf{
		$$=new astnode;
		$$->token="ForStatementNoShortIf";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		v.push_back($4);
		v.push_back($5);
		v.push_back($6);
		$$->children=v;
		$$->tac = $1->tac;
	} ;

ForInit:

	StatementExpressionList{
		$$=new astnode;
		$$->token="ForInit";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
	} |
	LocalVariableDeclaration{
		$$=new astnode;
		$$->token="ForInit";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
	} ;

ForUpdate:

	StatementExpressionList{
		$$=new astnode;
		$$->token="ForInit";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
	} ;

StatementExpressionList:

	StatementExpression{
		$$=new astnode;
		$$->token="StatementExpressionList";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
	} |
	StatementExpressionList C StatementExpression{
		$$=new astnode;
		$$->token="StatementExpressionList";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		$$->children=v;
		$$->tac = $1->tac;
	} ; 

BreakStatement:

	BREAK Identifier SC{
		$$=new astnode;
		$$->token="BreakStatement";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		$$->children=v;
		$$->tac = $1->lexeme;
	} |
    BREAK SC{
		$$=new astnode;
		$$->token="BreakStatement";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		$$->children=v;
		$$->tac = $1->lexeme;
	} ;

ContinueStatement:

	CONTINUE Identifier SC{
		$$=new astnode;
		$$->token="ContinueStatement";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		$$->children=v;
		$$->tac = $1->lexeme;
	} |
    CONTINUE SC{
		$$=new astnode;
		$$->token="ContinueStatement";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		$$->children=v;
		$$->tac = $1->lexeme;
	} ;
    
ReturnStatement:

	RETURN Expression SC{
		$$=new astnode;
		$$->token="ReturnStatement";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		$$->children=v;
		$$->tac = $1->lexeme;
		string tac;
		tac = "rax = "+$2->tac;
		prog.push_back(tac);
		tac = "pop BP_old ";
		prog.push_back(tac);
		tac = "BP = BP_old ";
		prog.push_back(tac);
		tac = "return ";
		prog.push_back(tac);
		tac = "endfunc ";
		prog.push_back(tac);
	} |
    RETURN SC{
		$$=new astnode;
		$$->token="ReturnStatement";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		$$->children=v;
		$$->tac = $1->lexeme;
		string tac;
tac = "pop BP_old ";
		prog.push_back(tac);
		tac = "BP = BP_old ";
		prog.push_back(tac);
		tac = "return ";
		prog.push_back(tac);
		tac = "endfunc ";
		prog.push_back(tac);
	} ;

ThrowStatement:

	THROW Expression SC{
		$$=new astnode;
		$$->token="ThrowStatement";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		$$->children=v;
		$$->tac = $1->lexeme;
	} ;

SynchronizedStatement:

	SYNCHRONIZED BO Expression BC Block{
		$$=new astnode;
		$$->token="SynchronizedStatement";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		v.push_back($4);
		v.push_back($5);
		$$->children=v;
		$$->tac = $1->lexeme;
	} ;

TryStatement:

    TRY Block Catches{
		$$=new astnode;
		$$->token="TryStatement";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		$$->children=v;
		$$->tac = $1->lexeme;
	} |
    TRY Block Catches Finally{
		$$=new astnode;
		$$->token="TryStatement";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		v.push_back($4);
		$$->children=v;
		$$->tac = $1->lexeme;
	} |
    TRY Block Finally{
		$$=new astnode;
		$$->token="TryStatement";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		$$->children=v;
		$$->tac = $1->lexeme;
	} ;

Catches:

	CatchClause{
		$$=new astnode;
		$$->token="Catches";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
	} |
	Catches CatchClause{
		$$=new astnode;
		$$->token="Catches";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		$$->children=v;
		$$->tac = $1->tac;
	} ;

CatchClause:

	CATCH BO FormalParameter BC Block{
		$$=new astnode;
		$$->token="CatchClause";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		v.push_back($4);
		v.push_back($5);
		$$->children=v;
		$$->tac = $1->tac;
	} ;

Finally:

	FINALLY Block{
		$$=new astnode;
		$$->token="Finally";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		$$->children=v;
		$$->tac = $1->lexeme;
	} ;

Primary:

	PrimaryNoNewArray{
		$$=new astnode;
		$$->token="Primary";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
		$$->flag=0;
	} |	
	ArrayCreationExpression{
		$$=new astnode;
		$$->token="Primary";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
	} ;

PrimaryNoNewArray:

	Literal{
		$$=new astnode;
		$$->token="PrimaryNoNewArray";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
		$$->flag=0;
	} | 
	THIS{
		$$=new astnode;
		$$->token="PrimaryNoNewArray";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->lexeme;
	} | 
	BO Expression BC {
		$$=new astnode;
		$$->token="PrimaryNoNewArray";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		$$->children=v;
		$$->tac = $2->tac;
	}| 
	ClassInstanceCreationExpression{
		$$=new astnode;
		$$->token="PrimaryNoNewArray";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
	} | 
	FieldAccess{
		$$=new astnode;
		$$->token="PrimaryNoNewArray";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
	} | 
	MethodInvocation{
		$$=new astnode;
		$$->token="PrimaryNoNewArray";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
	} | 
	ArrayAccess{
		$$=new astnode;
		$$->token="PrimaryNoNewArray";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
	} ;

ClassInstanceCreationExpression:

	NEW ClassType BO ArgumentList BC{
		$$=new astnode;
		$$->token="ClassInstanceCreationExpression";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		v.push_back($4);
		v.push_back($5);
		$$->children=v;
		$$->tac = $1->lexeme;
	} |
	NEW ClassType BO BC{
		$$=new astnode;
		$$->token="ClassInstanceCreationExpression";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		v.push_back($4);
		$$->children=v;
		$$->tac = $1->lexeme;
	} ; 

ArgumentList:

	Expression{
		$$=new astnode;
		$$->token="ArgumentList";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
	} | 
	ArgumentList C Expression{
		$$=new astnode;
		$$->token="ArgumentList";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		$$->children=v;
		$$->tac = $1->tac;
	} ;

ArrayCreationExpression:

	NEW PrimitiveType DimExprs Dims{
		$$=new astnode;
		$$->token="ArrayCreationExpression";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		v.push_back($4);
		$$->children=v;
		$$->tac = $1->lexeme;
	} | 
	NEW PrimitiveType DimExprs{
		$$=new astnode;
		$$->token="ArrayCreationExpression";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		$$->children=v;
		$$->tac = $1->lexeme;
	} | 
	NEW ClassOrInterfaceType DimExprs Dims{
		$$=new astnode;
		$$->token="ArrayCreationExpression";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		v.push_back($4);
		$$->children=v;
		$$->tac = $1->lexeme;
	} |
	NEW ClassOrInterfaceType DimExprs {
		$$=new astnode;
		$$->token="ArrayCreationExpression";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		$$->children=v;
		$$->tac = $1->lexeme;
	};

DimExprs:

	DimExpr{
		$$=new astnode;
		$$->token="DimExprs";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
	} | 
	DimExprs DimExpr{
		$$=new astnode;
		$$->token="DimExprs";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		$$->children=v;
		$$->tac = $1->tac;
	} ;

DimExpr:

	SBO Expression SBC{
		$$=new astnode;
		$$->token="DimExpr";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		$$->children=v;
		$$->tac = $1->lexeme;
	} ;

Dims:

	SBO SBC{
		$$=new astnode;
		$$->token="Dims";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		$$->children=v;
		$$->tac = $1->tac;
	} | 
	Dims SBO SBC{
		$$=new astnode;
		$$->token="Dims";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		$$->children=v;
		$$->tac = $1->tac;
	} ;

FieldAccess:

	Primary D Identifier{
		$$=new astnode;
		$$->token="FieldAccess";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		$$->children=v;
		$$->tac = $1->tac;
	} | 
	SUPER D Identifier{
		$$=new astnode;
		$$->token="FieldAccess";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		$$->children=v;
		$$->tac = $1->lexeme;
	} ;

MethodInvocation:

	Name BO ArgumentList BC{
		$$=new astnode;
		$$->token="MethodInvocation";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		v.push_back($4);
		$$->children=v;
		$$->tac = $1->tac;
		string s=$3->tac,tac;
		vector<string>token=split(s,',');
        for(int i=0;i<token.size();i++) {
            tac="push "+token[i];
            prog.push_back(tac);
        }
		$$->tac = "call " + $1->tac + " "+to_string(token.size())+"\n"+"deallocate "+to_string(token.size());
		int si=getsize(token);
            $$->tac=$$->tac+"\n"+"deallocate "+to_string(si);
	} |
	Name BO BC {
		$$=new astnode;
		$$->token="MethodInvocation";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		$$->children=v;
		$$->tac = $1->tac;
		string tac;
        // vector<string>token=split(s,',');
        // for(int i=0;i<token.size();i++) {
        //     tac="param "+token[i];
        //     prog.push_back(tac);
        // }
		$$->tac = "call " + $1->tac + " 0";
		$$->tac=$$->tac+"\n"+"deallocate 0";
		// prog.push_back(s);
	}|
	Primary D Identifier BO ArgumentList BC {
		$$=new astnode;
		$$->token="MethodInvocation";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		v.push_back($4);
		v.push_back($5);
		v.push_back($6);
		$$->children=v;
		$$->tac = $1->tac;
	}| 
	Primary D Identifier BO BC{
		$$=new astnode;
		$$->token="MethodInvocation";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		v.push_back($4);
		v.push_back($5);
		$$->children=v;
		$$->tac = $1->tac;
	}| 
	SUPER D Identifier BO ArgumentList BC{
		$$=new astnode;
		$$->token="MethodInvocation";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		v.push_back($4);
		v.push_back($5);
		v.push_back($6);
		$$->children=v;
		$$->tac = $1->lexeme;
	} |
	SUPER D Identifier BO BC {
		$$=new astnode;
		$$->token="MethodInvocation";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		v.push_back($4);
		v.push_back($5);
		$$->children=v;
		$$->tac = $1->lexeme;
	};

ArrayAccess:

	Name SBO Expression SBC{
		$$=new astnode;
		$$->token="ArrayAccess";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		v.push_back($4);
		$$->children=v;
		$$->tac = $1->tac;
		$$->tac = $1->tac+"["+$3->tac+"]";
	} | 
	PrimaryNoNewArray SBO Expression SBC{
		$$=new astnode;
		$$->token="ArrayAccess";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		v.push_back($4);
		$$->children=v;
		$$->tac = $1->tac;
		$$->tac = $1->tac+"["+$3->tac+"]";
	} ;

PostfixExpression:

	Primary{
		$$=new astnode;
		$$->token="PostfixExpression";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
		$$->flag=0;
	} | 
	Name{
		$$=new astnode;
		$$->token="PostfixExpression";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
	} | 
	PostIncrementExpression{
		$$=new astnode;
		$$->token="PostfixExpression";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
	} | 
	PostDecrementExpression{
		$$=new astnode;
		$$->token="PostfixExpression";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
	} ;

PostIncrementExpression:

	PostfixExpression PP{
		$$=new astnode;
		$$->token="PostIncrementExpression";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		$$->children=v;
		string s1 = get_tempvar(), s2;
		s2 = s1+" = "+$1->tac+"++";
		prog.push_back(s2);
		$$->tac = s1;
	} ;

PostDecrementExpression:

	PostfixExpression MM{
		$$=new astnode;
		$$->token="PostDecrementExpression";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		$$->children=v;
		string s1 = get_tempvar(), s2;
		s2 = s1+" = "+$1->tac+"--";
		prog.push_back(s2);
		$$->tac = s1;
	} ;

UnaryExpression:

	PreIncrementExpression{
		$$=new astnode;
		$$->token="UnaryExpression";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
	} | 
	PreDecrementExpression{
		$$=new astnode;
		$$->token="UnaryExpression";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
	}  | 
	P UnaryExpression{
		$$=new astnode;
		$$->token="UnaryExpression";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		$$->children=v;
		string s1 = get_tempvar(), s2;
		s2 = s1+" = +"+$2->tac;
		prog.push_back(s2);
		$$->tac = s1;
	}  | 
	M UnaryExpression{
		$$=new astnode;
		$$->token="UnaryExpression";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		$$->children=v;
		string s1 = get_tempvar(), s2;
		s2 = s1+" = -"+$2->tac;
		prog.push_back(s2);
		$$->tac = s1;
	}  | 
	UnaryExpressionNotPlusMinus{
		$$=new astnode;
		$$->token="UnaryExpression";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
		$$->flag=0;
	}  ;

PreIncrementExpression:

	PP UnaryExpression{
		$$=new astnode;
		$$->token="PreIncrementExpression";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		$$->children=v;
		string s1 = get_tempvar(), s2;
		s2 = s1+" = ++"+$2->tac;
		prog.push_back(s2);
		$$->tac = s1;
	}  ;

PreDecrementExpression:

	MM UnaryExpression{
		$$=new astnode;
		$$->token="PreDecrementExpression";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		$$->children=v;
		string s1 = get_tempvar(), s2;
		s2 = s1+" = --"+$2->tac;
		prog.push_back(s2);
		$$->tac = s1;
	}  ;

UnaryExpressionNotPlusMinus:

	PostfixExpression{
		$$=new astnode;
		$$->token="UnaryExpressionNotPlusMinus";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
		$$->flag=0;
	} | 
	TIL UnaryExpression{
		$$=new astnode;
		$$->token="UnaryExpressionNotPlusMinus";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		$$->children=v;
		string s1 = get_tempvar(), s2;
		s2 = s1+" = ~"+$2->tac;
		prog.push_back(s2);
		$$->tac = s1;
	}  | 
	N UnaryExpression{
		$$=new astnode;
		$$->token="UnaryExpressionNotPlusMinus";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		$$->children=v;
		string s1 = get_tempvar(), s2;
		s2 = s1+" = !"+$2->tac;
		prog.push_back(s2);
		$$->tac = s1;
	}  | 
	CastExpression{
		$$=new astnode;
		$$->token="UnaryExpressionNotPlusMinus";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
	} ;


CastExpression:

	BO PrimitiveType Dims BC UnaryExpression{
		$$=new astnode;
		$$->token="CastExpression";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		v.push_back($4);
		v.push_back($5);
		$$->children=v;
		$$->tac = $1->tac;
	} |
	BO PrimitiveType BC UnaryExpression{
		$$=new astnode;
		$$->token="CastExpression";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		v.push_back($4);
		$$->children=v;
		$$->tac = $1->tac;
	} |
	BO Expression BC UnaryExpressionNotPlusMinus{
		$$=new astnode;
		$$->token="CastExpression";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		v.push_back($4);
		$$->children=v;
		$$->tac = $1->tac;
	} |
	BO Name Dims BC UnaryExpressionNotPlusMinus{
		$$=new astnode;
		$$->token="CastExpression";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		v.push_back($4);
		v.push_back($5);
		$$->children=v;
		$$->tac = $1->tac;
	} ;


MultiplicativeExpression:

	UnaryExpression{
		$$=new astnode;
		$$->token="MultiplicativeExpression";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
		$$->flag=0;
	} | 
	MultiplicativeExpression MUL UnaryExpression{
		$$=new astnode;
		$$->token="MultiplicaticeExpression";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		$$->children=v;
		string s1=get_tempvar(),s2;
		s2=s1+" = "+$1->tac+"*"+$3->tac;
		prog.push_back(s2);
		$$->tac = s1;
	} | 
	MultiplicativeExpression DIV UnaryExpression{
		$$=new astnode;
		$$->token="MultiplicaticeExpression";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		$$->children=v;
		string s1=get_tempvar(),s2;
		s2=s1+" = "+$1->tac+"/"+$3->tac;
		prog.push_back(s2);
		$$->tac = s1;
	} | 
	MultiplicativeExpression MOD UnaryExpression{
		$$=new astnode;
		$$->token="MultiplicaticeExpression";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		$$->children=v;
		string s1=get_tempvar(),s2;
		s2=s1+" = "+$1->tac+"%"+$3->tac;
		prog.push_back(s2);
		$$->tac = s1;
	} ;

AdditiveExpression:

	MultiplicativeExpression{
		$$=new astnode;
		$$->token="AdditiveExpression";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
		$$->flag=0;
	} | 
	AdditiveExpression P MultiplicativeExpression{
		$$=new astnode;
		$$->token="AdditiveExpression";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		$$->children=v;
		string s1=get_tempvar(),s2;
		s2=s1+" = "+$1->tac+"+"+$3->tac;
		prog.push_back(s2);
		$$->tac = s1;
	} | 
	AdditiveExpression M MultiplicativeExpression{
		$$=new astnode;
		$$->token="AdditiveExpression";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		$$->children=v;
		string s1=get_tempvar(),s2;
		s2=s1+" = "+$1->tac+" - "+$3->tac;
		prog.push_back(s2);
		////cout<< s1<<" in additive ";
		$$->tac = s1;
	};

ShiftExpression:

	AdditiveExpression{
		$$=new astnode;
		$$->token="ShiftExpression";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac  = $1->tac;
		$$->flag=0;
	} | 
	ShiftExpression LL AdditiveExpression{
		$$=new astnode;
		$$->token="ShiftExpression";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		$$->children=v;
		string s1=get_tempvar(),s2;
		s2=s1+" = "+$1->tac+"<<"+$3->tac;
		$$->tac = s1;
		prog.push_back(s2);
	} | 
	ShiftExpression GG AdditiveExpression{
		$$=new astnode;
		$$->token="ShiftExpression";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		$$->children=v;
		string s1=get_tempvar(),s2;
		s2=s1+" = "+$1->tac+">>"+$3->tac;
		$$->tac = s1;
		prog.push_back(s2);
	} | 
	ShiftExpression GGG AdditiveExpression{
		$$=new astnode;
		$$->token="ShiftExpression";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		$$->children=v;
		////cout<<" in"<<$$->token;
		string s1=get_tempvar(),s2;
		s2=s1+" = "+$1->tac+">>>"+$3->tac;
		$$->tac = $1->tac;
		$$->tac = s1;
		prog.push_back(s2);
	} ;

RelationalExpression:

	ShiftExpression{
		$$=new astnode;
		$$->token="RelationalExpression";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
		$$->flag=0;
	} | 
	RelationalExpression LT ShiftExpression{
		$$=new astnode;
		$$->token="RelationalExpression";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		$$->children=v;
		string s1=get_tempvar(),s2;
		s2=s1+" = "+$1->tac+"<"+$3->tac;
		$$->tac = $1->tac;
		$$->tac = s1;
		prog.push_back(s2);
	} | 
	RelationalExpression GT ShiftExpression{
		$$=new astnode;
		$$->token="RelationalExpression";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		$$->children=v;
		string s1=get_tempvar(),s2;
		$$->tac = $1->tac;
		s2=s1+" = "+$1->tac+" > "+$3->tac;
		//cout<<$3->tac<<"in relational";
		$$->tac = s1;
		prog.push_back(s2);
	} | 
	RelationalExpression LE ShiftExpression{
		$$=new astnode;
		$$->token="RelationalExpression";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		$$->children=v;
		string s1=get_tempvar(),s2;
		s2=s1+" = "+$1->tac+"<="+$3->tac;
		$$->tac = $1->tac;
		$$->tac = s1;
		prog.push_back(s2);
	} | 
	RelationalExpression GE ShiftExpression{
		$$=new astnode;
		$$->token="RelationalExpression";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		$$->children=v;
		string s1=get_tempvar(),s2;
		$$->tac = $1->tac;
		s2=s1+" = "+$1->tac+">="+$3->tac;
		$$->tac = s1;
		prog.push_back(s2);
	} | 
	RelationalExpression INSTANCEOF ReferenceType{
		$$=new astnode;
		$$->token="RelationalExpression";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		$$->children=v;
	} ;

EqualityExpression:

	RelationalExpression {
		$$=new astnode;
		$$->token="EqualityExpression";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac  = $1->tac;
		$$->flag=0;
	}| 
	EqualityExpression EE RelationalExpression{
		$$=new astnode;
		$$->token="EqualityExpression";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		$$->children=v;
		string s1=get_tempvar(),s2;
		s2=s1+" = "+$1->tac+"=="+$3->tac;
		$$->tac = $1->tac;
		$$->tac = s1;
		prog.push_back(s2);
	} | 
	EqualityExpression NE RelationalExpression{
		$$=new astnode;
		$$->token="EqualityExpression";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		$$->children=v;
		$$->tac = $1->tac;
		string s1=get_tempvar(),s2;
		s2=s1+" = "+$1->tac+"!="+$3->tac;
		$$->tac = s1;
		prog.push_back(s2);
	};

AndExpression:

	EqualityExpression{
		$$=new astnode;
		$$->token="AndExpression";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
		$$->flag=0;
	} | 
	AndExpression A EqualityExpression{
		$$=new astnode;
		$$->token="AndExpression";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		$$->children=v;
		string s1=get_tempvar(),s2;
		s2=s1+" = "+$1->tac+"&"+$3->tac;
		$$->tac = s1;
		prog.push_back(s2);
	} ;

ExclusiveOrExpression:

	AndExpression{
		$$=new astnode;
		$$->token="ExclusiveOrExpression";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
		$$->flag=0;
	} | 
	ExclusiveOrExpression X AndExpression{
		$$=new astnode;
		$$->token="ExclusiveOrExpression";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		$$->children=v;
		string s1=get_tempvar(),s2;
		s2=s1+" = "+$1->tac+"^"+$3->tac;
		$$->tac = s1;
		prog.push_back(s2);
	} ;

InclusiveOrExpression:

	ExclusiveOrExpression{
		$$=new astnode;
		$$->token="InclusiveOrExpression";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
		$$->flag=0;
	} | 
	InclusiveOrExpression O ExclusiveOrExpression{
		$$=new astnode;
		$$->token="InclusiveOrExpression";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		$$->children=v;
		string s1=get_tempvar(),s2;
		s2=s1+" = "+$1->tac+"|"+$3->tac;
		prog.push_back(s2);
		$$->tac = s1;
	} ;

ConditionalAndExpression:

	InclusiveOrExpression{
		$$=new astnode;
		$$->token="ConditionalAndExpression";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac  = $1->tac;
		$$->flag=0;
	} | 
	ConditionalAndExpression AA InclusiveOrExpression{
		$$=new astnode;
		$$->token="ConditionalAndExpression";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		$$->children=v;
		string s1=get_tempvar(),s2;
		s2=s1+" = "+$1->tac+"&&"+$3->tac;
		prog.push_back(s2);
		$$->tac = s1;
	} ;

ConditionalOrExpression:

	ConditionalAndExpression{
		$$=new astnode;
		$$->token="ConditionalOrExpression";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
		$$->flag=0;
	} | 
	ConditionalOrExpression OO ConditionalAndExpression {
		$$=new astnode;
		$$->token="ConditionalOrExpression";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		$$->children=v;
		string s1=get_tempvar(),s2;
		s2=s1+" = "+$1->tac+"||"+$3->tac;
		$$->tac = s1;
		prog.push_back(s2);
	};

ConditionalExpression:

	ConditionalOrExpression{
		$$=new astnode;
		$$->token="ConditionalExpression";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
		$$->flag=0;
	} | 
	ConditionalOrExpression QM Expression COLON ConditionalExpression{
		$$=new astnode;
		$$->token="ConditionalExpression";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		v.push_back($4);
		v.push_back($5);
		$$->children=v;
		$$->tac = $1->tac;
	} ;

AssignmentExpression:

	ConditionalExpression {
		$$=new astnode;
		$$->token="AssignmentExpression";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
		$$->flag=0;
	}| 
	Assignment{
		$$=new astnode;
		$$->token="AssignmentExpression";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
	} ;

Assignment:

	LeftHandSide AssignmentOperator AssignmentExpression{
		$$=new astnode;
		$$->token="Assignment";
		vector<astnode*>v;
		v.push_back($1);
		v.push_back($2);
		v.push_back($3);
		$$->children=v;
		$$->tac = $1->tac;
		string t;
		if($3->flag==1)
		 t = $1->tac + " " + $2->tac + " t" + to_string(tempno-1);
		else  t = $1->tac + " " + $2->tac + " " +$3->tac;
		prog.push_back(t);
	} ;

LeftHandSide:

	Name{
		$$=new astnode;
		$$->token="LeftHandSide";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
	} | 
	FieldAccess{
		$$=new astnode;
		$$->token="LeftHandSide";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
	}  | 
	ArrayAccess{
		$$=new astnode;
		$$->token="LeftHandSide";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
	}  ;

AssignmentOperator: 

	EQ{
		$$=new astnode;
		$$->token="AssignmentOperator";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->lexeme;
	}  | 
	MULE{
		$$=new astnode;
		$$->token="AssignmentOperator";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->lexeme;
	} | 
	DIVE{
		$$=new astnode;
		$$->token="AssignmentOperator";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->lexeme;
	} | 
	MODE{
		$$=new astnode;
		$$->token="AssignmentOperator";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->lexeme;
	} | 
	PE {
		$$=new astnode;
		$$->token="AssignmentOperator";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->lexeme;
	}| 
	ME{
		$$=new astnode;
		$$->token="AssignmentOperator";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->lexeme;
	} | 
	LLE{
		$$=new astnode;
		$$->token="AssignmentOperator";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->lexeme;
	} | 
	GGE{
		$$=new astnode;
		$$->token="AssignmentOperator";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->lexeme;
	} | 
	GGGE{
		$$=new astnode;
		$$->token="AssignmentOperator";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->lexeme;
	} | 
	AE{
		$$=new astnode;
		$$->token="AssignmentOperator";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->lexeme;
	} | 
	XE{
		$$=new astnode;
		$$->token="AssignmentOperator";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->lexeme;
	} | 
	OE{
		$$=new astnode;
		$$->token="AssignmentOperator";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->lexeme;
	} ;

Expression:

	AssignmentExpression{
		$$=new astnode;
		$$->token="Expression";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
	} ;

ConstantExpression:

	Expression{
		$$=new astnode;
		$$->token="ConstantExpression";
		vector<astnode*>v;
		v.push_back($1);
		$$->children=v;
		$$->tac = $1->tac;
	} ;

%%
/*
void print_symtabb( ofstream& symbolTable ){
	string curr_fcn_scopee = "";
	symtab_t* sym;
	for( auto j : symtab_top ){
			curr_fcn_scopee = j.first;	
	symbolTable <<"----Symbtab for scope "<< curr_fcn_scopee <<"----"<<endl;
	symbolTable <<"-----Type-----Scope-----Name-----"<<endl;
	for( auto i=(j.second)->begin(); i != (j.second)->end(); i++ ){
		if( i->second != NULL ) 
			if(i->second->args!="args") symbolTable <<i->second->type<<","<<i->first<< "," << i->second->args<<endl;
			else symbolTable << i->second->type<<","<<i->first<<endl;
	}
	}
	symbolTable <<"----DONE----"<<endl;
}
*/

void print_symtab( ){
    ofstream symtabf;
    int count=0;
	string curr_fcn_scope = "";
    symtab_t* sym;
    
	for( auto symt : symtab_top ){

        symtabf.open("SymbolTable"+to_string(count++)+".csv");
        symtabf<<"name"<<","<<"lineno"<<","<<"type"<<","<<"size"<<","<<"args"<<","<<"flag"<<","<<"offset"<<endl;
		curr_fcn_scope = symt.first;
	    symtabf <<"----Symbtab for scope "<< curr_fcn_scope <<"----"<<endl;
	// symbolTable <<"Scope_num Sym_name"<<endl;
	for( auto i=(symt.second)->begin(); i != (symt.second)->end(); i++ ){
		// symbolTable <<i->first;
		if( i->second != NULL ) {
			// symbolTable << " " << i->second->type <<endl;
            // if(i->second->args!="args") symbolTable<<i->second->access_specifier<<" "<<i->second->type<< " " <<i->first  <<" "<<i->second->args <<endl;
            // else symbolTable  << i->second->type<<" " <<i->first<<endl;
            symtabf<<i->first<<","<<i->second->lineno<<","<<i->second->type<<","<<i->second->size<<","<<i->second->args<<","<<i->second->flag<<","<<i->second->offset<<endl;
        }
	}
	symtabf <<endl<<"----DONE----"<<endl<<endl;
    symtabf.close();
}
}

/*void print_symtab( ofstream& symbolTable){
	string curr_fcn_scope = "";
	for( auto i : symtab_top ){
		if( symtab == i.second ){
			curr_fcn_scope = i.first;
			break;
		}
	}
	symbolTable <<"----Symbtab for scope"<< curr_fcn_scope <<"----"<<endl;
	symbolTable <<"Scope_num Sym_name"<<endl;
	for( auto i=symtab->begin(); i != symtab->end(); i++ ){
		symbolTable <<i->first;
		if( i->second != NULL ) 
			symbolTable << " " << i->second->type<<endl;
	}
	symbolTable <<"----DONE----"<<endl;
}*/

void print_ast(astnode* node, int indent_level) {
	//std:://cout << std::string(indent_level, ' ') << "Token: " << node->token << std::endl;
	for(auto child : node->children){
		if(child)
		print_ast(child, indent_level + 2);
	}
}

int ID = 0;
void ast_gen(ofstream& astFile, astnode* root){
	if(root->children.size()!=1)
	astFile<< ID << " [label=\"" << root->token << "\"]" << endl;
	if(root->children.size()==1 && root->children[0]==NULL)
	astFile<< ID << " [label=\"" << root->token<<"("<< root-> lexeme<<")"<< "\"]" << endl;
	int PID = ID;
	for(auto child : root->children){
		if(child){
		if(root->children.size()>1)
		astFile<< PID << "->" << ++ID << endl;
		ast_gen(astFile ,child);}
	}
}
int IID=0;
void parse_gen(ofstream& Parse, astnode* root){
	//if(root->children.size()!=1)
	Parse<< IID << " [label=\"" << root->token << "\"]" << endl;
	if(root->children.size()==1 && root->children[0]==NULL)
	Parse<< IID << " [label=\"" << root->token<<"("<< root-> lexeme<<")"<< "\"]" << endl;
	int PID = IID;
	for(auto child : root->children){
		if(child){
		//if(root->children.size()>1)
		Parse<< PID << "->" << ++IID << endl;
		parse_gen(Parse ,child);}
	}
}
void print_tac(ofstream &tac)
{
	int i=0;
	int l = prog.size();
	for(int i=0;i<l;i++)
	{
		tac<<prog[i]<<endl;
	}
}
int main(int argc, char **argv)
{    

	//symbolTable.open("symbolTable.csv");
    yyparse();
	/*print_ast(program,0);*/
	print_symtab();
	//print_symtabb(symbolTable);
	//symbolTable.close();
	/* ofstream tac; */
	ofstream astFile;
	tac.open("tac.csv");
	astFile.open("ast.dot" );
	if(!astFile) {
		//cout << "couldn't open file" << endl;
		exit(1);
	}
	print_tac(tac);
	astFile<< "digraph \"src/parser.y\""<<endl<<"{"<<endl<<"node [fontname = courier, shape = box, colorscheme = paired6]"<<endl<<"edge [fontname = courier]" << endl;
	astFile<< ID << " [label=\"" << program->token << "\"]" << endl;
	astFile<< ID << "->" << ++ID << endl;
	ast_gen(astFile, program);
	astFile<< "}" << endl;
	astFile.close();
	ofstream Parse;
	Parse.open("parse.dot" );
	if(!Parse) {
		//cout << "couldn't open file" << endl;
		exit(1);
	}
	Parse<< "digraph \"src/parser.y\""<<endl<<"{"<<endl<<"node [fontname = courier, shape = box, colorscheme = paired6]"<<endl<<"edge [fontname = courier]" << endl;
	//Parse<< ID << " [label=\"" << program->token << "\"]" << endl;
	//Parse<< ID << "->" << ++ID << endl;
	parse_gen(Parse, program);
	Parse<< "}" << endl;
	Parse.close();
	

    return 0;
}
int yyerror(const char *s)
{
    printf("ERROR %s\n", s);
    return 0;
}

