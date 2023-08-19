// #ifndef SYMTAB_H
// #define SYMTAB_H

#include "type.h"
#include <string>
#include<bits/stdc++.h>
typedef map< string, Type* > symtab_t;

//see parser.ypp
extern map< string, symtab_t* > symtab_top; 
extern string fullscope;
extern symtab_t *symtab;
extern int yylineno;
extern FILE *yyin;

//symtab_t* init_symtab_top();

symtab_t* init_symtab_top(){
	string keywords[] = {
		"abstract", "continue", "for", "new", "switch", "assert", "default", "if", "package", "synchronized", "boolean", "do", "goto", "private", "this", "break", "double", "implements", "protected", "throw", "byte", "else", "import", "public", "throws", "case", "enum", "instanceof", "return", "transient", "catch", "extends", "int", "short", "try", "char", "final", "interface", "static", "void", "class", "finally", "long", "strictfp", "volatile", "while", "_", "exports", "opens", "requires", "uses", "module", "permits", "sealed", "var", "non-sealed", "provides", "to", "with", "open", "record", "transitive", "yield"
	};
    symtab_t *temp = new symtab_t;
	for( int i=0; keywords[i] != "yield"; i++ ){
		(*temp)[ keywords[i] ] = NULL;
		//NULL for keywords
	}
	(*temp)[ "yield" ] = NULL;
	//symtab_top["r"] = temp;
    // for global scope
    //temp = new symtab_t;
	symtab_top["0"] = temp;
	return temp;
}
// for functions
int getoffset(int flag) {
    int of=0;
    symtab_t*t=symtab;
    for( auto i=(t)->begin(); i != (t)->end(); i++ ){
		// symbolTable <<i->first;
		if( i->second != NULL && i->second->flag==flag) {
            of+=i->second->size;
        }
	}
    return of;
}

int getoffset(symtab_t*t,int flag){
    if(flag==0)
    {int of=0;
    for( auto i=(t)->begin(); i != (t)->end(); i++ ){
		// symbolTable <<i->first;
		if( i->second != NULL && i->second->flag==flag) {
            of+=i->second->size;
        }
	}
    return of;}
    else 
    {int of=0,x=0;
    for( auto i=(t)->begin(); i != (t)->end(); i++ ){
		// symbolTable <<i->first;
        if(i==t->begin()) x=i->second->size;
		if( i->second != NULL && i->second->flag==flag) {
            of+=i->second->size;
        }
	}
    return of+x;}
}
int getsize(vector<string>v) {
    int si=0;
    for(int i=0;i<v.size();i++) {
        if(v[i]=="byte") si+=1;
        if(v[i]=="short") si+=2;
        if(v[i]=="int") si+=4;
        if(v[i]=="long") si+=8;
        if(v[i]=="float") si+=4;
        if(v[i]=="double") si+=8;
        if(v[i]=="boolean") si+=1;
        if(v[i]=="char") si+=2;
    }
    return si;
}
void symadd(string symname, string symtype, string args)
{
    string scoped_name = fullscope + " " + symname ;
    Type*type=new Type;
    type->lineno=yylineno;
    type->type=symtype;
    //type->flag=flag;
    type->offset=getoffset(0);
    if(symtype=="byte") type->size=1;
    if(symtype=="short") type->size=2;
    if(symtype=="int") type->size=4;
    if(symtype=="long") type->size=8;
    if(symtype=="float") type->size=4;
    if(symtype=="double") type->size=8;
    if(symtype=="boolean") type->size=1;
    if(symtype=="char") type->size=2;
    type->args = "(" + args + ")";
    if(args=="notdefined")type->args="()";
    else if(args=="NULL")type->args="args";
    (*symtab)[scoped_name]=type;
}
// for literals
void symadd(string symname, string symtype)
{
    string second_name = fullscope + " " + symname ; 
    Type*type=new Type;
    type->lineno=yylineno;
    type->type=symtype;
    //type->flag=flag;
    type->offset=getoffset(0);
    if(symtype=="byte") type->size=1;
    if(symtype=="short") type->size=2;
    if(symtype=="int") type->size=4;
    if(symtype=="long") type->size=8;
    if(symtype=="float") type->size=4;
    if(symtype=="double") type->size=8;
    if(symtype=="boolean") type->size=1;
    if(symtype=="char") type->size=2;
    (*symtab)[second_name]=type;
}
// for stream of literals
void symadd_list(vector<string> tokens, string symtype)
{
    string scoped_name;
    Type*type=new Type;
    type->lineno=yylineno;
    type->type=symtype;
    //type->flag=flag;
    type->offset=getoffset(0);
    if(symtype=="byte") type->size=1;
    if(symtype=="short") type->size=2;
    if(symtype=="int") type->size=4;
    if(symtype=="long") type->size=8;
    if(symtype=="float") type->size=4;
    if(symtype=="double") type->size=8;
    if(symtype=="boolean") type->size=1;
    if(symtype=="char") type->size=2;
    int l = tokens.size();
    //cout<<"\n"<<l<<endl;
    for(int i=0;i<l;i++)
    {
        scoped_name = fullscope + " " + tokens[i];
            (*symtab)[scoped_name]=type;
    }
}
