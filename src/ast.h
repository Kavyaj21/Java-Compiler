#include<bits/stdc++.h>
using namespace std ;

struct astnode {
    string lexeme;
    string token;
    vector<astnode*> children;
    string type="notdefined";
    string name="name";
    string value="notdefined";
    string tac;
    string code;
    int flag=1;
};
