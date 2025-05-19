%{
#include <stdio.h>
#include "y.tab.h"
%}

%option noyywrap

%%

"main" { return MAIN; }
"int" { return INT; }
"vetor" { return VETOR; }
"void" {return VOID; }
"return" { return RETURN; }
"faz" { return FOR; }
"enquanto" { return WHILE; }
"se" { return IF; }
"senao" { return ELSE; }
"falso" { return FALSE; }
"vdd" { return TRUE; }
"leia" { return SCANF; }
"mostre" { return PRINTF; }
"e" {return AND; }
"dps" {return DPS; }
"ou" {return OR; }
";" { return PEV; }
"!=" { return DIF; }
"=" { return ATRIB; }
"==" { return COMP; }
"+" { return MAIS; }
"-" { return MENOS; }
">" { return MAIOR; }
">=" { return MAIORIGUAL; }
"<" { return MENOR; }
"<=" { return MENORIGUAL; }
"(" { return LPAR; }
")" { return RPAR; }
"{" { return LCHAV; }
"}" { return RCHAV; }
"[" { return LCOL; }
"]" { return RCOL; }
"/" { return DIV; }
"%" { return MOD; }
"*" { return MULT; }
"!" { return NEG; }
"," { return VIRG; }
\"([^\"\\]|\\.)*\" {
    yylval.str_val = strdup(yytext); 
    return STRING; 
    }
[0-9]+ {
    yylval.int_val = atoi(yytext);
    return NUM; 
    }
[a-zA-Z][a-zA-Z0-9_]* { 
    yylval.str_val = strdup(yytext);
    return ID; 
    }
"//".* {}
" " {}
"\n" {}
[\t] {} /*Aceita o tab, se nao da conflito com a linha de baixo*/
. { fprintf(stderr, "ERRO Lexico, caracter invalido : %s\n", yytext); }

%%
