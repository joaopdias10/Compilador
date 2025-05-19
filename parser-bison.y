%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "y.tab.h"


/*Analise semântica*/

typedef struct {
    char *id;
    int end;
} simbolo;

simbolo tabsimb[1000];
int nsimbs = 0;
int ini = 0;

int getendereco(char *id) {
    for (int i=0;i<nsimbs;i++)
        if (!strcmp(tabsimb[i].id, id))
            return tabsimb[i].end;
    return -1;
}

void semantica(char *id){
    for (int i=0;i<nsimbs;i++)
        if (!strcmp(tabsimb[i].id, id))
            return;
    fprintf(stderr, "Erro semantico: %s nao declarada\n", id);
}


/*  Pilha  */

typedef struct nodo{
    char *rot;
    struct nodo *prox;
}nodo;

typedef struct{
     nodo *topo;
}Pilha;

Pilha *criapilha(){
    Pilha *nova = (Pilha*)malloc(sizeof(Pilha));
    nova -> topo = NULL;
    return nova;
}

int R_unid=0;
int R_decim=0;
Pilha *p;
int tam = 0;

void push(){ 
    if(tam == 0){
        p = criapilha();
    }
    tam = tam+4;
    
    char aux[5];
    char num1[5];
    char num2[5];

    nodo *novo1 = (nodo*)malloc(sizeof(nodo));
    novo1->rot = (char *)malloc(5 * sizeof(char));
    novo1 -> prox = p -> topo;
    p -> topo = novo1;

    nodo *novo2 = (nodo*)malloc(sizeof(nodo));
    novo2->rot = (char *)malloc(5 * sizeof(char));
    novo2 -> prox = p -> topo;
    p -> topo = novo2;

    nodo *novo3 = (nodo*)malloc(sizeof(nodo));
    novo3->rot = (char *)malloc(5 * sizeof(char));
    novo3 -> prox = p -> topo;
    p -> topo = novo3;

    nodo *novo4 = (nodo*)malloc(sizeof(nodo));
    novo4->rot = (char *)malloc(5 * sizeof(char));
    novo4 -> prox = p -> topo;
    p -> topo = novo4;

    sprintf(num1, "%d", R_unid);
    sprintf(num2, "%d", R_decim);
    strcpy(aux, "R");
    strcat(aux, num2);
    strcat(aux, num1);
    strcpy(novo2->rot, aux);
    strcpy(novo4->rot, aux);
    R_unid++;
    if(R_unid == 10){
        R_unid = 0;
        R_decim++;
    }

    sprintf(num1, "%d", R_unid);
    sprintf(num2, "%d", R_decim);
    strcpy(aux, "R");
    strcat(aux, num2);
    strcat(aux, num1);
    strcpy(novo1->rot, aux);
    strcpy(novo3->rot, aux);
    R_unid++;
    if(R_unid == 10){
        R_unid = 0;
        R_decim++;
    }
}

char *pop(){
    nodo *aux = p -> topo -> prox;
    char *rotulo = (char *)malloc(5 * sizeof(char));
    strcpy(rotulo, p->topo->rot);
    free(p -> topo);
    p -> topo = aux;
    tam--;
    return rotulo;
}

void ajusta_R(){
    if(R_unid == 10){
        R_decim--;
    }
    R_unid--;
}

/* Contador global de registradores temporarios */
int T=0;

void zerar(){
    T = 0;
}

%}

//%define parse.error verbose

%union {
    char *str_val;
    int int_val;
}

%token MAIN INT VETOR VOID RETURN FOR WHILE IF ELSE FALSE TRUE SCANF PRINTF AND DPS OR PEV DIF ATRIB COMP MAIS MENOS MAIOR MAIORIGUAL MENOR MENORIGUAL LPAR RPAR LCHAV RCHAV LCOL RCOL DIV MOD MULT NEG VIRG <str_val>STRING <int_val>NUM <str_val>ID

%type <int_val>expr termo fator algo or_and
%type <str_val>cond mm dm ao

%%

main : INT MAIN LPAR RPAR LCHAV dec func return RCHAV | VOID MAIN LPAR RPAR LCHAV func RCHAV;

dec : INT ID { tabsimb[nsimbs] = (simbolo){$2, ini}; nsimbs++; ini++;} virg PEV dec 
    | VETOR ID LCOL NUM RCOL PEV dec { tabsimb[nsimbs] = (simbolo){$2, ini}; nsimbs++; ini+=$4;} | ;

virg : VIRG ID { tabsimb[nsimbs] = (simbolo){$2, ini}; nsimbs++; ini++;} virg | ;

for : FOR LPAR for_cond {push(); printf("label %s\n", pop());} or_and {printf("jf %%t%d, %s\n", $5, pop());} PEV ID {semantica($8);} ATRIB expr RPAR LCHAV func RCHAV {printf("mov %%r%d, %%t%d\n", getendereco($8), $11); printf("jump %s\n", pop()); printf("label %s\n", pop());};

for_cond : atrib | PEV;

while : WHILE LPAR {push(); printf("label %s\n", pop());} or_and {printf("jf %%t%d, %s\n", $4, pop());} RPAR LCHAV func RCHAV {printf("jump %s\n", pop()); printf("label %s\n", pop());};

if : IF LPAR or_and {push(); printf("jf %%t%d, %s\n", $3, pop());} RPAR LCHAV func RCHAV else;

else : ELSE LCHAV {printf("jump %s\n", pop()); printf("label %s\n", pop());} func RCHAV {printf("label %s\n", pop());} | {pop(); printf("label %s\n", pop()); pop(); ajusta_R();};

or_and : or_and ao algo {printf("%s %%t%d, %%t%d, %%t%d\n", $2, T, $1, $3); $$ = T++;}
| algo{$$ = $1;};

ao : AND {$$ = "and";} | OR {$$ = "or";}; 

algo : expr cond expr {printf("%s %%t%d, %%t%d, %%t%d\n", $2, T, $1, $3); $$ = T++;}
| LPAR or_and RPAR {$$ = $2;} 
| NEG LPAR or_and RPAR {printf("not %%t%d, %%t%d\n", T, $3);$$ = T++;};

cond : MAIOR {$$ = "greater";} | MENOR {$$ = "less";} | COMP {$$ = "equal";} | MAIORIGUAL {$$ = "greatereq";} | MENORIGUAL {$$ = "lesseq";} | DIF {$$ = "diff";};

func : for func | if func |while func | scan func | print func | atrib func | ;

atrib : ID {semantica($1);} ATRIB expr PEV {printf("mov %%r%d, %%t%d\n", getendereco($1), $4);}
| ID {semantica($1);} LCOL expr RCOL ATRIB expr PEV { printf("store %%t%d, %d(%%t%d)\n", $7, getendereco($1), $4); };

expr : expr mm termo {printf("%s %%t%d, %%t%d, %%t%d\n", $2, T, $1, $3); $$ = T++;}
| termo {$$ = $1;};

mm: MAIS{$$ = "add";} | MENOS {$$ = "sub";};

termo : termo dm fator {printf("%s %%t%d, %%t%d, %%t%d\n", $2, T, $1, $3); $$ = T++;}
| fator {$$ = $1;};

dm: DIV {$$ = "div";} | MULT {$$ = "mult";} | MOD {$$ = "mod";};

fator : ID {semantica($1); printf("mov %%t%d, %%r%d\n", T, getendereco($1));$$ = T++;}
| NUM {printf("mov %%t%d, %d\n", T, $1); $$ = T++;}
| ID {semantica($1);}  LCOL expr RCOL { printf("load %%t%d, %d(%%t%d)\n", T, getendereco($1), $4); $$ = T++; }
| LPAR expr RPAR {$$ = $2;};

scan : SCANF ID {semantica($2); printf("read %%r%d\n", getendereco($2));} dps PEV;
dps: DPS ID {semantica($2); printf("read %%r%d\n", getendereco($2));} dps | ;

print : PRINTF STRING {printf("printf %s\n", $2);} mostre PEV  
| PRINTF expr {printf("printv %%t%d\n", $2);} mostre PEV ;

mostre : DPS STRING {printf("printf %s\n", $2);} mostre 
| DPS expr {printf("printv %%t%d\n", $2);} mostre | ;

return : RETURN NUM PEV;

%%

extern FILE *yyin;                   // (*) descomente para ler de um arquivo

int main(int argc, char *argv[]){
    
    yyin = fopen("insertion.txt", "r");
    
    yyparse();
    
    fclose(yyin);

    return 0;
    }

void yyerror(char *s) { fprintf(stderr,"ERRO: %s\n", s); }
