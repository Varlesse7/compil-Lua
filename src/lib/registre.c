#include "registre.h"

extern char* lua_type_code[39][2];


constante chercherConstante(listeConstante l, int num){
    if (num >256) num = num-256;
    int i = 0;
    while(i < num){
        l = l->suivant;
        i++;
    }
    return l->cons;
}


void execute_liste_instructions(listeInstruction instr_list, listeConstante l, double* registre){
    listeInstruction instr_boucle = instr_list;

    while (instr_list) {
        instruction instr = instr_list->inst;

        switch (instr->opcode) {
            case 0: //MOVE
                registre[instr->a] = registre[instr->b];
                break;

            case 1: //LOADK
                constante cons = chercherConstante(l, instr->bx);

                if (cons->type == 3) {  // Type 3 : Nombre flottant
                    registre[instr->a] = cons->dataD;
                } else if (cons->type == 1) {  // Type 1 : Booléen (true/false)
                    registre[instr->a] = cons->dataC ? 1.0 : 0.0;
                } else if (cons->type == 4) {  // Type 4 : Chaîne de caractères
                    registre[instr->a] = (uintptr_t) cons->dataS;
                } else {
                    printf("Type de constante non supporté : %d\n", cons->type);
                }
                break;

            case 5: //GETGLOBAL
                constante cons2 = chercherConstante(l, instr->bx);
                uintptr_t val = (uintptr_t) cons2->dataS;
                registre[instr->a] = val;
                break;

            case 12: //ADD
                if (instr->b < 256 && instr->c < 256) {
                    registre[instr->a] = registre[instr->b] + registre[instr->c];
                } else if(instr->b > 256 && instr->c < 256){
                    registre[instr->a] = chercherConstante(l,instr->b)->dataD + registre[instr->c];
                } else if(instr->b < 256 && instr->c > 256){
                    registre[instr->a] = registre[instr->b] + chercherConstante(l,instr->c)->dataD;
                }else {
                    registre[instr->a] = chercherConstante(l,instr->b)->dataD + chercherConstante(l,instr->c)->dataD;
                }
                break;

            case 13: //SUB
                if (instr->b < 256 && instr->c < 256) {
                    registre[instr->a] = registre[instr->b] - registre[instr->c];
                } else if(instr->b > 256 && instr->c < 256){
                    registre[instr->a] = chercherConstante(l,instr->b)->dataD - registre[instr->c];
                } else if(instr->b < 256 && instr->c > 256){
                    registre[instr->a] = registre[instr->b] - chercherConstante(l,instr->c)->dataD;
                }else {
                    registre[instr->a] = chercherConstante(l,instr->b)->dataD - chercherConstante(l,instr->c)->dataD;
                }
                break;

            case 14: //MUL
                if (instr->b < 256 && instr->c < 256) {
                    registre[instr->a] = registre[instr->b] * registre[instr->c];
                } else if(instr->b > 256 && instr->c < 256){
                    registre[instr->a] = chercherConstante(l,instr->b)->dataD * registre[instr->c];
                } else if(instr->b < 256 && instr->c > 256){
                    registre[instr->a] = registre[instr->b] * chercherConstante(l,instr->c)->dataD;
                }else {
                    registre[instr->a] = chercherConstante(l,instr->b)->dataD * chercherConstante(l,instr->c)->dataD;
                }
                break;

            case 15: //DIV
                if (instr->b < 256 && instr->c < 256) {
                    if (registre[instr->c] != 0)
                        registre[instr->a] = registre[instr->b] / registre[instr->c];
                    else
                        fprintf(stderr, "Erreur: Division par zéro\n");
                } else if(instr->b > 256 && instr->c < 256){
                    if (registre[instr->c] != 0)
                        registre[instr->a] = chercherConstante(l,instr->b)->dataD / registre[instr->c];
                    else
                        fprintf(stderr, "Erreur: Division par zéro\n");
                } else if(instr->b < 256 && instr->c > 256){
                    double d = chercherConstante(l,instr->c)->dataD;
                    if (d != 0)
                        registre[instr->a] = registre[instr->b] / d;
                    else
                        fprintf(stderr, "Erreur: Division par zéro\n");
                }else {
                    double d = chercherConstante(l,instr->c)->dataD;
                    if (d != 0)
                        registre[instr->a] = chercherConstante(l,instr->b)->dataD / d;
                    else
                        fprintf(stderr, "Erreur: Division par zéro\n");
                }
                break;

            case 21: //CONCAT
                registre[instr->a] = (uintptr_t) concatener_registres(registre, instr->b, instr->c);

                break;

            case 22: //JMP
                int sautjmp = instr->sbx;
                for(int i =0; i<sautjmp; i++){
                    instr_list = instr_list->suivant;
                }
                break;

            case 23: //EQ
                double b,c;
                if(instr->b > 256) b = chercherConstante(l,instr->b)->dataD;
                else b = registre[instr->b];
                if(instr->c > 256) c = chercherConstante(l,instr->c)->dataD;
                else c = registre[instr->c];
                if((b == c)!=instr->a) instr_list = instr_list->suivant;
                break;

            case 24: //LT
                if(instr->b > 256) b = chercherConstante(l,instr->b)->dataD;
                else b = registre[instr->b];
                if(instr->c > 256) c = chercherConstante(l,instr->c)->dataD;
                else c = registre[instr->c];
                if((b < c)!=instr->a) instr_list = instr_list->suivant;
                break;

            case 25: //LE
                if(instr->b > 256) b = chercherConstante(l,instr->b)->dataD;
                else b = registre[instr->b];
                if(instr->c > 256) b = chercherConstante(l,instr->c)->dataD;
                else c = registre[instr->c];
                if((b <= c)!=instr->a) instr_list = instr_list->suivant;
                break;

            case 28: // CALL 
                if (registre[instr->a] != 0) {
                    char* str = (char*)(uintptr_t) registre[instr->a];  // Récupère la chaîne

                    if(strcmp(str,"print")==0){ //- Simulation de `print`
                        int num_args = instr->b - 1; // Nombre d'arguments à afficher
                        printf("[Lua print] ");
                        for (int i = 0; i < num_args; i++) {
                            int reg_index = instr->a + i+ 1;
                            uintptr_t val = (uintptr_t) registre[reg_index];
                            if (val < 256) { // Si c'est un nombre
                                printf("%.2f", registre[reg_index]);
                            } else { // Si c'est une chaîne de caractères
                                printf("%s ", (char*) val);
                            }
                        }
                        printf("\n");

                    } else { //D'autres appels peuvent être implémentés
                        printf("Appel de méthode non encore supportée\n");  // Si le registre est vide
                    }
                }
            break;

            case 30: //RETURN
                printf("\n=== Retour final ===\n");
                return;

            case 31: //FORLOOP 
                registre[instr->a] += registre[instr->a + 2];
                if(registre[instr->a] <= registre[instr->a + 1]){
                    int saut = instr->sbx;
                    registre[instr->a + 3] = registre[instr->a];
                    //printf("\n\ndébut loop\n");
                    if (instr_boucle == NULL) {
                        printf("Boucle vide\n");
                    } else {
                        listeInstruction temp = instr_boucle;
                        execute_liste_instructions(temp, l, registre);
                    }  
                }
                break;

            case 32: //FORPREP
                registre[instr->a] -= registre[instr->a + 2];
                int saut = instr->sbx;
                instr_boucle = create_liste_instr(saut);
                for(int i = 0; i< saut; i++){
                    instr_list = instr_list->suivant;
                    instr_boucle = insererInstruction(instr_boucle, instr_list->inst, instr_list->inst->opcode);
                }
                instr_boucle = insererInstruction(instr_boucle, instr_list->suivant->inst, instr_list->suivant->inst->opcode);

                break;

            default:
                fprintf(stderr, "Opcode inconnu: %d\n", instr->opcode);
                break;
        }

        instr_list = instr_list->suivant;
    }

}

void execute_chunk(chunk c) {
    if (!c) return;

    double registre[256] = {0}; 
    listeInstruction instr_list = c->instruction;
    listeConstante l = c->constant;

    execute_liste_instructions(instr_list,l,registre);
    afficher_registres(registre, 256);

}

void afficher_registres(double* registre, int taille) {
    printf("\n** État des registres **\n");
    for (int i = 0; i < taille; i++) {
        
        if (registre[i] != 0) {
            uintptr_t val = (uintptr_t) registre[i];
            if (val < 256) { // Si c'est un nombre
                printf("registre[%d] = %.2f\n", i, registre[i]);
            } else { // Si c'est une chaîne de caractères
                printf("registre[%d] = %s\n", i, (char*) val);
            }
        }
    }
    printf("\n");
}

char* concatener_registres(double* registre, int b, int c) {
    size_t buffer_size = 256;
    char *res = (char *)malloc(buffer_size); 
    if (!res) {
        printf("Erreur d'allocation mémoire\n");
        return NULL;
    }  
    res[0] = '\0'; 
    for (int i = b; i <= c; i++) {
        uintptr_t val = (uintptr_t)registre[i];

        if (val > 256) {
            char *str_val = (char *)val;
            if (strlen(res) + strlen(str_val) + 1 > buffer_size) {
                buffer_size *= 2;
                res = (char *)realloc(res, buffer_size);
                if (!res) {
                    printf("Erreur de réallocation mémoire\n");
                    return NULL;
                }
            }
            strcat(res, str_val);
        } else { 
            char temp[32];
            snprintf(temp, sizeof(temp), "%.2f", registre[i]);
            if (strlen(res) + strlen(temp) + 1 > buffer_size) {
                buffer_size *= 2;
                res = (char *)realloc(res, buffer_size);
                if (!res) {
                    printf("Erreur de réallocation mémoire\n");
                    return NULL;
                }
            }

            strcat(res, temp);
        }
    }

    return res;
}
