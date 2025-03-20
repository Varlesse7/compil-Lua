#include "registre.h"

void execute_chunk(chunk c) {
    if (!c) return;

    double registre[256] = {0}; 
    listeInstruction instr_list = c->instruction;
    int position = 0;

    while (instr_list) {
        instruction instr = instr_list->inst;

        switch (instr->opcode) {
            case 1: //LOADK
                if (instr->bx < c->constant->size) {
                    listeConstante l = c->constant;
                    for (int i = 0; i < instr->bx; i++) {
                        l = l->suivant;
                    }
                    constante cons = l->cons;

                    if (cons->type == 3) {  // Type 3 : Nombre flottant
                        registre[instr->a] = cons->dataD;
                    } else if (cons->type == 1) {  // Type 1 : Booléen (true/false)
                        registre[instr->a] = cons->dataC ? 1.0 : 0.0;
                    } else if (cons->type == 4) {  // Type 4 : Chaîne de caractères
                        registre[instr->a] = (uintptr_t) cons->dataS;
                    } else {
                        printf("Type de constante non supporté : %d\n", cons->type);
                    }
                }
                break;

            case 12: //ADD
                if (instr->b < 256 && instr->c < 256) {
                    registre[instr->a] = registre[instr->b] + registre[instr->c];
                } else {
                    printf("Erreur: Accès registre hors limites (B=%d, C=%d)\n", instr->b, instr->c);
                }
                break;

            case 13: //SUB
                if (instr->b < 256 && instr->c < 256) {
                    registre[instr->a] = registre[instr->b] - registre[instr->c];
                } else {
                    printf("Erreur: Accès registre hors limites (B=%d, C=%d)\n", instr->b, instr->c);
                }
                break;

            case 14: //MUL
                if (instr->b < 256 && instr->c < 256) {
                    registre[instr->a] = registre[instr->b] * registre[instr->c];
                } else {
                    printf("Erreur: Accès registre hors limites (B=%d, C=%d)\n", instr->b, instr->c);
                }
                break;

            case 15: //DIV
                if (instr->b < 256 && instr->c < 256) {
                    if (registre[instr->c] != 0)
                        registre[instr->a] = registre[instr->b] / registre[instr->c];
                    else
                        fprintf(stderr, "Erreur: Division par zéro\n");
                } else {
                    printf("Erreur: Accès registre hors limites (B=%d, C=%d)\n", instr->b, instr->c);
                }
                break;

            case 30: //RETURN
                printf("\n=== Résultat final ===\n");
                afficher_registres(registre, 256);
                return;

            default:
                fprintf(stderr, "Opcode inconnu: %d\n", instr->opcode);
                break;
        }

        afficher_registres(registre, 256);

        instr_list = instr_list->suivant;
    }
}

void afficher_registres(double* registre, int taille) {
    printf("\n** État des registres **\n");
    for (int i = 0; i < taille; i++) {
        if (registre[i] != 0) {
            printf("registre[%d] = %.2f\n", i, registre[i]);
        }
    }
    printf("\n");
}
