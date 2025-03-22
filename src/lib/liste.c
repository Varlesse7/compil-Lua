#include "liste.h"

extern char* lua_type_code[39][2];

listeInstruction create_liste_instr(int size){
    return NULL;
}

listeConstante create_liste_constante(){
    return NULL;
}

listeInstruction insererInstruction(listeInstruction liste, instruction inst, int opcode) {
    listeInstruction nouveau = (listeInstruction) malloc(sizeof(struct listeInstruction));


    if (!nouveau) {
        perror("Allocation échouée");
        exit(EXIT_FAILURE);
    }

    nouveau->inst = inst;
    nouveau->suivant = NULL;
    nouveau->size = 1;


    if (!liste) {
        return nouveau;
    }

    listeInstruction temp = liste;
    while (temp->suivant) {
        temp = temp->suivant;
    }
    temp->suivant = nouveau;
    return liste;
}

listeConstante insererConstant(listeConstante liste, constante cons) {
    listeConstante nouveau = (listeConstante) malloc(sizeof(struct listeConstante));
    if (!nouveau) {
        perror("Allocation échouée");
        exit(EXIT_FAILURE);
    }

    nouveau->cons = cons;
    nouveau->suivant = NULL;
    nouveau->size = 1;

    if (!liste) {
        return nouveau;
    }

    listeConstante temp = liste;
    while (temp->suivant) {
        temp = temp->suivant;
    }
    temp->suivant = nouveau;
    return liste;
}

void print_liste_instruction(listeInstruction li){
    listeInstruction temp = li;
    while (temp){
        print_instruction(lua_type_code[temp->inst->opcode], temp->inst);
        temp = temp->suivant;
    }
}

void print_liste_constante(listeConstante li){
    listeConstante temp = li;
    while (temp){
        print_constante( temp->cons);
        temp = temp->suivant;
    }
}

void libererListeConstante(listeConstante liste) {
    while (liste != NULL) {
        listeConstante temp = liste;
        liste = liste->suivant;
        free(temp->cons);
        free(temp);
    }

}

void libererListeInstruction(listeInstruction liste) {
    while (liste != NULL) {
        listeInstruction temp = liste;
        liste = liste->suivant;
        free(temp->inst);
        free(temp);
    }
}