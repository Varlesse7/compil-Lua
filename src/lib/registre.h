#include "chunk.h"
#include "liste.h"
#include <stdio.h>

void execute_chunk(chunk c);
void afficher_registres(double* registre, int taille);
char* concatener_registres(double* registre, int b, int c);

void execute_liste_instructions(listeInstruction instr_list, listeConstante l, double* registre);