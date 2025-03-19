//
// Created by Pierre on 19/03/2025.
//
#include "structure.h"
#include "instruction.h"
#include "constante.h"
#include "fichierToStruct.h"
#include <stdio.h>
#include <stdlib.h>

#ifndef SRC_LISTE_H
#define SRC_LISTE_H


listeInstruction create_liste_instr(int size);
listeConstante create_liste_constante();

listeInstruction insererInstruction(listeInstruction liste, instruction inst, int opcode);
listeConstante insererConstant(listeConstante liste, constante cons);

void print_liste_instruction(listeInstruction li);
void print_liste_constante(listeConstante li);

void libererListeConstante(listeConstante liste);
void libererListeInstruction(listeInstruction liste);

#endif //SRC_LISTE_H
