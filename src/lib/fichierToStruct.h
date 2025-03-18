//
// Created by Pierre on 17/03/2025.
//
#include "structure.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#ifndef SRC_FICHIERTOSTRUCT_H
#define SRC_FICHIERTOSTRUCT_H

unsigned char get_byte(codeLua f);
unsigned int get_bits(unsigned int num, int p, int s);
unsigned char* get_string(codeLua f, int size);
int convert_char_to_int(char c);
int from_bytes_big_endian(const unsigned char* bytes, int size);
int from_bytes_little_endian(const unsigned char* bytes, int size);
int get_int32(codeLua f);
int get_int(codeLua f);
int get_size(codeLua f);
double get_double(codeLua f);
//double get_double(codeLua parser);
chunk decode_chunk(codeLua f);
chunk createChunk();
void libererChunk(chunk c);
instruction createInstruction();
void print_liste_instruction(listeInstruction li);
void libererListeInstruction(listeInstruction liste);
constante createConstante();
void libererConstante(constante cons);



#endif //SRC_FICHIERTOSTRUCT_H
