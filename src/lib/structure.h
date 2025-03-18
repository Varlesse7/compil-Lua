//
// Created by Pierre on 17/03/2025.
//

#ifndef SRC_STRUCTURE_H
#define SRC_STRUCTURE_H

typedef struct codeLua{
    int index;
    unsigned char* bytecode;
    unsigned char* signature;
    unsigned char version;
    unsigned char format;
    unsigned char endian;
    unsigned char int_size;
    unsigned char size_t;
    unsigned char instr_size;
    unsigned char l_number_size;
    unsigned char l_integral_flag;
} *codeLua;

typedef struct listeInstruction{
    int size;
    struct listeInstruction* suivant;
    struct instruction* inst;
}*listeInstruction;

typedef struct instruction{
    unsigned int opcode;
    unsigned char* type;
    unsigned int a;
    unsigned int b;
    unsigned int c;
    unsigned int bx;
    unsigned int sbx;

}*instruction;

typedef struct listeConstante{
    int size;
    struct listeConstante* suivant;
    struct constante* cons;
}*listeConstante;

typedef struct constante{
    unsigned char type;
    unsigned char dataC;
    double dataD;
    unsigned char* dataS;

}*constante;

typedef struct chunk{
    struct listeInstruction* instruction;
    struct listeConstante* constant;
    struct listeChunk* prototypes;
    unsigned char* name;
    int first_line;
    int last_line;
    unsigned char upvalues;
    unsigned char arguments;
    unsigned char varg;
    unsigned char stack;
}*chunk;

typedef struct listeChunk{
    chunk c;
    struct listeChunk* suivant;
}*listeChunk;



#endif //SRC_STRUCTURE_H
