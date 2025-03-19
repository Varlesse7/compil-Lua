#include "instruction.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

instruction createInstruction(){
    instruction instr = (instruction) malloc(sizeof(struct instruction));
    instr->b = 0;
    instr->c = 0;
    instr->bx = 0;
    instr->sbx = 0;
    return instr;
}


void print_instruction(char** luacode, instruction instr){
    printf("%s {", luacode[1]);
    printf("OPCODE : %d,", instr->opcode);
    printf("TYPE : %s,", instr->type);
    printf("A : %d,", instr->a);

    if (strcmp(((const char*)instr->type), "ABC") == 0) {
        printf("B : %d,", instr->b);
        printf("C : %d}", instr->c);
    } else if (strcmp(((const char*)instr->type), "ABx") == 0) {
        printf("Bx : %d}", instr->bx);
    } else if (strcmp(((const char*)instr->type), "AsBx") == 0) {
        printf("sBx : %d}", instr->sbx);
    }
    printf("\n");
}