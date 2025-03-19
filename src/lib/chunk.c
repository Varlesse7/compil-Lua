#include "chunk.h"

extern char* lua_type_code[39][2];

chunk createChunk(){
    return (chunk) malloc(sizeof(struct chunk));
}

void libererChunk(chunk c){
    libererListeInstruction(c->instruction);
    libererListeConstante(c->constant);
    free(c->name);
    free(c);
    c = NULL;
}

chunk decode_chunk (codeLua f){
    chunk c;
    c = createChunk();

    c->name = get_string(f, 0);
    c->first_line = get_int(f);
    c->last_line = get_int(f);

    c->upvalues = get_byte(f);
    c->arguments = get_byte(f);
    c->varg = get_byte(f);
    c->stack = get_byte(f);

    printf("** Decoding instructions\n");

    int num = get_int(f);

    c->instruction = create_liste_instr(num);
    for (int i = 0; i < num; i++) {
        instruction instr;
        instr = createInstruction();
        int data = get_int32(f);


        unsigned int opcode = get_bits(data, 0, 6);
        printf("%d\n", opcode);
        char* tp =lua_type_code[opcode][0];

        instr->opcode = opcode;
        instr->type = (unsigned char *) tp;
        printf("%s\n", instr->type);
        instr->a = get_bits(data, 6, 8);

        if (strcmp(tp, "ABC") == 0) {
            instr->b = get_bits(data, 23, 9);
            instr->c = get_bits(data, 14, 9);
        } else if (strcmp(tp, "ABx") == 0) {
            instr->bx = get_bits(data, 14, 18);
        } else if (strcmp(tp, "AsBx") == 0) {
            instr->sbx = get_bits(data, 14, 18) - 131071;
        }
        c->instruction = insererInstruction(c->instruction, instr, opcode);
        print_instruction(lua_type_code[opcode], instr);

    }

    printf("\n** Decoding Constante\n");
    num = get_int(f);
    c->constant = create_liste_constante();

    for (int i = 0; i < num; i++) {
        constante cons;
        cons = createConstante();
        cons->type = get_byte(f);
        if (cons->type == 1){
            cons->dataC = (get_byte(f) != 0);
        }else if (cons->type == 3){
            cons->dataD = get_double(f);
        }else if (cons->type == 4) {
            cons->dataS = get_string(f, 0);
        }

        c->constant = insererConstant(c->constant, cons);

        print_constante(cons);
        printf("\n");
    }

    printf("\n** Decoding Protos\n");

    num = get_int(f);
    listeChunk lp = c->prototypes;
    for (int i = 0; i < num; i++) {
        lp->c = decode_chunk(f);
        lp = lp->suivant;
    }

    printf("\n** Decoding Debug Symbols\n");
    num = get_int(f);
    for (int i = 0; i < num; i++) {
        get_int32(f);
    }

    num = get_int(f);
    for (int i = 0; i < num; i++) {
        unsigned char* inter = get_string(f, 0);
        printf("%s\n", inter);
        free(inter);
        get_int32(f);
        get_int32(f);
    }

    num = get_int(f);

    for (int i = 0; i < num; i++) {
        unsigned char* inter = get_string(f, 0);
        printf("%s\n", inter);
        free(inter);
    }

    return c;
}