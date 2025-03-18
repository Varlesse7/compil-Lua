//
// Created by Pierre on 17/03/2025.
//
#include "fichierToStruct.h"

unsigned char get_byte(codeLua f){
    unsigned char b = f->bytecode[f->index];
    f->index = f->index+1;
    return b;
}

unsigned int get_bits(unsigned int num, int p, int s) {
    return (num >> p) & (~((~0) << s));
}

unsigned char* get_string(codeLua f, int size){
    if (size == 0){
        int t = get_size(f);
        if (t == 0){
            return "";
        }
        size = t;
    }
    unsigned char* s;
    s = (unsigned char*) malloc((size+1) * sizeof(unsigned char));
    s[size] = '\0';
    for (int i = 0; i < size; i++) {
        s[i] = f->bytecode[(i + f->index)];
    }
    f->index = f->index + size;
    return s;
}

int convert_char_to_int(char c){
    return c - '0';
}

int from_bytes_big_endian(const unsigned char* bytes, int size) {
    int result = 0;
    for (size_t i = 0; i < size; i++) {
        result |= (int)bytes[i] << (8 *(size - 1 - i));
    }
    return result;
}

int from_bytes_little_endian(const unsigned char* bytes, int size) {
    int result = 0;
    for (size_t i = 0; i < size; i++) {
        result |= (int)bytes[i] << (8 *i);
    }
    return result;
}

int get_int32(codeLua f){
    int i = 0;
    int size = 4;
    unsigned char* subbuf = (unsigned char*) malloc((size+1) * sizeof(unsigned char));
    memcpy(subbuf, &f->bytecode[f->index], size);
    if (f->endian == '0'){
        i = from_bytes_big_endian(subbuf, size);
    }else{
        i = from_bytes_little_endian(subbuf, size);
    }
    f->index += size;
    free(subbuf);
    return i;
}

int get_int(codeLua f){
    int i = 0;
    int size = (int) f->int_size;
    unsigned char* subbuf = (unsigned char*)malloc(size * sizeof(unsigned char));
    memcpy(subbuf, &f->bytecode[f->index], size);
    if (f->endian == '0'){
        i = from_bytes_big_endian(subbuf, size);
    }else{
        i = from_bytes_little_endian(subbuf, size);
    }
    f->index += size;
    free(subbuf);
    return i;
}

int get_size(codeLua f){
    int i = 0;
    int size = (int) f->size_t;
    unsigned char* subbuf = (unsigned char*)malloc(size * sizeof(unsigned char));

    if (subbuf == NULL) {
        // Gérer l'erreur d'allocation (par exemple, afficher un message et sortir du programme)
        perror("Erreur d'allocation mémoire");
        exit(1);
    }
    memcpy(subbuf, &f->bytecode[f->index], size);
    if (f->endian == '0'){
        i = from_bytes_big_endian(subbuf, size);
    }else{
        i = from_bytes_little_endian(subbuf, size);
    }
    f->index += size;
    free(subbuf);
    return i;
}

double get_double(codeLua parser) {
    double value;
    unsigned char buffer[8];

    // Extraire les 8 octets depuis le bytecode
    memcpy(buffer, parser->bytecode + parser->index, 8);
    parser->index += 8;

    // Vérifier l'endianness et convertir si nécessaire
    if (parser->endian != '0') {
        // Inverser les octets pour les convertir en Little-Endian si nécessaire
        for (int i = 0; i < 8; i++) {
            unsigned char temp;
            temp = buffer[i];
            buffer[i] = buffer[7 - i];
            buffer[7 - i] = temp;
        }

    }


    // Copier les octets dans la variable `double`
    memcpy(&value, buffer, sizeof(double));

    return value;
}
/*
double get_double(codeLua parser) {
    double value;
    uint8_t buffer[8];

    // Extraire les 8 octets depuis le bytecode
    memcpy(buffer, parser->bytecode + parser->index, 8);
    parser->index += 8;

    // Vérifier l'endianness et convertir si nécessaire
    if (parser->endian != '0') {
        // Inverser les octets pour les convertir en Little-Endian si nécessaire
        for (int i = 0; i < 4; i++) {
            uint8_t temp = buffer[i];
            buffer[i] = buffer[7 - i];
            buffer[7 - i] = temp;
        }
    }

    printf("buffer : %d", buffer[0]);
    for(int i =1; i < 8; i++){
        printf(" %d",buffer[i]);
    }
    // Copier les octets dans la variable `double`
    memcpy(&value, buffer, sizeof(double));

    return value;
}
*/
chunk createChunk(){
    return (chunk) malloc(sizeof(struct chunk));
}

void libererChunk(chunk c){
    free(c->name);
    free(c);
    c = NULL;
}

instruction createInstruction(){
    instruction instr = (instruction) malloc(sizeof(struct instruction));
    instr->b = 0;
    instr->c = 0;
    instr->bx = 0;
    instr->sbx = 0;
    return instr;
}

void libererInstruction(instruction instr){
    free(instr);
    instr = NULL;
}

constante createConstante(){
    return (constante) malloc(sizeof(struct constante));
}

void libererConstante(constante cons){
    free(cons);
    cons = NULL;
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

void print_constante(constante cons){
    printf("{TYPE : %d,", cons->type);
    if (cons->type == 1){
        printf("DATA : %c}", cons->dataC);
    }else if (cons->type == 3){
        printf("DATA : %lf}", cons->dataD);
    }else if (cons->type == 4) {
        printf("DATA : %s}", cons->dataS);
    }

}

chunk decode_chunk (codeLua f){
    char* lua_type_code[35][2] = {{"ABC", "MOVE"}, {"ABx", "LOADK"}, {"ABC", "LOADBOOL"},
                                  {"ABC", "LOADNIL"},{"ABC", "GETUPVAL"}, {"ABx", "GETGLOBAL"},
                                  {"ABC", "SETTABLE"}, {"ABC", "NEWTABLE"}, {"ABC", "SELF"},
                                  {"ABC", "ADD"}, {"ABC", "SUB"}, {"ABC", "MUL"},
                                  {"ABC", "DIV"}, {"ABC", "MOD"}, {"ABC", "POW"},
                                  {"ABC", "UNM"}, {"ABC", "NOT"}, {"ABC", "LEN"},
                                  {"ABC", "CONCAT"}, {"AsBx", "JMP"}, {"ABC", "EQ"},
                                  {"ABC", "LT"}, {"ABC", "LE"}, {"ABC", "TEST"},
                                  {"ABC", "TESTSET"}, {"ABC", "CALL"}, {"ABC", "TAILCALL"},
                                  {"ABC", "RETURN"}, {"AsBx", "FORLOOP"}, {"AsBx", "FORREP"},
                                  {"ABC", "TFORLOOP"}, {"ABC", "SETLIST"}, {"ABC", "CLOSE"},
                                  {"ABx", "CLOSURE"}, {"ABC", "VARARG"}};
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

    for (int i = 0; i < num; i++) {
        instruction instr;
        instr = createInstruction();
        int data = get_int32(f);


        unsigned int opcode = get_bits(data, 1, 6);
        char* tp =lua_type_code[opcode][0];

        instr->opcode = opcode;
        instr->type = (unsigned char *) tp;
        instr->a = get_bits(data, 6, 8);

        if (strcmp(tp, "ABC") == 0) {
            instr->b = get_bits(data, 23, 9);
            instr->c = get_bits(data, 14, 9);
        } else if (strcmp(tp, "ABx") == 0) {
            instr->bx = get_bits(data, 14, 18);
        } else if (strcmp(tp, "AsBx") == 0) {
            instr->sbx = get_bits(data, 14, 18) - 131071;
        }
        c->instruction = instr;

        print_instruction(lua_type_code[opcode], instr);

        libererInstruction(instr);
    }

    printf("\n** Decoding Constante\n");
    num = get_int(f);

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

        c->constant = cons;

        print_constante(cons);
        printf("\n");

        libererConstante(cons);
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