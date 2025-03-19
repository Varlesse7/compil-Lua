#include "fichierToStruct.h"

char* lua_type_code[39][2] = {{"ABC", "MOVE"}, {"ABx", "LOADK"}, {"ABC", "LOADBOOL"},
                                     {"ABC", "LOADNIL"},{"ABC", "GETUPVAL"}, {"ABx", "GETGLOBAL"},
                                     {"ABx", "GETTABLE"},{"ABx", "SETGLOBAL"}, {"ABC", "SETUPVAL"},
                                     {"ABC", "SETTABLE"},{"ABC", "NEWTABLE"}, {"ABC", "SELF"},
                                     {"ABC", "ADD"}, {"ABC", "SUB"}, {"ABC", "MUL"},
                                     {"ABC", "DIV"}, {"ABC", "MOD"}, {"ABC", "POW"},
                                     {"ABC", "UNM"}, {"ABC", "NOT"}, {"ABC", "LEN"},
                                     {"ABC", "CONCAT"}, {"AsBx", "JMP"}, {"ABC", "EQ"},
                                     {"ABC", "LT"}, {"ABC", "LE"}, {"ABC", "TEST"},
                                     {"ABC", "TESTSET"}, {"ABC", "CALL"}, {"ABC", "TAILCALL"},
                                     {"ABC", "RETURN"}, {"AsBx", "FORLOOP"}, {"AsBx", "FORPREP"},
                                     {"ABC", "TFORLOOP"}, {"ABC", "SETLIST"}, {"ABC", "CLOSE"},
                                     {"ABx", "CLOSURE"}, {"ABC", "VARARG"}};

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

