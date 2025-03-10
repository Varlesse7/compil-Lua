#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

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
}*codeLua;

unsigned char get_byte(codeLua f){
    unsigned char b = f->bytecode[f->index];
    f->index = f->index+1;
    return b;
}

unsigned char* get_string(codeLua f, int size){
    unsigned char* s;
    s = malloc(size * sizeof(unsigned char));

    s[size] = '\0';
    for (int i = 0; i < size; i++) {
        printf("%d %c \n", i+f->index, f->bytecode[i+f->index]);
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
    unsigned char subbuf[size];
    memcpy(subbuf, &f->bytecode[f->index], size);
    if (f->endian == '0'){
        i = from_bytes_big_endian(subbuf, size);
    }else{
        i = from_bytes_little_endian(subbuf, size);
    }
    f->index += size;
    return i;
}

int get_int(codeLua f){
    int i = 0;
    int size = convert_char_to_int(f->int_size);
    unsigned char subbuf[size];
    memcpy(subbuf, &f->bytecode[f->index], size);
    if (f->endian == '0'){
        i = from_bytes_big_endian(subbuf, size);
    }else{
        i = from_bytes_little_endian(subbuf, size);
    }
    f->index += size;
    return i;
}

int get_size(codeLua f){
    int i = 0;
    int size = convert_char_to_int(f->size_t);
    unsigned char subbuf[size];
    memcpy(subbuf, &f->bytecode[f->index], size);
    if (f->endian == '0'){
        i = from_bytes_big_endian(subbuf, size);
    }else{
        i = from_bytes_little_endian(subbuf, size);
    }
    f->index += size;
    return i;
}

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

    // Copier les octets dans la variable `double`
    memcpy(&value, buffer, sizeof(double));

    return value;
}

codeLua createFichier(){
    return (codeLua) malloc(sizeof(codeLua));
}

void libererFichier(codeLua f){
    free(f);
}

int main (){
    codeLua f;
    f = createFichier();
    FILE* file = fopen("luac.out", "rb"); // Ouvrir en mode lecture binaire
    if (!file) {
        perror("Erreur lors de l'ouverture du fichier");
        return EXIT_FAILURE;
    }

    // Déterminer la taille du fichier
    if (fseek(file, 0, SEEK_END)){
        perror("Erreur lors de la recherche dans le fichier");
        return EXIT_FAILURE;
    }


    long size = ftell(file);
    rewind(file);

    // Allouer un buffer pour stocker les données
    unsigned char* buffer = (unsigned char *)malloc(size);
    if (!buffer) {
        perror("Erreur d'allocation mémoire");
        fclose(file);
        return EXIT_FAILURE;
    }

    // Lire le fichier dans le buffer
    size_t bytesRead = fread(buffer, 1, size, file);
    if (bytesRead != size) {
        perror("Erreur de lecture");
    }

    f->bytecode = buffer;
    f->index = 1;
    f->signature = get_string(f, 3);
    f->version = get_byte(f);
    f->format = get_byte(f);
    f->endian = get_byte(f);
    f->int_size = get_byte(f);
    f->size_t = get_byte(f);
    f->instr_size = get_byte(f);
    f->l_number_size = get_byte(f);
    f->l_integral_flag = get_byte(f);

    printf("%02X\n", f->version);
    printf("%s\n", f->signature);

    // Affichage du contenu en hexadécimal (exemple)
    /*for (size_t i = 0; i < bytesRead; i++) {
        if (i < 4){
            printf("%02X", buffer[i]);
            if (i  == 3){
                printf("\t\t Header signature\n");
            }
        }else if (i == 4) {
            printf("%02X \t\t\t Version Lua %02X\n",buffer[i], buffer[i]);
        }
        else printf("%02X ", buffer[i]);

    }
    printf("\n");*/

    // Nettoyage
    free(buffer);
    fclose(file);

    return EXIT_SUCCESS;
}


