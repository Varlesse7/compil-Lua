#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include "./lib/chunk.h"
#include "./lib/registre.h"


codeLua createFichier(){
    codeLua f = (codeLua) malloc(sizeof(struct codeLua));
    if (!f ){
        exit(1);
    }
    f->bytecode = NULL;
    return f;
}

void libererFichier(codeLua f){
    free(f->signature);
    free(f->bytecode);
    free(f);
    f = NULL;
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

    f->bytecode=buffer;
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

    printf("%s\n",f->bytecode);
    printf("%02X\n", f->version);
    printf("%s\n", f->signature);

    chunk c = decode_chunk(f);

    printf("\n");
    print_liste_instruction(c->instruction);

    printf("\n=== Exécution du chunk ===\n");
    execute_chunk(c);

     // Nettoyage
    libererChunk(c);
    libererFichier(f);
    fclose(file);
    return EXIT_SUCCESS;
}


