#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct fichier{
    int index;
    unsigned char* signature;
    unsigned char version;
    unsigned char format;
    unsigned char endian;
    unsigned char int_size;
    unsigned char size_t;
    unsigned char instr_size;
    unsigned char l_number_size;
    unsigned char l_integral_flag;
}*fichier;

unsigned char get_byte(fichier f, const unsigned char* buffer){
    unsigned char b = buffer[f->index];
    f->index = f->index+1;
    return b;
}

unsigned char* get_string(fichier f, const unsigned char* buffer, int size){
    unsigned char s[size];
    for (int i = 0; i < size; i++) {
        s[i] = buffer[(i + f->index)];
    }
    f->index = f->index + size;
    return s;
}

fichier createFichier(){
    return (fichier) malloc(sizeof(fichier));
}

void libererFichier(fichier f){
    free(f);
}

int main (){
    fichier f;
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

    f->index = 0;
    f->signature = get_string(f, buffer, 4);
    f->version = get_byte(f, buffer);
    f->format = get_byte(f, buffer);
    f->endian = get_byte(f, buffer);
    f->int_size = get_byte(f, buffer);
    f->size_t = get_byte(f, buffer);
    f->instr_size = get_byte(f, buffer);
    f->l_number_size = get_byte(f, buffer);
    f->l_integral_flag = get_byte(f, buffer);

    printf("%02X\n", f->version);

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


