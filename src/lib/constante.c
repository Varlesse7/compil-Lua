#include "constante.h"
#include <stdlib.h>
#include <stdio.h>

constante createConstante(){
    return (constante) malloc(sizeof(struct constante));
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