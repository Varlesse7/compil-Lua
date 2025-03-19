CC = gcc
CFLAGS = -I ./lib -g
SRC = main.c ./lib/fichierToStruct.c ./lib/constante.c ./lib/instruction.c ./lib/liste.c ./lib/chunk.c
OBJ = $(SRC:.c=.o)
EXEC = test

all: $(EXEC)

$(EXEC): $(OBJ)
	$(CC) $(OBJ) $(CFLAGS) -o $(EXEC)

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -f $(OBJ) $(EXEC)

.PHONY: all clean