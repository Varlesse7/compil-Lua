CC = gcc
CFLAGS = -I ./lib
SRC = main.c ./lib/fichierToStruct.c ./lib/constante.c ./lib/instruction.c ./lib/liste.c ./lib/chunk.c ./lib/registre.c
OBJ = $(SRC:.c=.o)
EXEC = prog

all: $(EXEC)

$(EXEC): $(OBJ)
	$(CC) $(OBJ) $(CFLAGS) -o $(EXEC)

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

debug: CFLAGS += -g
debug: clean $(EXEC)

clean:
	rm -f $(OBJ) $(EXEC)

.PHONY: all debug clean