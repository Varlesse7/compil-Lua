# Nom de l'exécutable
EXEC = test

# Compilateur
CC = gcc

# Options de compilation
CFLAGS = -Wall -Wextra -I lib

# Fichiers sources et objets
SRCS = main.c ./lib/fichierToStruct.c ./lib/constante.c ./lib/instruction.c ./lib/liste.c ./lib/chunk.c
OBJS = $(SRCS:.c=.o)

# Règle par défaut
all: $(EXEC)

# Compilation de l'exécutable
$(EXEC): $(OBJS)
	$(CC) $(OBJS) -o $(EXEC)

# Compilation des fichiers objets
%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

# Nettoyage des fichiers objets et de l'exécutable
clean:
	rm -f $(OBJS) $(EXEC)
