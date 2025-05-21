# Installation

Pour éxécuter notre programme vous devrez lancer la commande : 

```
make
```

Celle ci vous génère de nombreux fichier mais vous aurez alors le fichier miniml_test qui est l'éxécutable vous pourrez alors faire : 

```
make clean
```

Celui ci supprimeras tout les fichier générer par la compilation du parser et de nos autre fichier à part notre éxécutable. 

# Lancer un programme

Vous pourrez alors maintenant lancer un de nos test dans le fichier ./tests en faisant : 

```
./miniml_test ./tests/test1.miniml
```

Vous pourrez alors directement voir le résultat du compilateur CAM et de l'interpréteur OCAML dans le terminal tandis que pour l'Eclat vous devrez regarder le fichier code.ecl.