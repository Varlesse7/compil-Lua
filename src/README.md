# Installation 

Pour compiler le programme vous avez uniquement à vous situer dans le dossier src et éxécuter la commande : 

```
make
```

Si vous voulez lancer ce code avec le mode débug vous pouvez faire :
```
make debug
```

---
Après avoir fait le make vous pouvez éxécuter le programme avec un fichier Lua binaire en faisant : 

```
./prog <nom du fichier avec .out>
```

Si vous voulez compiler un fichier lua vous devez faire: 
```
luac -o ./test/test_print.lua test.out
```

Avec notre exemple nous pourrons alors écrire: 
```
./prog test.out
```

---

Si vous voulez supprimer tout les fichiers créer vous pouvez faire : 

```
make clean // Ne supprime pas les fichiers générer par lua
```