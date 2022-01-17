# Compilateur Mini-Lustre vers LLVM

## Description

Projet réalisé dans le cadre du cours *Modèles et langages pour la programmation des systèmes réactifs*
du M1 MPRI. Il s'agit d'un compilateur d'un sous ensemble du langage Lustre (Mini-Lustre),
vers le langage intermédiaire LLVM.

## Usage

Il faut commencer par installer LLVM sur votre machine.
Nous avons écrit un script qui devrait le faire pour vous : `./install.sh`.

Ensuite, pour utiliser ce que nous avons fait vous pouvez faire par exemple
`make bin/filename.ll.exe` pour compiler le fichier `example/filename.mls` en executable (par llvm).

Il est aussi toujours possible de compiler un fichier par OCaml, en changeant la "sous-extension"
`ll` en `ml`, càd :  `make bin/filename.ml.exe`.

Sinon, lancer la commande `make example/filename.ll` pour simplement compiler un fichier `mls` en `ll`.

## Explication générale

Nous partons donc d'un fichier *.mls* comme indiqué plus tôt, et utilisons le compilateur
vers OCaml sur lequel nous avons déjà travaillé, en TP.

Nous branchons donc notre travail au niveau du l'AST `imp_ast`.
Ensuite, grâce aux bindings offerts par la librairie OCaml de LLVM, nous contruisons
un fichier `.ll` à partir de cet AST.

## Requis

Pour pouvoir compiler le projet, il faut absolument avoir les librairies *llvm* disponible pour
OCaml. Vous pouvez les obtenir de la manièr suivante:
```bash
$ opam install llvm    # peut ne pas marcher
```

ou dans la racine de ce projet:
```bash
$ ./install.sh
```

Ce script compilera toutes les librairies llvm, et peut prendre beaucoup de temps.

## Listes exhausistives pour la compilation

```bash
$ git clone https://github.com/emilienlemaire/MiniLustreLLVM
$ cd MiniLustreLLVM
$ ./install.sh    #Si vous devez installer llvm
$ make
```
