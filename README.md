# Compilateur Mini-Lustre vers LLVM

## Description

Projet réalisé dans le cadre du cours *Modèles et langages pour la programmation des systèmes réactifs*
du M1 MPRI. Il s'agit d'un compilateur d'un sous ensemble du langage Lustre (Mini-Lustre),
vers le langage intermédiaire LLVM.

## Usage

- Compilation
    ```bash
    $ make bin/filename.ll.exe
    $ make bin/filename.ml.exe
    ```

- Execution
    ```bash
    $ ./bin/filename.ll.exe
    $ ./bin/filename.ml.exe
    ```

## Explication générale

Nous partons donc d'un fichier *.mls* comme indiqué plus tôt, et utilisons le compilateur
vers OCaml sur lequel nous avons déjà travaillé, en TP.

Nous branchons donc notre travail au niveau du l'AST `imp_ast`.
Ensuite, grâce aux bindings offerts par la librairie OCaml de LLVM, nous contruisons
un fichier `.ll` à partir de cet AST.
