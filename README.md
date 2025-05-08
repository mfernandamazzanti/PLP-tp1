# Functional Programming Project – Recursive Data Structures (UBA)

This Haskell project was developed for the "Programming Paradigms" course at the University of Buenos Aires (FCEN, UBA).  
It defines and processes various recursive data structures using **higher-order functions**, **folds**, and **custom processors**.

## 🔧 Key Features

- Definition and processing of three recursive data structures:
  - **Ternary Trees (`AT`)**
  - **Rose Trees (`RoseTree`)**
  - **Tries (`Trie`)**
- Implementation of structural traversals (`preorder`, `inorder`, `postorder`) and processors (`procCola`, `procHijosAT`, etc.).
- **Recursive Folds**: Generic folds implemented for each data structure. 
- Functional combinators: `ifProc`, `(++!)`, and `(.!)` for composing processors.
- Includes **unit tests** using `HUnit`.
- **Correctness Proof**: A formal proof using induction and equational reasoning showing that `preorder` and `postorder` visit the same elements.

