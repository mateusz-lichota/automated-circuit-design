# Automated circuit design

A haskell program generating optimized circuits for Imperial Computing 40007 coursework 2 - combinatorial circuit design.
It can also output Digisim hardware.txt specification files, and LuaLaTeX code for a writeup document with a circuit diagram.


To use it, open `main.hs` in the `src` folder and modify the `main` function.
To run the code do `cabal run` (and pray that all dependencies will get installed correctly).
This will create `filled.tex` in the `writeup` folder, which needs to be compiled with LuaLaTeX.

If you have the computational resources, you can modify the implementation `createResultDatabase`
to increase the number of circuits tested and hopefully reduce the average circuit cost by finding
new and better candidates.
