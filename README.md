Instructions
------------

This project uses ASDF build system.

### Linux
1. Put the project folder at ~/common-lisp/ so that ASDF finds it automatically. If there's no ~/common-lisp folder create it before. At terminal, type:
   `mkdir -p ~/common-lisp; cd ~/common-lisp`
   `git clone git@github.com:joaompfe/genetic-alg-for-tsp.git`
   `cd genetic-alg-for-tsp`
2. Launch CLISP or other Common Lisp implementation.
3. Run `(require "asdf")`.
4. Run `(asdf:load-system :std-genetic-alg-test)` to load both *std-genetic-alg* and *std-genetic-alg-test*; or to load only *std-genetic-alg* run `(asdf:load-system :std-genetic-alg)`.
5. All functions and global variables (some TSPs) are now loaded and ready to use. Run `(AG berlin52 200 #'OX1 :max-iter 1000)` to optimize berlin52 using a population of 200 individuals and the OX1 crossover operator during 1000 iterations. To execute a complete test with 10 Monte Carlo runs for each combination, run `(test 10)`.

### CLISP on Windows
Launch CLISP and try `(require "asdf")`. If it fails download [asdf source code](https://common-lisp.net/project/asdf/archives/asdf.lisp) and put it in the project folder. Then:

2. Launch CLISP in project folder.
3. Run `(require "asdf")`.
4. Run `(asdf:load-system :std-genetic-alg-test)` to load either *std-genetic-alg* and *std-genetic-alg-test*.; or to load only *std-genetic-alg* run `(asdf:load-system :std-genetic-alg)`.
5. All functions and global variables (some TSPs) are now loaded and ready to use. Run `(AG berlin52 200 #'OX1 :max-iter 1000)` to optimize berlin52 using a population of 200 individuals and the OX1 crossover operator during 1000 iterations. To execute a complete test with 10 Monte Carlo runs for each combination, run `(test 10)`.


Customization
-------------
### AG function
###### Necessary arguments:
- `tsp` - TSP fonte.
- `pop-size`  - Tamanho da população.
- `cross-func` - Função de crossover.
###### Key arguments:
- `max-iter` - Máximo de iterações.
- `converge` - Número máximo de iterações sem optimizar.
- `mutation-prob` - Probabilidade de mutação para cada TSP.
- `elitism` - Número de TSPs que passam diretamente para a próxima geração.

### test function
###### Necessary arguments:
- `n` - Number of tests for each parameter combination.

Source code should be edited for better customization of this function.
