Instructions
------------

For loading std-genetic-alg or std-genetic-alg-test ASDF system should be used.

### Linux
1. Put the project folder in ~/common-lisp/ in order to ASDF find projects systems. If there's no ~/common-lisp folder create it before.
2. Launch CLISP or other Common Lisp implementation.
4. Run `(require "asdf")`.
5. Run `(asdf:load-system :std-genetic-alg-test)` to load either std-genetic-alg and std-genetic-alg-test. To load only std-genetic-alg run `(asdf:load-system :std-genetic-alg)`.
6. All functions and global variables (some TSPs) are loaded and ready to use. Run `(AG berlin52 200 #'OX1 :max-iter 1000)` to optimze berlin52 using 200 individuals as population and OX1 as crossover operator during 1000 iterations. To execute a complete test with 10 repetions for each combination run `(test 10)`.

### CLISP on Windows
Launch CLISP and try `(require "asdf")`.
If it fails:
1. Download [asdf source code](https://common-lisp.net/project/asdf/archives/asdf.lisp) and put it in project folder.
2. Launch CLISP in project folder.
3. Run `(require "asdf")`.
4. Run `(asdf:load-system :std-genetic-alg-test)` to load either std-genetic-alg and std-genetic-alg-test. To load only std-genetic-alg run `(asdf:load-system :std-genetic-alg)`.
5. All functions and global variables (some TSPs) are loaded and ready to use. Run `(AG berlin52 200 #'OX1 :max-iter 1000)` to optimze berlin52 using 200 individuals as population and OX1 as crossover operator during 1000 iterations. To execute a complete test with 10 repetions for each combination run `(test 10)`.


Customization
-------------
### AG function
###### Necessary arguments:
- tsp           - TSP fonte.
- pop-size      - Tamanho da população.
- cross-func    - Função de crossover.
###### Key arguments:
- max-iter      - Máximo de iterações.
- converge      - Número máximo de iterações sem optimizar.
- mutation-prob - Probabilidade de mutação para cada TSP.
- elitism       - Número de TSPs que passam diretamente para a próxima geração.

### test function
###### Necessary arguments:
- n           - Number of tests for each paramter combination.

Source code should beedited to better customize this test function.
