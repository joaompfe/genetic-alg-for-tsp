;(load "tsp.lisp" :external-format 'charset:iso-8859-1)

;;;; ======================== Métodos de Cruzamento =========================
;;; Todos os métodos de crossover alteram os pais in-place para dar origem aos filhos.

(defvar *crossover-operators-path* (make-pathname :directory '(:relative "crossover-operators")))

;;; Partially-mapped crossover (PMX)
;(load (make-pathname :name "PMX" :type "lisp" :defaults *crossover-operators-path*) :external-format 'charset:iso-8859-1)

;;; Cycle crossover (CX)
;(load (make-pathname :name "CX" :type "lisp" :defaults *crossover-operators-path*) :external-format 'charset:iso-8859-1)

;;; Order Crossover (OX1)
;(load (make-pathname :name "OX1" :type "lisp" :defaults *crossover-operators-path*) :external-format 'charset:iso-8859-1)

;;; Edge Recombination (ER)
;(load (make-pathname :name "ER" :type "lisp" :defaults *crossover-operators-path*) :external-format 'charset:iso-8859-1)

;;; Simple Random Crossover (SRC)
(defun SRC (tsp1 tsp2)
  (trocar-cidades-TSP tsp1 (random (length-TSP tsp1)) (random (length-TSP tsp1)))
  (trocar-cidades-TSP tsp2 (random (length-TSP tsp2)) (random (length-TSP tsp2))))

;;;; ==================  Generation of Initial Population ===================
(defun generate-population (source-tsp n)
  (do ((i (1- n) (1- i))
       (population (make-array n))
       (tsp (deep-copy-TSP source-tsp)))
      ((< i 0) population)
    (baralhar-TSP tsp)
    (setf (aref population i) (deep-copy-TSP tsp))))

;;;; ==================== Selection ==========================

;;; Array de 1/distâncias dos TSP na população. 
(defun population-fitnesses (population)
  (do ((i (1- (length population)) (1- i))
       (fitnesses (make-array (length population))))
      ((< i 0) fitnesses)
    (setf (aref fitnesses i) (/ 1 (TSP-dist (aref population i))))))

;;; Array de 1/distâncias dos TSP da população acumuladas.
;;; Destructive
(defun cumulative-fitnesses (fitnesses)
  (do ((i 0 (1+ i))
       (cumulative-fitness 0)
       (len (length fitnesses)))
      ((= i len) fitnesses)
    (setf cumulative-fitness (+ cumulative-fitness (aref fitnesses i)))
    (setf (aref fitnesses i) cumulative-fitness)))

(defun selection (source-pop k)
  (let* ((cumul-fits (cumulative-fitnesses (population-fitnesses source-pop)))
	 (fits-sum (aref cumul-fits (1- (length cumul-fits))))) ; o último elemento das fitnesses acumuladas é igual à soma total de todas as fitnesses)
    (do ((i (1- (length source-pop)) (1- i))
	 (rand (random fits-sum) (random fits-sum))
	 (selected-pop (copy-seq source-pop)))
	((< i k) (values selected-pop (/ fits-sum (length cumul-fits))))
      (setf (aref selected-pop i) (deep-copy-TSP (aref source-pop
						       (position-if (lambda (cumul-fit) (< rand cumul-fit)) cumul-fits)))))))

;;;; ==================== Crossover ========================

;;; Não protegido contra tamanhos de população ímpares
(defun crossover (population func k)
  (do ((i (1- (length population)) (- i 2)))
      ((< i k) population)
    (funcall func (aref population i) (aref population (1- i)))))

;;;; =================== Mutation =====================

(defun mutation (population mut-prob k)
  (do ((i (1- (length population)) (1- i)))
      ((< i k))
    (if (< (random 1.0) mut-prob)
	(progn
	  (trocar-cidades-TSP (aref population i)
			      (random (length (TSP-lst (aref population i))))
			      (random (length (TSP-lst (aref population i)))))
	  (trocar-cidades-TSP (aref population i)
			      (random (length (TSP-lst (aref population i))))
			      (random (length (TSP-lst (aref population i)))))
	  (trocar-cidades-TSP (aref population i)
			      (random (length (TSP-lst (aref population i))))
			      (random (length (TSP-lst (aref population i)))))
	  (trocar-cidades-TSP (aref population i)
			      (random (length (TSP-lst (aref population i))))
			      (random (length (TSP-lst (aref population i)))))
	  (trocar-cidades-TSP (aref population i)
			      (random (length (TSP-lst (aref population i))))
			      (random (length (TSP-lst (aref population i)))))))))

;;;; ====================== AG ========================

;;; Ordena por distâncias: menores primeiro.
(defun sort-population (population)
  (sort population
	(lambda (tsp1 tsp2)
	  (< (TSP-dist tsp1) (TSP-dist tsp2))))
  population)

(defun updatePopulationDists (population)
  (do ((i (1- (length population)) (1- i)))
      ((< i 0) population)
    (updateDist-TSP (aref population i))))

(defun print-population (pop)
  (do ((i 0 (1+ i)))
      ((= i (length pop)))
    (format t "~A " (TSP-dist (aref pop i))))
  (format t "~%~%"))

;;; tsp           TSP fonte.
;;; pop-size      Tamanho da população.
;;; cross-func    Função de crossover.
;;; max-iter      Máximo de iterações.
;;; converge      Número máximo de iterações sem optimizar.
;;; mutation-prob Probabilidade de mutação para cada TSP.
;;; elitism       Número de TSPs que passam diretamente para a próxima geração.
;;; stream        Stream de output para printar informações. Usado pela função de teste.
;;; prefix        Alguma informação a ser printada. Usado pela função de teste. 
(defun AG (tsp pop-size cross-func &key (max-iter -1) (converge -1) (mutation-prob -1) (elitism -1) (stream t) (prefix ""))
  ;; Tratamento de argumentos
  (if (and (< max-iter 0) (< converge 0))
      (setf max-iter 200))
  (if (< elitism 0)
      (setf elitism (round (* 0.04 pop-size))))
  (if (< mutation-prob 0)
      (setf mutation-prob 0.04))
  ;; Ciclo principal do algoritmo
  (do ((i max-iter (1- i))
       (population (sort-population (generate-population tsp pop-size)))
       (avg-fitness 0)        ; average fitness
       (best-avg-fitness 0)   ; melhor average fitness obtida até ao momento
       (convergence-counter converge (1- convergence-counter)))
      ((or (zerop i) (zerop convergence-counter))
       (sort-population population)
       (values (- max-iter i)
	       (aref population 0)))
    
    (setf (values population avg-fitness) (selection population elitism))
    (if (and (>= converge 0)
	     (> avg-fitness best-avg-fitness))
	     (progn
	       (setf best-avg-fitness avg-fitness)
	       (setf convergence-counter converge)))
    (crossover population cross-func elitism)
    (mutation population mutation-prob elitism)
    (updatePopulationDists population)
    (sort-population population)   ; ordenar por distâncias, menor primeiro. o selection espera que os TSPs estejam ordenados.
    (format t "~A ~A " #\return i)
    (format stream "~A~A, ~A, ~A~%" prefix i avg-fitness (TSP-dist (aref population 0)))))

