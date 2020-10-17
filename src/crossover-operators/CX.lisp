(defun CX (tsp1 tsp2)
  (CX-cycle tsp1 tsp2 (make-array (length (TSP-lst tsp1))) nil))

;; Função recursiva, vai alternando o trocar entre T e NIL.
;; O array-de-controlo serve para saber que índices dos TSP já foram processados pelo CX-operator.
(defun CX-cycle (tsp1 tsp2 array-de-controlo trocar)
  (let ((nexti (getNextIndex-CX array-de-controlo)))
    (if nexti
	(if trocar
	    (progn
	      (CX-operador-trocar tsp1 tsp2 array-de-controlo nexti)
	      (CX-cycle tsp1 tsp2 array-de-controlo nil))
	    (progn
	      (CX-operador-nao-trocar tsp1 tsp2 array-de-controlo nexti)
	      (CX-cycle tsp1 tsp2 array-de-controlo t)))
	(list tsp1 tsp2))))

;; Dá o menor índice do(s) TSP(s) que ainda não foi processado pelo CX-operador 
(defun getNextIndex-CX (ac)
  (do ((i 0 (1+ i))
       (len (length ac)))
      ((or (= i len) (null (aref ac i))) (if (/= i len)
					     i))))

(defun CX-operador-trocar (tsp1 tsp2 ac i)
  (let ((nexti (indexOfCidade-TSP tsp1 (getCidade-TSP tsp2 i))))
    (trocar-cidades tsp1 tsp2 i)
    (setf (aref ac i) t)
    (dolist (idx (CX-operador-aux tsp1 tsp2 ac nexti (getCidade-TSP tsp2 i)))
      (trocar-cidades tsp1 tsp2 idx))))

(defun CX-operador-nao-trocar (tsp1 tsp2 ac i)
  (let ((nexti (indexOfCidade-TSP tsp1 (getCidade-TSP tsp2 i))))
    (CX-operador-aux tsp1 tsp2 ac nexti (getCidade-TSP tsp2 i))))

(defun CX-operador-aux (tsp1 tsp2 ac i cidade)
  (setf (aref ac i) t)
  (if (/= (first (getCidade-TSP tsp2 i)) (first cidade))
      (cons i (CX-operador-aux tsp1 tsp2 ac (indexOfCidade-TSP tsp1 (getCidade-TSP tsp2 i)) cidade))
      (cons i nil)))

