;;; ================== ESTRUTURA TSP E FUNÇÕES DE MANIPULAÇÃO =====================

;(load "city.lisp" :external-format 'charset:iso-8859-1)

;;; Estrutura TSP, com os campos lst (array de cidades) e dist (distância circular).
;;; O array lst-map serve para acelerar a procura no array lst. Para encontrar um
;;; elemento no array lst basta (aref lst (aref lst-map (1- ELEMENTO-ID))), onde
;;; ELEMENTO-ID é o id da cidade que se quer encontrar no array lst.
(defstruct
  (TSP
   ;(:constructor nil)        ; não tem o constructor default
   (:constructor make-TSP    ; tem consctructor que recebe um TSP como argumento. o TSP pode ser uma lista ou um array
		 (lst &aux (lst (cond ((arrayp lst) lst)
				      ((listp lst) (make-array (length lst) :initial-contents lst))
				      (t (error "Invalid argument type. Pass an array or a list as constructor argument."))))
		      (dist (distCircular-IC lst))
		      (lst-map (do ((i (1- (length lst)) (1- i))
				    (arr (make-array (length lst))))
				   ((< i 0) arr)
				 (setf (aref arr (1- (city-id (aref lst i)))) i)))))
   (:print-function printById-TSP)
   (:copier nil))
  lst dist lst-map)

;;; @index int Índice no TSP onde se pretende colocar a cidade. 0 é a primeira posição.
(defun setCidade-TSP (TSP cidade index &optional (clean nil) (updateMap t))
  (if clean
      (setf (aref (TSP-lst-map TSP) (1- (city-id (getCidade-TSP TSP index)))) nil)) ; apagar no lst-map a correspondência do elemento que vai ser substituído
  (setf (aref (TSP-lst TSP) index) cidade)                        ; alterar tsp
  (if updateMap
      (setf (aref (TSP-lst-map TSP) (1- (city-id cidade))) index)))    ; alterar tsp-map

;;; @index int Índice da cidade que se quer.
(defun getCidade-TSP (TSP index)
  (aref (TSP-lst TSP) index))

;;; @cidade int|ICidade Id da cidade ou ICidade cujo index no TSP se pretende encontrar.
(defun indexOfCidade-TSP (TSP cidade)
  (aref (TSP-lst-map TSP) (1- (if (numberp cidade)
				  cidade                ; cidade é o id
				  (city-id cidade)))))  ; cidade é a list ICidade. (first cidade) é o index

(defun getCidadeAnterior-TSP (TSP cidade)
  (let ((i (indexOfCidade-TSP TSP cidade)))
    (cond ((zerop i) (getCidade-TSP TSP (1- (length (TSP-lst TSP))))) ; retornar última cidade
	  (t (getCidade-TSP TSP (1- i))))))

(defun getCidadeSeguinte-TSP (TSP cidade)
  (let ((i (indexOfCidade-TSP TSP cidade)))
    (cond ((= i (1- (length (TSP-lst TSP)))) (getCidade-TSP TSP 0))   ; retornar primeira cidade
	  (t (getCidade-TSP TSP (1+ i))))))

(defun setMapOfCity (TSP city value)
  (setf (aref (TSP-lst-map TSP) (1- (city-id city))) value))

(defun length-TSP (TSP)
  (length (TSP-lst TSP)))

;;; Recalcular a distância e atualizar o TSP-dist 
(defun updateDist-TSP (TSP)
  (setf (TSP-dist TSP) (distCircular-IC (TSP-lst TSP))))

(defun updateLstMap-TSP (tsp)
  (let ((lst-map (TSP-lst-map tsp)))
    (forEachCidade-TSP tsp
		       (lambda (city i)
			 (setf (aref lst-map (1- (city-id city))) i)))))

;;; Lista as cidades do TSP mostrando apenas o seu id e a distância no fim.
(defun printById-TSP (TSP stream depth)
  (declare (ignore depth))
  (do ((i 0 (1+ i))
       (stop (1- (length (TSP-lst TSP))))
       (tsp-lst (TSP-lst TSP)))
      ((= i stop) (format stream "~A    Dist: ~A~%" (city-id (aref tsp-lst i)) (TSP-dist TSP)))
    (format stream "~A-" (city-id (aref tsp-lst i)))))

;;; Cria um TSP a partir dum path para um ficheiro.
(defun make-TSP-ficheiro (path-para-ficheiro)
  (make-TSP (tsp2IC (readTSPLisp path-para-ficheiro))))

;;; Distância circular das cidades.
;;; @cidades array
(defun distCircular-IC (cidades)
  (do ((i (- (length cidades) 2) (1- i))
       (dist 0)
       (cidade-anterior (aref cidades (1- (length cidades)))))
      ((< i 0) (+ dist (dist-IC (aref cidades 0)
				(aref cidades (1- (length cidades))))))
    (setf dist (+ dist (dist-IC cidade-anterior (aref cidades i))))
    (setf cidade-anterior (aref cidades i))))

(defun tsp2IC (l)
  (cond ((null l) nil)
        (t (cons (list (first l) (second l) (third l))
                 (tsp2IC (rest (rest (rest l))))))))

(defun readTSPLisp (file)
  (with-open-file (stream file)
		  (read stream)))

;;; Fisher–Yates shuffle
(defun baralhar-TSP (TSP)
  (do ((i (1- (length (TSP-lst TSP))) (1- i))
       (r 0)
       (temp nil))
      ((< i 0))
    (setf temp (getCidade-TSP TSP i))
    (setf r (random (1+ i)))
    (setCidade-TSP TSP (getCidade-TSP TSP r) i)
    (setCidade-TSP TSP temp r)))

(defun TSP-len (TSP)
  (length (TSP-lst TSP)))

(defun deep-copy-TSP (TSP)
  (let ((novo-tsp (make-TSP (copy-seq (TSP-lst TSP)))))
    (setf (TSP-lst-map novo-tsp) (copy-seq (TSP-lst-map TSP)))
    novo-tsp))

(defun trocar-cidades-TSP (tsp i1 i2)
  (let ((temp (getCidade-TSP tsp i1)))
    (setCidade-TSP tsp (getCidade-TSP tsp i2) i1)
    (setCidade-TSP tsp temp i2)))

(defun trocar-cidades (tsp1 tsp2 index)
  (let ((temp (getCidade-TSP tsp1 index)))
    (setCidade-TSP tsp1 (getCidade-TSP tsp2 index) index)
    (setCidade-TSP tsp2 temp index)))

(defun randomCidade-TSP (tsp)
  (let ((tsp-lst (TSP-lst tsp)))
    (aref tsp-lst (random (length tsp-lst)))))

(defun forEachCidade-TSP (TSP func &rest args)
  (apply #'forEachCidadeInRange-TSP (append (list TSP func 0 (1- (length-TSP TSP))) args)))

(defun forEachCidadeInRange-TSP (TSP func inf sup &rest args)
  (do ((i sup (1- i))
       (tsp-lst (TSP-lst TSP)))
      ((< i inf))
    (apply func (append (list (aref tsp-lst i) i) args))))

