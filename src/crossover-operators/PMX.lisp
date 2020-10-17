;; (declaim (inline PMX))
;; (declaim (inline exchangeMappingSections))
;; (declaim (inline isCityInMappingSection))
;; (declaim (inline arrangeCitiesOutsideMappingSection))
;; (declaim (inline arrangeCity))
;; (declaim (inline alternativeCity))

(defun PMX (tsp1 tsp2)
  (let* ((rand-inf (round (/ (random (length-TSP tsp1)) 2)))
	 (rand-sup (random (1- (- (length-TSP tsp1) (round (/ (random (length-TSP tsp1)) 2))))))
	 (aux-structs (exchangeMappingSections tsp1 tsp2 rand-inf rand-sup)))
    (arrangeCitiesOutsideMappingSection tsp1 tsp2 (first aux-structs) (second aux-structs) rand-inf rand-sup)))

(defun exchangeMappingSections (tsp1 tsp2 inf sup)
  (let ((tsp1-aux-struct (make-array (length-TSP tsp1)))
	(tsp2-aux-struct (make-array (length-TSP tsp2))))
    (do ((i inf (1+ i)))
	((> i sup))
      (trocar-cidades tsp1 tsp2 i)
      (isCityInMappingSection tsp1-aux-struct (getCidade-TSP tsp1 i) i)
      (isCityInMappingSection tsp2-aux-struct (getCidade-TSP tsp2 i) i))
    (list tsp1-aux-struct tsp2-aux-struct)))

;; Se chamada com 2 argumentos retorna nil ou t conforme a cidade esteja na mapping section. Se for chamada com 3
;; argumento define se a cidade está na mapping section de acordo com o 3º argumento.
(defun isCityInMappingSection (tsp-aux-struct city &optional (value nil value-supplied-p))
  (if value-supplied-p
      (setf (aref tsp-aux-struct (1- (first city))) value)
      (aref tsp-aux-struct (1- (first city)))))

(defun arrangeCitiesOutsideMappingSection (tsp1 tsp2 aux1 aux2 inf sup)
  (forEachCidadeInRange-TSP tsp1 #'arrangeCity (1+ sup) (1- (length-TSP tsp1)) tsp1 tsp2 aux1 aux2)
  (forEachCidadeInRange-TSP tsp1 #'arrangeCity 0 (1- inf) tsp1 tsp2 aux1 aux2)
  (forEachCidadeInRange-TSP tsp2 #'arrangeCity (1+ sup) (1- (length-TSP tsp2)) tsp2 tsp1 aux2 aux1)
  (forEachCidadeInRange-TSP tsp2 #'arrangeCity 0 (1- inf) tsp2 tsp1 aux2 aux1))

(defun arrangeCity (city i main-tsp other-tsp main-aux other-aux)
  (if (isCityInMappingSection main-aux city)
      (setCidade-TSP main-tsp (alternativeCity city main-tsp other-tsp main-aux other-aux) i t)))

(defun alternativeCity (city main-tsp other-tsp main-aux other-aux)
  (let ((i (isCityInMappingSection main-aux city)))
    (if i
	(alternativeCity (getCidade-TSP other-tsp i) main-tsp other-tsp main-aux other-aux)
	city)))


;;;; TESTING
(defun PMX-test-base (source-tsp)
  (let ((tsp1 (deep-copy-TSP source-tsp))
	(tsp2 (deep-copy-TSP source-tsp)))
    (do ((i 1000 (1- i)))
	((zerop i) 'SUCCESS)
      (format t "~C ~A" #\return (- 10000 i))
      (baralhar-TSP tsp1)
      (baralhar-TSP tsp2)
      (PMX tsp1 tsp2))))

(defun PMX-test-tsp-integrity (tsp)
  (do ((i (1- (length-TSP tsp)) (1- i))
       (tsp-lst (TSP-lst tsp)))
      ((< i 0) 'SUCCESS)
    (do ((j (1- (length-TSP tsp)) (1- j)))
	((< j 0))
      (if (and (/= i j) (= (first (aref tsp-lst i)) (first (aref tsp-lst j))))
	  (break "TSP invalid")))))

(defun PMX-test-predifined ()
  (PMX (make-TSP '((1 5 5) (2 5 5) (3 5 5 ) (4 5 5) (5 5 5) (6 5 5) (7 5 5) (8 6 6))) (make-TSP '((3 2 3) (7 4 5) (5 5 3) (1 4 5) (6 3 4) (8 5 2) (2 3 2) (4 3 4)))))
