;;;; ================ Cidade operações ===================
(defun city-id (city)
  (first city))

(defun city-pos (city)
  (rest city))

;;; Devolve a distância entre duas cidades
(defun dist-IC (cidade1 cidade2)
  (sqrt (+ (expt (- (second cidade1) (second cidade2)) 2)
	   (expt (- (third cidade1) (third cidade2)) 2))))
