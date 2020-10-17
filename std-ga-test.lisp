;; (declaim (optimize (speed 3)
;; 		   (compilation-speed 0)
;; 		   (safety 0)
;; 		   (debug 0)))

;(load "std-ga.lisp" :external-format 'charset:iso-8859-1)

(defvar *tsps-path* (make-pathname :directory '(:relative "tsp-euc-2d-usable")))

(defvar berlin52 (make-TSP-ficheiro (make-pathname :name "berlin52.tsp" :type "lisp" :defaults *tsps-path*)))
(defvar eil51 (make-TSP-ficheiro (make-pathname :name "eil51.tsp" :type "lisp" :defaults *tsps-path*)))
(defvar eil76 (make-TSP-ficheiro (make-pathname :name "eil76.tsp" :type "lisp" :defaults *tsps-path*)))
(defvar pr76 (make-TSP-ficheiro (make-pathname :name "pr76.tsp" :type "lisp" :defaults *tsps-path*)))
(defvar rat99 (make-TSP-ficheiro (make-pathname :name "rat99.tsp" :type "lisp" :defaults *tsps-path*)))

(defun tspString (tsp)
  (cond ((eq tsp berlin52) "berlin52")
	((eq tsp eil51) "eil51")
	((eq tsp eil76) "eil76")
	((eq tsp pr76) "pr76")
	((eq tsp rat99) "rat99")
	(t (error "tspString"))))

(defun crossoverString (co)
  (cond ((eq co #'PMX) "PMX")
	((eq co #'CX) "CX")
	((eq co #'ER) "ER")
	((eq co #'OX1) "OX1")
	(t (error "crossoverString"))))

(defun test (n)
  (let ((tsps (list berlin52 eil51 eil76 pr76 rat99))
	(pop-sizes (list 100))
	(crossover-methods (list #'PMX))
	(time0 0)
	(time1 0)
	(iterations 0)
	(result-tsp nil))
    (with-open-file (stream0 (format nil "geral.csv" )
			     :direction :output
			     :if-exists :append
			     :if-does-not-exist :create)
      (dolist (pop-size pop-sizes)
	(dolist (cm crossover-methods)
	  (dolist (tsp tsps)
	    (with-open-file (stream1 (format nil "pop~A-~A-~A.csv" pop-size (crossoverString cm) (tspString tsp))
				     :direction :output
				     :if-exists :append
				     :if-does-not-exist :create)
	      (do ((i n (1- i)))
		  ((zerop i))
		(format t "pop~A-~A-~A ~A~%" pop-size (crossoverString cm) (tspString tsp) i)
		(setf time0 (get-internal-run-time))
		(setf (values iterations result-tsp)
		      (AG tsp pop-size cm
			  :max-iter 2500
			  :stream stream1
			  :prefix (format nil "~A, " i)))
		(setf time1 (get-internal-run-time))
		(format stream0 "~A, ~A, ~A, ~A, ~A, ~A, ~A~%"
			pop-size
			(crossoverString cm)
			(tspString tsp)
			i
			iterations
			(TSP-dist result-tsp)
			(* 1.0 (/ (- time1 time0) internal-time-units-per-second)))))))))))
