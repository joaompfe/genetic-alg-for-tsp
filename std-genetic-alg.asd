(defsystem std-genetic-alg
  :description "Standard genetic algorithm implementation."
  :author "João Fé <joaofe2000@gmail.com>"
  :version "0.0.1"
  :components ((:module "src"
			:components
			((:file "city")
			 (:file "tsp" :depends-on ("city"))
			 (:module "crossover-operators"
				  :depends-on ("tsp")
				  :components
				  ((:file "PMX")
				   (:file "CX")
				   (:file "ER")
				   (:file "OX1")))
			 (:file "std-ga" :depends-on ("crossover-operators"))))))
