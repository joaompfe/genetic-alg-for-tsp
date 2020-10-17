
(defun ER (tsp1 tsp2)
  (ER-logic (edgeMap tsp1 tsp2) (randomCidade-TSP tsp1) tsp1 0)
  (ER-logic (edgeMap tsp1 tsp2) (randomCidade-TSP tsp2) tsp2 0))

(defun ER-logic (edge-map city tsp i)
  (trocar-cidades-TSP tsp i (indexOfCidade-TSP tsp city))
  (removeOcurrencesOfCity edge-map city)
  (let ((connected-cities (connectedCities edge-map city))
	(next-city nil))
  (if connected-cities
      (ER-logic edge-map
		(randomCityFromCitiesWithFewestEntries edge-map connected-cities)
		tsp
		(1+ i))
      (progn
	(setf next-city (randomCityFromNotVisitedCities edge-map))
	(if next-city
	    (ER-logic edge-map next-city tsp (1+ i)))))))

(defun edgeMap (tsp1 tsp2)
  (let ((edge-map (make-array (length-TSP tsp1))))
    (forEachCidade-TSP tsp1 #'populateEdgeMap-1 edge-map tsp1)
    (forEachCidade-TSP tsp2 #'populateEdgeMap-2 edge-map tsp2)
    edge-map))

(defun populateEdgeMap-1 (city i edge-map tsp1)
  (declare (ignore i))
  (setConnectedCities edge-map city (list (getCidadeAnterior-TSP tsp1 city)
					  (getCidadeSeguinte-TSP tsp1 city))))

(defun populateEdgeMap-2 (city i edge-map tsp2)
  (declare (ignore i))
  (let ((previous-city (getCidadeAnterior-TSP tsp2 city))
	(next-city (getCidadeSeguinte-TSP tsp2 city)))
    (if (not (isConnectedCity edge-map city previous-city))
	(addConnectedCity edge-map city previous-city))
    (if (not (isConnectedCity edge-map city next-city))
	(addConnectedCity edge-map city next-city))))

(defun randomCityFromCitiesWithFewestEntries (edge-map cities)
  (let ((cwfe (citiesWithFewestEntries edge-map cities)))
    (nth (random (length cwfe)) cwfe)))

(defun citiesWithFewestEntries (edge-map cities)
  (let ((cities-wfe nil)
	(n-entries 5)    ; todas têm menos que 5
	(n 0))
    (dolist (city cities)
      (setf n (nConnectedCities edge-map city))
      (cond ((< n n-entries)
	     (setf cities-wfe (list city))
	     (setf n-entries n))
	    ((= n n-entries)
	     (setf cities-wfe (append (list city) cities-wfe)))))
    cities-wfe))

(defun randomCityFromNotVisitedCities (edge-map)
  (let ((nvc (notVisitedCities edge-map)))
    (and nvc (nth (random (length nvc)) nvc))))

(defun notVisitedCities (edge-map)
  (let ((not-visited-cities nil))
    (do ((i (1- (length edge-map)) (1- i)))
	((< i 0))
      (if (not (aref edge-map i))
	  (setf not-visited-cities (cons i not-visited-cities))))
    not-visited-cities))

(defun removeOcurrencesOfCity (edge-map city)
  (dolist (connected-city (connectedCities edge-map city))
    (removeFromConnectedCities edge-map connected-city city)))

(defun connectedCities (edge-map city)
  (aref edge-map (1- (city-id city))))

(defun nConnectedCities (edge-map city)
  (length (connectedCities edge-map city)))

(defun addConnectedCity (edge-map city new-city)
  ;(break "add1")
  (setf (aref edge-map (1- (city-id city)))
	(append (list new-city) (aref edge-map (1- (city-id city)))))
					;(break "add2")
  )

(defun setConnectedCities (edge-map city cities)
  (setf (aref edge-map (1- (city-id city))) cities))

(defun isConnectedCity (edge-map city city-test)
  (position city-test (connectedCities edge-map city)))

(defun removeFromConnectedCities (edge-map city remove-city)
  (let ((connected-cities (connectedCities edge-map city)))
    (setf (aref edge-map (1- (city-id city))) (remove remove-city connected-cities))))

(defun emptyConnectedCities (edge-map city)
  (setf (aref edge-map (city-id city)) nil))

(defun printEdgeMap (edge-map)
  (do ((i (1- (length edge-map)) (1- i)))
      ((< i 0))
    (format t "~A ~A ~%" i (entriesIds (aref edge-map i)))))

(defun entriesIds (entries)
  (if (endp entries)
      nil
      (cons (first (first entries)) (entriesIds (rest entries)))))
