(progn (setf (get 'nice 'adj) '((marseille va-marseille 188)))
(setf (get 'marseille 'adj) '((nice va-nice 188) (avignon va-avignon 99)))
(setf (get 'avignon 'adj) '((grenoble va-grenoble 227) (marseille va-marseille 99) (montpellier va-montpellier 121) (lyon va-lyon 216)))
(setf (get 'grenoble 'adj) '((avignon va-avignon 227) (lyon va-lyon 104)))
(setf (get 'lyon 'adj) '((grenoble va-grenoble 104) (avignon va-avignon 216) (limoges va-limoges 389) (dijon va-dijon 192)))
(setf (get 'dijon 'adj) '((strasbourg va-strasbourg 335) (lyon va-lyon 192) (paris va-paris 313) (nancy va-nancy 201)))
(setf (get 'strasbourg 'adj) '((dijon va-dijon 335) (nancy va-nancy 145)))
(setf (get 'nancy 'adj) '((strasbourg va-strasbourg 145) (dijon va-dijon 201) (paris va-paris 372) (calais va-calais 534)))
(setf (get 'calais 'adj) '((nancy va-nancy 534) (paris va-paris 297) (caen va-caen 120)))
(setf (get 'caen 'adj) '((calais va-calais 120) (paris va-paris 241) (rennes va-rennes 176)))
(setf (get 'rennes 'adj) '((caen va-caen 176) (paris va-paris 348) (brest va-brest 244) (nantes va-nantes 107)))
(setf (get 'brest 'adj) '((rennes va-rennes 244)))
(setf (get 'paris 'adj) '((calais va-calais 297) (nancy va-nancy 372) (dijon va-dijon 313) (limoges va-limoges 396) (rennes va-rennes 348) (caen va-caen 241)))
(setf (get 'nantes 'adj) '((limoges va-limoges 329) (bordeaux va-bordeaux 329) (rennes va-rennes 107)))
(setf (get 'limoges 'adj) '((lyon va-lyon 389) (toulouse va-toulouse 313) (bordeaux va-bordeaux 220) (nantes va-nantes 329) (paris va-paris 396)))
(setf (get 'bordeaux 'adj) '((limoges va-limoges 220) (toulouse va-toulouse 253) (nantes va-nantes 329)))
(setf (get 'toulouse 'adj) '((montpellier va-montpellier 240) (bordeaux va-bordeaux 253) (limoges va-limoges 313)))
(setf (get 'montpellier 'adj) '((avignon va-avignon 121) (toulouse va-toulouse 240))))

(progn (setf (get 'nice 'coordinates) '((43 42 0 N) (7 21 0 E)))
(setf (get 'rennes 'coordinates) '((48 07 0 N) (1 02 0 W)))
(setf (get 'brest 'coordinates) '((48 24 0 N) (4 20 0 W)))
(setf (get 'strasbourg 'coordinates) '((48 32 24 N) (7 37 34 E)))
(setf (get 'dijon 'coordinates) '((47 21 0 N) (5 02 0 E)))
(setf (get 'lyon 'coordinates) '((45 44 0 N) (5 52 0 E)))
(setf (get 'grenoble 'coordinates) '((45 21 36 N) (5 19 12 E)))
(setf (get 'avignon 'coordinates) '((43 50 9 N) (4 45 0 E)))
(setf (get 'marseille 'coordinates) '((43 18 0 N) (5 25 0 E)))
(setf (get 'bordeaux 'coordinates) '((44 50 0 N) (0 37 0 W)))
(setf (get 'toulouse 'coordinates) '((43 37 0 N) (1 27 0 E)))
(setf (get 'montpellier 'coordinates) '((43 38 0 N) (3 53 0 E)))
(setf (get 'limoges 'coordinates) '((45 30 0 N) (1 10 0 E)))
(setf (get 'caen 'coordinates) '((49 15 0 N) (0 20 0 W)))
(setf (get 'calais 'coordinates) '((50 57 36 N) (1 57 0 E)))
(setf (get 'nancy 'coordinates) '((48 50 0 N) (6 10 0 E)))
(setf (get 'nantes 'coordinates) '((47 15 0 N) (1 30 0 W)))
(setf (get 'paris 'coordinates) '((48 51 0 N) (2 20 0 E))))

(defparameter *earth-radius* 6372.8)
(defparameter *radian-conversion* (/ pi 180))

(defun degrees-to-radian (degrees)
	(let ((actual-degrees (+ (/ (second degrees) 60) (/ (third degrees) 3600))))
		(* actual-degrees *radian-conversion*)))

(defun haversine-a (x)
  (* (sin (/ x 2)) (sin (/ x 2))))

(defun haversine-b (lat1 lat2 long1 long2)
  (let* ((a (haversine-a (- long2 long1))) (b (haversine-a (- lat2 lat1))))
    (*  2 (* *earth-radius* (asin (sqrt (+ b (* (cos lat1) (cos lat2) a))))))))

(defun latlong (lat1 lat2 long1 long2)
  (haversine-b (degrees-to-radian lat1) (degrees-to-radian lat2) (degrees-to-radian long1) (degrees-to-radian long2)))

(defun distance (city-one city-two)
	(let* ((east-cities '(nantes caen bordeaux brest rennes)))
		(if (or (member city-one east-cities) (member city-two east-cities))
			0
			(latlong (first (get city-one 'coordinates)) (second (get city-one 'coordinates)) (first (get city-two 'coordinates)) (second (get city-two 'coordinates))))))

(defun successors (city-state)
	(get city-state 'adj))
