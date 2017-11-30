(defun min-max-i (numbers)
	(do* ((min (car numbers) (if (< number min) number min))
		(max (car numbers) (if (> number max) number max))
		(number (car numbers) (car numbers))
		(numbers (cdr numbers) (cdr numbers)))
	((null numbers) (list min max)))

(defun min-max-r-aux (numbers min max)
   (cond 
   	((null numbers) (list min max))
   	(t (let ((number (car numbers)))
     (if (< number min) (setf min number))
     (if (> number max) (setf max number))
     (list (min-max-r-aux (cdr numbers) min max))))))

(defun flat (flatlist)
  (cond ((null flatlist) nil)
        ((atom flatlist) (list flatlist))
        (t (loop for a in list appending (flat a)))))   

(defun min-max-r (numbers)
   (cond ((null numbers) (list '())))
   (let ((min (car numbers)) (max (car numbers)))
     (flat (min-max-r-aux (cdr numbers) min max))))

(defun min-max-a (numbers)
	(list (apply 'min numbers) (apply 'max numbers)))




	
