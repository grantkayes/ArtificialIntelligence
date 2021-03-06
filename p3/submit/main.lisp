(defun rewrite-not-and (formula)
  (cond
    ((atom formula) formula)
    ((= 1 (length formula)) formula)
    (t (let ((bindings (match formula '(not (and (? x) (? y))))))
          (cond ((not (null bindings))
                 (let ((phi (rewrite-not-and (second (first bindings))))
                       (psi (rewrite-not-and (second (second bindings)))))
                    (list 'or (list 'not phi) (list 'not psi))))
                ((eq 'not (car formula))
                  (list (car formula) (rewrite-not-and (second formula))))
                (t (list (car formula)
                         (rewrite-not-and (second formula))
                         (rewrite-not-and (third formula)))))))))

(defun rewrite-not-or (formula)
  (cond
    ((atom formula) formula)
    ((= 1 (length formula)) formula)
    (t (let ((bindings (match formula '(not (or (? x) (? y))))))
          (cond ((not (null bindings))
                 (let ((phi (rewrite-not-or (second (first bindings))))
                       (psi (rewrite-not-or (second (second bindings)))))
                    (list 'and (list 'not phi) (list 'not psi))))
                ((eq 'not (car formula))
                  (list (car formula) (rewrite-not-or (second formula))))
                (t (list (car formula)
                         (rewrite-not-or (second formula))
                         (rewrite-not-or (third formula)))))))))

(defun rewrite-not-not (formula)
  (cond
    ((atom formula) formula)
    ((= 1 (length formula)) formula)
    (t (let ((bindings (match formula '(not (not (? x))))))
          (cond ((not (null bindings))
                 (list (rewrite-not-not (second (first bindings)))))   
                ((eq 'not (car formula))
                  (list (car formula) (rewrite-not-not (second formula))))
                (t (list (car formula)
                          (rewrite-not-not (second formula))
                          (rewrite-not-not (third formula)))))))))

(defun rewrite-formula (formula)
	(cond
		((atom formula) formula)
		((= 1 (length formula)) formula)
		((null formula) formula)
		(t (rewrite-not-or (rewrite-not-not (rewrite-not-and (rewrite-conds formula)))))))

(defun prove (formula))

(defun example1 ()
  (let* ((wffs '((cond p q)
                 (cond q r)
                 ((not r))
                 (p)))       ; negated conclusion
         (clauses (rewrite-formulas wffs)))
    (format t "Original formulas: ~a~%" wffs)
    (format t "Formulas in CNF: ~a~%" clauses)
    (prove clauses)))

(defun example2 ()
  (let* ((wffs '((cond (not hf) w)
                 (cond (not w) (not brd))
                 (cond (not (and brd (not w))) hf)
                 (cond (not (or (not hf) (not brd))) (not brch))
                 (brd)
                 (brch)))    ; negated conclusion
         (clauses (rewrite-formulas wffs)))
    (format t "Original formulas: ~a~%" wffs)
    (format t "Formulas in CNF: ~a~%" clauses)
    (prove clauses)))