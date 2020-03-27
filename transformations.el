(require 'logic-lexer)

(defun simplify-and/or (tree)
  (if (not (memq (car tree) binary-connectives)) tree
    (let* ((op (car tree))
	   (args (mapcar #'simplify-and/or (cdr tree)))
	   (result (list op)))
      (if (not (memq op '(and or)))
	  (cons op args)
	(dolist (arg args result)
	  (setq result (append result
			       (if (eq (car arg) op)
				   (cdr arg)
				 (list arg)))))))))

(defun rewrite-implication (tree)
  (if (not (memq (car tree) binary-connectives)) tree
  (let ((op (car tree)))
    (simplify-and/or
     (cond
     ((eq op 'implication)
      (list 'or (list 'negation (rewrite-implication (cadr tree))) (rewrite-implication (caddr tree))))
     (t
      (cons op (mapcar #'rewrite-implication (cdr tree)))))))))

(defun rewrite-equivalence (tree)
  (if (not (memq (car tree) binary-connectives)) tree
    (let ((op (car tree)))
      (simplify-and/or
       (cond
	((eq op 'equivalence)
	 (let ((left (rewrite-equivalence (cadr tree)))
	       (right (rewrite-equivalence (caddr tree))))
	   (list 'and (list 'or left (list 'negation right)) (list 'or (list 'negation left) right))))
	((eq op 'xor)
	 (let ((left (rewrite-equivalence (cadr tree)))
	       (right (rewrite-equivalence (caddr tree))))
	   (list 'and (list 'or left right) (list 'or (list 'negation left) (list 'negation right)))))
	(t
	 (cons op (mapcar #'rewrite-equivalence (cdr tree)))))))))

(defun caddadr (l)
  (car (cddadr l)))

(defun rewrite-negation (tree)
  (if (memq (car tree) 0-ary-connectives) tree
  (let ((op (car tree)))
    (simplify-and/or
     (cond
      ((eq op 'negation)
       (let* ((inner-tree (cadr tree))
	      (inner-op (car inner-tree))
	      (inner-args (cdr inner-tree)))
	 (cond
	  ((eq inner-op 'negation)
	   (rewrite-negation (car inner-args)))
	  ((not (memq inner-op binary-connectives)) tree)
	  ((eq inner-op 'implication)
	   (let ((left (car inner-args))
		 (right (cadr inner-args)))
	     (list 'and (rewrite-negation left) (rewrite-negation (list 'negation right)))))
	  ((memq inner-op '(equivalence xor))
	   (let ((left (car inner-args))
		 (right (cadr inner-args)))
	     (list (if (eq inner 'xor) 'equivalence 'xor) (rewrite-negation left) (rewrite-negation right))))
	  ((memq inner-op '(and or))
	   (cons (if (eq inner-op 'or) 'and 'or) (mapcar #'rewrite-negation (mapcar (lambda (l) (list 'negation l)) inner-args)))))))
      (t
       (cons op (mapcar #'rewrite-negation (cdr tree)))))))))
	 
(defun transform-nnf (tree)
  (thread-first
      tree
    rewrite-implication
    rewrite-equivalence
    rewrite-negation))

(provide 'logic-transformations)
