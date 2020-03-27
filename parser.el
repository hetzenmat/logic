(require 'logic-lexer)
(require 'logic-transformations)

(defun parse (input)
  (catch 'error
    (when (stringp input) (setq input (lex input)))
    (cl-destructuring-bind (result rest) (parse-equivalence input)
      (if rest (format-message "Could not fully parse input (rest: %S)" rest)
	(simplify-and/or result)
      ))))

(defun parse-equivalence (tokens)
  (unless tokens (throw 'error "Tokens are empty"))
  (cl-destructuring-bind (result rest) (parse-implication tokens)
    (cond
     ((memq (caar rest) '(equivalence xor))
      (cl-destructuring-bind (operand rest2) (parse-implication (cdr rest))
	(list (list (caar rest) result operand) rest2)))
     (t (list result rest)))))

(defun parse-implication (tokens)
  (unless tokens (throw 'error "Tokens are empty"))
  (cl-destructuring-bind (result rest) (parse-and/or tokens 'or)
    (cond
     ((eq (caar rest) 'implication)
      (cl-destructuring-bind (operand rest2) (parse-and/or (cdr rest) 'or)
	(list (list 'implication result operand) rest2)))
     (t (list result rest)))))

(defun parse-and/or (tokens connective)
  (unless tokens (throw 'error "Tokens are empty"))
  (let* (add-op
	 (result-pair (if (eq connective 'or) (parse-and/or tokens 'and) (parse-subformula tokens)))
	 (result (car result-pair))
	 (rest   (cadr result-pair))
	 (args))
    (push result args)
    (while (eq (caar rest) connective)
      (setq result-pair (if (eq connective 'or) (parse-and/or (cdr rest) 'and) (parse-subformula (cdr rest)))
	    rest (cadr result-pair)
	    add-op t)
      (push (car result-pair) args))
    (list (if add-op (cons connective (reverse args)) result) rest)))

(defun parse-subformula (tokens)
  (unless tokens (throw 'error "Tokens are empty"))
  (let ((first (caar tokens)))
    (cond
     ((memq first '(true false)) (list first (cdr tokens)))
     ((eq first 'identifier)
      (list (car tokens) (cdr tokens)))
     ((eq first 'negation)
      (cl-destructuring-bind (result rest) (parse-subformula (cdr tokens))
	(list (list 'negation result) rest)))
     ((eq first 'lpar)
      (cl-destructuring-bind (result rest) (parse-equivalence (cdr tokens))
	(unless (eq (caar rest) 'rpar) (throw 'error "Parens did not match"))
	(list result (cdr rest))))
     (t (throw 'error "Subformula did not succeed")))))

(provide 'logic-parser)