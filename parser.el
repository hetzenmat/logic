(require 'logic-lexer)
(require 'logic-transformations)

(defconst operator-precedence '(true 6
				     false 6
				     identifier 6
				     negation 5
				     and 4
				     or 3
				     implication 2
				     equivalence 1
				     xor 1))

(defconst operator-strings '(and "&" or "|" implication "->" equivalence "<->" xor "<~>"))

(defun parse (input)
  (catch 'error
    (when (stringp input) (setq input (lex input)))
    (cl-destructuring-bind (result rest) (parse-equivalence input)
      (if rest (format-message "Could not fully parse input (rest: %S)" rest)
	(simplify-and/or result)
      ))))

(defun pretty-print (tree)
  (pcase tree
    (`(identifier ,var) (symbol-name var))
    ('(true) "$true")
    ('(false) "$false")
    (`(negation ,arg) (concat "~" (pretty-print arg)))
    ((and `(,op . ,args)
	  (guard (memq op binary-connectives)))
     (let ((prec (plist-get operator-precedence op)) arg-strings)
       (dolist (arg args)
	 (if (> (plist-get operator-precedence (car arg)) prec)
	     (push (pretty-print arg) arg-strings)
	   (push (concat "(" (pretty-print arg) ")") arg-strings)))
       (string-join (reverse arg-strings) (concat " " (plist-get operator-strings op) " "))))
    (_ (error "Wrong format. Given: %S" tree))))

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
  (pcase tokens
    (`((true) . ,tail) (list '(true) tail))
    (`((false) . ,tail) (list '(false) tail))
    (`((identifier ,id) . ,tail) (list (list 'identifier id) tail))
    (`((negation) . ,tail)
     (cl-destructuring-bind (result rest) (parse-subformula tail)
       (list (list 'negation result) rest)))
    (`((lpar) . ,tail)
     (cl-destructuring-bind (result rest) (parse-equivalence tail)
       (unless (eq (caar rest) 'rpar) (throw 'error "Parens did not match"))
       (list result (cdr rest))))
    (_ (throw 'error "Subformula did not succeed"))))

(provide 'logic-parser)
