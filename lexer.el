(defconst tokens '(("[[:space:]]*" ignore)
		   ("[[:alpha:]][[:alnum:]]*" identifier)
		   ("\\$true" true)
		   ("\\$false" false)
		   ("(" lpar)
		   (")" rpar)
		   ("\\(~\\|!\\)" negation)
		   ("&" and)
		   ("|" or)
		   ("\\(-\\|=\\)>" implication)
		   ("<\\(-\\|=\\)>" equivalence)
		   ("<~>" xor)))

(defconst binary-connectives '(and or implication equivalence xor))
(defconst 0-ary-connectives '(identifier true false))

(defun lex (input-string)
  (let (result
	longest
	current-tokens
	current-token
	current-match
	(original input-string)
	(position 0))
    (while (> (length input-string) 0)
      (setq current-tokens tokens
	    longest '("" nil))
      (while current-tokens
	(setq current-token (car current-tokens))
	(when (string-match (concat "^" (car current-token)) input-string)
	  (setq current-match (match-string-no-properties 0 input-string))
	  (when (> (length current-match) (length (cadr longest)))
	    (setq longest (list (cadr current-token) current-match))))
	(setq current-tokens (cdr current-tokens)))
      (if (= 0 (length (cadr longest)))
	  (throw 'error (format-message "No match at position %d (starting with '%s')" position (substring original (1- position))))
	(unless (eq 'ignore (car longest))
	  (if (eq 'identifier (car longest))
	      (push (list (car longest) (intern (cadr longest))) result)
	  (push (list (car longest)) result)))
	(setq position (+ position (length (cadr longest)))
	      input-string (substring input-string (length (cadr longest))))))
    (if (stringp result) result (reverse result))))

(provide 'logic-lexer)
