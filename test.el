(require 'logic-lexer)
(require 'logic-parser)
(require 'logic-transformations)

(setq result (parse "~(a|b|d|f) <-> c -> ~dd"))

(message "%S" (transform-nnf result))
		     
