(defun if (a b c) (If ((eval a).isTrue) (eval b) (return)) (eval c))
(addAlias NilFunctions 'setq 'set!)
(setq #t t)
(setq #f nil)

