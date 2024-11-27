(mapcar (lambda (c n) (addOp (char c) (eval (list 'defun (+ 'pow n) '(x) (list 'pow 'x n) )))) (mapcar 'ascii (split "²³⁴⁵⁶⁷⁸⁹" 1)) (range 2 9))
(addAlias 'm '√ 'sqrt)

