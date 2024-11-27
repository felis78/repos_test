;################################################
;###                                          ###
;##        IMPROVED SQUARE ROOT FUNCTION       ##
;#                                              #
;#         needs Maths library                  #
;#                                              #
;##   (c) 2017-2019 MICHAUD Yannick & SibIan   ##
;###                                          ###
;################################################


;INITIALIZATIONS
(needsLibraries '(LIB.Maths))
(needsLL '((libs/stdlib $stdlib_)))

;MAIN FUNCTIONS
(undef $sq_U)
(defun $sq_U (a Un Vn)
   (+ Un (* Vn (- a (^ Un 2))))
)
(undef $sq_V)
(defun $sq_V (Un Vn)
   (- (* Vn 2) (* 2 Un (^ Vn 2)))
)
(undef sq)
(defun sq (n prec?10 u?0)
   (if (< u (/p n 10 prec)) (let u (/p n 2 prec)))
   (let v (/p 1 (* u 2) prec))
   (let r "")
   (let rezt (first n (+ prec 2)))
   (let reza u)
   (while (and (<> r "q") (<> reza rezt))
      (let rezt reza)
      (let u1 u)
      (let u ($sq_U n u v))
      (let v ($sq_V u1 v))
      (let reza (first u (+ prec 2)))
   )
   reza
)

(setUserHelp "sq" (+ "Usage: sq N [PREC [U0]]" crlf tab "Computes the square root of N with by default precision of 10 significative digits" crlf tab "and starting with U0. If U0<N/10 then it is set to N/2."))
