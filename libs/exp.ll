;##############################################
;###                                        ###
;##        EXPONENTIAL LN & NthROOT          ##
;#                                            #
;#     NIL version of e^n, n√x                #
;#                                            #
;##  (c) 2017-2019 MICHAUD Yannick & KsyNET  ##
;###                                        ###
;##############################################

(needsLibraries '(LIB.Maths))
(require 'libs/utils '$utils_)

(unset $exp_loaded)
(undef $exp_compare_prec)
(defun $exp_compare_prec (n1 n2 p)
;(print "n1=" n1 crlf "n2=" n2)
   (cond ((and (contains n2 'E) (contains n1 'E))
            (let m1 (first n1 p))
            (let m2 (first n2 p))
            (let rez (and (== m1 m2) (== (after n1 'E) (after n2 'E))))
         )
         ((and (contains n2 '.) (contains n1 '.))
            (let m1 (first n1 (+  p (length (before n1 '.)) 1)))
            (let m2 (first n2 (+  p (length (before n2 '.)) 1)))
            (let rez (and (== (before n1 ".") (before n2 ".")) (== m1 m2)))
         )
         (t
            (let rez nil)
         )
   )
   rez
)
(undef qExp)
;n must be an integer
(defun qExp (n)
   (^ e n)
)
(undef qNthRoot)
(defun qNthRoot (n m p?30)
   (let nn (m.simplify))
   (let P ((m.getRational).getP))
   (let Q ((m.getRational).getQ))
   (let Pr (setPrecision (qExp (/p (ln P) n p)) p))
   (let Qr (setPrecision (qExp (/p (ln Q) n p)) p))
   (/p P Q p)
)
(undef spow)
(defun spow (n p pr?20)
   ;uses n^p=exp^(p*ln(n))
   (exp (* p (log n pr)) pr)
)
(undef nthRoot)
(defun nthRoot (n m p?10)
   (cond ((not (areNumbers (list n m p)))
            (print "Parameter(s) for nthRoot must be numbers")
            (let rez 0)
         )
         (t
;  Using the relation nthRoot(m)=exp(ln(m)/n)
            (let flag nil)
            (cond ((< m 0)
                           (print "The nth root of a negative number is complex, not supported yet.")
                  )
                  (t
                     (let nn (m.simplify))
                     (let P ((m.getRational).getP))
                     (let Q ((m.getRational).getQ))
                     (setPrecision (let Pr (exp (/p (log (setPrecision P p) p) n p) p)) p)
                     (setPrecision (let Qr (exp (/p (log (setPrecision Q p) p) n p) p)) p)
                     (let rez (/p Pr Qr p))
                  )
            )
         )
   )
   rez
)
;##        IMPROVED SQUARE ROOT FUNCTION       ##

;(undef $sq_U)
;(defun $sq_U (a Un Vn)
;   (+ Un (* Vn (- a (^ Un 2))))
;)
;(undef $sq_V)
;(defun $sq_V (Un Vn)
;   (- (* Vn 2) (* 2 Un (^ Vn 2)))
;)
;(undef square)
;(defun square (n p?10 u?0)
;   (if (< u (/p n 10 p)) (let u (/p n 2 p)))
;   (let u1 0)
;   (let v (/p 1 (* u 2) p))
;   (while (not ($exp_compare_prec u u1 p))
;      (let u1 u)
;      (let u (setPrecision ($sq_U n u v) p))
;      (let v (setPrecision ($sq_V u1 v) p))
;   )
;   u
;)

(defun squareRoot (n p?20)
  (let nn (n.simplify))
  (let P ((nn.getRational).getP))
  (let Q ((nn.getRational).getQ))
  (/p (nthRoot 2 P p) (nthRoot 2 Q p) p)
)
;***********************************
;(setUserHelp 'exp (+ "Usage: exp N [P]" crlf tab "Returns the exponential of N, calculated with an iterative loop, with precision P (default is 20)"))
(setUserHelp 'nthRoot (+ "Usage: nthRoot N M [P]" crlf tab "Returns the Nth root of M, with precision P (default is 10)" crlf tab "Please notice that the result could turn around the correct number"))
;(setUserHelp 'log (+ "Usage: log N [P]" crlf tab "Return the neperian logarithm of N [with precision P (default is 20)" ))
(setUserHelp 'squareRoot (+ "Usage: squareRoot N [PREC]" crlf tab "Computes the square root of N with by default precision of 10 significative digits"))

(print "Improved nthRoot loaded.")
(makeQuickLoad 'llog scriptFile)
