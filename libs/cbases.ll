;#######################################################
;###                                                 ###
;##      DEALING WITH CONVERSION BETWEEN BASES        ##
;#                                                     #
;#  Simply check a function name to know its purpose   #
;#  needs libs/stlib                                   #
;#                                                     #
;##       (c) 202x MICHAUD Yannick & KsyNET           ##
;###                                                 ###
;#######################################################

;# INITIALIZATIONS
;# QuickLoad
(require 'libs/stdlib '$stdlib_)
(unset $cbases_loaded)
(setq $cbases_symbols '(0 1 2 3 4 5 6 7 8 9 A B C D E F G H I J K L M N O P Q R S T U V W X Y Z a b c d e f g h i j k l m n o p q r s t u v w x y z ))
#(setq $cbases_symbols (range 48 1024))

(undef binaryToDecimal)
(defun binaryToDecimal (bstring)
  (let bitsl (split bstring 1))
  (let lbl (length bitsl))
  (let rez 0)
  (for (let i 0) (< i lbl) (let i (+ i 1))
    (let rez (+ rez (* (nth (- lbl i) bitsl) (^ 2 i))))
  )
  rez
)
(undef stringToBinary)
(defun stringToBinary (ts)
  (let lts (split ts 1))
  (let lbl (length lts))
  (let rez "")
  (for (let i 1) (<= i lbl) (let i (+ i 1))
    (let tmp (ascii (nth i lts)))
    (let tmp (tmp.toBinConstString 16))
    (let rez (+s rez tmp))
  )
  rez
)
(undef binaryToString)
(defun binaryToString (bs)
  (let lts (split bs 16))
  (let lbl (length lts))
  (let rez "")
  (for (let i 1) (<= i lbl) (let i (+ i 1))
    (let tmp (nth i lts))
    (let rez (+ rez (char (binaryToDecimal tmp))))
  )
  rez
)
(undef decimalToBinary)
(defun decimalToBinary (n l?0)
   (if (not (n.isNumber)) (let n 0))
   (cond ((<= l 0)
           (n.toBinaryString)
         )
         (t
           (n.toBinConstString l)
         )
   )
)
(undef isAlpha)
(defun isAlpha (c)
   (or (and (>= c 'A) (<= c 'Z)) (and (>= c 'a) (<= c 'z)))
)
(undef convBase)
(defun convBase (n b1?10 b2?2)
   (let l (+ 1 (length $cbases_symbols)))
   (cond ((or  (not (b1.isNumber)) (not (b2.isNumber)))
            (print "convBase: The from/to bases must be numbers (n from to)")
            (let rez 0)
         )
         ((or (> b1 (+ l 1)) (> b2 (+ l 1)))
            (print "One of the bases exceeds " (+ l 1))
            (let rez 0)
         )
         (t 
            (let s (reverse (split (cond ((<= b1 36) (upcase n)) (t n)) 1)))
            (let i 1)
            (let nToTen 0)
            (let flag t)
            (s.resetToFirst)
            (while (and (s.hasMoreElements) flag)
              (let e (s.nextElement))
              (if (not (e.isNumber))
                 (cond ((isAlpha e)
                          (let e (- (where e $cbases_symbols) 1))
                       )
                       (t
                          (let e 0)(let flag nil)
                       )
                 )
              )
              (let nToTen (+ nToTen (* e i)))
              (let i (* i b1))
            )
            (let rez emptyString)
            (let q nToTen)
            (while (> q 0)
              (let dar (divideAndRemainder q b2))
              #               (print "dar=" dar)
              (let q (car dar))
              (let r (cadr dar))
              (let tmp_s (nth (incr r) $cbases_symbols))
              (let rez (+s (nth (incr r) $cbases_symbols) rez))
            )
         )
   )
#   (if (<= b2 10) (let rez (new Number rez)))
   rez
)


;#END
(print "Bases conversion loaded : (convBase number/string b1 b2)")
(makeQuickLoad 'lbase scriptFile)

