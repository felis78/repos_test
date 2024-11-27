;#################################################
;###                                           ###
;##                                             ##
;#                ROMAN NUMBERS                  #
;#  Basic conversion between roman and arabic    #
;#  numerotation. It does not check the syntax   #
;#  while converting from roman, but when        #
;#  converting to roman it cannot generate       #
;#  more than 3 identical consecutive symbols    #
;#  (except obviously for M [1000], till a 5000  #
;#  symbol is proposed by an old roman guy ;op   #
;#                                               #
;##  (c) 2017-2019 MICHAUD Yannick & KsyNET     ##
;###                                           ###
;#################################################

;***************
;INITIALIZATIONS
;***************
;## Quick load
(needsLibraries '(Maths))
(require 'libs/objects '$nilplus_)

(if (existsClass 'Roman) ($nilplus_delClass 'Roman))
($nilplus_addClass 'Roman)
(Roman><value 0)

;#ROMAN VALUES
(Roman//RomanI)(RomanI<-value 1)
(Roman//RomanV)(RomanV<-value 5)
(Roman//RomanX)(RomanX<-value 10)
(Roman//RomanL)(RomanL<-value 50)
(Roman//RomanC)(RomanC<-value 100)
(Roman//RomanD)(RomanD<-value 500)
(Roman//RomanM)(RomanM<-value 1000)

(setq $roman_old_prefix (VARIABLE_NAME_PREFIX.Clone))
(register 'VARIABLE_NAME_PREFIX "$roman_")

;#CONVERSION FUNCTIONS
(undef romanToArab)
(defun romanToArab (s)
   (let s (split s 1))
   (let rez 0)
   (let vcur 0)
   (let vprev 0)
   (s.resetToFirst)
   (while (s.hasMoreElements)
      (let cur (s.nextElement))
;      (print "current is=" cur)
      (let r (+s 'Roman (upcase cur)))
      (cond ((r.isEmpty)
               (print "This roman symbol does not exist : " cur)
               0
            )
            (t
               (let vcur r:value)
;               (print "adding " r " " vcur)
               (let rez (+ rez vcur))
               (if (and (!= vprev 0) (< vprev vcur))
;                  (print "delete=>" rez " - " (* 2 vprev))
                  (let rez (- rez (* 2 vprev)))
               )
;               (print 'current " " vcur " previous " vprev)
;               (print "rez=" rez)
               (let vprev vcur)
            )
      )
   )
   rez
)

(undef arabToRoman)
(defun arabToRoman (N)
   (cond ((not (N.isNumber))
            (print "Error, " N " is not a number.")
            ""
         )
         (t
            (let s "")
            (let l (divideAndRemainder N 1000))
            (let s (fillstring (car l) 'M ))
            (let l (divideAndRemainder (cadr l) 900))
            (let s (+ s (fillstring (car l) 'CM)))
            (let l (divideAndRemainder (cadr l) 500))
            (let s (+ s (fillstring (car l) 'D)))
            (let l (divideAndRemainder (cadr l) 490))
            (let s (+ s (fillstring (car l) 'XD)))
            (let l (divideAndRemainder (cadr l) 100))
            (let s (+ s (fillstring (car l) 'C)))
            (let l (divideAndRemainder (cadr l) 90))
            (let s (+ s (fillstring (car l) 'XC)))
            (let l (divideAndRemainder (cadr l) 50))
            (let s (+ s (fillstring (car l) 'L)))
            (let l (divideAndRemainder (cadr l) 40))
            (let s (+ s (fillstring (car l) 'XL)))
            (let l (divideAndRemainder (cadr l) 10))
            (let s (+ s (fillstring (car l) 'X)))
            (let l (divideAndRemainder (cadr l) 9))
            (let s (+ s (fillstring (car l) 'IX)))
            (let l (divideAndRemainder (cadr l) 5))
            (let s (+ s (fillstring (car l) 'V)))
            (let l (divideAndRemainder (cadr l) 4))
            (let s (+ s (fillstring (car l) 'IV)))
            (let s (+ s (fillstring (cadr l) 'I)))
         )
   )
   s
)

;# setting help
(setUserHelp "romanToArab" (+ "Usage: romanToArab {M|D|C|L|X|V|I}" crlf tab "Converts the parameter to arab numerals"))
(setUserHelp "arabToRoman" (+ "Usage: arabToRoman N" crlf tab "Convert N to roman representation"))

;## END
(register 'VARIABLE_NAME_PREFIX old_prefix)
(unset $roman_old_prefix)

################################################
(print "Roman numbers loaded.")
(makeQuickLoad 'lrom scriptFile)
################################################
