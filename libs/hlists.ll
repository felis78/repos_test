;################################################
;###                                          ###
;##                HUGE LISTS                  ##
;#    (up to 4611686014132420609 elements       #
;#                                              #
;##     (c) 2019 MICHAUD Yannick & KsyNET      ##
;###                                          ###
;################################################


;INITIALIZATIONS
(needsLibraries '(LIB.Maths) )
(require 'libs/stdlib '$stdlib_)
(require 'libs/utils '$utils_)
(require 'libs/java '$java_)
(require 'libs/objects '$nilplus_)

(makeQuickLoad 'lhl scriptFile)
(if (existsClass 'HList) ($nilplus_delClass 'HList))
($nilplus_addClass 'HList)
(setq $hlist_max Integer.MAX_VALUE)
(HList><content '(()))
(HList><Car '$hlist_car)
(HList><Cdr '$hlist_cdr)
(HList><Cons '$hlist_cons)

(defun $hlist_new (hlist)
  (let mi (+ 1 HList:maxInstances))
  (let name (+ "$hl_" mi))
  ($nilplus_instantiate 'HList name)
  (name<-content hlist)
  name
)
(defun $hlist_cons (e hlist)
  (cond   (((car hlist:content).isEmpty)
      ($hlist_new (list (list e)))
    )
    ((== (length (car hlist:content)) $hlist_max)
      ($hlist_new (cons (list e) hlist:content))
    )
    (t
      ($hlist_new (cons (cons e (car hlist:content)) (cdr hlist:content)))
    )
  )

)
(defun $hlist_car (hlist)
  (cond   (((car hlist:content).isEmpty)
      nil
    )
    (t 
      (car (car hlist:content))
    )
  )
)
(defun $hlist_cdr (hlist)
  (cond   (  ((car hlist:content).isEmpty)
      nil
    )
    (t 
      (let fl (car hlist:content))
      (cond  ((== (length fl) 1)
          (cdr hlist:content)
        )
        (t  (let newlist '())
          (let lmax (length hlist:content))
          (for (let i lmax) (>= i 2) (let i (- i 1))
            (let newlist (cons (nth i hlist:content) newlist))
          )
          (let newlist (cons (cdr (car hlist:content)) newlist))
          ($hlist_new newlist)
        )
      )
    )
  )
)


