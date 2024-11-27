;#################################################
;## NIL MISC UTILITIES                          ##
;## (c) 2022 MICHAUD Yannick & KsyNET           ##
;#################################################


;INITIALIZATIONS
(register 'ra needsUndef)
(register 'needsUndef nil)

;#################################################
; MISCELLANEOUS FUNCTIONS
;#################################################

(undef stars (n?1)
   (let stars (makelist (+  "(" (fillstring n "\ | / - \ | / - ") ")" )))
   (mapcar (lambda (c) (NIL.broadcastC (+s backspace c)) (pause 10)) stars)
   ""
)
(threadObject '$utils_stars_waiting)
(addField '$utils_stars_waiting 'continue)
(setField '$utils_stars_waiting 'continue t)
(addMethod '$utils_stars_waiting '(run () (while (getField self 'continue) (stars))(NIL.broadcast crlf)))
(addMethod '$utils_stars_waiting '(stop () (NIL.broadcast _backSpace)(setField self 'continue nil)))
(defun loadAuth (distFile user pwd)
  (let tmp (getHttpsPageAuth distFile user pwd))
  (NIL.evaluate (list (makelist (trim tmp))))
)
(defun newBean(cb...) (new Bean NIL (cond (cb cb) (t (NIL.getBaseURL)))))

(undef execCommandLine (commandLine)
  (let r (Runtime.getRuntime))
  (r.exec commandLine)
)
(undef spaces (n)
   (fillstring n " ")
)
(undef hRule (n)
   (let s "0")
   (for (let i 1) (<= i n) (let i (incr i))
      (let dar (divideAndRemainder i 5))
      (cond ((== dar 1) let c "")
            ((== (cadr dar) 0)
               (let c '+)
               (if (even (car dar))
                        (let c (first (reverse i)))
               )
               (if (== (% i 100) 0)
                        (let c '0)
               )
            )
            (t
               (let c '-)
            )
      )
      (let s (+ s c) )
   )
   s
)
# Extract constant from NIST page
(undef extract (txt)
  (let st (after txt "Numerical value"))
  (let st (after st "<tt><b>&nbsp;"))
  (let st (before st "&nbsp;Standard uncertainty"))
  (let st (before st "</b></tt>"))
  (let st (before st (after st "</sup>")))
  (let st (before st "</sup>"))
  (let st (replace st "x&nbsp;10<sup>" "E"))
  (let st (replace PR "&nbsp;" ""))
  st
)
(undef champer (n)
  (let champerfile (file "champ.txt" 'W))
  (for (let i 1) (< i n) (let i (+ i 1)) (fprint champerfile i))
)
(undef fenetre (titre w h)
  (let ff (new JFrame titre))
   (ff.setSize w h)
  (ff.show)
  (ff.setAlwaysOnTop false)
  ff
)
(threadObject 'obj )
(addField 'obj 'fenetre)
(addMethod 'obj "(run ()
  (setField self 'fenetre (fenetre \"Objects count (every .75 sec)\" 500 350))
  (let fen (getField self 'fenetre))
   
   ;######################
   ;POUR LA NilVM en cours
   ;######################
  (let r (new Rectangle 10 50 400 100))
  (let text (new Label))
  (text.setBounds r)
  (text.setForeground (new Color 128 0 128))
  (text.setFont (new Font \"Arial\" 24 24))
  (text.setAlignment 1)
  (text.setText \"Total NilObjects = \")
  (fen.add text)

   ;####################
   ;POUR LA NilVM parent
   ;####################
  (let r (new Rectangle 10 150 400 150))
  (let text1 (new Label))
  (text1.setBounds 10 150 400 150)
  (text1.setForeground (new Color 0 0 0))
  (text1.setFont (new Font \"Arial\" 12 12))
  (text1.setAlignment 1)
  (text1.setText \"Total NilObjects (parent VM) = \")
  (fen.add text1)

   (let orig (NIL.getOrig)) 
   (while t
     (text.setText (+ \"Total NilObjects = \" (NIL.getNilObjectsNumber)))
     (text1.setText (+ \"Total NilObjects (parent VM) = \" (orig.getNilObjectsNumber)))
      (pause 750)
      (System.gc)
  )
)")
(addMethod 'obj "(fin () (invoke (getField self 'fenetre) 'dispose)(stop self))")

(undef run_obj ()
   (setq o (run 'obj))
)


(threadObject 'couleur 'coo)
(addField 'couleur 'fenetre)
(addMethod 'couleur "(run ()
  (setField self 'fenetre (fenetre \"Pour Aurore\" 900 600))
  (NIL.setOutput nil)
  (let fen (getField self 'fenetre))
  (let r (int (random 256)))
  (let g (int (random 256)))
  (let b (int (random 256)))
  (let w 0)
  (let text (new Label))
  (text.setBounds 10 50 450 125)
  (text.setForeground (new Color 0 0 0))
  (text.setFont (new Font \"Times\" 32 32))
  (text.setAlignment 1)
  (text.setText \"Aurore je t'aime\")
  (fen.add text)
  (text.display)
  (while t
    (let p (int (- 16 (random 32))))
    (let r (% (+ r p) 256))
    (let g (% (+ g (+ p 4)) 256))
    (let b (% (+ b (- p 4)) 256))
    (let color (new Color r g b))

    (let p (- p 64))
    (let r (% (- r p) 256))
    (let g (% (- g p) 256))
    (let b (% (- b p) 256))
    (let color2 (new Color r g b))

    (let coord (pol w))
    (let w (+ w 0.20))
    (if (> w PI2) (let w (- w PI2)))
    (text.setBounds (car coord) (cadr coord) 300 50)
    (text.setForeground color)
    (text.setBackground color2)
    (fen.setBackground color2)
    (pause 200)
  )
)")
(addMethod 'couleur "(fin () (let fen (getField self 'fenetre)) 
(fen.dispose)
(stop self)
t
)")

(undef pol (w)
  (let x (+ 450 (* 450 (* (sin (* w 2)) (cos (* w 3))))))
  (let y (+ 300 (* 200 (sin (* w 2)))))
  (let rez (list y))
  (let rez (cons x rez))
  ;rez
)
    
;(setq coul (run 'couleur))
;(setq vf (coul.getTarget))
;(setq fen (vf.getField 'fenetre))
;(fen.setTitle "coucou")

(threadObject 'horl)
(addField 'horl 'fenetre)
(addMethod 'horl "(run ()
    (setField self 'fenetre (fenetre \"Digital Time\" 200 100))
    (let win (getField self 'fenetre))
    (let left (new Label))
    (left.setBounds 10 50 190 25)
    (left.setBackground Grey)
    (left.setForeground Red)
    (left.setFont (new Font \"Arial Black\" 24 24))
    (left.setAlignment 1)
    (win.setBackground Grey)
    (win.add left)
    (win.show)
    (win.setAlwaysOnTop false)
    (while t
      (left.setText (time))
      (pause 10)
    )
  )"
)
(addMethod 'horl "(fin () (let fen (getField self 'fenetre)) 
(fen.dispose)
(stop self)
t
)")

;## SEARCH FOR HELP (IN LOADED ONES) and REPORT MISSING ONES
;## => '(scanLibs)'

(undef scanLibs ()
  (mapcar 'scanLib libraries)
   (scanFunctions)
)
(undef scanFunctions ()
  (let ff (file "functions.missing" 'W))
  (mapcar 'testHelp nilfunctions "functions")
  (fclose ff)
)
(undef scanLib (l)
  (let ff (file (+ l ".missing") 'W))
  (fprint ff (mapcar 'testHelp (eval (+ l ".functions")) l))
  (fclose ff)
)
(undef testHelp (f l)
  (cond
    ((<> l "functions")
      (let msg (call l 'help f))
    )
    (t (let msg (help f)))
  )
  (cond
    ((== (length msg) 0)
      (+ "Missing help for " f (char 13))
    )
    (t ""
    )
  )
)
(addAlias DUMMY 'help 'man)

;#####################
(undef saveObject (filename?"object.obj" obj?t append?nil)
  (if (not (contains ((new File filename).getName) ".")) (let filename (+s filename ".obj")))
  (let f (objectFile filename (cond (append 'a)(t 'w))))
  (printObject f obj)
  (fclose f)
)
(undef loadObject (filename?"object.obj" vname?"objLoaded")
  (if (not (contains ((new File filename).getName) ".")) (let filename (+s filename ".obj")))
  (let f (objectFile filename 'r))
  (if (!= f nil)
    (let tmpObj (readObject f))
    (fclose f)
    (setqv vname tmpObj)
  )
)

;#################################################
;## END
;#################################################
(require 'libs/java '$java_)
(print "NIL utilities loaded.")
(makeQuickLoad 'lu scriptFile)

