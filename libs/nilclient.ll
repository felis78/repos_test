(setq vmo (NIL.getOrig))
(if (!= vmo "null")
  (setq vmo (vmo.getOrig))
)
(defun socketClosed()
  (addVMVariable (NIL.getOrig) 'users (removeNth (where nom (showusers))))
  (disconnect)
)
(undef showusers)
(defun showusers () (vmo.getVariable 'users))
(undef msg)
(defun msg (m) (vmo.broadcast (+ "#message de " nom ": " m crlf)) (+ "message envoye" crlf))
(undef msgTo)
(defun msgTo (w m)
  (let users "")
  (if (<> (type vmo) 'String)
    (let users (vmo.getVariable 'users))
  )
  (cond ((contains users w)
          (sprint (getVMProp vmo w "socket")  (+ "#message de " nom ": " m crlf) )
          (sprint (getVMProp vmo w "socket") "? ")
          (+ "message envoye a " w)
         )
         (t
            (+ "L'utilisateur " w " n'existe pas.")
         )
  )
)

;exitfunction (see nilclient.ini
(undef disconnect)
(defun disconnect ()
  (print 'disconnecting)
  (let sock (getVMProp vmo nom 'socket))
  (evalVM vmo (list 'setq (quote users) (list 'remove (list 'quote nom) (quote users))))
  (rmVMProp vmo nom 'socket)
  (vmo.broadcast (+ nom " se deconnecte." crlf "?" crlf))
  (sock.close)
)
(register 'errOnStderr nil)
(print "nilclient.ll loaded")
