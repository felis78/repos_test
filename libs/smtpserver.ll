;(register 'stackTrace true)
(loadLibrary 'Security)
(setq Message 'mail.Message)
(setq Mail 'mail.Mail)
(setq mailDirectory 'mail)
(if (existsVariable 'smtpSocketServer) (smtpSocketServer.close))
(threadObject '$nil_smtpServer)
(addField '$nil_smtpServer 'socket)
(addMethod '$nil_smtpServer (quote (run ()
  (let rcptcmd "RCPT TO:")
  (let fromcmd "FROM :")
  (let datacmd "data")
  (let smtpSocketServer nil)
  (while (== smtpSocketServer nil)
    (let smtpSocketServer (server smtpport))
    (setq smtpport (+ smtpport 1))
  )
  (while t 
    (let s (accept smtpSocketServer))
    (setField self 'socket s)
    (if (<> s nil)
      (let cs 0)
      (let request "")
      (let to "")
      (let from "")
      (let data "")
      (while (<> request "QUIT")
        (let request (sread s))
        (if (== (request.startsWith rcptcmd) true)
          (let to (after request rcptcmd))
          (let cs (Bor cs 1))
        )
        (if (== (request.startsWith fromcmd) true)
          (let from (after request fromcmd))
          (let cs (Bor cs 2))
        )
        (if (== (request.startsWith datacmd) true)
          (while (<> request ".")
            (let request (sread s))
            (let data (+ data request))
          )
          (let cs (Bor cs 4))
        )
      )
      (sclose s)
      (if (== cs 7)
        (let message (java Message from to (data.getByteArray) charEncoding))
        (message.setDate (+ (date) " " (time) ))
        (addMessageTo to message)
      )
    )
  )
)) )
(undef addMessageTo)
(defun addMessageTo (who m)
  (let fname (+ mailDirectory "/" who ".mail"))
  (let f (objectFile fname 'R))
  (if (== f nil)
    (let fname (+ mailDirectory "/unknown.mail"))
    (let f (objectFile fname 'R))
  )
  (if (<> f nil)
    (let mail (readObject f))
    (fclose f)
    (let f (objectFile fname 'W))
    (mail.addMessage m)
    (printObject f mail)
    (fclose f)
  )
)
(undef addMailBox)
(defun addMailBox()
  (read 'mailuser 'username:)
  (let fname (+ mailDirectory "/" mailuser ".mail"))
  (let f (objectFile fname 'R))
  (if (<> f nil)
    (fclose f)
    (print "The mailbox " mailuser " already exists")
  )
  (if (== f nil)
    (setq mailbox (java Mail mailuser (getKeyPair)))
    (let f (objectFile fname 'W))
    (printObject f mailbox)
    (fclose f)
  )
)
      
;(run '$nil_smtpServer)
;(print "smtp server started")