;#################################################
;###                                           ###
;##                    LOGO                     ##
;#                                               #
;#                                               #
;##     (c) 2019 MICHAUD Yannick & KsyNET       ##
;###                                           ###
;#################################################

(undef NilLogo)
(defun NilLogo ()
  (cond   ((and isLinux (not (contains (NIL.getOwner) 'Bean)))
      (let f (file 'libs/NIL.banner 'R))(print (readAll f))(fclose f)
      (unlet f)
    )
    (t (NilLogoW))
  )
)

;# MAIN FUNCTIONS
(undef NilLogoW)
(defun NilLogoW ()
   (print "NNNNNNNN        NNNNNNNIIIIIIIIILLLLLLLLLLL")
   (print "N:::::::N       N::::::I::::::::L:::::::::L")
   (print "N::::::::N      N::::::I::::::::L:::::::::L")
   (print "N:::::::::N     N::::::II::::::ILL:::::::LL")
   (print "N::::::::::N    N::::::N I::::I   L:::::L")
   (print "N:::::::::::N   N::::::N I::::I   L:::::L")
   (print "N:::::::N::::N  N::::::N I::::I   L:::::L")
   (print "N::::::N N::::N N::::::N I::::I   L:::::L")
   (print "N::::::N  N::::N:::::::N I::::I   L:::::L")
   (print "N::::::N   N:::::::::::N I::::I   L:::::L")
   (print "N::::::N    N::::::::::N I::::I   L:::::L")
   (print "N::::::N     N:::::::::N I::::I   L:::::L         LLLLLL")
   (print "N::::::N      N::::::::II::::::ILL:::::::LLLLLLLLL:::::L")
   (print "N::::::N       N:::::::I::::::::L::::::::::::::::::::::L")
   (print "N::::::N        N::::::I::::::::L::::::::::::::::::::::L")
   (print "NNNNNNNN         NNNNNNIIIIIIIIILLLLLLLLLLLLLLLLLLLLLLLL")
   (print crlf)
)


