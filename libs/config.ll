####################################################
###                                              ###
##          DYNAMIC CONFIGURATION SCRIPT          ##
#                                                  #
#  Core functions to load the nil.ini file which   #
#  defines what have to be set when NIL starts     #
#  It automatically loads the library IO while     #
#  loading the file                                #
#                                                  #
##   (c) 2002-2018 MICHAUD Yannick & ksynet       ##
###                                              ###
####################################################

#UPPER LEVEL COMMAND FILE

# MAIN FUNCTIONS
(undef $exit_function (ef)
  (register 'exitFunction ef)
)
(undef $treat_parameters()
  (mapcar '$treat_param NilParams)
)
(undef $treat_param (p)
  (let pl (split p "="))
  (if (== (length pl) 2)
    (setqv (car pl) (cadr pl))
  )
)
(undef $configValue (v)
   (if (and (== (first v) '%) (== (last v) '%))
      (let v (before (after v '%) '%))
      (if (v.mayBeList)
         (let v (makelist v))
      )
      (let v (eval v))
   )
   v
)
(undef $load_config ()
  (print "Loading configuration : " inifilename)
  (let INIT "")
  (let inifile (IO.file inifilename 'R))
  (if (== inifile nil)
    (print "configuration file " inifilename " not found....")
    (return -1)
  )
  (let iniline (freadline inifile))
  (while (<> iniline eof)
    (if (and (<> iniline nil) (> (length iniline) 0) (<> (nth 1 iniline) "#"))
      (let inilist (split iniline " "))
      (setqv (car inilist) ($configValue (cadr inilist)))
    )
    (let iniline (freadline inifile))
  )
  (fclose inifile)
  (if (existsVariable 'INITS)
    (mapcar (lambda (v) ($treat_variable v) ) (split INITS ";"))
    (unset INITS)
  )
)
(undef prog (name)
   (cond   ((not (load (+ 'prgs/ name)))
                 (print (+ 'prgs/ name) " does not exist, or loading failed.") nil
    )
    (t name)
  )
)

(undef $register (s)
   (let l (split s "="))
   (let v ($configValue (last l)))
   (register (first l) v)
#   (print (+ (first l) "=" v))
)

(undef $treat_variable (v)
  (let lv (split v "="))
        (let vname (car lv))
  (if (and (<> lv nil) (existsVariable vname))
    (mapcar (cadr lv) (split (eval vname) ";"))

                (eval (cons 'unset (list vname)))
  )
)
(undef $evaluate_string (c)
  (mapcar 'eval (makelist (list (replace c '\\ '/))))
)

;ian own functions for exist -- sample code
(undef $ian_bye (l...)
   (register 'inheritLocals t)
   (if (atom l) (let l (list l)))
   (let $ian_bl l)
   (mapcar   (lambda (f) (cond
        ((not ($ian_l.isEmpty))
          (eval (append f $ian_bl))
        )
        (t
          (eval f)
        )
      )
    )
    $ian_bye_commands_list
  )
  (print "-------" (char 13) (char 10) "Bye")
#  (((let r 'java.lang.Runtime).getRuntime).exit 0)
)
(undef $ian_addByeCommand (l)
   (if (not (existsVariable '$ian_bye_commands_list))
      (setq $ian_bye_commands_list nil)
   )
;   (if (l.isList)
;      (print "Mustn't be a list like ((print 'bye))")
;      (return "")
;   )
   (setq $ian_bye_commands_list (append $ian_bye_commands_list (list l)))
)
($ian_addByeCommand "(print \"Thank you for your interest...\")")


#TRAITEMENT DES PARAMETRES DE LANCEMENT
(if (not (existsVariable 'inifilename))
   (setq inifilename "nil.ini")
)
($treat_parameters)

#TRAITEMENT DU FICHIER DE CONFIGURATION
($load_config)
(print "\\lfcr\\*** Configuration loaded ***")

