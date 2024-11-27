;#################################################
;## TCLSH Script Executor                       ##
;## (c) 2017 MICHAUD Yannick & KsyNET           ##
;#################################################

;INITIALIZATIONS
(needsLibraries '(LIB.IO NIL.System))

(unset $tclSE_loaded)
(setq $tclSE_tclPath "C:/Tcl/bin/tclsh.exe")
(setq $tclSE_tclSourcePath "C:/ian/dev/tcl/scripts/")

;# MAIN FUNCTION
(undef execTcl)
(defun execTcl (fn type params...)
   (let fns (list fn (+ (env 'user.dir) '/ fn) (+ $tclSE_tclSourcePath fn)))
   (fns.resetToFirst)
   (let flag false)
   (while (and (fns.hasMoreElements) (not flag))
      (let fn (fns.nextElement))
      (let flag (fexists fn))
   )
   (cond    ((not (fexists fn))
               (print (+ "The tcl script file " fn " does not exist."))
               nil
            )
            (t
               (let commandp "")
               (if (not (params.isList))
                  (let params (split params " "))
               )
               (params.resetToFirst)
               (while (params.hasMoreElements)
                  (let commandp (+ commandp " " (params.nextElement)))
               )
               (let commandp (trim commandp))
               (let tcl_streams (exec $tclSE_tclPath fn commandp))
               (let tcl_output (cadr tcl_streams))
               (let line "")
               (readAll tcl_output type)
               
            )
   )
)

;# setting help
(setUserHelp "execTcl" (+ "Usage: execTcl filename type [parameters]" crlf tab "Launches tclsh with the script filename as parameter and returns the stdout content." crlf tab "The type parameter is t for an output as list, nil for raw strings."))

;# END
(print "Tcl Script executor loaded.")
