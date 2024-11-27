;################################################
;###                                          ###
;##              FILE SIPLIFIER                ##
;#                                              #
;#                                              #
;##   (c) 2011-2017 MICHAUD Yannick & SibIan   ##
;###                                          ###
;################################################

(needsLibraries '(LIB.IO))
(require 'libs/utils '$utils_ )


;INITIALIZATIONS
(setq $simplifier_old_prefix (VARIABLE_NAME_PREFIX.Clone))
(register 'VARIABLE_NAME_PREFIX "$simplifier_")
(unset loaded)

(setq listOfSigns "+%$~•§°·*`0123456789;:,«»\"'!?()[]@&#{}=-_./\\<>")
(setq keepAccents false)
(setq dicoFileName 'D:/ian/nil/dico_file.txt)

;MAIN FUNCTIONS
(undef loadText)
(defun loadText (name)
  (if (== name nil) (let name "dico.txt"))
  (let infile (file name 'R))
  (let content (makelist (downcase (freadContent infile))))
  (fclose infile)
   content
)
(undef parseContent)
(defun parseContent (name)
  (let content (loadText name))
  (let outfile (file (newExtension name 'txt) 'W) )
  (print (+ "opening " (outfile.getName) " for writing output"))
  (let signs (split $simplifier_listOfSigns 1))
  (let limit (length signs))
   (signs.resetToFirst)
  (while (signs.hasMoreElements)
    (let content (replace content (signs.nextElement) " "))
  )
  (let content (replace content tab " "))
  (let content (replace content cr " "))
  (let content (replace content lf " "))
  (let content (replace content (ascii 133) " "))
  (cond  ((not keepAccents)
      (let content (replace content "é" "e"))
      (let content (replace content "è" "e"))
      (let content (replace content "ê" "e"))
      (let content (replace content "â" "a"))
      (let content (replace content "ë" "e"))
      (let content (replace content "î" "i"))
      (let content (replace content "â" "a"))
      (let content (replace content "ô" "o"))
      (let content (replace content "ù" "u"))
      (let content (replace content "û" "u"))
      (let content (replace content "à" "a"))
      (let content (replace content "ç" "c"))
    )
  )
    
  (print "replace ended -- starting sort")
  (let content (bbsort (split content " ")))

  (print "sorting ended -- starting 'unique")
  (let content (unique content))

  (print "-- writing")
  (fprintContent outfile content)
  (fclose outfile)
  (print "siplifying achieved -- wrote to " (outfile.getFile))
)
;## END
(setq loaded t)
(register 'VARIABLE_NAME_PREFIX old_prefix)
(unset $simplifier_old_prefix)
(print "File simplifier loaded.")
