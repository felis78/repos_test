;####################################
;##      Bindings for Kawa         ##
;##   (c) 2019-202x KsyNET & YM    ##
;####################################

(require 'libs/java '$java_)
(makeQuickLoad 'lkawa scriptFile)
(setq $clist 'gnu.lists.LList)
(defun clist (l) (gnu.lists.LList.makeList l))
(setq CLisp (gnu.commonlisp.lang.CommonLisp.getInstance))
(defun kawaShell () (kawa.GuiConsole.main nil))
(defun kawaGUI () (kawa.GuiConsole.main '(-w))))

(print "Quick kawa loaded.")
