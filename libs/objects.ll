;####################################################
;###                                              ###
;##  LIBRARY TO SIMULATE OBJECTS USING PROPERTIES  ##
;#                                                  #
;#  Objects are simulated with the getprop and      #
;#  putprop functions of NIL                        #
;#  stdlib.ll is needed, everything else is already #
;#  included into the core language                 #
;#                                                  #
;##  (c) 2017-2023 MICHAUD Yannick & KsyNET        ##
;###                                              ###
;####################################################

;***************
;INITIALIZATIONS
;***************
;## Quick load
(require 'libs/stdlib '$stdlib_)

;*****************
;SIMULATES CLASSES
;*****************
(setq $nilplus_classes nil)


;## Basic functions

;*core
(undef $nilplus_execProp (name func lp...)
  (if (atom lp) (let lp (list lp)))
  (let commande (cons (getprop name func) (cons 'name lp)))
  (NIL.evaluate (list commande))
)
(undef $nilplus_setProp (name propname value)
  (eval (cons 'putprop (list name value (+s "'" propname))))
)
#(undef $nilplus_getProp (name propname)
#  (getprop name propname)
#)


;*classes management
(undef existsClass (c)
  (contains $nilplus_classes c)
)
(undef $nilplus_addClass (name)
  (cond ((existsClass name)
        (print "Error, this class already exists (" name ")")
        nil
      )
      (t
        (setq $nilplus_classes (bbsort (cons name $nilplus_classes)))
        (putprop name name 'className)
        (putprop name 0 'maxInstances)
        t
      )
  )
)
(undef $nilplus_derivateClass (name parent)
  (cond   ((existsClass name)
        (print "Error, this class already exists (" name ")")
        nil
      )
      ((not (existsClass parent))
        (print "Error, this class does not exist (" parent ")")
        nil
      )
      (t
        (setq $nilplus_classes (bbsort (cons name $nilplus_classes)))
        (NIL.putProperty name ((NIL.getProperty parent).clone))
        (putprop name name 'className)
        (putprop name 0 'maxInstances)
        t
      )
  )
)
(undef $nilplus_addProp (classname prop value)
  (cond   ((not (contains $nilplus_classes classname))
      (print "Error, this class does not exist")
      nil
    )
    ((> (length value) 0)
      (eval (cons 'putprop (list classname value (+s "'" prop))))
      t
    )
    (t nil)
  )
)
(undef $nilplus_instantiate (classname objectname innername?"")
  (cond ((not (contains $nilplus_classes classname))
        (print "Error, this class does not exist : " classname)
        nil
      )
      ((== objectname classname)
        (print "Error, you cannot instantiate a class giving it the same object name")
        nil
      )
      (t
        (putVMProp NIL classname (+ classname.maxInstances 1) 'maxInstances)
        (let c (NIL.getProperty classname))
        (let ic (c.clone))
        (NIL.putProperty objectname ic)
        (putprop objectname innername 'name)
        t
      )
  )
)

(undef $nilplus_delClass (name)
  (cond ((not (contains $nilplus_classes name))
        (print "Error, this class does not exist (" name ")")
        nil
      )
      (t
        (print "Deleting class " name)
        (setq $nilplus_classes (remove name $nilplus_classes))
        (NIL.removeProperty name)
        (let np (NIL.getPropertiesAsList))
        (np.resetToFirst)
        (while (np.hasMoreElements)
          (let elem (np.nextElement))
          (let ename (car elem))
          (catch exception ((return)) (if (== ename.className name) (NIL.removeProperty ename)))
        )
        t
      )
  )
)

(undef $nilplus_getObject (name)
  (let rez (NIL.getProperty name))
  (if (rez.isEmpty) (let rez nil))
  rez
)
(undef $nilplus_delObject (o)
  (let rez (NIL.getProperty o))
  (let name o.className)
  (putVMProp NIL name (- name.maxInstances 1) 'maxInstances)
  (NIL.removeProperty o)
  rez
)

(undef $nilplus_getObjects (class?nil)
  (let p (NIL.getProperties))
  (let pl (makelist (NIL.getPropertiesAsList)))
  (let rez nil)
  (pl.resetToFirst)
  (while (pl.hasMoreElements)
    (let name (car (pl.nextElement)))
    (let name2 (NIL.getProperty p name 'className))
    (if (== class name2)
      (let rez (cons (NIL.getProperty name) rez))
    )
  )
  rez
)

(undef objToJson (objName?nil)
  (if (== objName nil) (return))
  (let obj ($nilplus_getObject objName))
  (if (== obj nil) (return))
  (let json_str "")
  json_str
)

;## Operators
(rmOp '->)
(addOp '-> '$nilplus_execProp)
(rmOp '<-)
(addOp '<- '$nilplus_setProp)
(rmOp '><)
(addOp '>< '$nilplus_addProp)
(rmOp '//)
(addOp '// '$nilplus_instantiate)
;## you can also use name.prop or name:prop instead of (getprop name 'prop)


;# setting help
(setUserHelp '-> (+ "Usage: O->M PARAMS" crlf tab "Evaluates (in the current NilVM) the method M for the 'object' O with PARAMS" crlf tab "The 1st parameter have to be the 'object'-name (see <- operator as well)"))
(setUserHelp '<- (+ "Usage: O<-P VALUE" crlf tab "Sets the property P (field or method) to VALUE for the 'object' O" crlf tab "You can invoke it with -> operator, or retrieve value with . or : operator"))
(setUserHelp '$nilplus_addClass (+ "Usage: $nilplus_addClass ClassName" crlf tab "Adds a simulated class (see // et >< opertors)"))
(setUserHelp '$nilplus_delClass (+ "Usage: $nilplus_delClass ClassName" crlf tab "Deletes the class ClassName"))
(setUserHelp '>< (+ "Usage: ClassName><FIELD|METHOD Value" crlf tab "Adds a simulated FIELD|METHOD to ClassName (see $nilplus_addClass, $nilplus_instantiate and // operator)" crlf tab "NOTICE : nothing is evaluated."))
(setUserHelp '// (+ "Usage: ClassName//ObjectName" crlf tab "Instantiates ClassName and names it ObjectName (see $nilplus_addClass, $nilplus_instantiate and >< operator)" crlf tab "NOTICE : no modification done with -> or >< operators will be reported to ObjectName, and nothing is evaluated."))


;## TESTS


## END
################################################
(print "Objects simulator loaded.")
(makeQuickLoad 'lo scriptFile)
################################################

