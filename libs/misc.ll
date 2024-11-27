#MISC
# (c) 2019-2023 MICHAUD Yannick

(require 'libs/exp)
(setq phi (/p (+ 1 (squareRoot 5 50)) 2 50))

(defun inter (a?nil b?nil)
  (if (or (atom a) (atom b)) (return "Args must be lists..."))
  (let tmp (mapcar (lambda (e) (cond ((contains b e) e) (t ""))) a))
#  (let tmp (mapcar (lambda (e) (cond ((> (where e b) 0) e) (t ""))) a))
  (while (> (let tmp2 (where "" tmp)) 0)
    (let tmp (removeNth tmp2 tmp))
  )
  (unique (bbsort tmp))
)
(defun inter2 (a?nil b?nil)
  (if (or (atom a) (atom b)) (return "Args must be lists..."))
  (let tmp (streamFilter a "((e) (contains b e))"))
  (unique (bbsort tmp))
)
( undef execStdout ( cmd param?"" ) ( let rez ( exec cmd param ) ) ( let rezout ( readAll ( cadr rez ) ) ) rezout )
( undef allDiff ( l ) ( == ( length ( unique ( sort l ) ) ) ( length l ) ) )
( undef redefun ( f?temp l... ) ( if ( == l nil ) ( return "bad usage" ) ) ( apply 'defun ( cons f l ) ) )
