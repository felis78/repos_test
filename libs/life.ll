(needsLibraries '(LIB.Maths))

(undef $life_value)
(defun $life_value (m i j)
  (cond
    (
      (< i 1)
      nil
    )
    (
      (< j 1)
      nil
    )
    (
      (> i (m.getRowNumber))
      nil
    )
    (
      (> j (m.getColNumber))
      nil
    )
    (
      t
      (<>
        (mgetelement m (- i 1) (- j 1))
        0
      )
    )
  )
)
(undef $life_alivenumber)
(defun $life_alivenumber (m i j)
  (let alivenumber 0)
  (for (let k -1) (<= k 1) (let k (incr k))
    (for (let l -1) (<= l 1) (let l (incr l))
      (let a 0)
      (cond 
        ((and (== k 0) (== l 0))
          (let a 0)
        )
        (($life_value m (+ i k) (+ j l))
          (let a 1)
        )
      )
      (let alivenumber (+ alivenumber a))
    )
  )
)
(undef $life_nextvalue)
(defun $life_nextvalue (m i j)
  (let v (mgetelement m (- i 1) (- j 1)))
  (let n ($life_alivenumber m i j))
  (cond
    ((== v 0)
      (cond 
        ((== n 3) 1)
        (t 0)
      )
    )
    (t
      (cond
        ((or (== n 2) (== n 3)) 1)
        (t 0)
      )
    )
  )
)


