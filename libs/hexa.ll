(needsLibraries 'IO)
(undef hexa)
(defun hexa (filename)
  (let f (file filename 'R))
  (let c " ")
  (let n 0)
  (let hline "")
  (let sline "")
  (while (<> c 'EOF)
    (let c (fread f))
    (let ac (ascii c))
    (let n (+ n 1))
    (let hline (+ hline (ac.toHexConstString 2) " "))
    (let sline (+ sline c))
    (if (== n 16)
      (print hline " : " sline)
      (let n 0)
      (let hline "")
      (let sline "")
    )
  )
  (print hline " : " sline)
  (fclose f)
)
(undef hexa2char)
(defun hexa2char (h)
   ((convBase h 16 10).charValue)
)
(setUserHelp 'hexa2char (+ "Usage: hexa2char BYTE" crlf tab "Converts BYTE_HEXA_BYE to character"))