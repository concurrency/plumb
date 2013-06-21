((undeclared 
  "You used '~a' on line ~a, but you did not define or declare it previously."
  (identifier line-number)
  "(.*?) is not declared"
  identifier)
 (missing-colon
  "You forgot a ':' at the end of a PROC (line ~a)."
  (line-number)
  "expected \":\", found end of file"
  IGNORED)
 (missing-colon
  "You need a ':' before the PROC on line ~a."
  (line-number)
  "expected \":\", found PROC"
  IGNORED)
 (default
   "Line ~a, ~a"
   (line-number message)
   "(.*?)$"
   message))