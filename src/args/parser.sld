;; Actual parser for CLI arguments. Not supposed to be used directly.

(define-library (args parser)
  (import (scheme base)
          (srfi 1)
          (args option)
          (args grammar)
          (args help optional)
          (args results))
  (cond-expand
    ((library (srfi 130))
      (import (srfi 130)))
    (else
      (import (only (args string)
                    string-prefix?
                    string-index
                    string-cursor-end
                    string-cursor=?
                    string-cursor->index
                    string-contains
                    string-every))))
  (export grammar-parse)

  (include "parser.scm"))
