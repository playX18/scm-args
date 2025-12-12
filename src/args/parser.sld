;; Actual parser for CLI arguments. Not supposed to be used directly.

(define-library (args parser)
  (import (scheme base)
          (srfi 13)
          (srfi 1)
          (args option)
          (args grammar)
          (args help optional)
          (args results))
  (export grammar-parse)

  (include "parser.scm"))
