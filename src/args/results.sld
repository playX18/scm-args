(define-library (args results)
  (import (scheme base)
          (args help optional)
          (args grammar)
          (args option))
 
  (export
    argument-results
    argument-results?
    argument-results-grammar
    argument-results-parsed
    argument-results-name
    argument-results-command
    argument-results-rest
    argument-results-arguments
    argument-results-flags
    argument-results-options
    argument-results-multi-options)
  
  (include "results.scm"))
