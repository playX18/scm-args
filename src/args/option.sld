(define-library (args option)
  (import (scheme base)
          (args help optional))
  (export 
    make-option
    option?
    option-flag?
    option-single?
    option-multi?

    option-name
    option-abbr
    option-help
    option-value-help
    option-allowed
    option-allowed-help
    option-defaults-to
    option-negatable?
    option-hide-negated-usage?
    option-callback
    option-type
    option-split-commas?
    option-mandatory?
    option-hide?
    option-aliases
    option-value)

  (include "option.scm"))