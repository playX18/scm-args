(define-library (args grammar)
  (import (scheme base)
          (only (srfi 1) fold)
          (args option)
          (args help optional))
  (cond-expand
    ((library (srfi 130))
      (import (srfi 130)))
    (else
      (import (only (args string)
                    string-join
                    string-pad-right))))
  (export 
    make-grammar
    make-grammar-builder
    grammar*
    define-grammar
    grammar?
    grammar-builder?
    grammar-builder-allow-trailing?
    grammar-builder-allow-anything?
    grammar-builder-operations
    grammar-builder-allow-trailing
    grammar-builder-allow-anything
    grammar-builder-add-option
    grammar-builder-add-flag
    grammar-builder-add-multi-option
    grammar-builder-add-command
    grammar-builder-add-separator
    grammar-builder-default-command
    grammar-builder-build
    grammar-options grammar-options-set!
    grammar-commands grammar-commands-set!
    grammar-aliases grammar-aliases-set!
    grammar-default-command grammar-default-command-set!
    grammar-options-and-separators grammar-options-and-separators-set!
    grammar-allow-trailing? grammar-allow-trailing?-set!
    grammar-allow-anything? grammar-allow-anything?-set!  
    grammar-add-option!
    grammar-add-flag! 
    grammar-add-multi-option!
    grammar-find-by-name-or-alias
    grammar-find-by-abbr
    grammar-add-command!
    grammar-add-separator!
    grammar-usage
    str-split)

  (include "grammar.scm")
)
