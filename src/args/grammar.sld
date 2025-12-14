(define-library (args grammar)
  (import (scheme base)
          (only (srfi 1) fold)
          (srfi 130)
          (args option)
          (args help optional))
  (export 
    make-grammar
    grammar?
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