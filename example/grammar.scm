;; Raw grammar usage example
;; This file will declare a grammar with options, multi-options, flags and a subcommand.
;; It will then parse a sample command line and display the parsed results.
;; Command line: `--load-path /usr/local/lib,/opt/lib --gc-plan=immix -L/usr/lib --verbose run --script myscript.scm`

(import (args grammar)
        (args results)
        (args parser)
        (scheme write)
        (scheme base))


(define grammar (make-grammar))

(grammar-add-separator! grammar "+========================+")

(grammar-add-multi-option! grammar "load-path"
  'abbr: "L"
  'help: "Add a directory to the load path"
  'value-help: "DIR"
  'split-commas?: #t)

(grammar-add-option! grammar "gc-plan" 

  'help: "Choose a GC algorithm"
  'allowed: '("immix" "marksweep")
  'allowed-help: '(
    ("immix" . "mark-region collector")
    ("marksweep" . "simple-marksweep collector"))
  'defaults-to: "immix")

(grammar-add-flag! grammar "verbose" 
  'abbr: "v"
  'help: "Enable verbose output")

(define run-grammar (make-grammar))

(grammar-add-option! run-grammar "script"
  'help: "Script file to execute"
  'value-help: "FILE")

(grammar-add-command! grammar "run" run-grammar)

(display (grammar-usage grammar))
(newline)

(define cli '("--load-path" "/usr/local/lib,/opt/lib"
              "--gc-plan=immix"
              "-L/usr/lib"
              "--verbose"
              "run"
              "--script" "myscript.scm"))

(define parsed (grammar-parse grammar cli))

(define flags (argument-results-flags parsed))
(define options (argument-results-options parsed))
(define multi-options (argument-results-multi-options parsed))

(display "verbose=")
(display (flags "verbose"))
(newline)

(display "gc-plan=")
(display (options "gc-plan"))
(newline)

(display "load-path=")
(display (multi-options "load-path"))
(newline) 

(define cmd-results (argument-results-command parsed))

(display "Command script=")
(display ((argument-results-options cmd-results) "script"))
(newline)
(display "Command name: ")
(display (argument-results-name cmd-results))
(newline)