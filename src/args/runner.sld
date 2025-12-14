;; A command-runner API for the args library

(define-library (args runner)
  (import (scheme base)
          (srfi 1)
          (srfi 13)
          (srfi 132) ; sorting for displaying help
          (args grammar)
          (args parser)
          (args results)
          (args help optional))
  (export 
    make-command-runner
    command-runner-add-command! 
    command-runner?
    command-runner-executable-name
    command-runner-description
    command-runner-commands
    command-runner-grammar
    command-runner-usage
    command-runner-parse
    command-runner-run-command
    command-runner-run
    
    command
    command?
    command-name
    command-parent
    command-runner
    command-global-results
    command-results
    command-grammar
    command-subcommands
    command-aliases
    command-hidden?

    command-description
    command-run 
    command-summary
    command-invocation
    command-category
    command-add-subcommand!)
  (include "runner.scm"))