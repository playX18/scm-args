;; A command-runner API for the args library

(define-library (args runner)
  (import (scheme base)
          (srfi 1)
          (args grammar)
          (args parser)
          (args results)
          (args help optional))
  (cond-expand
    ((library (srfi 130))
      (import (srfi 130)))
    (else
      (import (only (args string)
                    string-join
                    string-pad-right))))
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
