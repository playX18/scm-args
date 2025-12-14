(import (args runner) (args grammar) (scheme base))

(define runner (make-command-runner "example" "Simple showcase of args library."))

(define grammar (command-runner-grammar runner))
(grammar-add-flag! grammar "verbose" 
  'abbr: "v"
  'help: "Enable verbose output.")

(define cmd (command "run" 
  'description: "Run the project in current directory"
  'run: (lambda (command)
    #f)))
(define cmd-grammar (command-grammar cmd))
(grammar-add-option! cmd-grammar "path"
  'help: "Path to the project directory."
  'value-help: "DIR")

(command-runner-add-command! runner cmd)

(define args '())
(command-runner-run runner args)