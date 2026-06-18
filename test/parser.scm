(import (scheme base)
        (scheme write)
        (args grammar)
        (args parser)
        (args results)
        (args runner))

(define failures '())

(define (record-failure name message)
  (set! failures (cons (cons name message) failures)))

(define (check-equal name actual expected)
  (unless (equal? actual expected)
    (record-failure name (list 'expected expected 'actual actual))))

(define (check-true name value)
  (unless value
    (record-failure name (list 'expected-true 'actual value))))

(define (check-error name thunk)
  (let ((raised? #f))
    (guard (exn
            (else (set! raised? #t)))
      (thunk))
    (unless raised?
      (record-failure name "expected error"))))

(define (test-callback)
  (let ((grammar (make-grammar))
        (seen #f))
    (grammar-add-option! grammar "mode"
      'callback: (lambda (value) (set! seen value)))
    (grammar-parse grammar '("--mode" "fast"))
    (check-equal "callback gets parsed value" seen "fast")))

(define (test-mandatory)
  (let ((grammar (make-grammar)))
    (grammar-add-option! grammar "file" 'mandatory?: #t)
    (check-error "mandatory option is checked while parsing"
      (lambda () (grammar-parse grammar '())))))

(define (test-alias-results)
  (let ((grammar (make-grammar)))
    (grammar-add-option! grammar "output" 'aliases: '("out"))
    (let* ((results (grammar-parse grammar '("--out" "dist")))
           (options (argument-results-options results)))
      (check-equal "alias option lookup" (options "out") "dist")
      (check-true "alias was parsed" (argument-results-was-parsed? results "out")))))

(define (test-stop-parsing)
  (let ((grammar (make-grammar)))
    (grammar-add-flag! grammar "verbose" 'abbr: "v")
    (let ((results (grammar-parse grammar '("--" "--verbose" "-v" "file"))))
      (check-equal "-- is not included in rest"
                   (argument-results-rest results)
                   '("--verbose" "-v" "file")))))

(define (test-allow-anything)
  (let ((grammar (make-grammar)))
    (grammar-add-flag! grammar "verbose" 'abbr: "v")
    (grammar-allow-anything?-set! grammar #t)
    (let ((results (grammar-parse grammar '("--verbose" "-v" "file"))))
      (check-equal "allow-anything treats all args as rest"
                   (argument-results-rest results)
                   '("--verbose" "-v" "file"))
      (check-equal "allow-anything does not parse known flags"
                   ((argument-results-flags results) "verbose")
                   #f))))

(define (test-default-command)
  (let* ((root (make-grammar))
         (run (make-grammar)))
    (grammar-add-option! run "script")
    (grammar-add-command! root "run" run)
    (grammar-default-command-set! root "run")
    (let ((results (grammar-parse root '("--script" "main.scm"))))
      (check-equal "default command name"
                   (argument-results-name (argument-results-command results))
                   "run")
      (check-equal "default command receives args"
                   ((argument-results-options (argument-results-command results)) "script")
                   "main.scm"))))

(define (test-command-help)
  (let* ((runner (make-command-runner "tool" "Tool.")
         )
         (cmd (command "run"
                'description: "Run things."
                'run: (lambda (cmd) #t))))
    (grammar-add-option! (command-grammar cmd) "path" 'value-help: "DIR")
    (command-runner-add-command! runner cmd)
    (command-runner-run runner '("help" "run"))))

(define (test-grammar-builder)
  (let* ((base (make-grammar-builder))
         (with-flag (grammar-builder-add-flag base "verbose" 'abbr: "v"))
         (command-builder (grammar-builder-add-option
                            (make-grammar-builder)
                            "script"))
         (builder (grammar-builder-default-command
                    (grammar-builder-add-command
                      (grammar-builder-add-multi-option
                        (grammar-builder-add-option with-flag "output"
                          'aliases: '("out"))
                        "include"
                        'abbr: "I")
                      "run"
                      command-builder)
                    "run"))
         (base-grammar (grammar-builder-build base))
         (grammar (grammar-builder-build builder))
         (results (grammar-parse grammar
                    '("--verbose" "--out" "dist" "-Ilib,src" "--script" "main.scm"))))
    (check-equal "builder preserves previous values"
                 (grammar-options base-grammar)
                 '())
    (check-equal "builder flag"
                 ((argument-results-flags results) "verbose")
                 #t)
    (check-equal "builder alias option"
                 ((argument-results-options results) "out")
                 "dist")
    (check-equal "builder multi option"
                 ((argument-results-multi-options results) "include")
                 '("lib" "src"))
    (check-equal "builder default command"
                 (argument-results-name (argument-results-command results))
                 "run")
    (check-equal "builder command option"
                 ((argument-results-options (argument-results-command results)) "script")
                 "main.scm")))

(define (test-grammar-macro)
  (define command-grammar
    (grammar*
      (option "script" 'value-help: "FILE")))
  (define grammar
    (grammar*
      (separator "Options:")
      (flag "verbose" 'abbr: "v")
      (option "output" 'aliases: '("out"))
      (multi-option "include" 'abbr: "I")
      (subcommand "run" command-grammar)
      (default-command "run")))
  (let ((results (grammar-parse grammar
                   '("--verbose" "--out" "dist" "-Ilib,src" "--script" "main.scm"))))
    (check-equal "grammar* flag"
                 ((argument-results-flags results) "verbose")
                 #t)
    (check-equal "grammar* alias option"
                 ((argument-results-options results) "out")
                 "dist")
    (check-equal "grammar* multi option"
                 ((argument-results-multi-options results) "include")
                 '("lib" "src"))
    (check-equal "grammar* default command"
                 (argument-results-name (argument-results-command results))
                 "run")
    (check-equal "grammar* command option"
                 ((argument-results-options (argument-results-command results)) "script")
                 "main.scm")))

(define (test-define-grammar-macro)
  (define-grammar passthrough
    (allow-anything #t)
    (allow-trailing #f)
    (flag "verbose" 'abbr: "v"))
  (let ((results (grammar-parse passthrough '("--verbose" "file.scm"))))
    (check-true "define-grammar creates grammar" (grammar? passthrough))
    (check-equal "grammar macro allow-trailing"
                 (grammar-allow-trailing? passthrough)
                 #f)
    (check-equal "grammar macro allow-anything rest"
                 (argument-results-rest results)
                 '("--verbose" "file.scm"))))

(test-callback)
(test-mandatory)
(test-alias-results)
(test-stop-parsing)
(test-allow-anything)
(test-default-command)
(test-command-help)
(test-grammar-builder)
(test-grammar-macro)
(test-define-grammar-macro)

(if (null? failures)
  (begin
    (display "ok")
    (newline))
  (begin
    (for-each
      (lambda (failure)
        (display (car failure))
        (display ": ")
        (write (cdr failure))
        (newline))
      (reverse failures))
    (error "tests failed")))
