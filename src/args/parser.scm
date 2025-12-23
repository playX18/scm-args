
(define-record-type <parser>
  (%parser 
    command-name
    parent
    grammar
    args
    rest
    results)
  parser?
  (command-name parser-command-name)
  (parent parser-parent)
  (grammar parser-grammar)
  (args parser-args parser-args-set!)
  (rest parser-rest parser-rest-set!)
  (results parser-results parser-results-set!))

(define-syntax current 
  (syntax-rules () 
    ((_ parser)
      (car (parser-args parser)))))

(define-syntax advance!
  (syntax-rules ()
    ((_ parser)
      (let ((first (current parser))) 
        (parser-args-set! parser (cdr (parser-args parser)))
        first))))

(define (validate cond message . rest)
  (let-optionals rest 
    ((args #f)
     (source #f)
     (offset #f))
    (unless cond (error message 
           (if args 
               (list 'args args)
               '())
           (if source 
               (list 'source source)
               '())
           (if offset 
               (list 'offset offset)
               '())))))

(define (validate-allowed option value arg)
  (cond 
    ((not (option-allowed option)) #t)
    (else 
      
      (validate (member value (option-allowed option))
        (string-append "'" value "' is not an allowed value for option '" arg "'.")
        arg))))



(define (set-option! parser option value arg)
  (cond 
    ((not (option-multi? option))
      (validate-allowed option value arg)
      (parser-results-set! parser (cons (cons (option-name option) value) (parser-results parser))))
    (else 
      (let* ((list (assoc (option-name option) (parser-results parser)))
             (list (if list (cdr list) '())))
        (cond 
          ((option-split-commas? option)
           
            (for-each (lambda (elem)
                        (validate-allowed option elem arg)
                        (set! list (cons elem list)))
              (str-split value #\,)))
          (else 
            (validate-allowed option value arg)
            (set! list (cons value list))))
        (parser-results-set! parser
          (cons (cons (option-name option) (reverse list))
                (alist-delete (option-name option) (parser-results parser))))))))

(define (set-flag! parser option value)
  (parser-results-set! parser
    (cons (cons (option-name option) value)
          (alist-delete (option-name option) (parser-results parser)))))


(define (make-parser command-name grammar args . rest)
  (let-optionals rest 
    ((parent #f)
     (rest_ '()))
    (%parser command-name parent grammar args rest_ '())))

(define (handle-long-option parser name value)
  (define option (grammar-find-by-name-or-alias (parser-grammar parser) name))
  (cond 
    (option
      (advance! parser)
      (cond 
        ((option-flag? option)
          (validate (not value) (string-append "Flag option '" name "' does not take a value.") name)
          (set-flag! parser option #t)
          #t)
        (value 
          (set-option! parser option value name)
          #t)
        (else 
          (read-next-arg-as-value parser option name)
          #t)))
    ((string-prefix? "no-" name)
      (let* ((positive-value (substring name 3 (string-length name)))
             (option (grammar-find-by-name-or-alias (parser-grammar parser) positive-value)))
        (cond 
          ((not option)
            (validate (parser-parent parser) (string-append "Unknown option '--" name "'.") name)
            (handle-long-option (parser-parent parser) name value))
          (else 
            (advance! parser)
            (validate (option-flag? option) (string-append "Option '--" positive-value "' is not negatable.") positive-value)
            (validate (option-negatable? option) (string-append "Option '--" positive-value "' is not negatable.") positive-value)
            (set-flag! parser option #f)
            #t)) ))
    (else 
      (validate (parser-parent parser) (string-append "Unknown option '--" name "'.") name)
      (handle-long-option (parser-parent parser) name value))))

(define (parse-long-option parser)
  (define cur (current parser))
  (define end (string-cursor-end cur))
  
  (cond 
    ((not (string-prefix? "--" (current parser)))
      #f)
    (else 
      (let* ((index (string-index (current parser) #\=))
             (index* (string-cursor->index cur index))
             (name (if (not (string-cursor=? index end))
                       (substring cur 2 index*)
                       (substring cur 2 (string-length cur)))))
        (cond 
          ((not (string-every letter-or-digit-or-hyphen-or-underscore? name)) 
            (error (string-append "Invalid option name '--" name "'." ))
            ;(format #t "Invalid option name '--~a'.~%" name)
          #f)
          (else 
            (let ((value (if (= index* (string-length cur))
                           #f 
                           (substring cur (+ 1 index*) (string-length cur))
                           )))
              (if (and value (string-contains value "\n"))
                #f
                (handle-long-option parser name value)))))))))

(define (read-next-arg-as-value parser option arg)
  (validate (not (null? (parser-args parser))) 
    (string-append "Option '" arg "' requires a value, but none was provided.") arg)
  (set-option! parser option (current parser) arg)
  (advance! parser))

(define (parse-short-flag parser c)
  (define option (grammar-find-by-abbr (parser-grammar parser) c))
  (cond 
    ((not option)
      (validate (parser-parent parser) (string-append "Unknown option '-" (string c) "'.") (string c))
      (parse-short-flag (parser-parent parser) c))
    (else 
      (validate (option-flag? option) (string-append "Option '-" (string c) "' is not a flag.") (string c))
      (set-flag! parser option #t)
      #t)))

(define (handle-abbreviation parser letters-and-digits rest innermost-command)
  (define c (substring letters-and-digits 0 1))
  (define first (grammar-find-by-abbr (parser-grammar parser) c))
  (cond 
    ((not first)
      (validate (parser-parent parser) (string-append "Unknown option '-" c "'.") c)
      (handle-abbreviation (parser-parent parser) letters-and-digits rest innermost-command))
    ((not (option-flag? (cdr first)))
      (let ((value (string-append (substring letters-and-digits 1 (string-length letters-and-digits)) rest)))
        (set-option! parser (cdr first) value c))
      (advance! parser)
      #t)
    (else 
      (validate (string=? rest "")
        (string-append "Flag option '-" c "' cannot be combined with other options.") c)
      (string-for-each 
        (lambda (ch)
          (parse-short-flag innermost-command (string ch)))
        letters-and-digits)
      (advance! parser)
      #t)))
  
(define (parse-abbreviation parser innermost-command)
  (define cur (current parser))
  (cond 
    ((< (string-length cur) 2) #f)
    ((not (string-prefix? "-" cur)) #f)
    (else 
      (let loop ((index 1))
        (cond 
          ((and (< index (string-length cur))
                (letter-or-digit? (string-ref cur index)))
            (loop (+ index 1)))
          (else 
            (cond 
              ((= index 1) #f)
              (else 
                (handle-abbreviation parser 
                                     (substring cur 1 index) 
                                     (substring cur index (string-length cur)) 
                                     innermost-command)))))))))

(define (handle-solo-option parser opt)
  (define option (grammar-find-by-abbr (parser-grammar parser) opt))
  (cond 
    ((not option)
      (validate (parser-parent parser) (string-append "Unknown option '" opt "'.") opt)
      (handle-solo-option (parser-parent parser) opt))
    (else 
      (advance! parser)
      (cond 
        ((option-flag? (cdr option))
          (set-flag! parser (cdr option) #t))
        (else 
          (read-next-arg-as-value parser (cdr option) opt))))))

(define (parse-solo-option parser)
  (cond 
    ((not (= (string-length (current parser)) 2)) #f)
    ((not (string-prefix? "-" (current parser))) #f)
    (else 
      (let ((opt (substring (current parser) 1 (string-length (current parser)))))
        
        (if (letter-or-digit? (string-ref opt 0))
          (handle-solo-option parser opt)
          #f)))))


(define (letter-or-digit? ch)
  (or (and (char>=? ch #\a) (char<=? ch #\z))
      (and (char>=? ch #\A) (char<=? ch #\Z))
      (and (char>=? ch #\0) (char<=? ch #\9))))

(define (letter-or-digit-or-hyphen-or-underscore? ch)
  (or (letter-or-digit? ch)
      (char=? ch #\_)
      (char=? ch #\-)))

(define (parse parser) 
  (define arguments (map (lambda (x) x) (parser-args parser))) ; copy args
  (define command #f)

  (let loop ()
    (cond 
      ((null? (parser-args parser)) (finish-parsing parser command arguments))
      (else 
        (cond 
          ((string=? (current parser) "--") (finish-parsing parser command arguments))
          ((assoc (current parser) (grammar-commands (parser-grammar parser)))
            => (lambda (entry)
               
                (set! command (cons (advance! parser) (cdr entry)))
                (finish-parsing parser command arguments)))
          ((and (not (string=? (current parser) "--help"))
                (not (string=? (current parser) "-h")) 
                (grammar-default-command (parser-grammar parser))) => (lambda (command)
      
            (set! command (cons (advance! parser) (car (assoc command (grammar-commands (parser-grammar parser))))))))
          (else 
            (cond 
              ((parse-solo-option parser) (loop))
              ((parse-abbreviation parser parser) (loop))
              ((parse-long-option parser) (loop))
              ((not (grammar-allow-trailing? (parser-grammar parser)))
                (finish-parsing parser command arguments))
              (else     
                
                (parser-rest-set! parser 
                  (append (parser-rest parser) (list (advance! parser))))
                (loop)))))))))

(define (finish-parsing parser command arguments)
  ;; If there is a default command and we did not select any other commands
  ;; and we don't have any trailing arguments then select the default
  ;; command unless user requested help.
  (define command-results #f)
  (when (and (not command)
             (null? (parser-rest parser))
             (not (assoc "help" (parser-results parser))))
    
    (cond 
      ((grammar-default-command (parser-grammar parser)) => (lambda (default-command)
        (set! command (cons default-command (car (assoc default-command (grammar-commands (parser-grammar parser))))))))))

  (when command 
    (validate (null? (parser-rest parser)) "cannot specify arguments before command")
    (let ((command-parser (make-parser (car command) (cdr command) (parser-args parser) parser (parser-rest parser))))
      (set! command-results (parse command-parser))))
  
  (for-each 
    (lambda (entry)
      (define name (car entry))
      (define option (cdr entry))
      (define parsed-option (assoc name (parser-results parser)))
      (cond 
        ((not (option-callback option)) #f)
        ((and (option-mandatory? option)
              (not parsed-option))
          (validate #f (string-append "Mandatory option '" name "' not provided.") name))
        (else 
          ((option-callback entry) 
            (if parsed-option 
              (cdr parsed-option)
              (option-defaults-to option))))))
    (grammar-options (parser-grammar parser)))
  (parser-rest-set! parser 
    (append (parser-rest parser)
            (parser-args parser)))

  (argument-results 
    (parser-grammar parser)
    (parser-results parser)
    (parser-command-name parser)
    command-results
    (parser-rest parser)
    arguments))


(define (grammar-parse grammar args)
  (define parser (make-parser #f grammar args))
  (parse parser))