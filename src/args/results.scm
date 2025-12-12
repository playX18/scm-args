(define-record-type <argument-results>
  (%make-argument-results 
    grammar
    parsed
    name
    command
    rest
    arguments)
  argument-results?
  
  (grammar argument-results-grammar)
  (parsed argument-results-parsed)
  (name argument-results-name)
  (command argument-results-command)
  (rest argument-results-rest)
  (arguments argument-results-arguments))

(define (argument-results grammar parsed name command rest arguments)
  (%make-argument-results 
    grammar
    parsed
    name
    command
    rest
    arguments))

(define (argument-results-flags results)
  (define grammar (argument-results-grammar results))
  (define parsed (argument-results-parsed results))
  (lambda (flag)
    (define option (assoc flag (grammar-options grammar)))
    (cond 
      ((not option)
        (error (string-append "No flag named: '--" flag "'")))
      ((not (option-flag? (cdr option)))
        
        (error (string-append "Option '--" flag "' is not a flag option")))
      (else 
        (option-value (cdr option)
          (cond 
            ((assoc flag parsed) => cdr)
            (else #f)))))))

(define (argument-results-options results)
  (define grammar (argument-results-grammar results))
  (define parsed (argument-results-parsed results))
  (lambda (name)
    (define option (assoc name (grammar-options grammar)))
    (cond 
      ((not option)
        (error (string-append "No option named: '--" name "'")))
      ((not (option-single? (cdr option)))
        (error (string-append "Option '--" name "' is not a single-value option")))
      ((and (option-mandatory? (cdr option))
            (not (assoc name parsed)))
        (error (string-append "Mandatory option '--" name "' not provided")))
      (else 
        (option-value option
          (cond 
            ((assoc (option-name (cdr option)) parsed) => cdr)
            (else #f)))))))

(define (argument-results-multi-options results)
  (define grammar (argument-results-grammar results))
  (define parsed (argument-results-parsed results))
  (lambda (name)
    (define option (assoc name (grammar-options grammar)))
    (cond 
      ((not option)
        (error (string-append "No option named: '--" name "'")))
      ((not (option-multi? (cdr option)))
        (error (string-append "Option '--" name "' is not a multi-value option")))
      (else 
        (option-value option
          (cond 
            ((assoc (option-name (cdr option)) parsed) => cdr)
            (else #f)))))))
