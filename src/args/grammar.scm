

(define-record-type <grammar> 
  (%make-grammar 
    options
    commands
    aliases
    default-command
    options-and-separators
    allow-trailing?
    allow-anything?
    %usage)
  grammar?
  (options grammar-options grammar-options-set!)
  (commands grammar-commands grammar-commands-set!)
  (aliases grammar-aliases grammar-aliases-set!)
  (default-command grammar-default-command grammar-default-command-set!)
  (options-and-separators grammar-options-and-separators grammar-options-and-separators-set!)
  (allow-trailing? grammar-allow-trailing? grammar-allow-trailing?-set!)
  (allow-anything? grammar-allow-anything? grammar-allow-anything?-set!)
  (%usage %grammar-usage %grammar-usage-set!)) ; usage is populated with usage string once its generated

(define (make-grammar . args)
  (let-optionals args ((allow-trailing? #t))
    (%make-grammar 
      '()
      '()
      '()
      #f 
      '()
      allow-trailing?
      #f
      #f)))

(define (grammar-add-separator! grammar separator)
  (unless (string? separator)
    (error "Separator must be a string" separator))
  (grammar-options-and-separators-set! grammar
    (cons separator (grammar-options-and-separators grammar))))

(define (grammar-add-command! grammar command . subgrammar?)
  (when (assoc command (grammar-commands grammar))
    (error "Command already exists in grammar" command))
  (let ((subgrammar (if (null? subgrammar?)
                        (make-grammar)
                        (car subgrammar?))))
    (unless (grammar? subgrammar)
      (error "Grammar for command must be of type <grammar>" subgrammar))
    (grammar-commands-set! grammar
      (cons (cons command subgrammar) (grammar-commands grammar)))
    subgrammar))

(define (grammar-add-flag! grammar name . args)
  (let-keywords args ((abbr #f)
                      (help #f)
                      (defaults-to #f)
                      (negatable? #t)
                      (callback #f)
                      (hide? #f)
                      (hide-negated-usage? #f)
                      (aliases '()))
    (add-option! grammar 
      'name: name 
      'abbr: abbr
      'help: help
      'defaults-to: defaults-to
      'negatable?: negatable?
      'callback: callback
      'type: 'flag
      'hide?: hide?
      'hide-negated-usage?: hide-negated-usage?
      'aliases: aliases )))

(define (grammar-add-option! grammar name . args)
  (let-keywords* args ((abbr #f)
                      (help #f)
                      (value-help #f)
                      (allowed #f)
                      (allowed-help #f)
                      (defaults-to #f)
                      (callback #f)
                      (mandatory? #f)
                      (hide? #f)
                      (aliases '()))
    (add-option! grammar 
      'name: name 
      'abbr: abbr
      'help: help
      'value-help: value-help
      'allowed: allowed
      'allowed-help: allowed-help
      'defaults-to: defaults-to
      'callback: callback
      'type: 'single
      'mandatory?: mandatory?
      'hide?: hide?
      'aliases: aliases 
      )))

(define (grammar-add-multi-option! grammar name . args)
  (let-keywords args ((abbr #f)
                      (help #f)
                      (value-help #f)
                      (allowed #f)
                      (allowed-help #f)
                      (defaults-to '())
                      (callback #f)
                      (split-commas? #t)
                      (hide? #f)
                      (aliases '()))
    (add-option! grammar 
      'name: name 
      'abbr: abbr
      'help: help
      'value-help: value-help
      'allowed: allowed
      'allowed-help: allowed-help
      'defaults-to: defaults-to
      'callback: callback
      'type: 'multi
      'split-commas?: split-commas?
      'hide?: hide?
      'aliases: aliases 
      )))



(define (grammar-find-by-name-or-alias grammar name)
  (let ((alias (assoc name (grammar-aliases grammar))))
    (let ((name (if alias (cdr alias) name)))
      (cond 
        ((assoc name (grammar-options grammar)) => cdr)
        (else #f)))))

(define (grammar-find-by-abbr grammar abbr)
  (let loop ((options (grammar-options grammar)))
    (cond
      ((null? options) #f)
      ((equal? (option-abbr (cdr (car options))) abbr)
       (car options))
      (else (loop (cdr options))))))

(define (grammar-default-for grammar name)
  (let ((value (grammar-find-by-name-or-alias grammar name)))
    (cond 
      ((not value) (error "no such option in grammar" name))
      (else (option-defaults-to (cdr value))))))



(define (add-option! grammar . args)
  (let-keywords args ((name #f)
                      (abbr #f)
                      (help #f)
                      (value-help #f)
                      (allowed #f)
                      (allowed-help #f)
                      (defaults-to #f)
                      (negatable? #f)
                      (callback #f)
                      (type 'single)
                      (split-commas? #f)
                      (mandatory? #f)
                      (hide? #f)
                      (hide-negated-usage? #f)
                      (aliases '()))
    (for-each (lambda (name)
                (when (grammar-find-by-name-or-alias grammar name)
                  (error "Option already exists in grammar" name)))
      (cons name aliases))
    (when abbr 
      (when (grammar-find-by-abbr grammar abbr)
        (error "Option with abbreviation already exists in grammar" abbr)))

    (when (and mandatory? defaults-to)
      (error "Option cannot be both mandatory and have a default value" name))
    
    (when (and (not negatable?) hide-negated-usage?)
      (error "Option cannot have hide-negated-usage? set if negatable? is false" name))
    
    (let ((option (make-option
                    name
                    'abbr: abbr
                    'help: help
                    'value-help: value-help
                    'allowed: allowed
                    'allowed-help: allowed-help
                    'defaults-to: defaults-to
                    'negatable?: negatable?
                    'callback: callback
                    'type: type
                    'split-commas?: split-commas?
                    'mandatory?: mandatory?
                    'hide?: hide?
                    'hide-negated-usage?: hide-negated-usage?
                    'aliases: aliases)))
      
      (grammar-options-set! grammar
        (cons (cons name option) (grammar-options grammar)))
      (grammar-options-and-separators-set! grammar
        (cons option (grammar-options-and-separators grammar)))
      (grammar-aliases-set! grammar
        (append 
          (map (lambda (alias) (cons alias name)) aliases)
          (grammar-aliases grammar)))
      option)))

(define (generate-usage options-and-separators line-length)
  (define column-count 3)
  (define buffer (open-output-string))
  (define current-column 0)
  (define (calculate-column-width)
    (let loop ((ops options-and-separators) 
               (abbr 0)
               (title 0))
      (cond 
        ((null? ops) (list abbr (+ 4 title)))
        ((not (option? (car ops))) (loop (cdr ops) abbr title))
        ((option-hide? (car ops)) (loop (cdr ops) abbr title))
        (else 
          (let* ((option (car ops))
                 (abbr (max abbr (string-length (abbreviation option))))
                 (title (max title (+ (string-length (long-option option))
                                      (string-length (mandatory-option option)))))
                 (title* (if (not (option-allowed-help option)) 
                            title 
                            (fold (lambda (entry title)
                              (max title (string-length (allowed-title option (car entry)))))
                             title
                             (option-allowed-help option)))))
            (loop (cdr ops) abbr title*))))))
  (define (abbreviation opt)
    (cond 
      ((not (option-abbr opt)) "")
      (else (string-append "-" (option-abbr opt) " "))))
  (define (long-option opt)
    (define result 
      (cond 
        ((and (option-negatable? opt) (not (option-hide-negated-usage? opt)))
         (string-append "--[no-]" (option-name opt)))
        (else 
          (string-append "--" (option-name opt)))))
    (cond 
      ((option-value-help opt)
       (string-append result "=<" (option-value-help opt) ">"))
      (else result)))
  (define (mandatory-option opt)
    (cond 
      ((option-mandatory? opt) "(*)")
      (else "")))
  
  (define (allowed-title option allowed)
    (define default? (cond 
      ((list? (option-defaults-to option))
       (member (option-defaults-to option) allowed))
      (else (equal? (option-defaults-to option) allowed))))
    
    (define result (string-append "      [" allowed "]"))
    (if default? 
        (string-append result " (default)") 
        result))

  (define column-widths (calculate-column-width))
  (define newlines-needed 0)
  (define (write-separator separator)
    (if (not (zero? (string-length (get-output-string buffer))))
      (write-string "\n\n" buffer))
    (write-string separator buffer)
    (set! newlines-needed 1))
  (define (newline)
    (set! newlines-needed (+ 1 newlines-needed))
    (set! current-column 0))
  
  (define (%write column text)
    (define lines (str-split text #\newline))

    (for-each (lambda (line)
      (%write-line column line))
      lines))
  (define (%write-line column line)
    (let loop ()
      (when (> newlines-needed 0)
        (write-string "\n" buffer)
        (set! newlines-needed (- newlines-needed 1))
        (loop)))
    
    (let loop ()
      (unless (= current-column column)
        (cond 
          ((< current-column (- column-count 1))
            (write-string (make-string (list-ref column-widths current-column) #\space) buffer))
          (else 
            (write-string "\n" buffer)))
        (set! current-column (modulo(+ 1 current-column) column-count))
        (loop)))

    (if (< column (length column-widths))
      (write-string (string-pad-right line (list-ref column-widths column)) buffer)
      (write-string line buffer))
      
    (set! current-column (modulo (+ 1 current-column) column-count))
    (when (= column (- column-count 1))
      (set! newlines-needed (+ 1 newlines-needed))))
  (define (build-allowed-list option)
    (define (default? x) (cond 
      ((list? (option-defaults-to option))
       (member x (option-defaults-to option)))
      (else (equal? x (option-defaults-to option)))))
    (define allowed-buffer (open-output-string))
    (write-string "[" allowed-buffer)
    
    (let loop ((first #t) (allowed (option-allowed option)))
      (cond 
        ((null? allowed) 
          (write-string "]" allowed-buffer)
          (get-output-string allowed-buffer))
        (else 
          (unless first (write-string ", " allowed-buffer))
          (write-string (car allowed) allowed-buffer)
          (if (default? (car allowed))
            (write-string " (default)" allowed-buffer))
          (loop #f (cdr allowed))))))
  (define (write-option option)
    (%write 0 (abbreviation option))
    (%write 1 (string-append (long-option option) (mandatory-option option)))
   
    (when (option-help option)
      (%write 2 (option-help option)))
    (cond
      ((option-allowed-help option)
        (newline)
        (for-each (lambda (entry)
            (%write 1 (allowed-title option (car entry)))
            (%write 2 (cdr entry)))
          
          (option-allowed-help option))
        (newline))
      ((option-allowed option)
        (%write 2 (build-allowed-list option)))
      ((option-flag? option)
        (if (eq? #t (option-defaults-to option))
          (%write 2 "(defaults to on)")))
      ((option-multi? option)
        (if (and (list? (option-defaults-to option))
                 (not (null? (option-defaults-to option))))
          (%write 2 (string-append "(defaults to " 
                                  (string-join (map (lambda (x) x) (option-defaults-to option)) ", ")
                                  ")"))))
      ((option-defaults-to option)
        (%write 2 (string-append "(defaults to " 
                                (option-defaults-to option)
                                ")")))))
  (for-each (lambda (item)
    (cond 
      ((string? item) (write-separator item))
      ((and (option? item) (not (option-hide? item))) (write-option item))))
    options-and-separators)
  (get-output-string buffer))
    
;;; str-split : Apr 2006 Doug Hoyte, hcsw.org.
;;; ----
;;; Splits a string 'str into a list of strings
;;; that were separated by the delimiter character 'ch
;;; ----
;;; Efficient as possible given that we can't count on
;;; 'str being an immutable string.

(define (str-split str ch)
  (let ((len (string-length str)))
    (letrec
      ((split
        (lambda (a b)
          (cond
            ((>= b len) (if (= a b) '() (cons (substring str a b) '())))
              ((char=? ch (string-ref str b)) (if (= a b)
                (split (+ 1 a) (+ 1 b))
                  (cons (substring str a b) (split b b))))
                (else (split a (+ 1 b)))))))
                  (split 0 0))))

(define (grammar-usage grammar)
  (if (%grammar-usage grammar)
    (%grammar-usage grammar)
    (let* ((options-and-separators (reverse (grammar-options-and-separators grammar)))
           (line-length 80)
           (usage (generate-usage options-and-separators line-length)))
      (%grammar-usage-set! grammar usage)
      usage)))