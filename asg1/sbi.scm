#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr

;; Kevin Woodward (keawoodw@ucsc.edu)
;; Megan Sharp (mesharp@ucsc.edu)
;;
;; NAME
;;    sbi.scm - silly basic interpreter
;;
;; SYNOPSIS
;;    sbi.scm filename.sbir
;;
;; DESCRIPTION
;;    The file mentioned in argv[1] is read and assumed to be an SBIR
;;    program, which is the executed.
;;

;; Function table
(define *function-table* (make-hash))

(define (function-get key)
   (hash-ref *function-table* key))

(define (function-put! key value)
   (hash-set! *function-table* key value))

(define (function-has-key? key)
   (hash-has-key? *function-table* key))

;; Label table
(define *label-table* (make-hash))

(define (label-get key)
   (hash-ref *label-table* key))

(define (label-put! key value)
   (hash-set! *label-table* key value))

(define (label-has-key? key)
   (hash-has-key? *label-table* key))

;; Variable table
(define *variable-table* (make-hash))

(define (variable-get key)
   (hash-ref *variable-table* key 0))

(define (variable-put! key value)
   (hash-set! *variable-table* key value))

(define (variable-has-key? key)
   (hash-has-key? *variable-table* key))

(variable-put! 'e  2.718281828459045235360287471352662497757247093)
(variable-put! 'pi 3.141592653589793238462643383279502884197169399)

;; Helpers

(define (get-input-number varname)    
   (define result (read))
   (cond
      ((eof-object? result)
         (begin (variable-put! 'inputcount -1) result))
      ((number? result) result)      
      (else
         (begin
            (variable-put!
               'inputcount
               (- (variable-get 'inputcount) 1))
               +nan.0))))

(define (error-and-quit filename linenum)
   (display "Error in file: ")
   (display filename)
   (display " on line: ")
   (display linenum)
   (display "\n")
   (exit 1))

(define (eval-expr expr resolve-vars?)
   (cond
      ((number? expr) expr)
      ((string? expr) expr)
      ((symbol? expr) (if resolve-vars? (variable-get expr) expr))
      ((and
         (not resolve-vars?)
         (not (function-has-key? (car expr)))
         (list (car expr) (eval-expr (cadr expr) #t))))          
      ((pair? expr)
         (if
            (vector? (variable-get (car expr)))
            (vector-ref
               (variable-get (car expr))
               (- (eval-expr (cadr expr) #t) 1))
            (apply (function-get (car expr))
               (map (curryr eval-expr #t) (cdr expr)))))))

;; Function table declaration

(for-each
   (lambda (pair)
      (function-put! (car pair) (cadr pair)))
   `(
      (print   ,(case-lambda
                   (() (display "\n"))
                   ((arg . rest)
                      (display arg)
                      (display " ")
                      (if
                         (not (null? rest))
                         (apply (function-get 'print) rest)
                         (display "\n")))))

      (let     ,(lambda (mem expr)
                   (cond
                      ((and
                         (symbol? mem)
                         (number? expr))
                         (variable-put! mem expr)) 
                      ((and
                        (pair? mem)
                        (number? expr))
                        (vector-set!
                           (variable-get (car mem))
                           (- (cadr mem) 1) expr)))))

      (dim     ,(lambda (data)
                   (variable-put!
                      (car data)
                      (make-vector (cadr data)))))

      (input   ,(lambda (arg . rest)
                   (variable-put! 'inputcount (+ 1 (length rest)))
                   (define (inputi argi . resti)
                      (define unnested-resti (car resti))
                      (define user-input (get-input-number argi))
                      (cond
                         ((eof-object? user-input)
                            (variable-put! 'inputcount -1))
                         ((nan? user-input) #f)
                         (else (variable-put! argi user-input)))
                      (when
                         (and
                            (not (eof-object? user-input))
                            (not (null? unnested-resti)))
                         (inputi
                            (car unnested-resti)
                            (cdr unnested-resti))))
                   (inputi arg rest)
                    ))

      (goto    ,(lambda (label)
                   (if
                      (label-has-key? label)
                      (label-get label)
                      (error "Label does not exist."))))

      (if      ,(lambda (boolean label)
                   (when boolean ((function-get 'goto) label))))
        
      (abs     ,abs)
      (acos    ,acos)
      (asin    ,asin)
      (atan    ,atan)
      (ceil    ,ceiling)
      (cos     ,cos)        
      (exp     ,exp)
      (floor   ,floor)
      (log     ,(case-lambda
                   ((x) (log (+ 0.0 x)))
                   ((x y) (log (+ 0.0 x) (+ 0.0 y)))))
      (log2    ,(lambda (x) (log (+ 0.0 x) 2)))
      (log10   ,(lambda (x) (log (+ x 0.0) 10)))                        
      (round   ,round)
      (sin     ,sin)
      (sqrt    ,sqrt)
      (tan     ,tan)
      (trunc   ,truncate)
      (+       ,+)
      (-       ,-)
      (^       ,expt)
      (/       ,(lambda (x y) (/ x (+ y 0.0))))
      (*       ,*)
      (%       ,(lambda
                   (x y)
                   (- x (* (truncate (/ x (+ 0.0 y))) (+ 0.0 y)))))
      (=       ,=)
      (<       ,<)
      (>       ,>)
      (<=      ,<=)
      (>=      ,>=)
      (<>      ,(lambda (x y) (not (= x y))))
      ))

;; Variable table declaration

(variable-put! 'e       2.718281828459045235360287471352662497757247093)
(variable-put! 'pi      3.141592653589793238462643383279502884197169399)

;; Main and execution
(define *stderr* (current-error-port))

(define *run-file*
   (let-values
      (((dirpath basepath root?)
         (split-path (find-system-path 'run-file))))
      (path->string basepath)))

(define (die list)
   (for-each (lambda (item) (display item *stderr*)) list)
   (newline *stderr*)
   (exit 1))

(define (usage-exit)
   (die `("Usage: " ,*run-file* " filename")))

(define (readlist-from-inputfile filename)
   (let ((inputfile (open-input-file filename)))
      (if (not (input-port? inputfile))
         (die `(,*run-file* ": " ,filename ": open failed"))
         (let ((program (read inputfile)))
            (close-input-port inputfile)
            program))))

(define (scan-for-labels program)
   (when (< 0 (length program))
      (define line (car program))
      (define rest (cdr program))
      (when (< 1 (length line))
         (when (symbol? (cadr line))
            (label-put! (cadr line) program)))
      (scan-for-labels rest))) 

(define (run-program program filename)
   (when (< 0 (length program))
      (define line (car program))
         (define rest (cdr program))
         (define linenum (car line))
         (with-handlers
            ([exn:fail? (lambda
                           (exn)
                           (error-and-quit filename linenum))])
            (define line-result (interpret-line line))         
            (if
               (pair? line-result)
               (run-program line-result filename)
               (run-program rest filename))
            (exit 0))))              

(define (interpret-line line)
   (cond
      ((= 3 (length line))
         (call-function (caddr line)))
      ((= 2 (length line))
         (when
            (not(label-has-key? (cadr line)))
            (call-function (cadr line))))))

(define (call-function stmt)
   (define function-name (car stmt))
   (cond
      ((eq? 'dim function-name)
         ((function-get (car stmt))
            (eval-expr (cadr stmt) #f)))
      ((eq? 'let function-name)
         ((function-get (car stmt))
            (eval-expr (cadr stmt) #f)
            (eval-expr (caddr stmt) #t)))
      ((eq? 'goto function-name)
         ((function-get (car stmt))
            (cadr stmt)))
      ((eq? 'if function-name)
         ((function-get (car stmt))
            (eval-expr (cadr stmt) #t)
            (eval-expr (caddr stmt) #f)))
      ((eq? 'print function-name)
         (apply
            (function-get (car stmt))
            (map (lambda (x) (eval-expr x #t)) (cdr stmt))))
      ((eq? 'input function-name)
         (apply
            (function-get (car stmt))
            (map (lambda (x) (eval-expr x #f)) (cdr stmt))))))


(define (main arglist)
   (if (or (null? arglist) (not (null? (cdr arglist))))
      (usage-exit)
      (let* ((sbprogfile (car arglist))
             (program (readlist-from-inputfile sbprogfile)))
                (scan-for-labels program)               
                (run-program program sbprogfile))))

(main (vector->list (current-command-line-arguments)))
