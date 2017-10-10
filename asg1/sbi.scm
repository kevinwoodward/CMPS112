#!/usr/bin/racket -qr
#lang racket
;; $Id: sbi.scm,v 1.3 2016-09-23 18:23:20-07 - - $
;;
;; NAME
;;    sbi.scm - silly basic interpreter
;;
;; SYNOPSIS
;;    sbi.scm filename.sbir
;;
;; DESCRIPTION
;;    The file mentioned in argv[1] is read and assumed to be an SBIR
;;    program, which is the executed.  Currently it is only printed.
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
   (display "Input a numeric value for: ")
   (display varname)
   (display "\n")
   (define result (read))
   (if (not (number? result)) (begin (display "Error: Input not a number! Try again.\n") (get-input-number varname)) result))

(define (error-and-quit filename linenum)
   (display "Error in file: ")
   (display filename)
   (display " on line: ")
   (display linenum)
   (display "\n")
   (exit 1))

;;(define (func-intermediate expr)
    ;;(if ()

(define (eval-expr expr resolve-vars)
   (cond ((number? expr) expr)
         ((string? expr) expr)
         ((symbol? expr) (if resolve-vars (variable-get expr) expr))
         ((not (function-has-key? (car expr))) expr)         
         ((pair? expr)
               (define res (not (or (eq? (car expr) 'let) (eq? (car expr) 'dim) (eq? (car expr) 'input))))
               (apply (function-get (car expr))
               (map (curryr eval-expr res) (cdr expr))))))

;; Function table declaration

(for-each
    (lambda (pair)
            (function-put! (car pair) (cadr pair)))
    `(
        (print   ,(lambda (arg . rest)
                      (if (pair? arg) (if (variable-has-key? (car arg)) (display (vector-ref (variable-get (car arg)) (- (cadr arg) 1))) (error "error")) (display arg)) ;; If arg is array ref, get value else print arg                     
                      (display " ")
                      (when (not (null? rest)) (apply (function-get 'print) rest))
                      (when (null? rest) (display "\n"))))

        (let     ,(lambda (mem expr)
                      (cond
                          ((and (symbol? mem) (number? expr)) (variable-put! mem expr)) ;; Set var to number
                          ((and (symbol? mem) (symbol? expr) (variable-put! mem (variable-get expr)))) ;; Set var to var
                          ((and (symbol? mem) (pair? expr)) (variable-put! mem (vector-ref (variable-get (car expr)) (- (cadr expr) 1)))) ;; Set var to array value
                          ((and (pair? mem) (number? expr)) (vector-set! (variable-get (car mem)) (- (cadr mem) 1) expr)) ;; Set array value to a number 
                          ((and (pair? mem) (symbol? expr)) (vector-set! (variable-get (car mem)) (- (cadr mem) 1) (variable-get expr))) ;; Set array value to symbol
                          ((and (pair? mem) (pair? expr)) (vector-set! (variable-get (car mem)) (- (cadr mem) 1) (vector-ref (variable-get (car expr)) (- (cadr expr) 1))))
                          
                        )))

        (dim     ,(lambda (data)
                      (variable-put! (car data) (make-vector (cadr data)))))

        (input   ,(lambda (arg . rest)
                      (cond
                          ((symbol? arg) (variable-put! arg (get-input-number arg)))
                          ((pair? arg)
                              (if
                                  (variable-has-key? (car arg))
                                  (vector-set! (variable-get (car arg)) (- (cadr arg) 1) (get-input-number arg))
                                  (error))))
                          (when (not (null? rest)) (apply (function-get 'input) rest)))) 
                          
        
        (abs     ,abs)
        (acos    ,acos)
        (asin    ,asin)
        (atan    ,atan)
        (ceil    ,ceiling)
        (div     ,(lambda (x y) (floor (/ x y))))
        (exp     ,exp)
        (floor   ,floor)
        (log     ,log)
        (log2    ,(lambda (x) (log x 2)))
        (log10   ,(lambda (x) (log x 10)))
        (mod     ,(lambda (x y) (- x (* ((function-get 'div) x y) y))))
        (quot    ,(lambda (x y) (truncate (/ x y))))
        (rem     ,(lambda (x y) (- x (* ((function-get 'quot) x y) y))))
        (round   ,round)
        (sin     ,sin)
        (sqrt    ,sqrt)
        (tan     ,tan)
        (trunc   ,truncate)
        (+       ,+)
        (^       ,expt)
        (/       ,/)
        (*       ,*)
        (%       ,(lambda (x y) (- x (* (function-get 'trunc (/ x y)) y))))
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

;; Mackey's code
(define *stderr* (current-error-port))

(define *run-file*
    (let-values
        (((dirpath basepath root?)
            (split-path (find-system-path 'run-file))))
        (path->string basepath))
)

(define (die list)
    (for-each (lambda (item) (display item *stderr*)) list)
    (newline *stderr*)
    (exit 1)
)

(define (usage-exit)
    (die `("Usage: " ,*run-file* " filename"))
)

(define (readlist-from-inputfile filename)
    (let ((inputfile (open-input-file filename)))
         (if (not (input-port? inputfile))
             (die `(,*run-file* ": " ,filename ": open failed"))
             (let ((program (read inputfile)))
                  (close-input-port inputfile)
                         program))))

(define (write-program-by-line filename program)
    (printf "==================================================~n")
    (printf "~a: ~s~n" *run-file* filename)
    (printf "==================================================~n")
    (printf "(~n")
    (map (lambda (line) (printf "~s~n" line)) program)
    (printf ")~n"))

;; Scan for and add labels to table. TODO: Is tail recursive?
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
         ;;(with-handlers ([exn:fail? (lambda (exn) (error-and-quit filename linenum))]) (interpret-line line))
         (interpret-line line) ;;TODO check at this level if there is a goto, and if so then transfer control by (define rest (label-get labelname))
         (run-program rest filename)))

(define (interpret-line line)
    (cond
        ((= 3 (length line)) (eval-expr (caddr line) #t))
        ((= 2 (length line)) (when (not(label-has-key? (cadr line))) (eval-expr (cadr line) #t)))))


(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist))
               (program (readlist-from-inputfile sbprogfile)))
              ;;(write-program-by-line sbprogfile program))))
               (scan-for-labels program)
               (run-program program sbprogfile))))

(main '("00-hello-world.sbir"))
