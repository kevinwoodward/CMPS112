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
        (hash-ref *variable-table* key 0)) ;; Treats uninitialized vars as 0.

(define (variable-put! key value)
        (hash-set! *variable-table* key value))

(define (variable-has-key? key)
        (hash-has-key? *variable-table* key))

(variable-put! 'e  2.718281828459045235360287471352662497757247093)
(variable-put! 'pi 3.141592653589793238462643383279502884197169399)

;; Helpers

(define (get-input-number varname)
    (display "Input a numeric or string filepath value for: ")
    (display varname)
    (display "\n")
    (define result (read))
    (cond
        ((eof-object? result) (begin (display "EOF encountered. Exiting with -1.\n") (exit -1)))
        ((number? result) result)
        (else (begin (display "Error: Non-numeric value has been entered. Replaced with +nan.0 .\n") +nan.0))))   

(define (error-and-quit filename linenum)
   (display "Error in file: ")
   (display filename)
   (display " on line: ")
   (display linenum)
   (display "\n")
   (exit 1))

(define (eval-expr expr resolve-vars?)
   (cond ((number? expr) expr)
         ((string? expr) expr)
         ((symbol? expr) (if resolve-vars? (variable-get expr) expr))
         ((and (not resolve-vars?) (not (function-has-key? (car expr))) expr))         
         ((pair? expr)
               (if
                  (vector? (variable-get (car expr)))
                  (vector-ref (variable-get (car expr)) (- (eval-expr (cadr expr) #t) 1))
                  (apply (function-get (car expr))
                  (map (curryr eval-expr #t) (cdr expr)))))))

;; Function table declaration

(for-each
    (lambda (pair)
            (function-put! (car pair) (cadr pair)))
    `(
        (print   ,(lambda (arg . rest)
                      (display arg)
                      (display " ")
                      (if (not (null? rest)) (apply (function-get 'print) rest) (display "\n") )))

        (let     ,(lambda (mem expr)
                      (cond
                          ((and (symbol? mem) (number? expr)) (variable-put! mem expr)) ;; Set var to number                          
                          ((and (pair? mem) (number? expr)) (vector-set! (variable-get (car mem)) (- (cadr mem) 1) expr)) ;; Set array value to a number 
                          ;; TODO ELSE??
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

        (goto    ,(lambda (label)
                    (if (label-has-key? label) (label-get label) (error "Label does not exist."))))

        (if      ,(lambda (boolean label)
                      (when boolean ((function-get 'goto) label))))
        
        (abs     ,abs)
        (acos    ,acos)
        (asin    ,asin)
        (atan    ,atan)
        (ceil    ,ceiling)
        (div     ,(lambda (x y) (floor (/ x y)))) ;;TODO GET RID OF SYM TAB EXTRA SHIT
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
        (-       ,-)
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
         (define line-result (interpret-line line)) ;;TODO check at this level if there is a goto, and if so then transfer control by (define rest (label-get labelname))
         (if (pair? line-result) (run-program line-result filename) (run-program rest filename)))
         (exit 0))
         
         ;;(run-program rest filename)))

(define (interpret-line line)
    (cond
        ((= 3 (length line)) (call-function (caddr line)))
        ((= 2 (length line)) (when (not(label-has-key? (cadr line))) (call-function (cadr line))))))

(define (call-function stmt)
    (define function-name (car stmt))
    (cond
        ((eq? 'dim function-name) ((function-get (car stmt)) (eval-expr (cadr stmt) #f)))
        ((eq? 'let function-name) ((function-get (car stmt)) (eval-expr (cadr stmt) #f) (eval-expr (caddr stmt) #t)))
        ((eq? 'goto function-name) ((function-get (car stmt)) (cadr stmt)))
        ((eq? 'if function-name) ((function-get (car stmt)) (eval-expr (cadr stmt) #t) (eval-expr (caddr stmt) #f)))
        ((eq? 'print function-name) (apply (function-get (car stmt)) (map (lambda (x) (eval-expr x #t)) (cdr stmt))))
        ((eq? 'input function-name) (apply (function-get (car stmt)) (map (lambda (x) (eval-expr x #f)) (cdr stmt))))))


(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist))
               (program (readlist-from-inputfile sbprogfile)))
              ;;(write-program-by-line sbprogfile program))))
               (scan-for-labels program)               
               (run-program program sbprogfile))))

(main '("00-hello-world.sbir"))
;(main (vector->list (current-command-line-arguments)))
