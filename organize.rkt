#lang racket
;This code may make small babies cry
(require "generate.rkt")
(require "model.rkt")
;Where the PDF's need to be organized at
(define root-directory "/home/[USER]/Desktop/Test")

(define sep "/")
(define cur-directory root-directory)

(define (change-directory dir)
  (set! cur-directory (string-join (list root-directory sep dir) "")))

;Create temporary name for checking if it exists
(define (create-temp-dir-name dir)
  (string->path (string-join (list root-directory sep dir) "")))

;For building filepaths quickly
(define (quick-join a b)
  (string-join (list a sep b) ""))

(define (organize-pdf a-invoice)
  
  (define (check-directory-position a-show a-set)
    (cond
      ((directory-exists? (create-temp-dir-name (quick-join a-show a-set)))
       #t)
      ((directory-exists? (create-temp-dir-name a-show))
       'noset)
      (else
       'noshow)))
  
  (define (place-pdf a-show a-set)
    (copy-file
     (filename_complete a-invoice)
     (create-temp-dir-name
      (quick-join a-show
                  (quick-join a-set (build_invoice_name a-invoice))))
     #t))
  (letrec ([show (invoice-show a-invoice)]
           [set (invoice-set a-invoice)]
           [modeset (check-directory-position show set)])
    (cond
      ((equal? modeset #t)
       (place-pdf show set)
       #t)
      ((equal? modeset 'noset)
       (make-directory (create-temp-dir-name (quick-join show set)))
       (place-pdf show set))
      ((equal? modeset 'noshow)
       (make-directory (create-temp-dir-name show))
       (make-directory (create-temp-dir-name (quick-join show set)))
       (place-pdf show set))
      (else #f))))

(provide organize-pdf)
;I cried