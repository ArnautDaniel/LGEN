#lang racket

(require "model.rkt"
         "generate.rkt"
         "db.rkt"
         control)

(define dir (directory-list (build-path (current-directory) "massconvert") #:build? #t))

(define (csv-convert path)
  (define csv-invoice (initialize-invoice!))
  (define csv (open-input-file path))
  
  (define (trim-to n)
    (cond
      ((= n 0) #t)
      (else
       (read-line csv)
       (trim-to (- n 1)))))
  
  (define (project-show-person)
    (let ((b  (regexp-split #rx"," (read-line csv))))
      (cond ((>= (length b) 3)
             (set-invoice-show! csv-invoice (second b)))
            (else
             (set-invoice-show! csv-invoice "No Data"))))
    (let ((a  (regexp-split #rx"," (read-line csv))))
      (cond ((>= (length a) 3)
          (set-invoice-set! csv-invoice (second a))
          (set-invoice-person! csv-invoice (fifth a)))
            (else
             (set-invoice-set! csv-invoice "No Data")
             (set-invoice-person! csv-invoice "No Data")))))
    
  
  (define (date-inv)
    (read-line csv)
    (read-line csv)
    (let ((a (read-line csv)))
      (cond
        ((<= (length (regexp-split #rx"," a)) 2)
         (set-invoice-date! csv-invoice "No Date"))
        (else
         (set-invoice-date! csv-invoice (second (regexp-split #rx"," a)))))))
                                                
        (define (build-bodylist)
          (letrec ((str (read-line csv))
                   (str-spl (regexp-split #rx"," str)))
            
            (cond
              ((or (eq? (first str-spl) "")
                   (<= (length str-spl) 1)
                   (eq? (second str-spl) "")) #f)
              (else
               (if (> (length str-spl) 3)
               (let ((description (second str-spl))
                     (qty (third str-spl))
                     (price (fourth str-spl)))
                 (invoice-insert-bodylist! csv-invoice description qty price))
               #f)))))
        
        csv-invoice
        (trim-to 7)
        (project-show-person)
        (date-inv)
        (read-line csv)
        (read-line csv)
        (while (eq? (eof-object? (peek-string 8 0 csv)) #f)
               (build-bodylist))
        csv-invoice)
      
      
      (define csv-list
        (filter (lambda (r) (let ((x (path-get-extension r)))
                              (cond
                                ((eq? x #f) #f)
                                ((bytes=? x #".csv") #t)
                                (else
                                 #f)))) dir))
      
      
      (map (lambda (r)
             (insert-invoice-data! lgen-path r)) (map csv-convert csv-list))
      