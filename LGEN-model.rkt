#lang racket

(struct invoice (set show person date bodylist) #:mutable #:prefab)
(struct body (description qty price) #:mutable #:prefab)

(define (invoice-insert-bodylist! a-invoice description qty price)
  (set-invoice-bodylist!
   a-invoice
   (cons (body description qty price) (invoice-bodylist a-invoice))))

(define (initialize-invoice!)
  (invoice "" "" "" "" '()))

(provide invoice invoice? invoice-set invoice-show invoice-person invoice-date
         invoice-bodylist initialize-invoice!
         body body? body-description body-qty body-price
         invoice-insert-bodylist!)
         