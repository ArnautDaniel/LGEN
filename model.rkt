#lang racket

(struct invoice (set show person date bodylist) #:mutable #:prefab)
(struct body (description qty price) #:mutable #:prefab)
;An invoice is all strings except for bodylist which is a
;bodylist -> body?

;Create a body struct with the items that makeup a body
;and cons it onto the given invoice
(define (invoice-insert-bodylist! a-invoice description qty price)
  (set-invoice-bodylist!
   a-invoice
   (cons (body description qty price) (invoice-bodylist a-invoice))))

;default empty invoice
(define (initialize-invoice!)
  (invoice "" "" "" "" '()))

(provide invoice invoice? invoice-set invoice-show invoice-person invoice-date
         invoice-bodylist initialize-invoice!
         body body? body-description body-qty body-price
         invoice-insert-bodylist! set-invoice-show! set-invoice-set!
         set-invoice-person! set-invoice-date! )

