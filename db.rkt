#lang racket
(require db
         racket/date
         "model.rkt")

(define lgen-path "/home/silver/Desktop/lgen/lgen.db")

(define db (sqlite3-connect #:database lgen-path #:mode 'create))

(define dateToday (string-join
                   (map number->string
                        (list
                         (date-month (current-date))
                         (date-day (current-date))
                         (date-year (current-date)))) "-"))
(define (clean-show-name a-showname)
  (string-join (string-split a-showname " ") ""))

;init-invoice! : path? invoice? ->

(define (init-invoice! a-invoice)
  (query-exec db
              (string-append
               "CREATE TABLE [" (clean-show-name (invoice-show a-invoice))
               "] (id INTEGER PRIMARY KEY, nameSet TEXT, item TEXT, price INTEGER, qty INTEGER, contact TEXT, todaysDay TEXT, checkd TEXT)")))

(define (insert-invoice-data! lgen-path a-invoice)
  (define (insert-bodylist-data! a-bodylist)
    (cond
      ((not (pair? a-bodylist)) #t)
      ((empty? a-bodylist) #t)
      (else
       (query-exec db
                   (string-append
                    "INSERT INTO [" (clean-show-name (invoice-show a-invoice))
                    "] (nameSet, item, price, qty, contact, todaysDay, checkd) VALUES (?, ?, ?, ?, ?, ?, ?)")
                   (invoice-set a-invoice)
                   (body-description (car a-bodylist))
                   (body-price (car a-bodylist))
                   (body-qty (car a-bodylist))
                   (invoice-person a-invoice)
                   dateToday
                   "false")
       (insert-bodylist-data! (cdr a-bodylist)))))
    
  (unless (table-exists? db (clean-show-name (invoice-show a-invoice)))
    (init-invoice! a-invoice))
  (insert-bodylist-data! (invoice-bodylist a-invoice)))

(provide insert-invoice-data! init-invoice! dateToday db lgen-path)
  