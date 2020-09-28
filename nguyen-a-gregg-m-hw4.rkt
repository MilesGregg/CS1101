;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname nguyen-a-gregg-m-hw4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;Aaron Nguyen (anguyen3) , Miles Gregg (mgregg)

;;----------------------------------------------------------------------------------------
;; 1

;; a Order is a (make-order String Natural TreeNode)
;;  interp:  represents a order in a tree of orders
;;     order-number is the unique order number(the key value)
;;     name is the person's name
;;     credit-card is their credit card number
;;     aloi is the list of items

;; a TreeNode is one of
;;    false
;;    ListOfItems
(define-struct order (order-number name credit-card aloi left right))

; Natural String Natural Natural -> item
; consumes a item-number, description, quantity, and price then produces a item
; A item consists of: a item-number (the item number) 
;                         description (description about the item)
;                         quantity (quantity of the item)
;                         price (price of the item)
(define-struct item (item-number description quantity price))

;;----------------------------------------------------------------------------------------
;; 2

(define item1 (make-item 54503 "A toy" 2 5.00))
(define item2 (make-item 42013 "A brick" 2 5.00))
(define item3 (make-item 92503 "A stick" 3 5.00))
(define item4 (make-item 60240 "A computer" 1 5.00))
(define item5 (make-item 25460 "Hot Dog" 4 3.00))
(define item6 (make-item 32046 "Calculator" 3 6.00))

(define order1 (make-order 10 "Paul" 5002200053026 (list item1 item2)
                           (make-order 8 "Bob" 5002200053026 (list item2 item3)
                                       (make-order 5 "Bobby" 5002200020456 (list item1 item3)
                                                   (make-order 3 "Max" 20145656323102 (list item2 item4)
                                                               false
                                                               false)
                                                   false)
                                       false)
                           (make-order 12 "Greg" 555043135435 (list item3 item1)
                                       false
                                       (make-order 18 "Aaron" 555043135425 (list item1 item2 item3)
                                                   (make-order 16 "Karen" 555043135445 (list item2 item4)
                                                               false
                                                               false)
                                                   (make-order 20 "Jack" 555043135415 (list item1 item2)
                                                               false
                                                               false)))))

(define order2 (make-order  30 "Bill" 12390967812 (list item1) false
                            (make-order 38 "Mary" 12390967701 (list item4 item1)
                                        (make-order 35 "Gary" 1239092310 (list item2) false false)
                                        false)))

(define order3 (make-order  30 "Bill" 12390967812 (list item1)
                            (make-order 25 "Joe" 12390967882 (list item6 item5)
                                        (make-order 20 "Gavin" 12390967820 (list item4 item6) false false)
                                        false)
                            (make-order 38 "Mary" 12390967701 (list item4 item1)
                                        (make-order 35 "Gary" 1239092310 (list item2) false false)
                                        false)))

;;----------------------------------------------------------------------------------------
;; 3

; ;; order-fcn:  order ->
; ;;
; (define (order-fcn a-order)
;   (... (order-number a-order)
;        (ordername a-order)
;        (order-credit-card a-order)
;        (order-aloi a-order)
;        (order-right a-order)
;        (order-left a-order)))

; ;; item-fcn:  item ->
; ;;
; (define (item-fcn a-item)
;   (... (item-item-number a-item)
;        (item-description a-item)
;        (item-quantity a-item)
;        (item-price a-item)))

; ;; los-fcn:  ListOfItems ->
; ;;
; (define (los-fcn aloi)
;   (cond [(empty? aloi)  (...) ]
;         [(cons? aloi)   (...        (string-fcn (first aloi))
;                                     (los-fcn (rest aloi)))]))

;; tree-fcn:  TreeNode ->
;;
#;(define (tree-fcn atree)
    (cond [(boolean? atree) (...)]
          [(order? atree) (... (order-order-number atree)
                               (order-name atree)
                               (order-credit-card atree)
                               (order-aloi atree)
                               (tree-fcn (order-right atree))
                               (tree-fcn (order-left atree)))]))

;;----------------------------------------------------------------------------------------
;; 4

;; add-cost: ListOfItem -> Natural
;; Takes a list of items and adds up the total cost of the items in the list
(define (add-cost aloi)
  (cond [(empty? aloi) 0]
        [(cons? aloi) (+ (* (item-quantity (first aloi)) (item-price (first aloi))) (add-cost (rest aloi)))]))

(check-expect (add-cost empty) 0)
(check-expect (add-cost (list item1)) 10)
(check-expect (add-cost (list item1 item2)) 20)
(check-expect (add-cost (list item1 item2 item3)) 35)
(check-expect (add-cost (list item1 item2 item3 item4)) 40)



;; order-cost: -> BST Natural -> Natural
;; Takes in a binary seatch tree and order number and produces the total cost of the orders
(define (order-cost atree order-number)
  (cond [(boolean? atree) -1]
        [(order? atree) (if (< order-number (order-order-number atree))
                            (if (= order-number (order-order-number atree))
                                (add-cost (order-aloi atree))
                                (order-cost (order-left atree) order-number))
                            (if (= order-number (order-order-number atree))
                                (add-cost (order-aloi atree))
                                (order-cost (order-right atree) order-number)))]))

(check-expect (order-cost order1 3) 15)
(check-expect (order-cost order1 16) 15)
(check-expect (order-cost order1 18) 35)
(check-expect (order-cost order1 8) 25)
(check-expect (order-cost order1 5) 25)
(check-expect (order-cost order1 4) -1)

;;----------------------------------------------------------------------------------------
;; 5

;; remove-item: ListOfItem Natural -> ListOfNatural
;; Takes in a list of item and item number and removes the item number from the list of item and returns a list of natural with the remaining items numbers
(define (remove-item aloi item-num)
  (cond [(empty? aloi) empty]
        [(cons? aloi) (if (= item-num (item-item-number (first aloi)))
                         (remove-item (rest aloi) item-num)
                         (cons (item-item-number (first aloi)) (remove-item (rest aloi) item-num)))]))

(check-expect (remove-item empty 54503) empty)
(check-expect (remove-item (list item1 item2) 54503) (list 42013))
(check-expect (remove-item (list item1 item2) 42013) (list 54503))
(check-expect (remove-item (list item2 item1 item3) 92503) (list 42013 54503))
(check-expect (remove-item (list item2 item1 item3 item4) 60240) (list 42013 54503 92503))

;; remove-item-from-all-orders: BST Natural -> BST
;; Takes in a binary search tree and item number and returns a binary search tree with the any item with the same item number removed from every order
(define (remove-item-from-all-orders atree item-num)
  (cond [(boolean? atree) atree]
        [(order? atree) (make-order (order-order-number atree)
                                    (order-name atree)
                                    (order-credit-card atree)
                                    (remove-item (order-aloi atree) item-num)
                                    (remove-item-from-all-orders (order-left atree) item-num)
                                    (remove-item-from-all-orders (order-right atree) item-num))]))


(check-expect (remove-item-from-all-orders order1 54503)(make-order
                                                         10
                                                         "Paul"
                                                         5002200053026
                                                         (list 42013)
                                                         (make-order 8 "Bob" 5002200053026 (list 42013 92503) (make-order 5 "Bobby" 5002200020456 (list 92503) (make-order 3 "Max" 20145656323102 (list 42013 60240) #false #false) #false) #false)
                                                         (make-order
                                                          12
                                                          "Greg"
                                                          555043135435
                                                          (list 92503)
                                                          #false
                                                          (make-order 18 "Aaron" 555043135425 (list 42013 92503) (make-order 16 "Karen" 555043135445 (list 42013 60240) #false #false) (make-order 20 "Jack" 555043135415 (list 42013) #false #false)))))
(check-expect (remove-item-from-all-orders order2 42013) (make-order 30 "Bill" 12390967812 (list 54503) #false (make-order 38 "Mary" 12390967701 (list 60240 54503) (make-order 35 "Gary" 1239092310 '() #false #false) #false)))

;;----------------------------------------------------------------------------------------
;; 6

;; list-sorted-order-numbers: BST -> ListOfNatural
;; Consumes a binary search tree and then produces a list of naturals of the order-numbers in accending order of the binary search tree
(define (list-sorted-order-numbers atree)
  (cond [(boolean? atree) empty]
        [(order? atree) (append (list-sorted-order-numbers (order-left atree))
                                (list (order-order-number atree))
                                (list-sorted-order-numbers (order-right atree)))]))

(check-expect (list-sorted-order-numbers order1) (list 3 5 8 10 12 16 18 20))
(check-expect (list-sorted-order-numbers order2) (list 30 35 38))
(check-expect (list-sorted-order-numbers order3) (list 20 25 30 35 38))

;;----------------------------------------------------------------------------------------
;; 7

;; add-new-oder: BST Natural String Natural ListOfItems -> BST
;; consumes a BST, order number, custormer name, credit card number, and a ListOfItems then produces a new BST with the new node inserted 
(define (add-new-order atree a-order-number a-customer-name a-credit-card-number a-aloi)
  (cond [(boolean? atree) (make-order a-order-number a-customer-name a-credit-card-number a-aloi false false)]
        [(order? atree) (if (< a-order-number (order-order-number atree))
                            (make-order (order-order-number atree)
                                        (order-name atree)
                                        (order-credit-card atree)
                                        (order-aloi atree)
                                        (add-new-order (order-left atree) a-order-number a-customer-name a-credit-card-number a-aloi)
                                        (order-right atree))
                            (make-order (order-order-number atree)
                                        (order-name atree)
                                        (order-credit-card atree)
                                        (order-aloi atree)
                                        (order-left atree)
                                        (add-new-order (order-right atree) a-order-number a-customer-name a-credit-card-number a-aloi)))]))

(check-expect (add-new-order order1 2 "jill" 1231219902 (list item1)) (make-order
                                                                       10
                                                                       "Paul"
                                                                       5002200053026
                                                                       (list (make-item 54503 "A toy" 2 5) (make-item 42013 "A brick" 2 5))
                                                                       (make-order
                                                                        8
                                                                        "Bob"
                                                                        5002200053026
                                                                        (list (make-item 42013 "A brick" 2 5) (make-item 92503 "A stick" 3 5))
                                                                        (make-order
                                                                         5
                                                                         "Bobby"
                                                                         5002200020456
                                                                         (list (make-item 54503 "A toy" 2 5) (make-item 92503 "A stick" 3 5))
                                                                         (make-order 3 "Max" 20145656323102 (list (make-item 42013 "A brick" 2 5) (make-item 60240 "A computer" 1 5))
                                                                                     (make-order 2 "jill" 1231219902 (list (make-item 54503 "A toy" 2 5)) #false #false)
                                                                                     #false)
                                                                         #false)
                                                                        #false)
                                                                       (make-order
                                                                        12
                                                                        "Greg"
                                                                        555043135435
                                                                        (list (make-item 92503 "A stick" 3 5) (make-item 54503 "A toy" 2 5))
                                                                        #false
                                                                        (make-order
                                                                         18
                                                                         "Aaron"
                                                                         555043135425
                                                                         (list (make-item 54503 "A toy" 2 5) (make-item 42013 "A brick" 2 5) (make-item 92503 "A stick" 3 5))
                                                                         (make-order 16 "Karen" 555043135445 (list (make-item 42013 "A brick" 2 5) (make-item 60240 "A computer" 1 5)) #false #false)
                                                                         (make-order 20 "Jack" 555043135415 (list (make-item 54503 "A toy" 2 5) (make-item 42013 "A brick" 2 5)) #false #false)))))

(check-expect (add-new-order order1 15 "Toby" 1231219902 (list item2 item6)) (make-order
                                                                              10
                                                                              "Paul"
                                                                              5002200053026
                                                                              (list (make-item 54503 "A toy" 2 5) (make-item 42013 "A brick" 2 5))
                                                                              (make-order
                                                                               8
                                                                               "Bob"
                                                                               5002200053026
                                                                               (list (make-item 42013 "A brick" 2 5) (make-item 92503 "A stick" 3 5))
                                                                               (make-order 5 "Bobby" 5002200020456 (list (make-item 54503 "A toy" 2 5) (make-item 92503 "A stick" 3 5))
                                                                                           (make-order 3 "Max" 20145656323102 (list (make-item 42013 "A brick" 2 5) (make-item 60240 "A computer" 1 5)) #false #false)
                                                                                           #false)
                                                                               #false)
                                                                              (make-order
                                                                               12
                                                                               "Greg"
                                                                               555043135435
                                                                               (list (make-item 92503 "A stick" 3 5) (make-item 54503 "A toy" 2 5))
                                                                               #false
                                                                               (make-order
                                                                                18
                                                                                "Aaron"
                                                                                555043135425
                                                                                (list (make-item 54503 "A toy" 2 5) (make-item 42013 "A brick" 2 5) (make-item 92503 "A stick" 3 5))
                                                                                (make-order 16 "Karen" 555043135445 (list (make-item 42013 "A brick" 2 5) (make-item 60240 "A computer" 1 5)) (make-order 15 "Toby" 1231219902 (list (make-item 42013 "A brick" 2 5) (make-item 32046 "Calculator" 3 6)) #false #false) #false)
                                                                                (make-order 20 "Jack" 555043135415 (list (make-item 54503 "A toy" 2 5) (make-item 42013 "A brick" 2 5)) #false #false)))))

(check-expect (add-new-order order1 30 "Dom" 1231219952 (list item3 item4)) (make-order
                                                                              10
                                                                              "Paul"
                                                                              5002200053026
                                                                              (list (make-item 54503 "A toy" 2 5) (make-item 42013 "A brick" 2 5))
                                                                              (make-order
                                                                               8
                                                                               "Bob"
                                                                               5002200053026
                                                                               (list (make-item 42013 "A brick" 2 5) (make-item 92503 "A stick" 3 5))
                                                                               (make-order 5 "Bobby" 5002200020456 (list (make-item 54503 "A toy" 2 5) (make-item 92503 "A stick" 3 5)) (make-order 3 "Max" 20145656323102 (list (make-item 42013 "A brick" 2 5) (make-item 60240 "A computer" 1 5)) #false #false) #false)
                                                                               #false)
                                                                              (make-order
                                                                               12
                                                                               "Greg"
                                                                               555043135435
                                                                               (list (make-item 92503 "A stick" 3 5) (make-item 54503 "A toy" 2 5))
                                                                               #false
                                                                               (make-order
                                                                                18
                                                                                "Aaron"
                                                                                555043135425
                                                                                (list (make-item 54503 "A toy" 2 5) (make-item 42013 "A brick" 2 5) (make-item 92503 "A stick" 3 5))
                                                                                (make-order 16 "Karen" 555043135445 (list (make-item 42013 "A brick" 2 5) (make-item 60240 "A computer" 1 5)) #false #false)
                                                                                (make-order 20 "Jack" 555043135415 (list (make-item 54503 "A toy" 2 5) (make-item 42013 "A brick" 2 5)) #false (make-order 30 "Dom" 1231219952 (list (make-item 92503 "A stick" 3 5) (make-item 60240 "A computer" 1 5)) #false #false))))))