;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname nguyen-a-gregg-m-hw4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;Aaron Nguyen (anguyen3) , Miles Gregg (mgregg)

; 1

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

; 2

(define item1 (make-item 54503 "A toy" 2 5.00))
(define item2 (make-item 42013 "A brick" 2 5.00))
(define item3 (make-item 92503 "A stick" 3 5.00))
(define item4 (make-item 60240 "A computer" 1 5.00))

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
                                                   false))))

; 3

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

; 4

(define (add-cost aloi)
  (cond [(empty? aloi) 0]
        [(cons? aloi) (+ (* (item-quantity (first aloi)) (item-price (first aloi))) (add-cost (rest aloi)))]))

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

;(check-expect (order-cost order1 3) -1)
;; (order-left atree)

; insert: Node BST -> BST
; insert node into BST
#;(define (insert a-node a-tree);update programs will look like this
  (cond [(boolean? a-tree) a-node];most complicated base case we've seen!
        [(node? a-node) (if (< (node-key a-node) (node-key a-tree))
                            (make-node (node-key a-tree)
                                       (node-object a-tree)
                                       (node-index a-tree)
                                       (insert a-node (node-left a-tree))
                                       (node-right a-tree))
                            
                            (make-node (node-key a-tree)
                                       (node-object a-tree)
                                       (node-index a-tree)
                                       (node-left a-tree)
                                       (insert a-node (node-right a-tree))))]))

;; 5

(define (remove-item loi item-num)
  (cond [(empty? loi) empty]
        [(cons? loi) (if (= item-num (item-item-number (first loi)))
                         (remove-item (rest loi) item-num)
                         (cons (item-item-number (first loi)) (remove-item (rest loi) item-num)))]))

(define (remove-item-from-all-orders atree item-num)
  (cond [(boolean? atree) atree]
        [(order? atree) (make-order (order-order-number atree)
                                    (order-name atree)
                                    (order-credit-card atree)
                                    (remove-item (order-aloi atree) item-num)
                                    (remove-item-from-all-orders (order-left atree) item-num)
                                    (remove-item-from-all-orders (order-right atree) item-num))]))

;; 6

#;(define (list-sorted-order-numbers atree)
  (cond [(boolean? atree) empty]
        [(order? atree) (append (list (list-sorted-order-numbers (order-left atree)) (order-order-number atree))
                                (list (list-sorted-order-numbers (order-right atree))))]))

(define (list-sorted-order-numbers atree)
  (cond [(boolean? atree) empty]
        [(order? atree) (append (list (list-sorted-order-numbers (order-left atree)) (order-order-number atree))
                                (list (list-sorted-order-numbers (order-right atree))))]))

(list-sorted-order-numbers order1)

#;(define (gather-blue-eyed atree)
  (cond [(boolean? atree) empty]
        [(person? atree) (if (string=? "blue" (person-eye atree))
                             (cons (person-name atree)
                                   (append
                                    (gather-blue-eyed (person-mother atree))
                                    (gather-blue-eyed (person-father atree))))
                             (append
                                    (gather-blue-eyed (person-mother atree))
                                    (gather-blue-eyed (person-father atree))))]))

;; 7

(define (add-new-order order-number customer-name credit-card-number aloi atree)
  (cond [(boolean? atree) atree]
        [(order? atree) (if (>  (order-order-number (make-order order-number customer-name credit-card-number aloi false false)) (order-order-number atree))
                            (make-order (order-order-number atree)
                                        (order-name atree)
                                        (order-credit-card atree)
                                        (order-aloi atree)
                                        (make-order order-number customer-name credit-card-number aloi false false)
                                       (order-right atree))
                            
                            (make-order (order-order-number atree)
                                        (order-name atree)
                                        (order-credit-card atree)
                                        (order-aloi atree)
                                        (order-left atree)
                                       (make-order order-number customer-name credit-card-number aloi false false)))]))
                                
                                
(add-new-order 2 "jill" 1231219902 (list item1) order1)                            
(define ORDERJ (make-order 20 "jill" 1231219902 (list item1) false false))
