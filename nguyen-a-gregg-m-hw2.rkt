;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname nguyen-a-gregg-m-hw2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;Aaron Nguyen (anguyen3) , Miles Gregg (mgregg)

;;----------------------------------------------------------------------------------------
;; 1.

; dvd plan
;; itemization:
;; sigature: a dvd is (make-dvd boolean, number, boolean)
;; standard-definition is if the subscriber has sd or hd
;; number is 1-4
;; unlimited? is if the plan allows unlimited rental or not
(define-struct dvd (standard-definition? number unlimited?))

(define DVD1 (make-dvd false 1 true))
(define DVD2 (make-dvd true 2 false))
(define DVD3 (make-dvd false 3 false))
(define DVD4 (make-dvd true 4 true))
; streaming plan
;; itemization:
;; a streaming is (make-streaming boolean, number, boolean)
;; platform is the platform the user is using
;; number is 1-4
;; unlimited? is if the plan allows unlimited rental or not
(define-struct streaming (platform standard-definition? unlimited?))

(define STREAM1 (make-streaming "PC" true true))
(define STREAM2 (make-streaming "PC" false false))
(define STREAM3 (make-streaming "PC" true false))
(define STREAM4 (make-streaming "Mac" false false))
(define STREAM5 (make-streaming "Android Phone" true true))
(define STREAM6 (make-streaming "Roku" false true))

; plan
;; itemization:
;; the type of plan, either dvd or streaming
(define-struct plan (type))

(define PLAN1 DVD1)
(define PLAN2 STREAM1)

;;----------------------------------------------------------------------------------------
;; 2.

;; plan is one of:
;; - streaming
;; - dvd

; ;; DVD-fcn:  DVD -> ...
; ;; ...
; (define (DVD-fcn a-DVD)
;   (...  (DVD-definition a-DVD)        ;; Boolean
;         (DVD-amount a-DVD)        ;;Natural
;         (DVD-rent-period a-DVD)))  ;;Boolean

; ;; streaming-fcn:  streaming -> ...
; ;; ...
; (define (streaming-fcn a-streaming)
;   (...  (streaming-platform a-streaming)        ;; String
;         (streaming-defintion a-streaming)        ;;Boolean
;         (streaming-time a-streaming)))  ;;Boolean

; ;; rental-plan-fcn:  rental-plan -> ...
; ;; ...
; (define (rental-plan-fcn a-rental-plan)
;   (...  (rental-plan-type a-rental-plan)        ;; (make-fcn)

;;----------------------------------------------------------------------------------------
;; 3.

;;check-HD: plan Number -> Number
;;consumes the plan and the price then it produces cost of a dvd based off the definition of it
(define (check-HD plan price)
  (cond [(and (dvd? plan)(boolean=? (dvd-standard-definition? plan) false)) (+ price (* .50 (dvd-number plan)))]
        [(and (dvd? plan) (boolean=? (dvd-standard-definition? plan) true)) price]
        [else 0]))

(check-expect (check-HD 5 0) 0)
(check-expect (check-HD DVD1 3) 3.50)
(check-expect (check-HD DVD2 4) 4)

;;check-unlimited: plan -> Number
;;consumes the plan then it produces cost of the plan based off the unlimited or not
(define (check-unlimited plan)
  (cond [(and (dvd? plan)(boolean=? (dvd-unlimited? plan) true)) 3]
        [(and (streaming? plan)(boolean=? (streaming-unlimited? plan) true)) 5]
        [else 0]))

(check-expect (check-unlimited STREAM1) 5)
(check-expect (check-unlimited DVD1) 3)
(check-expect (check-unlimited DVD2) 0)
(check-expect (check-unlimited STREAM2) 0)

;;monthly-cost: plan -> Number
;;consumes the plan then it produces cost of the monthly plan with the other features
(define (monthly-cost plan)
  (cond [(dvd? plan) (cond [(= (dvd-number plan) 1) (+ (check-unlimited plan) (check-HD plan 7.99))]
                           [(= (dvd-number plan) 2) (+ (check-unlimited plan) (check-HD plan 8.99))]
                           [(= (dvd-number plan) 3) (+ (check-unlimited plan) (check-HD plan 9.99))]
                           [(= (dvd-number plan) 4) (+ (check-unlimited plan) (check-HD plan 10.99))])]
        [(streaming? plan) (if (boolean=? (streaming-standard-definition? plan) true)
                               (+ 4.99 (check-unlimited plan))
                               (+ 2.99 (check-unlimited plan)))]))

(check-expect (monthly-cost DVD1) 11.49)  
(check-expect (monthly-cost DVD2) 8.99)
(check-expect (monthly-cost DVD3) 11.49)
(check-expect (monthly-cost DVD4) 13.99)
(check-expect (monthly-cost STREAM1) 9.99)
(check-expect (monthly-cost STREAM2) 2.99)

;;----------------------------------------------------------------------------------------
;; 4.

;;make-high-def: aplan -> Plan
;;consumes the plan then it produces the same plan but with high def included
(define (make-high-def aplan)
  (cond [(dvd? aplan) (make-dvd false (dvd-number aplan) (dvd-unlimited? aplan))]
        [(streaming? aplan) (make-streaming (streaming-platform aplan) false (streaming-unlimited? aplan))]))

(check-expect (make-high-def DVD1) (make-dvd false 1 true))
(check-expect (make-high-def DVD2) (make-dvd false 2 false))
(check-expect (make-high-def STREAM1) (make-streaming "PC" false true))
(check-expect (make-high-def STREAM2) (make-streaming "PC" false false))

;;----------------------------------------------------------------------------------------
;; 5. 

(define STRING1 (cons "testing" (cons "2" empty)))

;;contains-all-numbers?: alos -> boolean
;;consumes a list of strings then it produces true if at least one of the strings is made up of all numbers
(define (contains-all-numbers? alos)
  (cond [(empty? alos) false]
        [(cons? alos) (if (number? (string->number (first alos)))
                          true
                          (contains-all-numbers? (rest alos)))]))

(check-expect (contains-all-numbers? empty) false)
(check-expect (contains-all-numbers? STRING1) true)
(check-expect (contains-all-numbers? (cons "dog" (cons "cat" empty))) false)
(check-expect (contains-all-numbers? (cons "34t" (cons "1" empty))) true)
(check-expect (contains-all-numbers? (cons "34t" (cons "1" empty))) true)

;;----------------------------------------------------------------------------------------
;; 6.

(define STRINGTEST (cons "testX" (cons "XjxXxX" empty)))

;;count-x: alos -> Number
;;consumes a list of strings then counts the number of x's or X's in a string list then produces the total
(define (count-x alos)
  (cond [(empty? alos) 0]
        [else (+ (x-counter (explode (first alos))) (count-x (rest alos)))]))

(check-expect (count-x empty) 0)
(check-expect (count-x STRINGTEST) 6)
(check-expect (count-x (cons "apple" (cons "dog" empty))) 0)

;;x-counter: alos -> Number
;;consumes a list of strings then counts the number strings with x's or X's
(define (x-counter input)
  (cond [(empty? input) 0]
        [(or (string-contains? "x" (first input)) (string-contains? "X" (first input))) (+ 1 (x-counter (rest input)))]
        [else (x-counter (rest input))]))

(check-expect (x-counter empty) 0)
(check-expect (x-counter STRINGTEST) 2)
(check-expect (x-counter (cons "apple" (cons "dog" empty))) 0)
(check-expect (x-counter (cons "applex" (cons "dog" empty))) 1)

(count-x STRINGTEST)

;;----------------------------------------------------------------------------------------
;; 7.

;; a ListOfNatural is one of
;;     empty
;;     (cons Natural ListOfNatural)

; ;; lon-fcn:  ListOfNatural ->
; ;;
; (define (lon-fcn alos)
;   (cond [(empty? alos)  (...)      ]
;         [(cons? alos)   (... (first alos)
;                              (lon-fcn (rest alos)))]))

(define list-N1 (cons 1 (cons 3 (cons 100 empty))))
(define StringL1 (cons "add" (cons "bear" empty)))

;;lengths-of-strings1: alos -> alon
;;consumes a list of strings then produces a list with their lengths
(define (lengths-of-strings alos)
  (cond [(empty? alos) empty]
        [(cons? alos) (cons (string-length (first alos)) (lengths-of-strings (rest alos)))]))

(check-expect (lengths-of-strings StringL1) (cons 3 (cons 4 ' ())))










