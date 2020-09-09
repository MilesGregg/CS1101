;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname nguyen-a-gregg-m-hw1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;Aaron Nguyen (anguyen3) , Miles Gregg (mgregg)
;; 1.
(define-struct restaurant (name food tables seats vegetarian))

(define R1 (make-restaurant "Blank" "French" 10 40 true))
(define R2 (make-restaurant "Blank1" "Pizza" 5 20 false))
(define R3 (make-restaurant "Blank2" "Steakhouse" 50 250 true))

;; 2.
;;constructor
;;make-restaurant: String String Number Number Boolean -> restaurant (make-restaurant...)

;;Selectors
;;restaurant-name: restaurant -> String
;;restaurant-food: restaurant -> String
;;restaurant-tables: restaurant -> Number
;;restaurant-seats: restaurant -> Number
;;restaurant-vegetarian: restaurant -> Boolean

;;predicate:
;;restaurant?: Anytype -> Boolean

;; 3.

;;restaurant-type: restaurant -> String
;;consumes the restaurant and produces the type of restaurant
(define (restaurant-type restaurant)
  (cond [(and (>= (restaurant-tables restaurant) 30) (>= (restaurant-seats restaurant) 250)) "event venue"]
        [(boolean=? (restaurant-vegetarian restaurant) true) "vegetarian-friendly"]
        [else (restaurant-food restaurant)]))

(check-expect (restaurant-type R1) "vegetarian-friendly")
(check-expect (restaurant-type R2) "Pizza")
(check-expect (restaurant-type R3) "event venue")

;; 4.
(define-struct date (month day year ))
(define-struct reservation (restaurant-name person phone date party))

(define Res (make-reservation "Blank" "Bob" "111-212-3333" (make-date 9 6 2020) 12))
(define Res1 (make-reservation "Blank1" "Joe" "123-212-3333" (make-date 9 4 2020) 10))
(define Res2 (make-reservation "Blank2" "Lily" "511-212-3233" (make-date 9 9 2020) 4))

;; 5.
;;add-to-party: reservation Number -> reservation
;;consumes a reservation and number and produces a reservation party increased by the number

(define (add-to-party reservation people)
   (make-reservation (reservation-restaurant-name reservation)
                    (reservation-person reservation)                    
                    (reservation-phone reservation)
                    (reservation-date reservation)
                    (+ (reservation-party reservation) people)))

(check-expect (add-to-party Res 12) (make-reservation "Blank" "Bob" "111-212-3333" (make-date 9 6 2020) 24))
(check-expect (add-to-party Res1 -6) (make-reservation "Blank1" "Joe" "123-212-3333" (make-date 9 4 2020) 4))
(check-expect (add-to-party Res2 0) (make-reservation "Blank2" "Lily" "511-212-3233" (make-date 9 9 2020) 4))

;; 6.

;;precedes: Number Number -> Boolean
;;Takes two dates and returns true if first date precedes second

;(define (precedes? date1 date2)
;  (> date1 date2))
(define (precedes? date1 date2)
  (if (> (date-year date1) (date-year date2))
      false
     (if (> (date-month date1) (date-month date2))
         false
         (if (>= (date-day date1) (date-day date2))
             false
             true))))

(check-expect (precedes? (make-date 9 6 2020) (make-date 9 10 2020)) true)
(check-expect (precedes? (make-date 9 4 2020) (make-date 9 2 2020)) false)
(check-expect (precedes? (make-date 9 1 2020) (make-date 9 1 2020)) false)
(check-expect (precedes? (make-date 8 2 2020) (make-date 9 5 2020)) true)
(check-expect (precedes? (make-date 9 2 2001) (make-date 9 5 2004)) true)
(check-expect (precedes? (make-date 9 2 2010) (make-date 9 5 2008)) false)
;; 7.

;;reservation-OK?: reservation number -> boolean
;;consumes reservation and date to get OK or Not OK

(define (reservation-OK? reservation today-date restaurant)
  (cond [(or (precedes? (reservation-date reservation) today-date)
             (< (restaurant-seats restaurant) (reservation-party reservation))) "not OK"]
        [else "OK"]))

(check-expect (reservation-OK? Res (make-date 9 9 2020) R1) "not OK")
(check-expect (reservation-OK? Res (make-date 9 4 2020) R1) "OK")


