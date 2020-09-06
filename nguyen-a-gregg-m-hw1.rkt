;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname nguyen-a-gregg-m-hw1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;Aaron Nguyen, Miles Gregg
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

;; 4.

(define-struct reservation (restaurant-name person phone date party))

(define Res (make-reservation "Blank" "Bob" "111-212-3333" 9/6 12))
(define Res1 (make-reservation "Blank1" "Joe" "123-212-3333" 9/4 10))
(define Res2 (make-reservation "Blank2" "Lily" "511-212-3233" 9/9 4))

;;add-to-party: reservation Number -> reservation
;;consumes a reservation and number and produces a reservation party increased by the number

(define (add-to-party reservation people)
  (+ (reservation-party reservation) people))

;(define (add-to-party1 reservation people)
;  (make-reservation (reservation-restaurant-name reservation)
;                    (reservation-person reservation)                    
;                    (reservation-phone reservation)
;                    (reservation-date reservation)
;                    (+ (reservation-party reservation) people)))

(define (add-to-party1 reservation people new-res)
  (define new-res(make-reservation (reservation-restaurant-name reservation)
                    (reservation-person reservation)                    
                    (reservation-phone reservation)
                    (reservation-date reservation)
                    (+ (reservation-party reservation) people))))




  


