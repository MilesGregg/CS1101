;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname nguyen-a-gregg-m-hw1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;Aaron Nguyen, Miles Gregg
;; 1.
(define-struct restaurant (name food tables seats vegetarian))

(define R1 (make-restaurant "Blank" "French" 10 40 true))
(define R2 (make-restaurant "Blank1" "Pizza" 5 20 false))
(define R2 (make-restaurant "Blank2" "Steakhouse" 50 250 true))

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