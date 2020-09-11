;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname nguyen-a-gregg-m-hw2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; dvd plan
;; itemization:
;; sigature: a dvd is (make-dvd boolean, number, boolean)
;; standard-definition is if the subscriber has sd or hd
;; number is 1-4
;; unlimited? is if the plan allows unlimited rental or not
(define-struct dvd (standard-definition? number unlimited?))

(define DVD1 (make-dvd false 1 true))
(define DVD2 (make-dvd true 3 false))
; streaming plan
;; itemization:
;; a streaming is (make-streaming boolean, number, boolean)
;; platform is the platform the user is using
;; number is 1-4
;; unlimited? is if the plan allows unlimited rental or not
(define-struct streaming (platform standard-definition? unlimited?))

(define STREAM2 (make-streaming "PC" true true))
(define STREAM3 (make-streaming "PC" false false))

; plan
;; itemization:
;; the type of plan, either dvd or streaming
(define-struct plan (type))

(define PLAN1 DVD1)

;; 3.

(define (check-HD plan price)
  (cond [(and (dvd? plan)(boolean=? (dvd-standard-definition? plan) false)) (+ price (* .50 (dvd-number plan)))]
        [(and (dvd? plan) (boolean=? (dvd-standard-definition? plan) true)) price]))

(define (check-unlimited plan)
  (cond [(and (dvd? plan)(boolean=? (dvd-unlimited? plan) true)) 3]
        [(and (streaming? plan)(boolean=? (streaming-unlimited? plan) true)) 5]
        [else 0]))

(define (monthly-cost plan)
  (cond [(dvd? plan) (cond [(= (dvd-number plan) 1) (+ (check-unlimited plan) (check-HD plan 7.99))]
                           [(= (dvd-number plan) 2) (+ (check-unlimited plan) (check-HD plan 8.99))]
                           [(= (dvd-number plan) 3) (+ (check-unlimited plan) (check-HD plan 9.99))]
                           [(= (dvd-number plan) 4) (+ (check-unlimited plan) (check-HD plan 10.99))])]
        [(streaming? plan) (if (boolean=? (streaming-standard-definition? plan) true)
                               (+ 4.99 (check-unlimited plan))
                               (+ 2.99 (check-unlimited plan)))]))

;; 4.

(define (make-high-def aplan)
  (cond [(dvd? aplan) (make-dvd false (dvd-number aplan) (dvd-unlimited? aplan))]
        [(streaming? aplan) (make-streaming (streaming-platform aplan) false (streaming-unlimited? aplan))]))

;; 5. 

(define STRING1 (cons "testing" (cons "2" empty)))

(define (contains-all-numbers? alos)
  (cond [(empty? alos) false]
        [(cons? alos) (if (number? (string->number (first alos)))
                          true
                          (contains-all-numbers? (rest alos)))]))













