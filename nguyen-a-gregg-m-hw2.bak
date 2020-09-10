;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname nguyen-a-gregg-m-hw2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; dvd plan
;; itemization:
;; sigature: a dvd is (make-dvd boolean, number, boolean)
;; standard-definition is if the subscriber has sd or hd
;; number is 1-4
;; unlimited? is if the plan allows unlimited rental or not
(define-struct dvd (standard-definition number unlimited?))

(define DVD1 (make-dvd true 3 true))

; streaming plan
;; itemization:
;; a streaming is (make-streaming boolean, number, boolean)
;; platform is the platform the user is using
;; number is 1-4
;; unlimited? is if the plan allows unlimited rental or not
(define-struct streaming (platform standard-definition unlimited?))

(define DVD2 (make-dvd "PC" false false))

; plan
;; itemization:
;; the type of plan, either dvd or streaming
(define-struct plan (type))

(define PLAN1 DVD1)
