;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname nguyen-a-gregg-m-hw3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;Aaron Nguyen (anguyen3) , Miles Gregg (mgregg)

;;1

(define-struct volunteer-org (kind name age consent license training hours languages))

(define ORG1 (make-volunteer-org "animal shelter" "blank" 16 true false 8 10 (cons "spanish" (cons "english" empty))))
(define ORG2 (make-volunteer-org "nursing home" "blank1" 18 false true 24 15 (cons "english" (cons "chinese" (cons "spanish" empty)))))
(define ORG3 (make-volunteer-org "soup kitchen" "blank2" 10 true false 4 5 (cons "english" empty)))
;; a Volunteer-org is a (make-volunteer-org String String Natural Boolean Boolean Natural Natural alos)
;;  interp:  represents a volunteer organization
;;    kind is the type of organization String
;;    name is the name of the organization String
;;    age is the minimum age reuirement to volunteer Natural
;;    consent is whehther or not people under 18 require parentel consent Boolean
;;    license is if a driving license is required Boolean
;;    training is the number of hours training required Natual
;;    hours is the minimum hours of work required each week Natural
;;    languages is a list of languages spoken by clients alos

;; 2

; ;; volunteer-org-fcn:  volunteer-org ->
; ;;
; (define (volunteer-org-fcn a-volunteer-org)
;   (... (volunteer-org-kind a-volunteer-org)
;        (volunteer-org-name a-volunteer-org)
;        (volunteer-org-age a-volunteer-org)
;        (volunteer-org-consent a-volunteer-org)
;        (volunteer-org-license a-volunteer-org)
;        (volunteer-org-training a-volunteer-org)
;        (volunteer-org-hours a-volunteer-org)
;        (volunteer-org-languages volunteer-org)))


;; a ListOfString is one of
;;    empty
;;    (cons Course ListOfString)

; ;; los-fcn:  ListOfString ->
; ;;
; (define (los-fcn alos)
;   (cond [(empty? alos)  (...) ]
;         [(cons? alos)   (...        (string-fcn (first alos))
;                                     (los-fcn (rest alos)))]))

;;3

;; a ListOfVolunteerOrg is one of
;; empty
;; (cons volunteer-org ListOfVolunteerOrg)

(define ORGS (cons ORG1 (cons (make-volunteer-org "animal shelter" "blank" 13 true false 8 10 (cons "spanish" empty)) empty)))
(define ORGS1 (cons ORG2 (cons (make-volunteer-org "soup kitchen" "blank2" 11 true false 4 5 (cons "english" empty)) empty)))
(define ORGS2 (cons ORG3 (cons (make-volunteer-org "soup kitchen" "blank2" 11 true false 4 5 (cons "english" empty)) empty)))

;;4

;; lov-fcn:  ListOfVolunteerOrg ->
; 
; (define (lov-fcn alov)
;   (cond [(empty? alov)  (...) ]
;         [(cons? alov)   (...        (volunteerorg-fcn (first alov))
;                                     (lov-fcn (rest alov)))]))

;; 5
;; hs-eligble?: volunteer-org -> Boolean
;; Consumes a volunteer-org and returns true if the minimum age is 13 and younger
(define (hs-eligble? a-volunteerorg)
  (>= 13 (volunteer-org-age a-volunteerorg)))

(check-expect (hs-eligble? ORG1) false)
(check-expect (hs-eligble? ORG3) true)

;; count-hs-eligible: ListOfVolunteerOrg -> Natural
;; Consumes ListOfVolunteerOrg and returns the number of orgs with highschool eligibility (13 years and older)
(define (count-hs-eligble alov)
  (cond [(empty? alov) 0]
        [(cons? alov) (if (hs-eligble? (first alov))
                          (+ 1 (count-hs-eligble (rest alov)))
                          (count-hs-eligble (rest alov)))]))

(check-expect (count-hs-eligble empty) 0)
(check-expect (count-hs-eligble ORGS) 1)
(check-expect (count-hs-eligble ORGS2) 2)


;; 6  ---------------CHECK THIS------------------

;; has-license?: volunteer-org -> Boolean
;; Consumes a volunteer org and returns true if voulnteer org requires license
(define (has-license? a-volunteerorg)
  (volunteer-org-license a-volunteerorg))
(check-expect (has-license? ORG1) false)
(check-expect (has-license? ORG2) true)

;; list-license-training: ListOfVolunteerOrg Natural -> ListOfVolunteerOrg
;; Consumes ListOfVolunteerOrg and a Natural then returns a ListOfVolunteerOrg only contains those from the orginal list that require volunteers to be licensed


(define (list-license-training alov hours-of-training)
  (cond [(empty? alov) empty]
        [(cons? alov) (if (and (has-license? (first alov)) (< (volunteer-org-training (first alov)) hours-of-training))
                          (cons (first alov) (list-license-training (rest alov) hours-of-training))
                          (list-license-training (rest alov) hours-of-training))]))

(check-expect (list-license-training empty 15) empty)
(check-expect (list-license-training ORGS 15) empty)
(check-expect (list-license-training ORGS1 25)(cons
                                               (make-volunteer-org
                                                "nursing home"
                                                "blank1"
                                                18
                                                #false
                                                #true
                                                24
                                                15
                                                (cons "english" (cons "chinese" (cons "spanish" '()))))
                                               '()))
;; 7

;; languages-spoken : ListOfVolunteerOrg -> ListOfString
;; consumes a list of volunteerorg and produces a list of all the languages spoken by the clients of organizations
(define (languages-spoken alov)
  (cond [(empty? alov) empty]
        [(cons? alov) (cons (volunteer-org-languages (first alov)) (languages-spoken (rest alov)))]))

(check-expect (languages-spoken empty) empty)
(check-expect (languages-spoken ORGS1) (cons
                                        (cons "english" (cons "chinese" (cons "spanish" '())))
                                        (cons (cons "english" '()) '())))
;; 8
;; check-spanish?: volunteer-org -> Boolean
;; Consumes a volunteer org and returns true if it contains spanish as a language
(define (check-spanish? volunteer-org)
  (cond [(empty? (volunteer-org-languages volunteer-org)) empty]
        [(cons? (volunteer-org-languages volunteer-org)) (string=? "spanish" (first (volunteer-org-languages volunteer-org)))]))

(check-expect (check-spanish? ORG1) true)
(check-expect (check-spanish? ORG3) false)

;; need-spanish-speakers: ListOfVoulunteerOrg -> ListOfVolunteerOrg
;; Consumes a ListOfVolunteerOrg and returns a ListOfVolunteerOrg with only Orgs that list spanish as a language

(define (need-spanish-speakers alov)
  (cond [(empty? alov) empty]
        [(cons? alov) (if (check-spanish? (first alov))
                          (cons (first alov) (need-spanish-speakers (rest alov)))
                          (need-spanish-speakers (rest alov)))]))

(check-expect (need-spanish-speakers empty) empty)
(check-expect (need-spanish-speakers ORGS) (cons
                                            (make-volunteer-org "animal shelter" "blank" 16 #true #false 8 10 (cons "spanish" (cons "english" '())))
                                            (cons (make-volunteer-org "animal shelter" "blank" 13 #true #false 8 10 (cons "spanish" '())) '())))
(check-expect (need-spanish-speakers ORGS2) empty)























