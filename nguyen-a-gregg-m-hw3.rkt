;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname nguyen-a-gregg-m-hw3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;Aaron Nguyen (anguyen3) , Miles Gregg (mgregg)

(define-struct volunteer-org (kind name age consent license training hours languages))

(define ORG1 (make-volunteer-org "animal shelter" "blank" 16 true false 8 10 (cons "english" (cons "spanish" empty))))
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


;; a ListOfVolunteerOrg is one of
;; empty
;; (cons volunteer-org ListOfVolunteerOrg)

(define ORGS (cons ORG1 (cons (make-volunteer-org "animal shelter" "blank" 13 true false 8 10 (cons "english" empty)) empty)))
(define ORGS1 (cons ORG3 (cons (make-volunteer-org "soup kitchen" "blank2" 11 true false 4 5 (cons "english" empty)) empty)))

;; lov-fcn:  ListOfVolunteerOrg ->
; 
; (define (lov-fcn alov)
;   (cond [(empty? alov)  (...) ]
;         [(cons? alov)   (...        (volunteerorg-fcn (first alov))
;                                     (lov-fcn (rest alov)))]))

;; 5

;; count-hs-eligible: ListOfVolunteerOrg -> Natural
;; Consumes ListOfVolunteerOrg and returns the number of orgs with highschool eligibility (13 years and older)

(define (hs-eligble a-volunteerorg)
  (if (>= 13 (volunteer-org-age a-volunteerorg))
      true
      false))

(define (count-hs-eligble alov)
  (cond [(empty? alov) 0]
        [(cons? alov) (if (hs-eligble (first alov))
                          (+ 1 (count-hs-eligble (rest alov)))
                          (count-hs-eligble (rest alov)))]))
  
                    