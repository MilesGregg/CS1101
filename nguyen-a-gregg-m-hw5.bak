;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname nguyen-a-gregg-m-hw5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; 1

;; a River is a (make-river String String Natural ListOfRivers)
;;  interp:  represents a river along with other rivers
;;     name is the name of the river
;;     pH is pH of the water
;;     DO in measused in parts per million
;;     rivers is the list of other rivers
(define-struct river (name pH DO rivers))

;; 2

(define RIVERS
  (make-river "Missouri" 12 5
               (list (make-river "Jefferson" 6.5 6
                                 (list (make-river "Beaverhead" 8.5 7 empty)
                                       (make-river "Big Hole" 7.5 3 empty)))
                     (make-river "Sun" 2.2 4 empty)
                     (make-river "Yellowstone" 12 5
                                 (list (make-river "Gardner" 3 12 empty)
                                       (make-river "Shields" 4 0 empty)
                                       (make-river "Boulder" 6 3 empty)))
                     (make-river "Madison" 8 4 empty)
                     (make-river "Gallatin" 8.5 7 empty))))

(define RIVERS2
  (make-river "Missouri" 7 7
              (list (make-river "Mississippi" 7 7 empty))))

;; 3

; ;; river-fcn:  River ->
; ;;
; (define (river-fcn: a-river)
;   (...  (river-name a-river)
;         (river-pH a-river)
;         (river-DO a-person)
;         (lor-fcn: (river-rivers a-river))))
; 
; 
; ;; lor-fcn:  ListOfRivers ->
; ;;
; (define (lor-fcn: alor)
;   (cond [(empty? alor) (...)]
;         [(cons? alor)  (...  (river-fcn: (first alor))
;                              (lor-fcn: (rest alor)))]))
; 


;; 4

(define (lower-ph-than river pH)
  (if (< (river-pH river) pH)
      (cons (river-name river)
            (lower-ph-than-list pH (river-rivers river)))
      (lower-ph-than-list pH (river-rivers river))))


(define (lower-ph-than-list pH alor)
  (cond [(empty? alor) empty]
        [(cons? alor) (append (lower-ph-than (first alor) pH)
                              (lower-ph-than-list pH (rest alor)))]))

;; 5

;; healthy?: river -> boolean
;; consumes a river and produces true if every river in the hiarchy
;; has a pH between 6.5 and 8.5 and DO has to be least 6ppm
(define (healthy? a-river)
  (and (> (river-pH a-river) 6.5)
       (< (river-pH a-river) 8.5)
       (>= (river-DO a-river) 6)
       (all-healthy? (river-rivers a-river))))

;; all-healthy?: ListOfRiver -> boolean
;; consumes a list of river and produces true if all the rivers
;; in the list have a pH between 6.5 and 8.5 and the DO has to be at least 6ppm
(define (all-healthy? alor)
  (cond [(empty? alor) true]
        [(cons? alor) (and (healthy? (first alor))
                           (all-healthy? (rest alor)))]))

;; 6

;; lower-all-ph: river -> river
;; consumes a river and prodcues the same river except the pH is lowered by 0.1
(define (lower-all-ph a-river)
  (make-river (river-name a-river)
              (- (river-pH a-river) 0.1)
              (river-DO a-river)
              (lower-all-ph-list (river-rivers a-river))))

;; lower-all-ph-list: ListOfRiver -> ListOfRiver
;; consumes a list of river and produces the same list as before except the pH is lowered by 0.1
(define (lower-all-ph-list alor)
  (cond [(empty? alor) empty]
        [(cons? alor) (cons (lower-all-ph (first alor))
                            (lower-all-ph-list (rest alor)))]))


;; --------------------------------------------- PART 2 --------------------------------------------- 

(define-struct volunteer-org (type name age consent? license? training hours languages))
;;a VolunteerOrg is a (make-volunteer-org String String Natural Boolean Boolean Natural ListOfString)
;;interp:
;;  VolunteerOrg represents information about a volunteer organization, where
;;  type is the type of organization, such as animal shelter or nursing home
;;  name is the name of the organization;;  age is the minimum age required for volunteering
;;  consent?  is true if parental consent is needed for volunteers under age 18
;;  license? is true if a valid driver's license is required of volunteers
;;  training is the number of hours of training required prior to volunteering
;;  hours is the minimum number of volunteer hours required per week
;;  languages is a list of the languages spoken by clients of the organization

;; an ListOfVolunteerOrg is one of
;; empty, or
;; (cons VolunteerOrg ListOfVolunteerOrg)

(define ORG1 (make-volunteer-org "animal shelter" "blank" 16 true false 8 10 (cons "spanish" (cons "english" empty))))
(define ORG2 (make-volunteer-org "nursing home" "blank1" 18 false true 24 15 (cons "english" (cons "chinese" (cons "spanish" empty)))))
(define ORG3 (make-volunteer-org "soup kitchen" "blank2" 10 true false 4 5 (cons "english" empty)))
(define ORG4 (make-volunteer-org "soup kitchen" "blank2" 10 true true 4 5 (cons "english" (cons "german" empty))))

(define ORGS (cons ORG1 (cons (make-volunteer-org "animal shelter" "blank" 13 true false 8 10 (cons "spanish" empty)) empty)))
(define ORGS1 (cons ORG2 (cons (make-volunteer-org "soup kitchen1" "blank2" 11 true false 4 5 (cons "english" empty)) empty)))
(define ORGS2 (cons ORG3 (cons (make-volunteer-org "soup kitchen2" "blank3" 11 true false 4 5 (cons "english" empty)) empty)))
(define ORGS3 (cons ORG4 (cons (make-volunteer-org "soup kitchen3" "blank4" 11 true true 3 5 (cons "german" (cons "french" empty))) empty)))


;; 7

;; has-license?: volunteer-org -> Boolean
;; Consumes a volunteer org and returns true if voulnteer org requires license
(define (has-license? a-volunteerorg)
  (volunteer-org-license? a-volunteerorg))

;; list-license-training:  ListOfVolunteerOrg Natural -> ListOfVolunteerOrg
;; consumes a ListOfVolunteerOrg and produces only those items in the list that require a driver's license and
;; less than the specified number of hours of training
(define (list-license-training alov hours-of-training)
  (local [(define (is-license-training? a-volunteerorg)
            (and (volunteer-org-license? a-volunteerorg) (< (volunteer-org-training a-volunteerorg) hours-of-training)))]
    (filter is-license-training? alov)))

(check-expect (list-license-training empty 15) empty)
(check-expect (list-license-training ORGS 15) empty)
(check-expect (list-license-training ORGS1 25) (cons
                                                (make-volunteer-org
                                                "nursing home"
                                                "blank1"
                                                18
                                                #false
                                                #true
                                                24
                                                15
                                                (cons "english" (cons "chinese" (cons "spanish" empty))))
                                               empty))
(check-expect (list-license-training ORGS3 5) ORGS3)

;; 8

;; names-by-age:  ListOfVolunteerOrg Natural -> ListOfString
;; consumes a list of volunteer organizations and the age of a volunteer and produces a list
;; of the names of all organizations that will accept volunteers of the given age
(define (names-by-age alov age-of-volunteer)
  (local [(define (accept-volunteer? a-volunteerorg)
            (> (volunteer-org-age a-volunteerorg) age-of-volunteer))]
    (map volunteer-org-name (filter accept-volunteer? alov))))

;; 9

;; need-spanish-speakers:  ListOfVolunteerOrg -> ListOfVolunteerOrg
;; consumes a list of volunteer organizations and returns a list of those organizations that have clients who speak Spanish
(define (need-spanish-speakers alov)
  (local [(define (check-spanish? a-volunteerorg)
            (check-languages (volunteer-org-languages a-volunteerorg)))]
    (filter check-spanish? alov)))

(define (check-languages alos)
  (cond [(empty? alos) false]
        [(cons? alos) (if (string=? "spanish" (first alos))
                          true
                          (check-languages (rest alos)))]))












