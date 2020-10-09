;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname nguyen-a-gregg-m-hw6) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;; 1

(define-struct course (department course-number faculty max-enrollment students))

;; a Course is a (make-course String Natural string Natural alos)
;;  interp:  represents a course along with students enrolled in course
;;     department    is the name of the department offering the course
;;     course-number is the course number
;;     faculty       is the person teaching the course
;;     max-enrollment is the max amount of students allowed to enroll
;;     students      is a list of students taking the course

;; ListOfStudents is one of:
;;       - empty
;;       - (cons Student ListOfStudent)

(define-struct student (name id courses))

;; a Student is a (make-student String Number aloc)
;; interp: represents a student along with the course they are enrolled in
;; name     is the name of the student
;; ID       is the students ID number
;; courses  is a list of courses the student is enrolled in

;; ListOfCourses is one of:
;;   -empty
;;   - (cons Courses ListOfCouses)

;; 2

;; c represents a list of courses that is empty
(define courses empty)

;; s represents a list of students that is empty
(define students empty)

;; 3

(define (add-student name id)
  (if (student-id-in-list? students name id)
      "student already exists"
      (set! students (cons (make-student name id empty) students))))

(define (student-id-in-list? alos name id)
  (cond [(empty? alos) false]
        [(cons? alos) (if (= id (student-id (first alos)))
                          true
                          (student-id-in-list? (rest alos) name id))]))

;; 4

(define (add-course dept course-num faculty max-enroll)
  (if (course-in-list? courses dept course-num)
      "course already exists"
      (set! courses (cons (make-course dept course-num faculty max-enroll empty)))))

(define (course-in-list? aloc dept course-num)
  (cond [(empty? aloc) false]
        [(cons? aloc) (if (and (string=? dept (course-department (first aloc)))
                               (= course-num (course-course-number (first aloc))))
                          true
                          (course-in-list? (rest aloc) dept course-num))]))

;(define (add-course dept course-num faculty max-enroll)
;  (if (string? (course?? c dept course-num faculty max-enroll))
;      "course already exists"
;  (set! c (course?? c dept course-num faculty max-enroll))))
;
;(define (course?? aloc dept course-num faculty max-enroll)
;  (cond [(empty? aloc) (cons (make-course dept course-num faculty max-enroll empty) c)]
;        [else
;         (if (and (string=? dept (course-department (first aloc))) (= course-num (course-course-number (first aloc))))
;             "course already exists"
;             (course?? (rest aloc) dept course-num faculty max-enroll))]))

;; 5

(define)
