;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Lab 6|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
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

(define-struct student (name ID courses))

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
(define c (list ))

;; s represents a list of students that is empty
(define s (list ))

;; 3

(define (add-student n id)
  (if (string? (student-id? s n id))
      "student already exists"
      (set! s (student-id? s n id))))

(define (student-id? alos n id)
  (cond [(empty? alos) (cons (make-student n id empty) s)]
        [else
          (if (not (= id (student-ID (first s))))
              (student-id? (rest alos) n id)
              "student already exists")]))
              

;; 4

(define (add-course dept course-num faculty max-enroll)
  (if (string? (course?? c dept course-num faculty max-enroll))
      "course already exists"
  (set! c (course?? c dept course-num faculty max-enroll))))

(define (course?? aloc dept course-num faculty max-enroll)
  (cond [(empty? aloc) (cons (make-course dept course-num faculty max-enroll empty) c)]
        [else
         (if (and (string=? dept (course-department (first aloc))) (= course-num (course-course-number (first aloc))))
             "course already exists"
             (course?? (rest aloc) dept course-num faculty max-enroll))]))
