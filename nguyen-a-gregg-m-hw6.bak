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
      (set! courses (cons (make-course dept course-num faculty max-enroll empty) courses))))

(define (course-in-list? aloc dept course-num)
  (cond [(empty? aloc) false]
        [(cons? aloc) (if (and (string=? dept (course-department (first aloc)))
                               (= course-num (course-course-number (first aloc))))
                          true
                          (course-in-list? (rest aloc) dept course-num))]))

(add-student "Joe" 123)
(add-student "Ann" 456)
(add-student "Chris" 789)
(add-course "CS" 1101 "Hamel" 100)
(add-course "BI" 1000 "Rulfs" 20)
(add-course "MA" 2201 "Servatius" 50)
(add-course "MA" 2202 "Bob" 0)

;; 5

(define (add-to-course id dept course-num)
  (set! courses (add-student-to-course id dept course-num courses)))

(define (add-student-to-course id dept course-num aloc)
  (cond [(empty? aloc) (error "course doesn't exist")]
        [(cons? aloc) (if (and (= course-num (course-course-number (first aloc)))
                               (string=? dept (course-department (first aloc))))
                          (if (> (course-max-enrollment (first aloc)) (length (course-students (first aloc))))
                              (begin
                                (set-student-courses! (find-student id students) (first aloc))
                                (cons (make-course dept course-num (course-faculty (first aloc))
                                                 (course-max-enrollment (first aloc))
                                                 (cons (find-student id students) (course-students (first aloc)))) (rest aloc)))
                              (error "course is full!"))
                          (cons (first aloc) (add-student-to-course id dept course-num (rest aloc))))]))

(define (find-student id alos)
  (cond [(empty? alos) (error "student doesn't exist")]
        [(cons? alos) (if (= id (student-id (first alos)))
                          (first alos)
                          (find-student id (rest alos)))]))

(define TEST 1)
(define TEST2 1)

(define (testing)
  (begin
    (set! TEST 3)
    (set! TEST2 5)))

;(add-student-to-course 104 "CS" 1101 courses)

;(define (add-course-to-student))


;(define (add-to-course id dept course-num)
;  (set! courses (course-helper courses id dept course-num)))
;
;(define (course-helper aloc id dept course-num)
;  (cond [(empty? aloc) (error "course doesn't exist")]
;        [(cons? aloc) (if (and (string=? dept (course-department (first aloc)))
;                               (= course-num (course-course-number (first aloc)))
;                               (>= (course-max-enrollment (first aloc)) (length (course-students (first aloc)))))
;                          (set-course-students! (first aloc) (cons (make-student (student-helper students id) id empty) students))
;                          (course-helper (rest aloc) id dept course-num))]))
;
;         ;(if (>= (course-max-enrollment (first aloc)) (course-students (first aloc))))]))
;
;(define (student-helper alos id)
;  (cond [(empty? alos) (error "student doesn't exist")]
;        [(cons? alos) (if (= id (student-id (first alos)))
;                          (first alos)
;                          (student-helper (rest alos) id))]))


;(add-to-course 456 "MA" 2201)
;(add-to-course 456 "CS" 1101)
;(add-to-course 123 "CS" 1101)
;(add-to-course 789 "BI" 1000)










