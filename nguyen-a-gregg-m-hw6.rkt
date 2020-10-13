;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname nguyen-a-gregg-m-hw6) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
 ;;Aaron Nguyen (anguyen3) , Miles Gregg (mgregg)

;;----------------------------------------------------------------------------------------
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

;;----------------------------------------------------------------------------------------
;; 2

;; courses represents a list of courses that is empty
(define courses empty)

;; students represents a list of students that is empty
(define students empty)

;;----------------------------------------------------------------------------------------
;; 3

;; add-student: String Natural -> void
;; adds a student to the students list
;; EFFECT: Changes the number of students in students
(define (add-student name id)
  (if (student-id-in-list? students name id)
      (error "student already exists")
      (set! students (cons (make-student name id empty) students))))

;; student-id-in-list?: ListOfStudent String Natural -> Boolean
;; Consumes a list of student a name and id and produces true if the student exists and false if the student doesn't exist.
(define (student-id-in-list? alos name id)
  (cond [(empty? alos) false]
        [(cons? alos) (if (= id (student-id (first alos)))
                          true
                          (student-id-in-list? (rest alos) name id))]))

(check-expect (student-id-in-list? empty "Joe" 123) false)
(check-expect (student-id-in-list? students "Joe" 123) true)
(check-expect (student-id-in-list? students "Christ" 789) true)
(check-expect (student-id-in-list? students "Bob" 123) true)

;;----------------------------------------------------------------------------------------
;; 4

;; add-course: String Natural String Natural -> void
;; adds a course to the courses list
;; EFFECT: Changes the number of courses in courses
(define (add-course dept course-num faculty max-enroll)
  (if (course-in-list? courses dept course-num)
      "course already exists"
      (set! courses (cons (make-course dept course-num faculty max-enroll empty) courses))))

;; course-in-list?: ListOfCouse String Natural -> Boolean
;; Consumes A list of course and produces true if the course exists and false if it doesn't
(define (course-in-list? aloc dept course-num)
  (cond [(empty? aloc) false]
        [(cons? aloc) (if (and (string=? dept (course-department (first aloc)))
                               (= course-num (course-course-number (first aloc))))
                          true
                          (course-in-list? (rest aloc) dept course-num))]))

(check-expect (course-in-list? courses "CS" 2201) false)
(check-expect (course-in-list? courses "MA" 2201) true)
(check-expect (course-in-list? empty "CS" 1101) false)
(check-expect (course-in-list? courses "CS" 1101) true)

(add-student "Joe" 123)
(add-student "Ann" 456)
(add-student "Chris" 789)
(add-course "CS" 1101 "Hamel" 100)
(add-course "BI" 1000 "Rulfs" 20)
(add-course "MA" 2201 "Servatius" 50)
(add-course "MA" 2202 "Bob" 0)

;;----------------------------------------------------------------------------------------
;; 5

;; NEW

;; add-to-coures: Natural String Natural-> void
;; Consumes a students id and course department and number and adds student to the courses list of students
;; and adds the course to the students list of students if the course is not full.
;; EFFECT: Adds student to a course's list of students, Adds course to a student's list of courses
(define (add-to-course id dept course-num)
  (local [(define a-student (find-student id students))
          (define a-course (find-course dept course-num courses))]
    (if (> (course-max-enrollment a-course) (length (course-students a-course)))
        (begin
          (set-course-students! a-course (cons a-student (course-students a-course)))
          (set-student-courses! a-student (cons a-course (student-courses a-student))))
        (error "course is full!"))))

;; find-student: Natural ListOfStudent -> Student
;; Consumes an ID and a list of student and produces the student with given ID or an error if the student doesn't exist
(define (find-student id alos)
  (cond [(empty? alos) (error "student doesn't exist")]
        [(cons? alos) (if (= id (student-id (first alos)))
                          (first alos)
                          (find-student id (rest alos)))]))

(check-error (find-student 123 empty) "student doesn't exist")
(check-error (find-student 101 students) "student doesn't exist")
(check-expect (find-student 456 students) (make-student "Ann" 456 '()))
(check-expect (find-student 123 students) (make-student "Joe" 123 '())) 

;; find-course: String Natural ListOfCourse -> Course
;; Consume a department, course number and a list of course and returns the course if it is in the list and
;; an error course doesn't exist if it is not in the list
(define (find-course dept course-num aloc)
  (cond [(empty? aloc) (error "course doesn't exist")]
        [(cons? aloc) (if (and (= course-num (course-course-number (first aloc)))
                               (string=? dept (course-department (first aloc))))
                          (first aloc)
                          (find-course dept course-num (rest aloc)))]))

(check-error (find-course "ECON" 1 courses) "course doesn't exist")
(check-error (find-course "MA" 2 courses) "course doesn't exist")
(check-error (find-course "MA" 2201 empty) "course doesn't exist")
(check-expect (find-course "MA" 2201 courses) (make-course "MA" 2201 "Servatius" 50 '()))
(check-expect (find-course "CS" 1101 courses) (make-course "CS" 1101 "Hamel" 100 '()))





;; add-to-coures: Natural String Natural -> void
;; Consumes a students id and course department and number and adds student to the courses list of students
;; and adds the course to the students list of students if the course is not full.
;; EFFECT: Adds student to a course's list of students, Adds course to a student's list of courses
#;(define (add-to-course id dept course-num)
  (set! courses (add-student-to-course id dept course-num courses)))

;; add-student-to-course: Natural String Natural ListOfCouse -> ListOfCourse
;; Consumes a student ID a course department and number and a list of courses and produces a list of courses with
;; the student added to the course with the given department and course number in the list of courses
#;(define (add-student-to-course id dept course-num aloc)
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

;(check-error (add-student-to-course 123 "MA" 2201 empty) "course doesn't exist")
;(check-error (add-student-to-course 123 "ECON" 1101 courses)  "course doesn't exist")
;(check-error (add-student-to-course 101 "MA" 2201 courses) "student doesn't exist")
;(check-error (add-student-to-course 123 "MA" 2202 courses) "course is full!")
;(check-expect (add-student-to-course 123 "MA" 2201 courses) (shared ((-2- "MA") (-6- "Servatius"))
;                                                              (list
;                                                               (make-course -2- 2202 "Bob" 0 '())
;                                                               (make-course -2- 2201 -6- 50 (list (make-student "Joe" 123 (make-course -2- 2201 -6- 50 '()))))
;                                                               (make-course "BI" 1000 "Rulfs" 20 '())
;                                                               (make-course "CS" 1101 "Hamel" 100 '()))))

;; find-student: Natural ListOfStudent -> Student
;; Consumes an ID and a list of student and produces the student with given ID or an error if the student doesn't exist
#;(define (find-student id alos)
  (cond [(empty? alos) (error "student doesn't exist")]
        [(cons? alos) (if (= id (student-id (first alos)))
                          (first alos)
                          (find-student id (rest alos)))]))

;(check-error (find-student 123 empty) "student doesn't exist")
;(check-error (find-student 101 students) "student doesn't exist")
;(check-expect (find-student 456 students) (make-student "Ann" 456 '()))
;(check-expect (find-student 123 students) (make-student "Joe" 123 (make-course "MA" 2201 "Servatius" 50 '()))) 

;;----------------------------------------------------------------------------------------
;; 6

;; largest-enrollment: -> Course
;; produces the course with the most amount of students enrolled
(define (largest-enrollment)
  (local [(define (find-largest aloc acc)
            (cond [(empty? aloc) acc]
                  [(cons? aloc) (if (> (length (course-students (first aloc))) (length (course-students acc)))
                                    (find-largest (rest aloc) (first aloc))
                                    (find-largest (rest aloc) acc))]))]
    (if (empty? courses)
        (error "no courses")
        (find-largest (rest courses) (first courses)))))

(check-expect (largest-enrollment) (make-course "MA" 2202 "Bob" 0 '()))