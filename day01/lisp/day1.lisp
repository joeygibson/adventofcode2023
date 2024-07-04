;; day01

(defparameter *input* (uiop:read-file-lines "input1.txt"))

(defun extract-digits (lst)
  (mapcar #'(lambda (str)
           (remove-if-not #'digit-char-p str))
       lst))

(defun create-numbers (lst)
  (mapcar #'(lambda (str)
              (let* ((fst (elt str 0))
                     (lst (elt str (1- (length str))))
                     (res (format nil "~a~a" fst lst)))
                (parse-integer res)))
          lst))
  
(reduce #'+ (create-numbers (extract-digits *input*))) ; 54304

;; part 2

(ql:quickload 'cl-ppcre)

(defun possibly-convert-digit (str)
  (cond ((digit-char-p (elt str 0)) (parse-integer str))
        ((string-equal str "one") 1)
        ((string-equal str "two") 2)
        ((string-equal str "three") 3)
        ((string-equal str "four") 4)
        ((string-equal str "five") 5)
        ((string-equal str "six") 6)
        ((string-equal str "seven") 7)
        ((string-equal str "eight") 8)
        ((string-equal str "nine") 9)))

(defun extract-numbers (str)
  (let ((nums))
    (cl-ppcre:do-register-groups (num)
        ("(?=(one|two|three|four|five|six|seven|eight|nine|[1-9]))"
         str)
      (push (possibly-convert-digit num) nums))
    (nreverse nums)))

(defun extract-numbers-from-all (lst)
  (mapcar #'extract-numbers lst))

(defun create-numbers (lst)
  (mapcar #'(lambda (lst1)
              (let* ((first-num (first lst1))
                     (last-num (car (last lst1)))
                     (res (format nil "~a~a" first-num last-num)))
                (parse-integer res)))
          lst))

(reduce #'+ (create-numbers (extract-numbers-from-all *input*)))








