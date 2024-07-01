;; day01

(defparameter *input* (uiop:read-file-lines "input1.txt"))

(defun extract-numbers0 (lst)
  (dolist (str lst)
    (remove-if-not #'digit-char-p str)))

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

