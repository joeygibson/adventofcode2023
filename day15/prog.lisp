;; day 15

(ql:quickload :cl-ppcre)
(ql:quickload :split-sequence)
(ql:quickload :lisp-utils)
(ql:quickload :alexandria)

(use-package :lisp-utils)
(use-package :cl-ppcre)

(defun hash (str)
  (reduce (lambda (cur val)
            (mod (* (+ cur val) 17) 256))
          (map 'list #'char-code str)
          :initial-value 0))

(defun part1 (file-name)
  (let* ((input (string-trim '(#\Space #\Tab #\Newline)
                             (uiop:read-file-string file-name)))
         (items (cl-ppcre:split "," input)))
    (loop for item in items
          summing (hash item))))

(print (part1 "input0.txt"))
(print (part1 "input1.txt"))


