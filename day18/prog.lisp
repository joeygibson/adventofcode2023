;; day 18

(ql:quickload :cl-ppcre)
(ql:quickload :split-sequence)
(ql:quickload :lisp-utils)
(ql:quickload :alexandria)

(use-package :lisp-utils)
(use-package :cl-ppcre)

(defun parse (file-name)
  (let ((lines (uiop:read-file-lines file-name)))
    (mapcar (lambda (line)
              (let ((chunks (cl-ppcre:split " " (string-trim '(#\Newline) line))))
                (list (car chunks)
                      (parse-integer (second chunks))
                      (third chunks))))
            lines)))

(defun dig (plan)
  (let* ((pos (cons 0 0))
         (trench (list pos)))
    (dolist (next plan)
      (destructuring-bind (dir units color) next
        (loop for i in (range units)
              do (progn
                   (cond ((equal dir "U")
                          (setf pos (cons (car pos) (1- (cdr pos)))))
                         ((equal dir "D")
                          (setf pos (cons (car pos) (1+ (cdr pos)))))
                         ((equal dir "R")
                          (setf pos (cons (1+ (car pos)) (cdr pos))))
                         ((equal dir "L")
                          (setf pos (cons (1- (car pos)) (cdr pos)))))
                   (when (equal pos (cons 0 0))
                     (return))
                   
                   (push pos trench)))))
    (nreverse trench)))

(defun shoelace-area (trench)
  (let* ((t-arr (coerce trench 'vector))
         (t-length (length t-arr))
         (s1 0)
         (s2 0))
    (loop for i from 0
          for pos across t-arr
          do (let* ((x1 (car pos))
                    (y1 (cdr pos))
                    (x2 0)
                    (y2 0))
               (if (= (1+ i) t-length)
                   (progn (setf x2 (car (aref t-arr 0)))
                          (setf y2 (cdr (aref t-arr 0))))
                   (progn (setf x2 (car (aref t-arr (1+ i))))
                          (setf y2 (cdr (aref t-arr (1+ i))))))
               (incf s1 (* x1 y2))
               (incf s2 (* x2 y1))))
    (abs (/ (- s1 s2) 2))))

(defun convert (line)
  (let* ((value (nth 2 line))
         (hex-digit (subseq value 2 7))
         (dir-digit (parse-integer (subseq value 7 8)))
         (dir (case dir-digit
                (0 "R")
                (1 "D")
                (2 "L")
                (3 "U"))))
    (list dir (parse-integer hex-digit :radix 16) "color")))

(defun part1 (file-name)
  (let* ((plan (parse file-name))
         (trench (dig plan))
         (area (shoelace-area trench)))
    (print (1+ (+ (/ (length trench) 2) area)))))

(defun part2 (file-name)
  (let* ((plan (parse file-name))
         (converted-plan (mapcar #'convert plan))
         (trench (dig converted-plan))
         (area (shoelace-area trench)))
    (print (1+ (+ (/ (length trench) 2) area)))))

(print (part1 "input0.txt"))
(print (part1 "input1.txt"))

(print (part2 "input0.txt"))
(print (part2 "input1.txt"))



