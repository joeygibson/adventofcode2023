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
                   
                   (setf trench (append trench (list pos)))))))
    trench))

(defun shoelace-area (trench)
  (let* ((s1 0)
         (s2 0))
    (loop for i from 0
          for pos in trench
          do (let* ((x1 (car pos))
                    (y1 (cdr pos))
                    (x2 0)
                    (y2 0))
               (if (= (1+ i) (length trench))
                   (progn (setf x2 (car (car trench)))
                          (setf y2 (cdr (car trench))))
                   (progn (setf x2 (car (nth (1+ i) trench)))
                          (setf y2 (cdr (nth (1+ i) trench)))))
               (incf s1 (* x1 y2))
               (incf s2 (* x2 y1))))
    (abs (/ (- s1 s2) 2))))

(defun part1 (file-name)
  (let* ((plan (parse file-name))
         (trench (dig plan))
         (area (shoelace-area trench)))
    (print (1+ (+ (/ (length trench) 2) area)))))

(print (part1 "input0.txt"))
(print (part1 "input1.txt"))

