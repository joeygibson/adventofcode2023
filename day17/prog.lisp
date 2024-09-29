;; day 17

(ql:quickload :cl-ppcre)
(ql:quickload :split-sequence)
(ql:quickload :lisp-utils)
(ql:quickload :alexandria)
(ql:quickload :priority-queue)

(defun parse-data (file-name)
  (mapcar (lambda (line)
            (mapcar (lambda (s)
                      (parse-integer s))
                    (cl-ppcre:split "" line)))
          (uiop:read-file-lines file-name)))

(defun range (a &optional b c)
  "analog to Python's `range`"
  (let* ((start (if b a 0))
         (end (if b b a))
         (step (or c 1)))
    (loop for i from start below end by step
          collecting i)))

(defun get-neighbors (grid pos direction min-dist max-dist)
  (let ((grid-height (length grid))
        (grid-width (length (car grid)))
        (neighbors (mapcan (lambda (d)
                             (when (or (null direction) (search direction "EW"))
                               (list (list (cons (- (car pos) d) (cdr pos)) "N")
                                     (list (cons (+ (car pos) d) (cdr pos)) "S")))
                             (when (or (null direction) (search direction "NS"))
                               (list (list (cons (car pos) (+ (cdr pos) d)) "E")
                                     (list (cons (car pos) (- (cdr pos) d)) "W"))))
                           (range min-dist (1+ max-dist)))))
    (print neighbors)
    (remove-if-not (lambda (neighbor)
                     (destructuring-bind ((i . j) _) neighbor
                       (and (and (>= i 0) (< i grid-height))
                            (and (>= j 0) (< j grid-width)))))
                   neighbors)))

(defun a-star (grid min-dist max-dist)
  (let* ((goal (cons (1- (length grid))
                     (1- (length (nth 0 grid)))))
         (unsolved (pqueue:make-pqueue #'<))
         (scores (make-hash-table :test #'equal)))
    (pqueue:pqueue-push (list (cons 0 0) nil) 0 unsolved)
    (setf (gethash (list (cons 0 0) nil) scores) 0)
    (loop until (pqueue:pqueue-empty-p unsolved)
          do (multiple-value-bind (closest-point score) (pqueue:pqueue-pop unsolved)
               (destructuring-bind (cur dir) closest-point
                 (when (equal cur goal)
                   (return score))
                 (dolist (n (get-neighbors grid cur dir min-dist max-dist))
                   (format t "~&cur: ~a, n: ~a" cur n)
                   (let* ((n-coord (car n))
                          (n-dir (cadr n))
                          (cost (cond ((equal n-dir "N")
                                       (loop for i in (range (car n-coord) (car cur))
                                             summing (nth (cdr n-coord) (nth i grid))))
                                      ((equal n-dir "S")
                                       (loop for i in (range (1+ (car cur) (1+ (car n-coord))))
                                             summing (nth (cdr n-coord) (nth i grid))))
                                      ((equal n-dir "E")
                                       (reduce #'+ (subseq (nth (car n-coord) grid) (1+ (car cur)) (1+ (cdr n-coord)))))
                                      ((equal n-dir "W")
                                       (reduce #'+ (subseq (nth (car n-coord) grid) (cdr n-coord) (cdr cur))))))
                          (n-score (+ score cost)))
                     (when (< n-score (gethash n scores most-positive-fixnum))
                       (setf (gethash n scores) n-score)
                       (pqueue:pqueue-push n n-score unsolved)))))))))


(defun part1 (file-name)
  (let ((grid (parse-data file-name)))
    (a-star grid 1 3)))

(part1 "input0.txt")

(let ((grid (parse-data "input0.txt")))
  (print (get-neighbors grid (cons 0 0) nil 1 3)))





