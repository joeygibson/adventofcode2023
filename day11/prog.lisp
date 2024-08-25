;; day 11

(declaim (optimize (speed 0) (space 0) (debug 3)))

(ql:quickload :cl-ppcre)
(use-package :cl-ppcre)

(setf *print-circle* t)

(defun build-universe (lines)
  (mapcar (lambda (line)
            (split "" line))
          lines))

(defun find-expandable-rows (universe)
  (loop for i from 0
        for row in universe
        if (every (lambda (c)
                    (equal c "."))
                  row)
          collect i))

(defun find-expandable-cols (universe)
  (loop for i below (length universe)
        if (every (lambda (row)
                    (equal (nth i row) "."))
                  universe)
          collect i))

(defun find-galaxies-as-dict (universe)
  (let ((galaxies (make-hash-table :test #'equal)))
    (loop for i from 0
          for row in universe
          do (loop for j from 0
                   for col in row
                   if (equal col "#")
                     do (setf (gethash (cons i j) galaxies) (cons i j))))
    galaxies))

(defun expand-universe (galaxies ex-rows ex-cols &key is-part-two)
  (let ((scale (if is-part-two
                   999999
                   1)))
    (loop for i in ex-rows
          do (maphash (lambda (k v)
                        (when (> (car k) i)
                          (setf (gethash k galaxies) (cons (+ (car v) scale)
                                                           (cdr v)))))
                      galaxies))
    (loop for i in ex-cols
          do (maphash (lambda (k v)
                        (when (> (cdr k) i)
                          (setf (gethash k galaxies) (cons (car v)
                                                           (+ (cdr v) scale)))))
                      galaxies)))
  galaxies)

(defun combinations (items n)
  (cond ((= n 0) '(()))
        ((null items) nil)
        (t (append (mapcar (lambda (comb) (cons (car items) comb))
                           (combinations (cdr items) (1- n)))
                   (combinations (cdr items) n)))))

(defun compute-distances (pairs)
  (mapcar (lambda (pair)
            (let* ((a (car pair))
                   (b (cadr pair))
                   (a0 (car a))
                   (a1 (cdr a))
                   (b0 (car b))
                   (b1 (cdr b)))
              (+ (abs (- a0 b0))
                 (abs (- a1 b1)))))
          pairs))

(defun do-it (lines &key is-part-two)
  (let* ((universe (build-universe lines))
         (ex-rows (find-expandable-rows universe))
         (ex-cols (find-expandable-cols universe))
         (original-galaxies (find-galaxies-as-dict universe))
         (galaxies (expand-universe original-galaxies ex-rows ex-cols :is-part-two is-part-two))
         (pairs (combinations (loop for v being the hash-values of galaxies collecting v) 2))
         (distances (compute-distances pairs)))
    (reduce #'+ distances)))

(defun part1 (file-name)
  (let ((lines (uiop:read-file-lines file-name)))
    (do-it lines)))

(defun part2 (file-name)
  (let ((lines (uiop:read-file-lines file-name)))
    (do-it lines :is-part-two t)))

(print (part1 "input0.txt"))
(print (part1 "input1.txt"))

(print (part2 "input0.txt"))
(print (part2 "input1.txt"))
