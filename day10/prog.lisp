;; day10

(declaim (optimize (speed 0) (space 0) (debug 3)))

(ql:quickload :cl-ppcre)
(use-package :cl-ppcre)

(setf *print-circle* t)

;; constants describing parts
(defconstant +start+ "S")
(defconstant +vertical+ "|")
(defconstant +horizonal+ "-")
(defconstant +north-east+ "7")
(defconstant +north-west+ "F")
(defconstant +south-east+ "J")
(defconstant +south-west+ "L")
(defconstant +ground+ ".")
(defconstant +loop-ground+ ",")
(defconstant +loop-vertical+ "1")
(defconstant +loop-horizontal+ "2")
(defconstant +loop-north-east+ "3")
(defconstant +loop-north-west+ "4")
(defconstant +loop-south-east+ "5")
(defconstant +loop-south-west+ "6")

(defun build-map (lines)
  (let ((map (make-hash-table :test #'equal)))
    (loop for y from 0
          for line in lines
          do (loop for x from 0
                   for c in (split "" line)
                   do (setf (gethash (cons x y) map) c)))
    map))

(defun get-neighbors (map cell)
  (let* ((x (car cell))
         (y (cdr cell))
         (cell-shape (gethash cell map))
         (keys (loop for k being the hash-keys of map collecting k))
         (n-coords
           (cond
             ((equal cell-shape +vertical+) (list (cons x (1- y))
                                            (cons x (1+ y))))
             ((equal cell-shape +horizonal+) (list (cons (1- x) y)
                                             (cons (1+ x) y)))
             ((equal cell-shape +south-west+) (list (cons (1+ x) y)
                                              (cons x (1- y))))
             ((equal cell-shape +south-east+) (list (cons (1- x) y)
                                              (cons x (1- y))))
             ((equal cell-shape +north-east+) (list (cons (1- x) y)
                                              (cons x (1+ y))))
             ((equal cell-shape +north-west+) (list (cons (1+ x) y)
                                              (cons x (1+ y))))
             ((equal cell-shape +start+)
              (let* ((coords (list (cons x (1- y))
                                   (cons x (1+ y))
                                   (cons (1- x) y)
                                   (cons (1+ x) y))))
                (loop for coord in coords
                      when (member cell (get-neighbors map coord) :test #'equal)
                        collect coord))))))
    
    (remove-if-not (lambda (coord)
                     (member coord keys :test #'equal))
                   n-coords)))

(defun find-ends (map)
  (let ((start (loop for k being the hash-keys of map
                     if (equal (gethash k map) +start+)
                       return k)))
    (get-neighbors map start)))



(defun part1 (file-name)
  (let* ((lines (uiop:read-file-lines file-name))
         (map (build-map lines))
         (ends (find-ends map)))
    (print ends)))

(print (part1 "input0.txt"))





