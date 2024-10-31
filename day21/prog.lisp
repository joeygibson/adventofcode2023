;; day 21

(ql:quickload :cl-ppcre)
(ql:quickload :split-sequence)
(ql:quickload :lisp-utils)
(ql:quickload :alexandria)

(ql:quickload :queues)
(require :queues.simple-queue)


(use-package :lisp-utils)

(defun parse-data (lines)
  (mapcar (lambda (line)
            (cl-ppcre:split "" line))
          lines))

(defun find-start (grid)
  (loop named outer
        for y in (range (length grid))
        do (loop for x in (range (length (nth y grid)))
                 do (when (equal (nth x (nth y grid)) "S")
                      (return-from outer (cons x y))))))

(defun get-neighbors (grid pos)
  (destructuring-bind (row . col) pos
    (let ((neighbors nil)
          (grid-length (length grid))
          (grid-width (length (nth 0 grid))))
      (push (cons (1- row) col) neighbors)
      (push (cons (1+ row) col) neighbors)
      (push (cons row (1+ col)) neighbors)
      (push (cons row (1- col)) neighbors)
      (remove-if-not (lambda (neighbor)
                       (destructuring-bind (row . col) neighbor
                         (and (>= row 0)
                              (< row grid-length)
                              (>= col 0)
                              (< col grid-width)
                              (equal (nth col (nth row grid)) "."))))
                     neighbors))))

(defun is-garden-spot (graph node)
  (destructuring-bind (row . col) node
    (and (>= row 0)
         (< row (length graph))
         (>= col 0)
         (< col (length (nth row graph)))
         (not (equal (nth col (nth row graph)) "#")))))

(defun bfs (graph node max-steps)
  (let ((visited nil)
        (queue (queues:make-queue :simple-queue)))
    (queues:qpush queue (cons node 0))
    (loop for val = (queues:qpop queue)
          while val
          do (destructuring-bind ((row . col) . steps) val
               (when (< steps max-steps)
                 (dolist (neighbor (get-neighbors graph (cons row col)))
                   (when (and (is-garden-spot graph neighbor)
                              (not (member neighbor visited :test #'equal)))
                     (push neighbor visited)
                     (queues:qpush queue (cons neighbor (1+ steps))))))))
    (length (remove-if-not (lambda (node)
                             (destructuring-bind (row . col) node
                               (= (mod (+ row col) 2)
                                  (mod max-steps 2))))
                           visited))))

(defun expand-matrix (matrix factor)
  (let* ((rows (length matrix))
         (cols (length (first matrix)))
         (new-rows (* factor rows))
         (new-cols (* factor cols)))
    (loop for i from 0 below new-rows
          collect (loop for j from 0 below new-cols
                        collect (aref (nth (mod i rows) matrix) 
                                      (mod j cols))))))

(defun part1 (file-name how-far-away)
  (let* ((lines (uiop:read-file-lines file-name))
         (the-map (parse-data lines))
         (start (find-start the-map))
         (spots (bfs the-map start how-far-away)))
    (1+ spots)))

(defun part2 (file-name how-far-away)
  (let* ((lines (uiop:read-file-lines file-name))
         (the-map (parse-data lines))
         (start (cons (floor (length the-map) 2)
                      (floor (length the-map) 2)))
         (expanded-map (expand-matrix the-map 5))
         (ys (mapcar (lambda (max-steps)
                       (bfs expanded-map start max-steps))
                     '(65 196 327)))
         (xs ))
    ))





(print (part1 "input0.txt" 6))
(print (part1 "input1.txt" 64))





