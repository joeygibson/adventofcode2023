;; day 14

(ql:quickload :cl-ppcre)
(ql:quickload :split-sequence)
(ql:quickload :lisp-utils)
(ql:quickload :alexandria)

(use-package :lisp-utils)
(use-package :cl-ppcre)
(use-package :alexandria)

(setf *print-circle* t)

(defun create-map (lines)
  (let ((map (make-hash-table :test #'equal)))
    (loop for row-num from 0
          for row in lines
          do (loop for col-num from 0
                   for col in (cl-ppcre:split "" row)
                   do (setf (gethash (cons row-num col-num) map) col)))
    map))

(defun find-empty-row (map row-num col-num)
  (let ((row-range (reverse (iota row-num)))
        (row-to-move-to row-num))
    (loop for i in row-range
          until (or (equal (gethash (cons i col-num) map) "#")
                    (equal (gethash (cons i col-num) map) "O"))
          do (decf row-to-move-to))
    row-to-move-to))

(defun tilt (map)
  (let ((pairs (remove-if (lambda (x)
                            (let ((val (cdr x)))
                              (or (equal (car x) 0)
                                  (equal val "#")
                                  (equal val "."))))
                          (reverse (hash-table-keys map)))))
    (loop for pair in pairs
          do (let* ((row-num (car pair))
                    (col-num (cdr pair))
                    (row-to-move-to (find-empty-row map row-num col-num)))
               (format t "~&~a -> ~a~%" row-num row-to-move-to)
               (if (not (equal row-to-move-to row-num))
                   (progn
                     (setf (gethash (cons row-to-move-to col-num) map) "O")
                     (setf (gethash pair map) ".")))))
    map))

(defun part1 (file-name)
  (let* ((lines (uiop:read-file-lines file-name))
         (max-rows (length lines))
         (max-cols (length (cl-ppcre:split "" (car lines))))
         (map (create-map lines))
         (tilted-map (tilt map))
         (total 0))
    ;; (mapcar (lambda (a b)
    ;;           (format t "~&~a - ~a~%" a b))
    ;;         (hash-table-alist map)
    ;;         (hash-table-alist tilted-map))
    (loop for row-num below max-rows
        do (let ((rock-count (length (loop for col-num below max-cols
                                           if (equal (gethash (cons row-num col-num) tilted-map) "O")
                                             collect 1))))
             (setf total (+ total (* rock-count (- max-rows row-num))))))
    total))

(print (part1 "input0.txt"))

(let ((m (create-map (uiop:read-file-lines "input0.txt"))))
  (loop for pair in (reverse (hash-table-keys m))
        do (let* ((row-num (car pair))))
           (print row-num)))



