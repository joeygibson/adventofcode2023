;; day 09

(ql:quickload :cl-ppcre)
(use-package :cl-ppcre)

(setf *print-circle* t)

(defun pairwise (list)
  (loop for (a b) on list
        while b
        collect (list a b)))

(defun compute-differences (values)
  (let ((pairs (pairwise values)))
    (mapcar (lambda (pair)
              (destructuring-bind (a b) pair
                (- b a)))
            pairs)))

(defun solve (lines)
  (let ((results '()))
    (mapc (lambda (line)
            (let ((rev-sequences nil)
                  (sequences nil)
                  (values (loop for val in (all-matches-as-strings "\\-?\\d+" line)
                                collecting (parse-integer val))))
              (push values rev-sequences)
              (loop named diffs
                    do (let ((differences (compute-differences values)))
                         (push differences rev-sequences)
                         (if (every (lambda (v) (= v 0))
                                    differences)
                             (return-from diffs))
                         (setf values differences)))
              (setf sequences (reverse rev-sequences))
              (loop for i from 0
                    for sequence in rev-sequences
                    do (progn
                         (if (not (= i 0))
                             (nconc sequence (list (+ (car (last sequence))
                                                      (car (last (nth (1- i) rev-sequences))))))
                             (nconc sequence (list (car (last sequence)))))))
              (push (car (last (first sequences))) results)))
          lines)
    (reduce #'+ results)))

(defun part1 (file-name)
  (let ((lines (uiop:read-file-lines file-name)))
    (solve lines)))

(print (part1 "input0.txt"))
(print (part1 "input1.txt"))




