;; day 09

(ql:quickload :cl-ppcre)
(use-package :cl-ppcre)

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
  (let ((results nil))
    (mapc (lambda (line)
            (let ((sequences (make-array 5 :fill-pointer 0))
                  (values (loop for val in (all-matches-as-strings "\\d+" line)
                                collecting (parse-integer val))))
              (vector-push values sequences)
              (loop named diffs
                    do (let ((differences (compute-differences values)))
                         (if (every (lambda (v) (= v 0))
                                    differences)
                             (return-from diffs))
                         (setf values differences)))
              (let ((rev-sequences (reverse sequences)))
                (loop for i from 0
                      for sequence across sequences
                      do (if (not (= i 0))
                             (vector-push (+ (aref sequence (1- (length sequence)))
                                             (let ((s (aref rev-sequences (1- i))))
                                               (aref s (1- (length s))))))
                             (vector-push (aref sequence (1- (length sequence))))))
                (vector-push (let ((s (aref sequences 0)))
                               (aref s (1- (length s))))))))
          lines)
    (reduce #'+ results)))



(defun part1 (file-name)
  (let ((lines (uiop:read-file-lines file-name)))
    (solve lines)))

(print (part1 "input0.txt"))

(let ((v (make-array 5 :fill-pointer 0)))
  (vector-push "a" v)
  (vector-push "b" v)
  (vector-push "c" v)
  (print (aref v (1- (length v)))))

(loop named foo
      for i below 10
      do (if (= i 5)
             (return-from foo 23)
             (print i)))
