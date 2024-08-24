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
  (let ((results nil))
    (mapc (lambda (line)
            (print "-----")
            (let ((sequences (make-array 5 :fill-pointer 0))
                  (values (loop for val in (all-matches-as-strings "\\d+" line)
                                collecting (parse-integer val))))
              (vector-push values sequences)
              (loop named diffs
                    do (let ((differences (compute-differences values)))
                         (vector-push differences sequences)
                         (if (every (lambda (v) (= v 0))
                                    differences)
                             (return-from diffs))
                         (setf values differences)))
              (let ((rev-sequences (reverse sequences)))
                (format t "~&rs: ~a~%" rev-sequences)
                (loop for i from 0
                      for sequence across rev-sequences
                      do (progn
                           (format t "~&~a ~a~%" i sequence)
                           (if (not (= i 0))
                               (nconc sequence (list (+ (car (last sequence))
                                                        (car (last (aref rev-sequences (1- i)))))))
                               (nconc sequence (list (car (last sequence)))))))
                (nconc results (let ((s (aref sequences 0)))
                                 (car (last s)))))))
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

(let ((l (list 1 2 3)))
  (nconc l (list 23))
  (print l))

(let ((l (list 1 2 3)))
  (nth 1 l))

