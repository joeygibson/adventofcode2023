;; day 3

(ql:quickload "cl-ppcre")
(require 'cl-ppcre)

(defun find-numbers (lines)
  (let ((results (loop for line-no from 0
                       for line in lines
                       collecting (let* ((matches (cl-ppcre:all-matches "\\d+" line))
                                         (groups (loop for (a b) on matches
                                                       by #'cddr
                                                       while b
                                                       collecting (list a b))))
                                    (mapcar (lambda (group)
                                              (destructuring-bind (start end) group
                                                (list (parse-integer
                                                       (subseq line start end)) 
                                                      line-no
                                                      start
                                                      end)))
                                            groups)))))
    (apply #'append (remove-if #'null results))))

(defun find-symbols (lines)
  (let ((results (loop for line-no from 0
                       for line in lines
                       collecting (let* ((matches (cl-ppcre:all-matches "[^\\d.]+" line))
                                         (groups (loop for (a b) on matches
                                                       by #'cddr
                                                       while b
                                                       collecting (list a b))))
                                    (mapcan (lambda (group)
                                              (destructuring-bind (start end) group
                                                (list (subseq line start end)
                                                      line-no
                                                      start
                                                      end)))
                                            groups)))))
    (remove-if #'null results)))

(defun is-num-near-symbol (num symbols)
  (remove-if-not (lambda (sym)
                   (or (<= (nth 2 num)
                           (nth 2 sym)
                           (nth 3 num))
                       (or (eql (nth 2 sym)
                                (1- (nth 2 num)))
                           (eql (nth 3 sym)
                                (1+ (nth 3 num))))))
                 symbols))

(defun find-numbers-near-symbols (symbols numbers)
  (remove-if #'null
             (mapcar (lambda (num)
                       (let ((prev-row-symbols (remove-if-not (lambda (s)
                                                                (equal (cadr s)
                                                                       (1- (cadr num))))
                                                              symbols))
                             (next-row-symbols (remove-if-not (lambda (s)
                                                                (equal (cadr s)
                                                                       (1+ (cadr num))))
                                                              symbols))
                             (cur-row-symbols (remove-if-not (lambda (s)
                                                               (equal (cadr s)
                                                                      (cadr num)))
                                                             symbols))
                             )
                         (if (or (and prev-row-symbols
                                      (is-num-near-symbol num prev-row-symbols))
                                 (and next-row-symbols
                                      (is-num-near-symbol num next-row-symbols))
                                 (and cur-row-symbols
                                      (is-num-near-symbol num cur-row-symbols)))
                             (car num))))
                     numbers)))

(defun part1 (file-name)
  (let* ((lines (uiop:read-file-lines file-name))
         (symbols (find-symbols lines))
         (numbers (find-numbers lines))
         (vals (find-numbers-near-symbols symbols numbers)))
    (print vals)
    (reduce #'+ vals)))

(part1 "input0.txt")

(let* ((numbers (find-numbers (uiop:read-file-lines "input0.txt")))
       (symbols (find-symbols (uiop:read-file-lines "input0.txt"))))
  (print numbers)
  (print symbols))








