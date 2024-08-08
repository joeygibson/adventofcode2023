;; day 04

(ql:quickload "cl-ppcre")
(require 'cl-ppcre)

(defun get-input (file-name)
  (let ((lines (uiop:read-file-lines file-name)))
    (mapcar (lambda (line)
              (let* ((first-chunks (uiop:split-string line :separator ":"))
                     (number-chunks (uiop:split-string (second first-chunks)
                                                       :separator "|"))
                     (winning-numbers (mapcar #'parse-integer
                                              (cl-ppcre:split "\\s+"
                                                              (string-trim " " (first number-chunks)))))
                     (my-numbers (mapcar #'parse-integer
                                         (cl-ppcre:split "\\s+"
                                                         (string-trim " " (second number-chunks))))))
                (list winning-numbers my-numbers)))
            lines)))

(defun find-matches (file-name)
  (let* ((data (get-input file-name)))
    (mapcar (lambda (card)
              (intersection (first card)
                            (second card)))
            data)))

(defun part1 (file-name)
  (let* ((winners (remove-if #'null (find-matches file-name)))
         (values (mapcar (lambda (numbers)
                           (let ((cnt (length numbers)))
                             (expt 2 (1- cnt))))
                         winners)))
    (reduce #'+ values)))

(part1 "input0.txt")
(part1 "input1.txt")




