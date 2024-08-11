;; day 04

(ql:quickload "cl-ppcre")
(ql:quickload "queues")
(require 'cl-ppcre)
(require :queues.simple-queue)

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
    (print (reduce #'+ values))))

(defun part2 (file-name)
  (let* ((tickets (find-matches file-name))
         (queue (queues:make-queue :simple-queue))
         (total-tickets 0))
    (loop for i from 0
          for ticket in tickets do (queues:qpush queue (list i ticket)))
    (loop for val = (queues:qpop queue) while val
          do (progn
               (incf total-tickets)
               (if (cadr val)
                   (let* ((start (1+ (car val)))
                          (end (+ start (length (cadr val))))
                          (copies (subseq tickets start end)))
                     (loop for i from start
                           for ticket in copies do (queues:qpush queue (list i ticket)))))))
    (print total-tickets)))

(part1 "input0.txt")
(part1 "input1.txt")

(part2 "input0.txt")
(part2 "input1.txt")





