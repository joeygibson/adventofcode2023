;; day 06

(require :cl-ppcre)

(defun process-races (durations records)
  (let ((possibilities nil))
    (loop for dur in durations
          for rec in records
          do (progn
               (let ((poss 0))
                 (loop for i upto (1- dur)
                       do (let* ((dur-minus-i (- dur i))
                                 (dur-minus-i-times-i (* dur-minus-i i)))
                            (if (> dur-minus-i-times-i rec)
                                (incf poss))))
                 (push poss possibilities))))
    possibilities))


(defun part1 (file-name)
  (let* ((lines (uiop:read-file-lines file-name))
         (durations (mapcar #'parse-integer
                            (rest (cl-ppcre:split "\\s+" (first lines)))))
         (records (mapcar #'parse-integer
                          (rest (cl-ppcre:split "\\s+" (second lines)))))
         (possibililties (process-races durations records)))
    (reduce #'* possibililties)))

(defun process-part2 (duration record)
  (let* ((range-start (floor (/ (- duration (sqrt (- (expt duration 2)
                                                     (* record 4))))
                                2)))
         (range-end (floor (/ (+ duration (sqrt (- (expt duration 2)
                                                   (* record 4))))
                              2))))
    (1- (- range-end range-start))))


(defun part2 (file-name)
  (let* ((lines (uiop:read-file-lines file-name))
         (duration (parse-integer
                    (remove #\space (cadr (cl-ppcre:split ":" (first lines))))))
         (record (parse-integer
                  (remove #\space (cadr (cl-ppcre:split ":" (second lines)))))))
    (print duration)
    (print record)
    (process-part2 duration record)))

(part2 "input0.txt")
(part2 "input1.txt")
(part1 "input0.txt")
(part1 "input1.txt")

(remove #\Space (cadr (cl-ppcre:split ":" "Time:      7  15   30")))


