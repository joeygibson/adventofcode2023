;; day 12

(declaim (optimize (speed 0) (space 0) (debug 3)))

(ql:quickload :cl-ppcre)
(ql:quickload :lisp-utils)
(use-package :lisp-utils)
(use-package :cl-ppcre)

(setf *print-circle* t)

(defun parse (lines)
  (mapcar (lambda (line)
            (let* ((chunks (split "\\s+" line))
                   (springs (split "" (car chunks)))
                   (broken-sprint-counts (mapcar (lambda (x)
                                                   (parse-integer x))
                                                 (split "," (cadr chunks)))))
              (list springs
                    broken-sprint-counts)))
          lines))

(defun calc (record groups)
  (cond
    ((not groups) (if (not (member "#" record :test #'equal))
                      1
                      0))
    ((not record) 0)
    (t (let ((next-character (car record))
             (next-group (car groups)))
         (labels ((pound ()
                    (let ((this-group (apply #'concatenate 'string (mapcar (lambda (x)
                                                                             (if (equal x "?")
                                                                                 "#"
                                                                                 x))
                                                                           (take record next-group)))))
                      (cond
                        ((not (equal this-group (mul-string next-group #\#))) 0)
                        ((eq (length record) next-group) (if (eq (length groups) 1)
                                                             1
                                                             0))
                        ((or (equal (nth next-group record) "?")
                             (equal (nth next-group record) "."))
                         (calc (drop record (+ next-group 1))
                               (cdr groups)))
                        (t 0))))
                  (dot ()
                    (calc (cdr record) groups)))
           (cond
             ((equal next-character "#") (pound))
             ((equal next-character ".") (dot))
             ((equal next-character "?") (+ (dot) (pound)))
             (t (error (format nil "unknown character: ~a" next-character)))))))))


(defun part1 (file-name)
  (let* ((lines (uiop:read-file-lines file-name))
         (data (parse lines))
         (results (mapcar (lambda (row)
                            (destructuring-bind (record groups) row
                              (calc record groups)))
                          data)))
    (reduce #'+ results)))

(print (part1 "input0.txt"))
(print (part1 "input1.txt"))

