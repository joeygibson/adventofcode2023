;; day 13

(ql:quickload :cl-ppcre)
(ql:quickload :split-sequence)
(ql:quickload :lisp-utils)
(use-package :lisp-utils)
(use-package :cl-ppcre)

(setf *print-circle* t)

(defun rotate-90 (matrix)
  (apply #'mapcar #'list (reverse matrix)))

(defun list-to-string (list)
  (format nil "~{~A~}" list))

(defun rotate-strings-90 (matrix)
    (mapcar (lambda (row)
              (list-to-string row))
            (rotate-90 (mapcar (lambda (line)
                                 (split "" line))
                               matrix))))

(defun mirror (lines divider)
  (let* ((first-half (reverse (lisp-utils:take lines divider)))
         (second-half (lisp-utils:drop lines divider))
         (max-rows (min (length first-half) (length second-half)))
         (fst (take first-half max-rows))
         (snd (take second-half max-rows)))
    (if (equal fst snd)
        (1+ max-rows)
        0)))

(defun mirror-match (section)
  (reduce #'+ (loop for i from 1 upto (1- (length section))        
                    collecting (mirror section i))))

(defun part1 (file-name)
  (let* ((sections (split-file-into-sections file-name))
         (x-matches (reduce #'+ (mapcar (lambda (section)
                                          (mirror-match section))
                                        sections)))
         (y-matches (reduce #'+ (mapcar (lambda (section)
                                          (mirror-match (rotate-strings-90 section)))
                                        sections))))
    (+ (* x-matches 100) y-matches)))

(print (part1 "input0.txt"))
(print (part1 "input1.txt"))


