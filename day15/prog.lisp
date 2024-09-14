;; day 15

(ql:quickload :cl-ppcre)
(ql:quickload :split-sequence)
(ql:quickload :lisp-utils)
(ql:quickload :alexandria)

(use-package :lisp-utils)
(use-package :cl-ppcre)

(defun hash (str)
  (reduce (lambda (cur val)
            (mod (* (+ cur val) 17) 256))
          (map 'list #'char-code str)
          :initial-value 0))

(defun part1 (file-name)
  (let* ((input (string-trim '(#\Space #\Tab #\Newline)
                             (uiop:read-file-string file-name)))
         (items (cl-ppcre:split "," input)))
    (loop for item in items
          summing (hash item))))

(defstruct lens label focal-length)

(defun part2 (file-name)
  (let* ((input (string-trim '(#\Space #\Tab #\Newline)
                             (uiop:read-file-string file-name)))
         (items (cl-ppcre:split "," input))
         (boxes (make-hash-table :test #'equal))
         (total 0))
    (dolist (item items)
      (if (search "=" item)
          (destructuring-bind (label value) (cl-ppcre:split "=" item)
            (let* ((box-id (hash label))
                   (lens (make-lens :label label
                                    :focal-length (parse-integer value)))
                   (box (alexandria:ensure-gethash box-id boxes))
                   (lens-index (position-if (lambda (x)
                                              (equal (lens-label x) label))
                                            box)))
              (if lens-index
                  (setf (nth lens-index box) lens)
                  (setf box (append box (list lens))))
              (setf (gethash box-id boxes) box)))
          (let* ((label (string-trim '(#\-) item))
                 (box-id (hash label))
                 (box (alexandria:ensure-gethash box-id boxes)))
            (setf box (remove-if (lambda (x)
                                   (equal (lens-label x) label))
                                 box))
            (setf (gethash box-id boxes) box))))
    (maphash (lambda (box-id box)
               (incf total (loop for i from 0
                                 for lens in box
                                 summing (* (* (1+ box-id)
                                               (1+ i))
                                            (lens-focal-length lens)))))
             boxes)
    total))


(print (part1 "input0.txt"))
(print (part1 "input1.txt"))

(print (part2 "input0.txt"))
(print (part2 "input1.txt"))

