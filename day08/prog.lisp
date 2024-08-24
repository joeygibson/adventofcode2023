;; day 08

(declaim (optimize (speed 0) (space 0) (debug 3)))

(ql:quickload :cl-itertools)

(require :cl-ppcre)
(use-package :cl-itertools)

(defstruct node name left right)

(defun build-nodes (lines)
  (let ((nodes (make-hash-table :test #'equal))
        (dirs (cl-ppcre:split "" (first lines))))
    (mapc (lambda (line)
            (let* ((chunks (cl-ppcre:all-matches-as-strings "\\w{3}" line))
                   (name (first chunks))
                   (left-name (second chunks))
                   (right-name (third chunks))
                   (node (let ((n (gethash name nodes (make-node :name name))))
                           (setf (gethash name nodes) n)
                           n))       
                   (left (let ((n (gethash left-name nodes (make-node :name left-name))))
                           (setf (gethash left-name nodes) n)
                           n))
                   (right (let ((n (gethash right-name nodes (make-node :name right-name))))
                            (setf (gethash right-name nodes) n)
                            n)))
              (setf (node-left node) left)
              (setf (node-right node) right)))
          (cddr lines))
    (values nodes dirs)))

(defun print-node (node)
  (format t "~&~s: (~s, ~s)~%"
          (node-name node)
          (node-name (node-left node))
          (node-name (node-right node))))

(defun find-node (node directions)
  (let ((steps 0))
    (loop
      (let* ((dir (inext directions)))
        (if (equal (node-name node) "ZZZ")
            (return-from find-node steps))
        (incf steps)
        (if (equal dir "L")
            (setf node (node-left node))
            (setf node (node-right node)))))))

(defun part1 (file-name)
  (let ((lines (uiop:read-file-lines file-name)))
    (multiple-value-bind (nodes dirs) (build-nodes lines)
      (find-node (gethash "AAA" nodes)
                 (icycle dirs)))))

(part1 "input1.txt")





