;; day 16

(ql:quickload :cl-ppcre)
(ql:quickload :split-sequence)
(ql:quickload :lisp-utils)
(ql:quickload :alexandria)

(use-package :lisp-utils)
(use-package :cl-ppcre)

(defstruct spot shape is-energized)

(defstruct beam pos dir)

(defun move (beam)
  (let* ((dir (beam-dir beam))
         (x (car (beam-pos beam)))
         (y (cdr (beam-pos beam)))
         (new-pos (cond
                    ((equal dir "u") (cons x (1- y)))
                    ((equal dir "d") (cons x (1+ y)))
                    ((equal dir "l") (cons (1- x) y))
                    ((equal dir "r") (cons (1+ x) y)))))
    ;; (format t "~&~a, (~a, ~a) -> ~a~%" dir x y new-pos)
    (setf (beam-pos beam) new-pos)))

(defun build-map (lines)
  (let ((cave (make-hash-table :test #'equal)))
    (loop for y from 0
          for line in lines
          do (loop for x from 0
                   for c in (cl-ppcre:split "" line)
                   do (setf (gethash (cons x y) cave)
                            (make-spot :shape c))))
    cave))


(defun solve (lines)
  (let* ((cave (build-map lines))
         (beam (make-beam :pos (cons 0 0) :dir "r"))
         (beams (list beam))
         (split-spots nil))
    (loop until (not beams)
          do (let ((beam (pop beams)))
               (loop named inner
                     until (not (gethash (beam-pos beam) cave))
                     do (let* ((spot (gethash (beam-pos beam) cave))
                               (shape (spot-shape spot))
                               (dir (beam-dir beam)))
                          ;; (format t "~&~a: ~a~%" (beam-pos beam) spot)
                          (cond
                            ((equal shape "/")
                             (let ((new-dir (cond
                                              ((equal dir "u") "r")
                                              ((equal dir "d") "l")
                                              ((equal dir "l") "d")
                                              ((equal dir "r") "u"))))
                               (setf (beam-dir beam) new-dir)))
                            ((equal shape "\\")
                             (let ((new-dir (cond
                                              ((equal dir "u") "l")
                                              ((equal dir "d") "r")
                                              ((equal dir "l") "u")
                                              ((equal dir "r") "d"))))
                               (setf (beam-dir beam) new-dir)))
                            ((equal shape "-")
                             (when (or (equal dir "u")
                                       (equal dir "d"))
                               (setf (beam-dir beam) "r")
                               (let ((new-beam (make-beam :pos (beam-pos beam)
                                                          :dir "l")))
                                 (when (not (member new-beam split-spots :test #'equalp))
                                   (setf split-spots (append split-spots (list (copy-beam new-beam))))
                                   (setf beams (append beams (list new-beam))))
                                 (when (member beam split-spots :test #'equalp)
                                   (return-from inner))
                                 (setf split-spots (append split-spots (list (copy-beam beam)))))))
                            ((equal shape "|")
                             (when (or (equal dir "l")
                                       (equal dir "r"))
                               (setf (beam-dir beam) "d")
                               (let ((new-beam (make-beam :pos (beam-pos beam)
                                                          :dir "u")))
                                 (when (not (member new-beam split-spots :test #'equalp))
                                   (setf split-spots (append split-spots (list (copy-beam new-beam))))
                                   (setf beams (append beams (list new-beam))))
                                 (when (member beam split-spots :test #'equalp)
                                   (return-from inner))
                                 (setf split-spots (append split-spots (list (copy-beam beam))))))))
                          (move beam)
                          (setf (spot-is-energized spot) t)))))
    (count-if (lambda (spot)
                (spot-is-energized spot))
              (alexandria:hash-table-values cave))))

(defun part1 (file-name)
  (let ((lines (uiop:read-file-lines file-name)))
    (solve lines)))

(print (part1 "input0.txt"))
(print (part1 "input1.txt"))




