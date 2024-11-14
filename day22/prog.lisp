;; day 22

(ql:quickload :cl-ppcre)
(ql:quickload :split-sequence)
(ql:quickload :lisp-utils)
(ql:quickload :alexandria)

(ql:quickload :queues)
(require :queues.simple-queue)


(use-package :lisp-utils)

(defclass brick ()
  ((start :initarg :start :reader start)
   (end :initarg :end :reader end)
   (flr :initarg :flr :accessor flr)
   (height :initarg :height :reader height)
   (support :initform nil :accessor support)
   (supporting :initform nil :accessor supporting)))

(defmethod print-object ((self brick) stream)
  (print-unreadable-object (self stream :type t)
    (with-slots (start end flr height support supporting) self
      (format stream "Brick(~a, ~a, ~a, ~a, ~a, ~a)" start end flr height support supporting))))

(defgeneric compare-bricks (b0 b1)
  (:method (b0 b1)
    (< (flr b0) (flr b1))))

(defun make-brick (start end)
  (let* ((flr (min (third start) (third end)))
         (height (abs (- (third start) (third end)))))
    (make-instance 'brick
                   :start start
                   :end end
                   :flr flr
                   :height height)))

(defun parse (lines)
  (let* ((bricks (queues:make-queue :simple-queue :comparison #'compare-bricks))
         (lowest-level nil))
    (dolist (line lines)
      (let* ((chunks (cl-ppcre:split "~" line))
             (left (mapcar #'parse-integer (cl-ppcre:split "," (first chunks))))
             (right (mapcar #'parse-integer (cl-ppcre:split "," (second chunks))))
             (brick (make-brick left right)))
        (queues:qpush bricks brick)

        (when (or (null lowest-level)
                  (< (flr brick) lowest-level))
          (setf lowest-level (flr brick)))))
    (list bricks lowest-level)))

(defun do-the-work (lines)
  (destructuring-bind (bricks floor-level) (parse lines)
    (let ((stable-bricks nil)
          (count-part1 0))
      (loop for brick = (queues:qpop bricks)
            while brick
            do (if (eq (flr brick) floor-level)
                   (push brick stable-bricks)
                   (let ((support nil)
                         (support-level 0))
                     (dolist (sb (reverse stable-bricks))
                       (when (and (<= (first (start sb))
                                      (first (end brick)))
                                  (<= (first (start brick))
                                      (first (end sb)))
                                  (<= (second (start sb))
                                      (second (end brick)))
                                  (<= (second (start brick))
                                      (second (end sb))))
                         (let ((stable-brick-top (+ (flr sb) (height sb))))
                           (when (> stable-brick-top support-level)
                             (setf support nil)
                             (setf support-level stable-brick-top))
                           (when (eq stable-brick-top support-level)
                             (push sb support)))))
                     
                     (setf (support brick) support)
                     (setf (flr brick) (1+ support-level))
                     (push brick stable-bricks)
                     
                     (dolist (support-brick support)
                       (push brick (supporting support-brick))))))

      (dolist (stable-brick (reverse stable-bricks))
        (if (> (length (supporting stable-brick)) 0)
            (when (eq (count-if (lambda (s)
                                  (> (length (support s)) 1))
                                (supporting stable-brick))
                      (length (supporting stable-brick)))
              (incf count-part1))
            (incf count-part1)))
      count-part1)))

(defun part1 (file-name)
  (let* ((lines (uiop:read-file-lines file-name))
         (res (do-the-work lines)))
    res))

(print (part1 "input0.txt"))
(print (part1 "input1.txt"))






