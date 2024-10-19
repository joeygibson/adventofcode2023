;; day 20

(ql:quickload :cl-ppcre)
(ql:quickload :split-sequence)
(ql:quickload :lisp-utils)
(ql:quickload :alexandria)
(ql:quickload :queues)

(require :queues.simple-queue)

(defconstant FLIP-FLOP #\%)
(defconstant CONJUNCTION #\&)
(defconstant BROADCASTER #\X)
(defconstant UNTYPED nil)
(defconstant LOW-PULSE 0)
(defconstant HIGH-PULSE 1)

(defclass component ()
    ((name :initarg :name :reader name)
     (comp-type :initarg :comp-type :reader comp-type)
     (is-on :accessor is-on :initform nil)
     (inputs :accessor inputs :initform nil)
     (output :accessor  outputs :initform nil)))

(defmethod hash ((self component))
  (hash (name self)))

(defmethod print-object ((self component) stream)
  (print-unreadable-object (self stream :type t)
    (with-slots (name comp-type) self
        (format stream "~a(~a) in: ~a, out: ~a"
                name
                comp-type
                (length (inputs self))
                (length (outputs self))))))

(defclass conjunction (component)
  ((previous-pulses :reader previous-pulses :initform (make-hash-table :test #'equal))))

(defmethod setup ((self conjunction))
  (dolist (input (inputs self))
    (setf (gethash input (previous-pulses self)) LOW-PULSE)))

(defmethod all-high ((self conjunction))
  (every (lambda (pulse)
           (equal pulse HIGH-PULSE))
         (alexandria:hash-table-values (previous-pulses self))))

(defmethod update ((self conjunction) comp pulse)
  (setf (gethash comp (previous-pulses self)) pulse))

(defmethod print-object ((self conjunction) stream)
  (print-unreadable-object (self stream :type t)
    (with-slots (name comp-type previous-pulses)
        self
      (format stream "~a(~a) -> ~a" name comp-type previous-pulses))))

(defmethod is-conjunction ((self component))
  (equal (comp-type self) CONJUNCTION))

(defun make-component (name)
  (let* ((comp-name (if (equal name "broadcaster")
                        "broadcaster"
                        (subseq name 1)))
         (first-char (char name 0)))
    (cond ((equal first-char CONJUNCTION)
           (make-instance 'conjunction
                          :name comp-name
                          :comp-type CONJUNCTION))
          ((equal first-char FLIP-FLOP)
           (make-instance 'component
                          :name comp-name
                          :comp-type FLIP-FLOP))
          ((equal comp-name "broadcaster")
           (make-instance 'component
                          :name comp-name
                          :comp-type BROADCASTER))
          (t
           (make-instance 'component
                          :name comp-name
                          :comp-type UNTYPED)))))

(defun parse (lines)
  (let* ((connections (make-hash-table :test #'equal))
         (components (make-hash-table :test #'equal)))
    (dolist (line lines)
      (destructuring-bind (type-name sends-to) (cl-ppcre:split " -> " line)
        (let ((c (make-component type-name)))
          (setf (gethash (name c) components) c)
          (setf (gethash (name c) connections) (cl-ppcre:split ",\\s*" sends-to)))))
    (alexandria:maphash-values (lambda (connections)
                                 (dolist (conn connections)
                                   (when (not (gethash conn components))
                                     (let ((component (make-component conn)))
                                       (setf (gethash conn components) component)))))
                               connections)
    (maphash (lambda (name comp)
               (when (gethash name connections)
                 (setf (outputs comp) (mapcar (lambda (c)
                                                (gethash c components))
                                              (gethash name connections)))
                 (let ((input-components (loop for conn being the hash-keys of connections
                                               for connex = (gethash conn connections)
                                               if (member name connex :test #'equal)
                                                 collect (gethash conn components))))
                   (setf (inputs comp) input-components))))
             components)
    (alexandria:maphash-values (lambda (comp)
                                 (if (is-conjunction comp)
                                     (setup comp)))
                               components)
    components))

(defun press-the-button (circuit &optional (hits nil) (iteration 1))
  (let* ((pulse-counts (make-hash-table :test #'equal))
         (queue (queues:make-queue :simple-queue)))
    (setf (gethash LOW-PULSE pulse-counts) 1)
    (queues:qpush queue (list nil (gethash "broadcaster" circuit) LOW-PULSE))
    (loop for val = (queues:qpop queue)
          while val
          do (destructuring-bind (input-comp comp pulse) val
               (cond ((equal (comp-type comp) BROADCASTER)
                      (dolist (output (outputs comp))
                        (queues:qpush queue (list comp output pulse))
                        (incf (gethash pulse pulse-counts 0))))
                     ((equal (comp-type comp) FLIP-FLOP)
                      (unless (equal pulse HIGH-PULSE)
                        (if (is-on comp)
                            (progn
                              (setf (is-on comp) nil)
                              (dolist (output (outputs comp))
                                (queues:qpush queue (list comp output LOW-PULSE))
                                (incf (gethash LOW-PULSE pulse-counts 0))))
                            (progn
                              (setf (is-on comp) t)
                              (dolist (output (outputs comp))
                                (queues:qpush queue (list comp output HIGH-PULSE))
                                (incf (gethash HIGH-PULSE pulse-counts 0)))))))
                     ((equal (comp-type comp) CONJUNCTION)
                      (update comp input-comp pulse)
                      (let ((output-pulse (if (all-high comp)
                                              LOW-PULSE
                                              HIGH-PULSE)))
                        (dolist (output (outputs comp))
                          (queues:qpush queue (list comp output output-pulse))
                          (incf (gethash output-pulse pulse-counts 0)))
                        (if (and hits
                                 (all-high comp)
                                 (not (member (name comp) hits :test #'equal)))
                            (setf (gethash (name comp) hits) iteration)))))))
    pulse-counts))


(defun part1 (file-name)
  (let* ((lines (uiop:read-file-lines file-name))
         (circuit (parse lines))
         (totals (make-hash-table :test #'equal)))
    (dotimes (i 1000)
      (let ((pulse-counts (press-the-button circuit)))
        (maphash (lambda (pulse count)
                   (incf (gethash pulse totals 0) count))
                 pulse-counts)))
    (* (gethash HIGH-PULSE totals)
       (gethash LOW-PULSE totals))))

(print (part1 "input0.txt"))





