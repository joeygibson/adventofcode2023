;; day 14

(ql:quickload :cl-ppcre)
(ql:quickload :split-sequence)
(ql:quickload :lisp-utils)
(ql:quickload :alexandria)

(use-package :lisp-utils)
(use-package :cl-ppcre)
;; (use-package :alexandria)

(setf *print-circle* t)

(defun create-map (lines)
  (let ((map (make-hash-table :test #'equal)))
    (loop for row-num from 0
          for row in lines
          do (loop for col-num from 0
                   for col in (cl-ppcre:split "" row)
                   do (setf (gethash (cons row-num col-num) map) col)))
    map))

(defun find-empty-row (map row-num col-num)
  (let ((row-range (reverse (alexandria:iota row-num)))
        (row-to-move-to row-num))
    (loop for i in row-range
          until (or (equal (gethash (cons i col-num) map) "#")
                    (equal (gethash (cons i col-num) map) "O"))
          do (decf row-to-move-to))
    row-to-move-to))

(defun tilt (map)
  (let ((pairs (remove-if (lambda (x)
                            (or (equal (car x) 0)
                                (or (equal (gethash x map) "#")
                                    (equal (gethash x map) "."))))
                          (reverse (alexandria:hash-table-keys map)))))
    (loop for pair in pairs
          do (let* ((row-num (car pair))
                    (col-num (cdr pair))
                    (row-to-move-to (find-empty-row map row-num col-num)))
               (if (not (equal row-to-move-to row-num))
                   (progn
                     (setf (gethash (cons row-to-move-to col-num) map) "O")
                     (setf (gethash pair map) ".")))))
    map))

(defun rotate (map cols)
  (let ((rotated (make-hash-table :test #'equal)))
    (maphash (lambda (key value)
               (let* ((x (car key))
                      (y (cdr key))
                      (new-x y)
                      (new-y (- (1- cols) x)))
                 (setf (gethash (cons new-x new-y) rotated) value)))
             map)
    rotated))

(defun cycle (map cols)
  (let ((tilted-map nil))
    (loop for i below 4
          do (setf tilted-map (rotate (tilt map) cols)))
    tilted-map))

(defun part1 (file-name)
  (let* ((lines (uiop:read-file-lines file-name))
         (max-rows (length lines))
         (max-cols (length (cl-ppcre:split "" (car lines))))
         (map (create-map lines))
         (tilted-map (tilt map))
         (total 0))
    (loop for row-num below max-rows
        do (let ((rock-count (length (loop for col-num below max-cols
                                           if (equal (gethash (cons row-num col-num) tilted-map) "O")
                                             collect 1))))
             (setf total (+ total (* rock-count (- max-rows row-num))))))
    total))

(defun deep-copy (map)
  (let ((pairs (alexandria:hash-table-alist map)))
    (alexandria:alist-hash-table pairs)))

(defun part2 (file-name)
  (let* ((lines (uiop:read-file-lines file-name))
         (rows (length lines))
         (cols (length (car lines)))
         (the-map (create-map lines))
         (seen (list the-map))
         (cycles 1000000000)
         (loop-index 0)
         (loop-length 0)
         (final-map nil)
         (total 0))
    (loop for i below cycles
          do 
             (progn
               (setf the-map (cycle (deep-copy the-map) cols))
               (print i)
               (when (member the-map seen :test #'equalp)
                 (progn
                   (print "hit!")
                   (setf loop-index (position the-map seen :test #'equalp))
                   (setf loop-length (- (1+ i) loop-index))
                   (return)))
               
               (setf seen (append seen (list the-map)))))

    (setf final-map (nth (+ (mod (- cycles loop-index) loop-length) loop-index) seen))
    
    (loop for row-num below rows
          do (let ((rock-count (length (loop for col-num below cols
                                             if (equal (gethash (cons row-num col-num) final-map) "O")
                                               collect 1))))
               (setf total (+ total (* rock-count (- rows row-num))))))
    total))

(print (part1 "input0.txt"))
(print (part1 "input1.txt"))

(print (part2 "input0.txt"))

(let* ((m (create-map (uiop:read-file-lines "input0.txt")))
       (rm (rotate m 10))
       (rm1 (rotate m 10))
       (seen (list m rm)))
  (member rm seen :test #'equalp))




