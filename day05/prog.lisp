;; day 5

(ql:quickload :cl-ppcre)
(ql:quickload :split-sequence)

(defun map-thing-to-thing (lines)
  (let ((ranges (make-hash-table :test #'equal)))
    (loop for line in lines
          do (let* ((chunks (mapcar #'parse-integer
                                    (cl-ppcre:split "\\s" line)))
                    (dest-start (first chunks))
                    (src-start (second chunks))
                    (length (third chunks))
                    (dest-range (loop for i from dest-start to (1- (+ dest-start length)) collecting i))
                    (src-range (loop for i from src-start to (1- (+ src-start length)) collecting i)))
               (setf (gethash src-range ranges) dest-range)))
    ranges))

(defun map-input (thing-map input)
  (loop for k being the hash-keys of thing-map
        when (member input k)
          return (+ (- input (first k))
                    (first (gethash k thing-map)))))

(defun print-things (thing-map)
  (maphash (lambda (k v)
             (format t "~&~a: ~a~%" k v)) thing-map))

(defun create-maps (sections)
  (let* ((maps (make-hash-table :test #'equal)))
    (loop for s in sections
          do (let* ((name (first (cl-ppcre:split "\\s+" (first s))))
                    (thing-map (map-thing-to-thing (rest s))))
               (setf (gethash name maps) thing-map)))
    maps))

(defun part1 (file-name)
  (let* ((lines (uiop:read-file-lines file-name))
         (seeds-line (car lines))
         (seeds (mapcar #'parse-integer (rest (cl-ppcre:split "\\s+" seeds-line))))
         (sections (split-sequence:split-sequence-if
                    (lambda (line) (eql (length line) 0))
                    (cddr lines)))
         (maps (create-maps sections))
         (locations
           (mapcar (lambda (seed)
                     (let* ((soil (map-input (gethash "seed-to-soil" maps) seed))
                            (fertilizer (map-input (gethash "soil-to-fertilizer" maps) soil))
                            (water (map-input (gethash "fertilizer-to-water" maps) fertilizer))
                            (light (map-input (gethash "water-to-light" maps) water))
                            (temperature (map-input (gethash "light-to-temperature" maps) light))
                            (humidity (map-input (gethash "temperature-to-humidity" maps) temperature))
                            (location (map-input (gethash "humidity-to-loation" maps) humidity)))
                       location))
                   seeds)))
    locations))

(part1 "input0.txt")












;; (defparameter m (map-thing-to-thing (subseq lns 3 5)))
;; (print-things m)
;; (map-input m 98)

