;; day02

(ql:quickload "cl-ppcre")
(require 'cl-ppcre)

(defvar *max-red* 12)
(defvar *max-green* 13)
(defvar *max-blue* 14)

(defun extract-results (line)
  (let ((results nil))
    (cl-ppcre:do-register-groups (match) ("(\\d+ \\w+)" line)
      (let ((chunks (cl-ppcre:split " " match)))
        (push (list (parse-integer (first chunks))
                    (cadr chunks)) results)))
    (nreverse results)))

(defun get-input (file-name)
  (with-open-file (in file-name :direction :input)
    (let ((game-lines (loop for line = (read-line in nil)
                             while line
                             collecting line))
          (games (make-hash-table :test #'equal)))
      (mapcar (lambda (line)
                (let* ((chunks (cl-ppcre:split ":" line))
                       (game-no (second (cl-ppcre:split " " (car chunks))))
                       (game (or (gethash game-no games)
                                 (make-hash-table :test #'equal)))
                       (results (extract-results line)))
                  (dolist (result results)
                    (let* ((num (car result))
                           (color (cadr result))
                           (new-value (max (gethash color game 0) num)))
                      (setf (gethash color game) new-value)))
                  (setf (gethash game-no games) game)))
              game-lines)
      games)))

(defun part1 (file_name)
  (let ((games (get-input file_name))
        (possible-games nil))
    (loop for key being the hash-keys of games
            using (hash-value value)
          do (if (and (<= (gethash "red" value) *max-red*)
                      (<= (gethash "green" value) *max-green*)
                      (<= (gethash "blue" value) *max-blue*))
                 (push (parse-integer key) possible-games)))
    (reduce #'+ possible-games)))

(print (part1 "input1.txt"))













