;; day 7

(require :cl-ppcre)

(defparameter *face-cards* (let ((ht (make-hash-table :test #'equal)))
                             (loop for (k . v) in
                                   '(("T" . 10)
                                     ("J" . 11)
                                     ("Q" . 12)
                                     ("K" . 13)
                                     ("A" . 14))
                                   do (setf (gethash k ht) v))
                             ht))

(defclass hand ()
  ((cards
    :initarg :cards
    :reader cards)
   (bid
    :initarg :bid
    :reader bid)
   (card-counts
    :initarg :card-counts
    :reader card-counts)))

(defun make-hand (cards bid)
  (let ((split-cards (cl-ppcre:split "" cards))
        (card-counts (make-hash-table :test #'equal)))
    (loop for card in split-cards
          do (incf (gethash card card-counts 0)))
    
    (make-instance 'hand :cards split-cards
                         :bid bid
                         :card-counts card-counts)))

(defmethod print-hand ((hand hand))
  (with-slots (cards bid card-counts) hand
      (format t "~&Hand(~a, ~a, {~a})" cards bid
              (format nil "~{~a~^, ~}" ; separate with commas
                      (loop for k being the hash-keys of card-counts
                            collecting (format nil "~a: ~a" k (gethash k card-counts)))))))

(defmethod is-five-of-a-kind ((hand hand))
  (= (hash-table-count (card-counts hand)) 1))

(defmethod hand-contains ((hand hand)
                          key-count
                          key-value-of-interest)
  (with-slots (card-counts) hand
    (and (= (hash-table-count card-counts) key-count)
         (loop for value being the hash-values of card-counts
               when (equal value key-value-of-interest)
                 return t))))

(defun is-four-of-a-kind (hand)
  (hand-contains hand 2 4))

(defun is-full-house (hand)
  (hand-contains hand 2 3))

(defun is-three-of-a-kind (hand)
  (hand-contains hand 3 3))

(defun is-two-pair (hand)
  (hand-contains hand 3 2))

(defun is-one-pair (hand)
  (hand-contains hand 4 2))

(defun is-high-card (hand)
  (hand-contains hand 5 1))

(defmethod strength ((hand hand))
  (cond ((is-five-of-a-kind hand) 7)
        ((is-four-of-a-kind hand) 6)
        ((is-full-house hand) 5)
        ((is-three-of-a-kind hand) 4)
        ((is-two-pair hand) 3)
        ((is-one-pair hand) 2)
        ((is-high-card hand) 1)
        (t (error "unknown hand"))))

(defun convert-face-card (card)
  (gethash card *face-cards* (parse-integer card :junk-allowed t)))

(defmethod compare-cards ((self hand)
                          (other hand))
  (let ((return-value 0))
    (loop named loop
          for self-card in (cards self)
          for other-card in (cards other)
          do (let ((sc (convert-face-card self-card))
                   (oc (convert-face-card other-card)))
               (if (< sc oc)
                   (progn
                     (setf return-value -1)
                     (return-from loop))
                   (progn
                     (setf return-value 1)
                     (return-from loop)))))
    return-value))

(defmethod is-less ((self hand)
                    (other hand))
  (cond ((< (strength self)
            (strength other))
         t)
        ((< (strength other)
            (strength self))
         nil)
        (t (< (compare-cards self other) 0))))

(defun part1 (file-name)
  (let* ((lines (uiop:read-file-lines file-name))
         (hands (mapcar (lambda (line)
                          (let ((chunks (cl-ppcre:split "\\s+" line)))
                            (make-hand (first chunks)
                                       (second chunks))))
                        lines)))
    (mapcar #'print-hand hands)))


(part1 "input0.txt")


