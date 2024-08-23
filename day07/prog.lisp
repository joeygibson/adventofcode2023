;; day 7

(declaim (optimize (speed 0) (space 0) (debug 3)))

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

(defun make-hand (cards bid &key has-wild-cards)
  (let ((split-cards (cl-ppcre:split "" cards))
        (card-counts (make-hash-table :test #'equal)))
    (loop for card in split-cards
          do (incf (gethash card card-counts 0)))

    (let ((hand (make-instance 'hand :cards split-cards
                                     :bid (parse-integer bid)
                                     :card-counts card-counts)))
      (if has-wild-cards
          (let ((jokers (count "J" split-cards :test #'equal))
                (cur-strength (strength hand)))
            (if jokers
                (progn
                  (remhash "J" (card-counts hand))
                  (let* ((high-key
                           (cond ((= cur-strength 7) "K")
                                 ((= cur-strength 6) (get-high-card hand 4))
                                 ((= cur-strength 5) (get-high-card hand 3))
                                 ((= cur-strength 4) (get-high-card hand 3))
                                 ((= cur-strength 3) (get-high-card hand 2))
                                 ((= cur-strength 2) (get-high-card hand 2))
                                 ((= cur-strength 1) (car (sort (loop for k being the hash-keys of card-counts
                                                                      collecting k)
                                                                #'string-lessp))))))

                    (if (not high-key)
                        (error "unknown strength"))
                    
                    (setf (gethash high-key card-counts)
                          (+ (gethash high-key card-counts 0)
                             jokers)))))))
      hand)))

(let ((cc (make-hash-table :test #'equal)))
  (setf (gethash 5 cc) 1)
  (setf (gethash 5 cc) 1)
  (last (sort (loop for k being the hash-keys of cc collecting k)
              #'string-lessp)))

(defmethod get-high-card ((hand hand) cnt)
  (with-slots (card-counts) hand
    (let ((high-card (loop for k being the hash-keys of card-counts
                           when (= (gethash k card-counts) cnt)
                             return k)))
      (if high-card
          high-card
          (let ((res (car (sort (loop for k being the hash-keys of card-counts
                                      collecting k)
                                #'string-lessp))))
            res)))))

;; (defmethod print-object ((hand hand) stream)
;;   (print-unreadable-object (hand stream :type t :identity t)
;;     (format stream (print-hand hand))))

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
                     (return-from loop)))
               (if (> sc oc)
                   (progn
                     (setf return-value 1)
                     (return-from loop)))))
    return-value))

(defmethod is-less ((self hand)
                    (other hand))
  (cond ((< (strength self)
            (strength other))
         t)
        ((> (strength self)
            (strength other))
         nil)
        (t (< (compare-cards self other) 0))))

(defun part1 (file-name)
  (let* ((lines (uiop:read-file-lines file-name))
         (hands (mapcar (lambda (line)
                          (let ((chunks (cl-ppcre:split "\\s+" line)))
                            (make-hand (first chunks)
                                       (second chunks))))
                        lines))
         (sorted-hands (sort hands #'is-less))
         (values (loop for i from 1
                       for h in sorted-hands
                       collecting (* (bid h) i))))
    (reduce #'+ values)))

(defun part2 (file-name)
  (setf (gethash "J" *face-cards*) 1)
  
  (let* ((lines (uiop:read-file-lines file-name))
         (hands (mapcar (lambda (line)
                          (let ((chunks (cl-ppcre:split "\\s+" line)))
                            (make-hand (first chunks)
                                       (second chunks)
                                       :has-wild-cards t)))
                        lines))
         (sorted-hands (sort hands #'is-less))
         (values (loop for i from 1
                       for h in  sorted-hands
                       collecting (* (bid h) i))))
    (reduce #'+ values)))

(print (part1 "input0.txt"))
(print (part1 "input1.txt"))

(print (part2 "input0.txt"))
(print (part2 "input1.txt"))






