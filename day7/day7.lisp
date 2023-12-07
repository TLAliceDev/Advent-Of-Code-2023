(ql:quickload :split-sequence)
(ql:quickload :parse-number)
(ql:quickload :alexandria)

(defun label->strength (label)
  (let ((digit (digit-char-p label)))
    (if digit
        digit
        (case label (#\T 10)
                    (#\J 11)
                    (#\Q 12)
                    (#\K 13)
                    (#\A 14)))))

(defun parse-row (row)
  (let ((split (split-sequence:split-sequence #\Space row)))
    (list :hand (map 'list #'label->strength (first split))
          :bid (parse-number:parse-number (second split)))))

(defun list= (a b)
  (every #'= a b))

(defun hand-type (hand)
  (let ((count (sort (map 'list (lambda (x) (count x hand)) hand) #'>)))
    (cond ((list= count '(5 5 5 5 5)) 7)
          ((list= count '(4 4 4 4 1)) 6)
          ((list= count '(3 3 3 2 2)) 5)
          ((list= count '(3 3 3 1 1)) 4)
          ((list= count '(2 2 2 2 1)) 3)
          ((list= count '(2 2 1 1 1)) 2)
          ((list= count '(1 1 1 1 1)) 1)
          (t 0))))


(defun get-hand-comparison (a b)
  (lambda (x y) 
    (if (> x y)
        a
        (if (< x y)
            b
            nil))))

(defun stronger-numbers (a b)
  (first (remove-if #'null (mapcar (get-hand-comparison a b) a b))))

(defun stronger-hand (a b)
  (let ((type-a (hand-type a))
        (type-b (hand-type b)))
    (if (> type-a type-b)
        a
        (if (< type-a type-b)
            b
            (stronger-numbers a b)))))

(defun compare-hands (a b)
  (if (list= (stronger-hand a b) a)
      nil
      t))

(defun compare-rows (a b)
  (compare-hands (getf a :hand) (getf b :hand)))

(defun part1 (input)
  (let* ((rows (mapcar #'parse-row (uiop:read-file-lines input)))
         (sorted-rows (sort rows #'compare-rows)))
    (apply #'+ (mapcar (lambda (x y) (* (getf x :bid) (1+ y))) sorted-rows (alexandria:iota (length sorted-rows))))))

(defun convert-jokers (row)
  (list :hand (substitute 1 11 (getf row :hand)) :bid (getf row :bid)))

(defun most-common-non-joker-card (hand)
  (let* ((freqs (mapcar (lambda (x) (list x (count x hand))) (remove 1 hand)))
         (sorted-freqs (sort freqs (lambda (x y) (> (second x) (second y))))))
    (first (first sorted-freqs))))

(defun strongest-replacement (hand)
  (substitute (most-common-non-joker-card hand) 1 hand))

(defun stronger-hand-p2 (a b)
  (let ((type-a (hand-type (strongest-replacement a)))
        (type-b (hand-type (strongest-replacement b))))
    (if (> type-a type-b)
        a
        (if (< type-a type-b)
            b
            (stronger-numbers a b)))))

(defun compare-hands-p2 (a b)
  (if (list= (stronger-hand-p2 a b) a)
      nil
      t))

(defun compare-rows-p2 (a b)
  (compare-hands-p2 (getf a :hand) (getf b :hand)))

(defun part2 (input)
  (let* ((rows (mapcar #'convert-jokers (mapcar #'parse-row (uiop:read-file-lines input))))
         (sorted-rows (sort rows #'compare-rows-p2)))
    (apply #'+ (mapcar (lambda (x y) (* (getf x :bid) (1+ y))) sorted-rows (alexandria:iota (length sorted-rows))))))
