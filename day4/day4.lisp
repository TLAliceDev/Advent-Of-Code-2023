(ql:quickload :parse-number)
(ql:quickload :split-sequence)
(ql:quickload :alexandria)

(defun sethash (key hash value)
  (setf (gethash key hash) value)
  value)

(defun split-on-space (sequence)
  (split-sequence:split-sequence #\Space sequence :remove-empty-subseqs t))

(defun get-winning-numbers-string (card)
  (subseq card (1+ (position #\: card :test #'equal)) (position #\| card :test #'equal)))

(defun get-has-numbers-string (card)
  (subseq card (1+ (position #\| card :test #'equal))))

(defun get-winning-numbers (card)
  (mapcar #'parse-number:parse-number (split-on-space (get-winning-numbers-string card))))

(defun get-has-numbers (card) 
  (mapcar #'parse-number:parse-number (split-on-space (get-has-numbers-string card))))

(defun get-card-number-string (card)
  (subseq card 5 (position #\: card :test #'equal)))

(defun get-card-number (card)
  (parse-number:parse-number (get-card-number-string card)))

(defun parse-card (card)
  (list :number (get-card-number card)
        :winning (get-winning-numbers card)
        :has (get-has-numbers card)))

(defun card-wins (card)
  (length (intersection (getf card :winning) (getf card :has))))

(defun card-score (card)
  (let ((wins (card-wins card)))
    (if (/= 0 wins)
        (expt 2 (1- wins))
        0)))

(defun calculate-card-duplicates (card)
  (mapcar (lambda (x) (+ 1 x (getf card :number)))
          (alexandria:iota (card-wins card))))

(defun get-card-duplicates (card hash)
  (let ((on-hash (gethash (getf card :number) hash)))
    (if on-hash
        on-hash
        (sethash (getf card :number)
                 hash
                 (calculate-card-duplicates card)))))

(defun part2-recurse (cards hash card dupes-hash)
  (let ((dupes-n (gethash card dupes-hash)))
    (if dupes-n
        dupes-n
        (let ((duplicates (get-card-duplicates (aref cards (1- card)) hash)))
          (if duplicates
              (let ((duplicates-n (apply #'+ (length duplicates) (mapcar (lambda (x) (part2-recurse cards hash x dupes-hash)) duplicates))))
                (sethash card dupes-hash duplicates-n))
              0)))))

(defun part1 (input)
  (let ((cards (map 'list #'parse-card (uiop:read-file-lines input))))
    (apply #'+ (map 'list #'card-score cards))))

(defun part2 (input)
  (let* ((cards (map 'vector #'parse-card (uiop:read-file-lines input)))
         (cards-hash (make-hash-table))
         (dupes-hash (make-hash-table))
         (cards-n (map 'vector (lambda (x) (getf x :number)) cards)))
    (apply #'+ (length cards-n) (map 'list (lambda (x) (part2-recurse cards cards-hash x dupes-hash)) cards-n))))
