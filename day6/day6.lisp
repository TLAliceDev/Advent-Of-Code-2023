(ql:quickload :split-sequence)
(ql:quickload :parse-number)

(defun parse-time (string)
  (mapcar #'parse-number:parse-number
          (split-sequence:split-sequence #\Space (subseq string 5) :remove-empty-subseqs t)))

(defun parse-distance (string)
  (mapcar #'parse-number:parse-number
          (split-sequence:split-sequence #\Space (subseq string 10) :remove-empty-subseqs t)))

(defun parse-time-p2 (string)
  (parse-number:parse-number (apply #'concatenate
                                    'string
                                    (split-sequence:split-sequence #\Space (subseq string 5) :remove-empty-subseqs t))))

(defun parse-distance-p2 (string)
  (parse-number:parse-number (apply #'concatenate
                                    'string
                                    (split-sequence:split-sequence #\Space (subseq string 10) :remove-empty-subseqs t))))

(defun parse-input (input)
  (map 'list (lambda (x y) (list :time x :distance y)) (parse-time (first input)) (parse-distance (second input))))

(defun parse-input-p2 (input)
  (list :time (parse-time-p2 (first input))
        :distance (parse-distance-p2 (second input))))

; t = max time; p = time pressed; d = distance
; d = p * (t - p)
; d = p² - pt
; p² - pt - d = 0

(defun bhaskara (b c)
  (let ((delta (- (* b b) (* 4.0 c))))
    (unless (< delta 0)
      (list (/ (- b (sqrt delta)) 2.0)
            (/ (+ b (sqrt delta)) 2.0)))))

(defun get-number-of-ways (time distance)
  (let* ((times (bhaskara (float time) (float distance)))
         (min-time (ceiling (first times)))
         (max-time (floor (second times))))
    (if (and (= min-time (first times)) (= max-time (second times)))
        (- max-time min-time 1)
        (1+ (- max-time min-time)))))

(defun part1 (input)
  (let ((races (parse-input (uiop:read-file-lines input))))
    (apply #'* (map 'list (lambda (x) (get-number-of-ways (getf x :time) (getf x :distance))) races))))


(defun part2 (input)
  (let ((race (parse-input-p2 (uiop:read-file-lines input))))
    (get-number-of-ways (getf race :time) (getf race :distance))))
