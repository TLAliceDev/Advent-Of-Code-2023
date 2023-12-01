(defmacro while (condition &body body)
  `(do ()
       (,condition)
      ,@body))

(defun part1 ()
  (apply #'+
    (mapcar (lambda (s) 
      (let ((digits (remove-if (lambda (x) (or (char> x #\9) (char< x #\0))) s)))
        (+ (* (digit-char-p (char digits 0)) 10) (digit-char-p (char digits (1-(length digits)))))))
  (uiop:read-file-lines "input"))))

(defun parse-on (substr)
  (unless (< (length substr) 1)
    (case (char substr 0)
      (#\e 1)
      (otherwise nil))))

(defun parse-o (substr)
  (unless (< (length substr) 1)
    (case (char substr 0)
      (#\n (parse-on (subseq substr 1)))
      (otherwise nil))))

(defun parse-tw (substr)
  (unless (< (length substr) 1)
    (case (char substr 0)
      (#\o 2)
      (otherwise nil))))

(defun parse-th (substr)
  (unless (< (length substr) 3)
    (cond 
      ((string= (subseq substr 0 3) "ree") 3)
      (t nil))))

(defun parse-t (substr)
  (unless (< (length substr) 1)
    (case (char substr 0)
      (#\w (parse-tw (subseq substr 1)))
      (#\h (parse-th (subseq substr 1)))
      (otherwise nil))))

(defun parse-fiv (substr)
  (unless (< (length substr) 1)
    (case (char substr 0)
      (#\e 5)
      (otherwise nil))))

(defun parse-fi (substr)
  (unless (< (length substr) 2)
    (cond 
      ((string= (subseq substr 0 2) "ve") 5)
      (t nil))))

(defun parse-fo (substr)
  (unless (< (length substr) 2)
    (cond 
      ((string= (subseq substr 0 2) "ur") 4)
      (t nil))))

(defun parse-f (substr)
  (unless (< (length substr) 1)
    (case (char substr 0)
      (#\i (parse-fi (subseq substr 1)))
      (#\o (parse-fo (subseq substr 1)))
      (otherwise nil))))

(defun parse-si (substr)
  (unless (< (length substr) 1)
    (case (char substr 0)
      (#\x 6)
      (otherwise nil))))

(defun parse-se (substr)
  (unless (< (length substr) 1)
    (cond 
      ((string= (subseq substr 0 3) "ven") 7)
      (t nil))))

(defun parse-s (substr)
  (unless (< (length substr) 1)
    (case (char substr 0)
      (#\i (parse-si (subseq substr 1)))
      (#\e (parse-se (subseq substr 1)))
      (otherwise nil))))

(defun parse-e (substr)
  (unless (< (length substr) 4)
    (cond 
      ((string= (subseq substr 0 4) "ight") 8)
      (t nil))))

(defun parse-n (substr)
  (unless (< (length substr) 3)
    (cond 
      ((string= (subseq substr 0 3) "ine") 9)
      (t nil))))

(defun parse-digit (substr)
  (unless (< (length substr) 1)
    (case (char substr 0)
      (#\0 0)
      (#\1 1)
      (#\2 2)
      (#\3 3)
      (#\4 4)
      (#\5 5)
      (#\6 6)
      (#\7 7)
      (#\8 8)
      (#\9 9)
      (#\o (parse-o (subseq substr 1)))
      (#\t (parse-t (subseq substr 1)))
      (#\f (parse-f (subseq substr 1)))
      (#\s (parse-s (subseq substr 1)))
      (#\e (parse-e (subseq substr 1)))
      (#\n (parse-n (subseq substr 1))))))

(defun parse-word (word)
  (let ((digits (make-array 0 :adjustable t :fill-pointer t)))
    (dotimes (i (length word))
      (let ((digit (parse-digit (subseq word i))))
        (when digit (vector-push-extend digit digits))))
  digits))


(defun part2 ()
  (apply #'+
    (mapcar (lambda (s)
      (let ((digits (parse-word s)))
        (+ (* 10 (aref digits 0)) (aref digits (1-(length digits))))))
  (uiop:read-file-lines "input"))))
