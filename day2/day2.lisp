(ql:quickload :split-sequence)
(ql:quickload :parse-number)

(defun parse-reveal (reveal)
  (let ((cubes (split-sequence:split-sequence #\, reveal :remove-empty-subseqs t))
        (fcubes (list 0 0 0)))
    (dolist (cube cubes fcubes)
      (let ((n (parse-number:parse-number (subseq cube 0 (1+ (position #\Space (subseq cube 1) :test #'equal)))))
            (colour (subseq cube (+ 2 (position #\Space (subseq cube 1) :test #'equal)))))
        (cond
          ((string= colour "red") (setf (first fcubes) n))
          ((string= colour "green") (setf (second fcubes) n))
          ((string= colour "blue") (setf (third fcubes) n)))))))

(defun parse-game (game)
  (let ((id (parse-number:parse-number (subseq game 5 (position #\: game :test #'equal))))
        (reveals (split-sequence:split-sequence #\; (subseq game (1+ (position #\: game :test #'equal))) :remove-empty-subseqs t)))
    (list id (mapcar #'parse-reveal reveals))))

(defun get-max-cubes (game)
  (let* ((parsed (parse-game game))
         (id (first parsed))
         (cubes (second parsed)))
    (list id (apply #'mapcar (lambda (&rest rest) (apply #'max rest)) cubes))))


(defun is-possible? (game)
  (let ((cubes (second game)))
    (if (and (<= (first cubes) 12)
             (<= (second cubes) 13)
             (<= (third cubes) 14))
      (first game)
      0)))

(defun cubes-power (cubes)
  (apply #'* (second cubes)))

(defun part1 ()
  (reduce #'+ (mapcar #'is-possible? (mapcar #'get-max-cubes (uiop:read-file-lines "input.txt")))))

(defun part2 ()
  (reduce #'+ (mapcar #'cubes-power (mapcar #'get-max-cubes (uiop:read-file-lines "input.txt")))))
