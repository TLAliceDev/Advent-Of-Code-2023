(ql:quickload :parse-number)
(ql:quickload :split-sequence)
(ql:quickload :alexandria)

(defun puzzle-symbol-p (c)
  (and (not (digit-char-p c)) (char/= c #\.)))

(defun at-schematic-pos (schematic x y)
  (char (nth y schematic) x))

(defun adjacent-to-symbol-p (schematic x y length)
  (let ((x0 (max (1- x) 0))
        (y0 (max (1- y) 0))  
        (x1 (min (+ 1 length x) (length (first schematic))))  
        (y1 (min (+ 2 y) (length schematic)))
        (adjacent nil))        
    (dotimes (yi (- y1 y0) adjacent)
      (dotimes (xi (- x1 x0))
        (when (puzzle-symbol-p (at-schematic-pos schematic (+ x0 xi) (+ y0 yi)))
              (setf adjacent t))))))

(defun split-numbers-from-line (line)
  (split-sequence:split-sequence-if (lambda (x) (not (digit-char-p x)))
                                      line
                                      :remove-empty-subseqs t))

(defun extract-number-info (number line line-number min-number-row)
  (list (search number line :start2 min-number-row) line-number (length number)))

(defun extract-numbers-from-line (line line-number)
  (let ((numbers (split-numbers-from-line line))
        (min-number-row 0))
    (mapcar (lambda (x) 
              (let ((info (extract-number-info x line line-number min-number-row)))
                (setf min-number-row (+ (first info) (third info)))
                info))
            numbers)))

(defun extract-numbers (schematic)
  (apply #'append
    (remove-if #'null
               (mapcar #'extract-numbers-from-line
                       schematic
                       (alexandria:iota (length schematic))))))

(defun number-adjacent-p (number)
  (apply #'adjacent-to-symbol-p number))

(defun parse-number-from-schematic (schematic number)
  (parse-number:parse-number (subseq (nth (second number) schematic)
                                     (first number)
                                     (+ (first number) (third number)))))

(defun get-adjacent-numbers (schematic)
  (remove-if-not (lambda (x) (apply #'adjacent-to-symbol-p schematic x))
                 (extract-numbers schematic)))

(defun part1 (input)
  (let ((schematic (uiop:read-file-lines input)))
    (apply #'+
           (mapcar (lambda (x) (parse-number-from-schematic schematic x))
                   (get-adjacent-numbers schematic)))))

(defun extract-gears-from-line (line line-number min-row current-list)
  (let ((gear-row (position #\* line :start min-row)))
    (if gear-row
      (extract-gears-from-line line
                               line-number
                               (1+ gear-row)
                               (push (vector gear-row line-number) current-list))
      current-list)))

(defun extract-gears (schematic)
  (apply #'append
    (remove-if #'null
               (mapcar (lambda (x i) (extract-gears-from-line x i 0 '()))
                       schematic
                       (alexandria:iota (length schematic))))))

(defun point-in-number-range (point number)
  (let ((x0 (1- (first number)))
        (y0 (1- (second number)))
        (x1 (+ (third number) (first number)))
        (y1 (1+ (second number))))
    (and (>= (aref point 0) x0)
         (<= (aref point 0) x1)
         (>= (aref point 1) y0)
         (<= (aref point 1) y1))))

(defun numbers-adjacent-to-gear (gear numbers)
  (remove-if-not (lambda (x) (point-in-number-range gear x)) numbers))
  
(defun get-proper-gears (adjacents)
  (remove-if-not (lambda (x) (= 2 (list-length x))) adjacents))

(defun gear-ratios (gears schematic)
  (mapcar (lambda (x) (* (parse-number-from-schematic schematic (first x)) (parse-number-from-schematic schematic (second x)))) gears))

(defun part2 (input)
  (let ((schematic (uiop:read-file-lines input)))
    (let ((gears (extract-gears schematic))
          (numbers (extract-numbers schematic)))
      (apply #'+ (gear-ratios (get-proper-gears (mapcar (lambda (x) (numbers-adjacent-to-gear x numbers)) gears)) schematic)))))
