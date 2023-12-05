(ql:quickload :split-sequence)
(ql:quickload :parse-number)
(ql:quickload :alexandria)

(defun parse-seeds (seeds)
  (let ((split (split-sequence:split-sequence #\Space (subseq seeds 7))))
    (map 'vector #'parse-number:parse-number split)))

(defun parse-range (range)
  (let* ((split (split-sequence:split-sequence #\Space range))
         (range (1- (parse-number:parse-number (third split))))
         (source-min (parse-number:parse-number (second split)))
         (destination-min (parse-number:parse-number (first split))))
    (list :source-min source-min
          :source-max (+ (parse-number:parse-number (second split)) range)
          :offset (- destination-min source-min))))

(defun mapping-subseq (input mapping)
  (let ((start (subseq input
                       (1+ (search (list (concatenate 'string mapping " map:"))
                                   input
                                   :test #'string=)))))
    (subseq start 0 (search (list "") start :test #'string=))))

(defun parse-mapping (input mapping)
  (let ((ranges (mapping-subseq input mapping)))
    (map 'vector #'parse-range ranges)))

(defun parse-input (input)
  (list :seeds (parse-seeds (first input))
        :seed->soil (parse-mapping input "seed-to-soil")
        :soil->fertilizer (parse-mapping input "soil-to-fertilizer")
        :fertilizer->water (parse-mapping input "fertilizer-to-water")
        :water->light (parse-mapping input "water-to-light")
        :light->temperature (parse-mapping input "light-to-temperature")
        :temperature->humidity (parse-mapping input "temperature-to-humidity")
        :humidity->location (parse-mapping input "humidity-to-location")))

(defun in-range (value min max)
  (and (>= value min) (<= value max)))

(defun source->destination (almanac source mapping)
  (let ((destination source)
        (max-i (length (getf almanac mapping)))
        (found-range nil))
    (do ((i 0 (1+ i)))
        ((or (>= i max-i) found-range) destination)
        (let ((range (aref (getf almanac mapping) i)))
          (when (in-range source (getf range :source-min) (getf range :source-max))
            (setf destination (+ source (getf range :offset)))
            (setf found-range t))))))

(defun seed->soil (almanac seed)
  (source->destination almanac seed :seed->soil))

(defun soil->fertilizer (almanac soil)
  (source->destination almanac soil :soil->fertilizer))

(defun fertilizer->water (almanac fertilizer)
  (source->destination almanac fertilizer :fertilizer->water))

(defun water->light (almanac water)
  (source->destination almanac water :water->light))

(defun light->temperature (almanac light)
  (source->destination almanac light :light->temperature))

(defun temperature->humidity (almanac temperature)
  (source->destination almanac temperature :temperature->humidity))

(defun humidity->location (almanac humidity)
  (source->destination almanac humidity :humidity->location))

(defun seed->location (almanac seed)
  (humidity->location almanac
    (temperature->humidity almanac
      (light->temperature almanac
        (water->light almanac
          (fertilizer->water almanac
            (soil->fertilizer almanac
              (seed->soil almanac seed))))))))

(defun part1 (input)
  (let ((almanac (parse-input (uiop:read-file-lines input))))
    (apply #'min (map 'list (lambda (x) (seed->location almanac x)) (getf almanac :seeds)))))

(defun range-intersection (x1 x2 y1 y2)
  (let ((start (max x1 y1))
        (end (min x2 y2)))
    (if (> start end)
        nil
        (list start end))))

(defun range-split (x1 x2 min max)
  (remove-if #'null 
    (list (range-intersection x1 x2 0 (1- min))
          (range-intersection x1 x2 min max)
          (range-intersection x1 x2 (1+ max) x2))))

(defun seeds->range (seeds)
  (let ((seed-ranges '()))
    (dotimes (i (/ (length seeds) 2) seed-ranges)
      (push (list (aref seeds (* 2 i)) (+ (aref seeds (* 2 i)) (aref seeds (+ 1 (* 2 i))))) seed-ranges))))

(defun source->destination-range (almanac source mapping)
  (if (integerp (first source))
    (let ((destination source)
          (max-i (length (getf almanac mapping)))
          (found-range nil))
      (do ((i 0 (1+ i)))
          ((or (>= i max-i) found-range)
           destination)
        (let* ((range (aref (getf almanac mapping) i))
               (intersection (range-intersection (first source) (second source) (getf range :source-min) (getf range :source-max))))
          (when intersection
            (setf found-range t)
            (if (and (= (first source) (first intersection)) (= (second source) (second intersection)))
                (setf destination (list (+ (first source) (getf range :offset)) (+ (second source) (getf range :offset))))
                (setf destination
                  (mapcar (lambda (x) (source->destination-range almanac x mapping)) (range-split (first source) (second source) (getf range :source-min) (getf range :source-max)))))))))
    (mapcar (lambda (x) (source->destination-range almanac x mapping)) source)))

(defun seed->soil-range (almanac seed)
  (source->destination-range almanac seed :seed->soil))

(defun soil->fertilizer-range  (almanac soil)
  (source->destination-range almanac soil :soil->fertilizer))

(defun fertilizer->water-range  (almanac fertilizer)
  (source->destination-range almanac fertilizer :fertilizer->water))

(defun water->light-range  (almanac water)
  (source->destination-range almanac water :water->light))

(defun light->temperature-range  (almanac light)
  (source->destination-range almanac light :light->temperature))

(defun temperature->humidity-range  (almanac temperature)
  (source->destination-range almanac temperature :temperature->humidity))

(defun humidity->location-range  (almanac humidity)
  (source->destination-range almanac humidity :humidity->location))

(defun seed->location-range (almanac seed)
  (humidity->location-range almanac
    (temperature->humidity-range almanac
      (light->temperature-range almanac
        (water->light-range almanac
          (fertilizer->water-range almanac
            (soil->fertilizer-range almanac
              (seed->soil-range almanac seed))))))))

(defun part2 (input)
  (let ((almanac (parse-input (uiop:read-file-lines input))))
    (apply #'min (alexandria:flatten (mapcar (lambda (x) (seed->location-range almanac x)) (seeds->range (getf almanac :seeds)))))))
