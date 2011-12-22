;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-
;;;     Last Updated : 2011/12/22 19:52:19 tkych

;;====================================================================
;; Solve Every Sudoku Puzzle
;;====================================================================
;; This is Peter Norvig's sudoku solver, translated into common lisp
;; by Takaya Ochiai (tkych.repl@gmail.com).
;; Norvig's original code is written in Python.

;; Original code:
;;   http://norvig.com/sudoku.py

;; More details:
;;   http://norvig.com/sudoku.html

;;   Japanese translation by Yasushi Aoki
;;   http://www.aoky.net/articles/peter_norvig/sudoku.htm

;;   Korean translation by JongMan Koo
;;   ttps://github.com/jongman/articles/wiki/solving-every-sudoku-puzzle


;; Throughout this program we have:
;;   r is a row,     e.g. "A"
;;   c is a column,  e.g. "3"
;;   s is a square,  e.g. "A3"
;;   d is a digit,   e.g. #\9
;;   u is a unit,    e.g. ("A1" "B1" "C1" "D1" "E1" "F1" "G1" "H1" "I1")
;;   grid is a grid, e.g. 81 non-blank chars e.g. starting with ".18...7...
;;   values is a hash-table of possible values, e.g. {"A1":"12349" "A2":"8" ...}
;;   values[s] means (gethash s values)


(in-package :cl-sudoku)

;;--------------------------------------------------------------------
;; Special Variables
;;--------------------------------------------------------------------
(defun cross (string1 string2)
"Cross product of chars in string1 and chars in string2."
  (loop for char1 across string1
     nconc (loop for char2 across string2
              collect (format nil "~A~A" char1 char2))))

(defparameter *digits* "123456789")
(defparameter *rows*   "ABCDEFGHI")
(defparameter *cols*   *digits*)

(defparameter *squares* (cross *rows* *cols*))

(defparameter *unit-list*
  (nconc (loop for col across *cols*
            collect (cross *rows* (string col)))
         (loop for row across *rows*
            collect (cross (string row) *cols*))
         (loop for rs in '("ABC" "DEF" "GHI")
            nconc (loop for cs in '("123" "456" "789")
                     collect (cross rs cs)))))

(defparameter *units*
  (let ((ht (make-hash-table :size 81 :test #'equal)))
    (dolist (s *squares* ht)
      (push-hash s (loop for u in *unit-list*
                      when (member s u :test #'string=) collect u)
                 ht))))

(defparameter *peers*
  (let ((ht (make-hash-table :size 81 :test #'equal)))
    (dolist (s *squares* ht)
      (push-hash s (delete s (delete-w (flatten (gethash s *units*))
                                       :test #'equal)
                           :test #'equal)
                 ht))))

;;--------------------------------------------------------------------
;; Unit Tests
;;--------------------------------------------------------------------
(defun test ()
"A set of tests that must pass."
  (assert (= (length *squares*) 81))
  (assert (= (length *unit-list*) 27))
  (dolist (s *squares*)
    (assert (= (length (gethash s *units*)) 3)))
  (dolist (s *squares*)
    (assert (= (length (gethash s *peers*)) 20)))
  (assert (equal (gethash "C2" *units*)
                 '(("A2" "B2" "C2" "D2" "E2" "F2" "G2" "H2" "I2")
                   ("C1" "C2" "C3" "C4" "C5" "C6" "C7" "C8" "C9")
                   ("A1" "A2" "A3" "B1" "B2" "B3" "C1" "C2" "C3"))))
  (assert (set-equal (gethash "C2" *peers*)
                     '("A2" "B2" "D2" "E2" "F2" "G2" "H2" "I2"
                       "C1" "C3" "C4" "C5" "C6" "C7" "C8" "C9"
                       "A1" "A3" "B1" "B3")))
  (format t "All test pass."))

;;--------------------------------------------------------------------
;; Parse a Grid
;;--------------------------------------------------------------------
(defun parse-grid (grid)
"Convert grid to a hash-table of possible values, {square: digits}, or
return NIL if a contradiction is detected."
  ;; To start, every square can be any digit;
  ;; then assign values from the grid.
  (let ((values (make-hash-table :size 81 :test #'equal)))
    (dolist (s *squares*)
      (push-hash s *digits* values))
    (dohash (s d (grid-values grid))
      (when (and (find d *digits*)
                 (not (assign values s d)))
        ;; Fail if we can't assign d to square s.
        (return-from parse-grid nil)))
    values))

(defun grid-values (grid)
"Convert grid into a hash-table of {square: char} with #\0 or #\.
for empties."
  (let ((ht (make-hash-table :size 81 :test #'equal))
        (chars (find-all-if (^ (c) (or (digit-char-p c) (char= #\. c)))
                            grid)))
    (assert (= (length chars) 81))
    (loop for s in *squares*
          for c across chars
       do (push-hash s c ht))
    ht))

;;--------------------------------------------------------------------
;; Constraint Propagation
;;--------------------------------------------------------------------
(defun assign (values s d)
"Eliminate all the other values (except d) from values[s] and propagate.
Return values, except return NIL if a contradiction is detected."
  (let ((other-values (remove d (gethash s values))))
    (if (every (^ (d2) (eliminate values s d2))
               other-values)
        values
        nil)))

(defun eliminate (values s d)
"Eliminate d from values[s]; propagate when values or places <= 2.
Return values, except return NIL if a contradiction is detected."
  (when (not (find d (gethash s values)))
    (return-from eliminate values))     ; Already eliminated
  (let ((vals (remove d (gethash s values))))
    (push-hash s vals values)
    ;; (1) If a square s is reduced to one value d2,
    ;;     then eliminate d2 from the peers.
    (case (length vals)
      (0 ;; Contradiction: removed last value
         (return-from eliminate nil))
      (1 (let ((d2 (char vals 0)))
           (when (not (every (^ (s2) (eliminate values s2 d2))
                             (gethash s *peers*)))
             (return-from eliminate nil))))
      (t nil))
    ;; (2) If a unit u is reduced to only one place for a value d, 
    ;;     then put it there.
    (dolist (u (gethash s *units*))
      (let ((dplaces (loop for s in u                    
                        when (find d (gethash s values)) collect s)))
        (case (length dplaces)
          (0 ;; Contradiction: no place for this value
             (return-from eliminate nil))
          (1 ;; d can only be in one place in unit; assign it there
             (when (not (assign values (car dplaces) d))
               (return-from eliminate nil)))
          (t nil))))
    values))

;;--------------------------------------------------------------------
;; Display as 2-D grid
;;--------------------------------------------------------------------
(defun display (values)
"Display these values as a 2-D grid."
  (when values
    (let* ((width (1+ (if (stringp (gethash "A1" values))
                          (loop for s in *squares*
                             maximize (length (gethash s values)))
                          1)))
           (val   (format nil "~~~D:<~~A~~>" width))
           (line  (format nil (format nil "~% ~~{~~,,~D,'-@A~~}"
                                      (* 3 width))
                          '("+" "-+" ""))))
      (~%)
      (dostr (r *rows*)
        (dostr (c *cols*)
          (format t val (gethash (format nil "~A~A" r c) values))
          (when (find c "36") (princ " |")))
        (when (find r "CF") (princ line))
        (~%)))))

;;--------------------------------------------------------------------
;; Search
;;--------------------------------------------------------------------
(defun solve (grid)
  (df-search (parse-grid grid)))

(defun df-search (values)
"Using depth-first search and propagation, try all possible values."
  (cond ((null values) nil)             ; Failed earlier
        ((every (^ (s) (= 1 (length (gethash s values))))
                *squares*)
         values)                        ; Solved!
        (t 
         ;; Chose the unfilled square s with the fewest possibilities
         (let ((s (let ((min-l 11) (min-s nil))
                    (dolist (s1 *squares* min-s)
                      (when (< 1 (length (gethash s1 values)) min-l)
                        (setf min-s s1
                              min-l (length (gethash s1 values))))))))
           (some (^ (d) (df-search (assign (copy-hash values) s d)))
                 (gethash s values))))))

;;--------------------------------------------------------------------
;; Utilities
;;--------------------------------------------------------------------
#+cl-ppcre
(defun from-file (file-name &optional (sep #\Newline))
"Parse a file into a list of strings, separated by sep (string or char)."
   (let ((delim (if (stringp sep) sep (string sep))))
     (delete "" (ppcre:split delim (file-to-string file-name))
             :test #'equal)))

(defun shuffled (lst)
"Return a randomly shuffled copy of the input list."
  (let ((vec (make-array (length lst) :initial-contents lst)))
    (coerce (nshuffle-vector vec) 'list)))

;;--------------------------------------------------------------------
;; System test
;;--------------------------------------------------------------------
(defun solve-all (grids &optional (name "") (show-if 0.0))
"Attempt to solve a sequence of grids. Report results.
When show-if is a number of seconds, display puzzles that take longer.
When show-if is NIL, don't display any puzzles."
  (flet ((time-solve (grid)
           (let* ((start    (get-internal-run-time))
                  (values   (solve grid))
                  (run-time (/ (- (get-internal-run-time) start)
                               internal-time-units-per-second
                               1.0)))
             ;; Display puzzles that take long enough
             (when (and show-if
                        (< show-if run-time))
               (display (grid-values grid))
               (when values
                 (display values)
                 (format t "~%(~,2F seconds)~%" run-time)))
             (cons run-time (solved? values)))))
    (let ((n (length grids)))
      (when (< 1 n)
        (let* ((times-solves (loop for g in grids
                                collect (time-solve g)))
               (times        (mapcar #'car times-solves))
               (results      (mapcar #'cdr times-solves))
               (total-time   (reduce #'+ times))
               (success      (count t results)))
          (format t "~%Solved~,,1@A of~,,1@A ~A puzzles ~
                     (avg ~,2F secs (~,2F Hz), max ~,2F secs)."
                  success n name (/ total-time n) (/ n total-time)
                  (apply #'max times)))))))

(defun solved? (values)
"A puzzle is solved if each unit is a permutation of the digits 1 to 9."
  (let ((digits (loop for d across *digits* collect (string d))))
    (flet ((unit-solved? (unit)
             (set-equal (loop for s in unit collect (gethash s values))
                        digits)))
      (and (not (null values))
           (every (^ (u) (unit-solved? u)) *unit-list*)))))

(defun random-puzzle (&optional (n 17))
"Make a random puzzle with N or more assignments. 
Restart on contradictions.
Note the resulting puzzle is not guaranteed to be solvable, 
but empirically about 99.8% of them are solvable. 
Some have multiple solutions."
  (let ((values (make-hash-table :size 81 :test #'equal)))
    (LOOP
       (dolist (s *squares*)
         (push-hash s *digits* values))
       (dolist (s (shuffled *squares*))
         (when (not (assign values s (random-char (gethash s values))))
           (return))
         (let ((ds (loop for s in *squares*
                      when (= 1 (length (gethash s values)))
                      collect (gethash s values))))
           (when (and (<= n (length ds))
                      (<= 8 (length (delete-w ds))))
             (RETURN-FROM random-puzzle
               (let ((acc nil))
                 (dolist (s *squares*
                          (apply #'string-append (nreverse acc)))
                   (if (= 1 (length (gethash s values)))
                       (push (gethash s values) acc)
                       (push "." acc)))))))))))

;;====================================================================
