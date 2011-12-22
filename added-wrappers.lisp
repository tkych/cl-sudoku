;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-
;;;     Last Updated : 2011/12/22 19:52:52 tkych

(in-package :cl-sudoku)

;;--------------------------------------------------------------------
;; Wrappers for interface added by tkych
;;--------------------------------------------------------------------
;;; added by tkych
(defun make-puzzle (&optional (n 17))
  (let ((grid (random-puzzle n)))
    (if (solved? (solve grid))
        (with-output-to-string (*standard-output*)
          (display (grid-values grid)))
        (make-puzzle n))))

;;; added by tkych
(defun solve-it (&optional (grid *))
  (let* ((start    (get-internal-run-time))
         (values   (solve grid))
         (run-time (/ (- (get-internal-run-time) start)
                      internal-time-units-per-second
                      1.0)))
    (if (null values)
        (format t "Fail")
        (progn
          (display values)
          (format t "~%(~,2F seconds)~%" run-time)))))

;;--------------------------------------------------------------------