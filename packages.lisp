;;; -*- Mode: LISP; Syntax: ANSI-COMMON-LISP; Base: 10 -*-
;;;     Last Updated : 2011/12/22 19:50:25 tkych

;;--------------------------------------------------------------------
;; Packages
;;--------------------------------------------------------------------
(in-package :cl-user)

(defpackage #:cl-sudoku
  (:nicknames :sudoku)
  (:use :cl :cl-ppcre)
  (:export :test
           :parse-grid
           :grid-values
           :display
           :solve
           :solve-all
           :from-file
           :random-puzzle
           :make-puzzle
           :solve-it
           ))


;;--------------------------------------------------------------------
