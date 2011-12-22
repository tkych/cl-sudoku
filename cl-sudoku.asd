;;; -*- Mode: LISP; Syntax: ANSI-COMMON-LISP; Base: 10 -*-
;;;     Last Updated : 2011/12/22 19:49:34 tkych

;;--------------------------------------------------------------------
;; System
;;--------------------------------------------------------------------

(in-package :cl-user)
(defpackage #:cl-sudoku-asd (:use :cl :asdf))
(in-package #:cl-sudoku-asd)

(defsystem :cl-sudoku
  :name    "cl-sudoku"
  :description "CL translation of Peter Norvig's sudoku solver."
  :author "Takaya Ochiai (translator) <tkych.repl@gmail.com>"
  :version "1.0.0"
  :depends-on (:cl-ppcre)
  :serial t
  :components ((:file "packages")
               (:file "cl-utilities")
               (:file "sudoku")
               (:file "added-wrappers")
               ))

;;--------------------------------------------------------------------