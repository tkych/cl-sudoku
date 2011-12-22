;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-
;;;     Last Updated : 2011/12/22 19:51:27 tkych

(in-package :cl-sudoku)

;;--------------------------------------------------------------------
;; Common Lisp Utilities
;;--------------------------------------------------------------------
;;; ^ -> lambda
;;; !!! Don't use "^" in the car of the compound form. !!!
;;; !!! Ex. ((^ (x) (1+ x)) 3) => error                !!!
(defmacro ^ (lambdalist &rest body)
  `(lambda ,lambdalist ,@body))

(setf (symbol-function '~%) #'terpri)
(setf (symbol-function 'find-all-if) #'remove-if-not)
(setf (symbol-function 'delete-w) #'delete-duplicates)


(defun set-equal (set1 set2)
  (null (set-exclusive-or set1 set2 :test #'equal)))

(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

(defun string-append (&rest string)
  (apply #'concatenate 'string string))

(defmacro dostr ((char string &optional (result nil))
                 &body body)
  (let ((i (gensym)) (len (gensym)))
    `(let ((,len (length ,string)))
       (do ((,i 0 (1+ ,i)))
           ((= ,len ,i) ,result)
         (let ((,char (char ,string ,i)))
           ,@body)))))

(defmacro push-hash (key val ht)
  `(setf (gethash ,key ,ht) ,val))

(defmacro dohash ((key val ht &optional (result nil))
                  &body body)
  (let ((next (gensym)) (more? (gensym)))
    `(with-hash-table-iterator (,next ,ht)
       (loop (multiple-value-bind (,more? ,key ,val) (,next)
               (declare (ignorable ,key ,val))
               (unless ,more? (return ,result))
               ,@body)))))

(defun copy-hash (ht)
  (let ((ht1 (make-hash-table :test (hash-table-test ht)
                              :size (hash-table-size ht)
                              :rehash-size
                              (hash-table-rehash-size ht)
                              :rehash-threshold
                              (hash-table-rehash-threshold ht))))
    (dohash (k v ht ht1)
      (push-hash k v ht1))))

(defun file-to-string (file)
  (with-open-file (in file :direction :input)
    (let* ((f-len (file-length in))
           (str   (make-string f-len))
           (r-len (read-sequence str in)))
      (if (< r-len f-len)
          (subseq str 0 r-len)
          str))))

(defun random-char (string)
"Return a randomly chosen char of the input string."
  (let ((len (length string)))
    (char string (random len))))

(defun nshuffle-vector (vec)
  (do* ((i (1- (length vec)) (1- i))
        (j (random (1+ i)) (random (1+ i))))
       ((<= i 0) vec)
    (when (/= i j)
      (rotatef (aref vec i) (aref vec j)))))

;;--------------------------------------------------------------------