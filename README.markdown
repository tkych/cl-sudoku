Last Updated : 2011/12/22 19:48:15 tkych


CL-SUDOKU: Norvig's Sudoku Solver in Common Lisp
======================================================================

Sudoku is a puzzle, we put a number from 1 to 9 so as to satisfy the rules in the square of the 9 × 9.
Cl-sudoku is Common Lisp translation of Peter Norvig's sudoku solver, by Takaya Ochiai.
Norvig's [original code][] is written in Python.

The main algorithm used in this program is a constraint-propagation and depth-first-search.
The solution procedure is the following.
First, consider the placement of the number allowed in each square.
The square, all may be placed from one to nine digits.
Then, one by one we put a number in the square already know the numbers in question.
As it will be reducing the number of possible placement in the other square (constraint-propagation).
Finally, if there is a possibility of a few numbers in the square, look for consistent placement under each of these possibilities (depth-first-search).

For more details, please see his essay:

* [Solving Every Sudoku Puzzle][] by Peter Norvig

This essay has been translated into: 

* [Japanese][] by Yasushi Aoki
* [Korean][] by JongMan Koo


  [original code]: http://norvig.com/sudoku.py
  [Solving Every Sudoku Puzzle]: http://norvig.com/sudoku.html
  [Japanese]: http://www.aoky.net/articles/peter_norvig/sudoku.htm
  [Korean]: ttps://github.com/jongman/articles/wiki/solving-every-sudoku-puzzle


----------------------------------------------------------------------
 Dependency
----------------------------------------------------------------------

* [cl-ppcre](http://weitz.de/cl-ppcre/) by Edmund Weitz


----------------------------------------------------------------------
 Installation
----------------------------------------------------------------------

If your REPL has been loaded [asdf][] or [quicklisp][], type on the REPL,

1.  CL-REPL> `(push path-to-cl-sudoku-directory/cl-sudoku/ asdf:*central-registry*)`
2.  CL-REPL> `(asdf:operate 'asdf:load-op :cl-sudoku)` or `(ql:quickload :cl-sudoku)`


 [quicklisp]: http://www.quicklisp.org/
 [asdf]: http://common-lisp.net/project/asdf/


----------------------------------------------------------------------
 Examples
----------------------------------------------------------------------

      CL-REPL> (sudoku:make-puzzle)
      "
       . . . | . . 7 | . 8 . 
       . 1 . | . 4 . | . 9 6 
       . . 2 | . . . | 4 3 . 
       ------+-------+------
       . . . | 2 . . | . . . 
       . . 1 | . . . | . 5 . 
       . . . | . . . | . . . 
       ------+-------+------
       . . . | 8 . . | . . . 
       . . . | . . . | . . . 
       3 . . | 7 . 6 | . . 5 
      "
      CL-REPL> (sudoku:solve-it)

       4 3 9 | 5 6 7 | 2 8 1 
       7 1 8 | 3 4 2 | 5 9 6 
       5 6 2 | 1 8 9 | 4 3 7 
       ------+-------+------
       9 5 3 | 2 7 1 | 6 4 8 
       8 2 1 | 6 3 4 | 7 5 9 
       6 4 7 | 9 5 8 | 1 2 3 
       ------+-------+------
       2 9 6 | 8 1 5 | 3 7 4 
       1 7 5 | 4 9 3 | 8 6 2 
       3 8 4 | 7 2 6 | 9 1 5 

      (0.01 seconds)
      NIL
      CL-REPL> (sudoku:solve-all
                 (loop repeat 1000 collect (sudoku:random-puzzle))
                 "random" 100.0)

       1 . . | . 5 3 | . . 7 
       . . . | . . 6 | . 9 . 
       . . . | . . . | . . . 
       ------+-------+------
       . . . | . . 5 | . . . 
       . . . | . . . | . . . 
       . . . | . . . | . . . 
       ------+-------+------
       8 . . | . . 9 | . . . 
       9 . . | . . 2 | 7 1 . 
       4 . . | . 7 . | 2 . 9 

       1 8 9 | 2 5 3 | 4 6 7 
       5 7 4 | 8 1 6 | 3 9 2 
       2 3 6 | 4 9 7 | 1 8 5 
       ------+-------+------
       6 4 1 | 7 8 5 | 9 2 3 
       3 5 2 | 9 6 1 | 8 7 4 
       7 9 8 | 3 2 4 | 6 5 1 
       ------+-------+------
       8 2 7 | 1 3 9 | 5 4 6 
       9 6 3 | 5 4 2 | 7 1 8 
       4 1 5 | 6 7 8 | 2 3 9 

      (102.38 seconds)

      Solved 998 of 1000 random puzzles (avg 0.01 secs (78.03 Hz), max 102.38 secs).
      NIL
      CL-REPL> 
      (progn
        (sudoku:test) 
        (sudoku:solve-all (sudoku:from-file "easy50.txt" "=========") "easy" nil)
        (sudoku:solve-all (sudoku:from-file "top95.txt") "hard" nil)
        (sudoku:solve-all (sudoku:from-file "hardest.txt") "hardest" nil)
        (sudoku:solve-all (loop repeat 99 collect (sudoku:random-puzzle)) "random" 100.0)
        (format t  "~%~A~A, ~A" (lisp-implementation-type)
                                (lisp-implementation-version)
                                (machine-version))
      )

      All test pass.
      Solved 50 of 50 easy puzzles (avg 0.01 secs (101.63 Hz), max 0.02 secs).
      Solved 95 of 95 hard puzzles (avg 0.03 secs (31.05 Hz), max 0.16 secs).
      Solved 11 of 11 hardest puzzles (avg 0.01 secs (76.39 Hz), max 0.02 secs).
      Solved 99 of 99 random puzzles (avg 0.01 secs (93.75 Hz), max 0.02 secs).
      SBCL1.0.51, Intel(R) Pentium(R) M processor 1.73GHz
      NIL


----------------------------------------------------------------------
 Functions
----------------------------------------------------------------------

The usages of function name in sudoku.lisp, obey in Novig's original code, except the name of the function 'search'(-> df-search).
The following two functions in added-wrappers.lisp, added by translator for user convenience.

1.  __make-puzzle__ &optional n => grid(sudoku-puzzle-string)

    n---number of initial numbers, default is 17.

    The function make-puzzle makes sudoku puzzle at random, and outputs it in string.


2.  __solve-it__ &optional grid => NIL
    
    grid---sudoku-puzzle-string, default is *.

    The function solve-it solves sudoku puzzle, and displays solution & run time.


----------------------------------------------------------------------
 Python->CL Translator
----------------------------------------------------------------------

* Takaya Ochiai <tkych.repl@gmail.com>


----------------------------------------------------------------------
