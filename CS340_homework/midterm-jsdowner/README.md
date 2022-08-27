# CS 340 Spring 2021 Midterm Exam (Take-home)

## Instructions

To work on and submit your solutions to this exam, you must claim your private
repository containing the starter code using this invitation link:
<https://classroom.github.com/a/s2Qcc2Gq>.

After cloning your repository, open the source file “src/Midterm.hs” to get
started. This will be the only source file you need to modify.

Before continuing, read and sign the honor pledge at the top of the source file
by entering your name and AID as the (String) values for the `student_name` and
`student_id` variables. While this exam is open-book/notes/IDE, it is strictly
an *individual* undertaking. Please don’t discuss the exam with anyone until
after the due date & time. We will run plagiarism detection software on all
submissions, and will strictly enforce the [IIT code of academic
honesty](https://web.iit.edu/student-affairs/handbook/fine-print/code-academic-honesty).

Each of the problems below requires that you provide a working implementation for one or more functions in “src/Midterm.hs”.

You must push your solutions to GitHub by **Saturday, March 13, 2021, 12PM Central Time**. 

Have fun!

## Part A: Polymorphic functions

For functions `pa_1`, `pa_2`, `pa_3`, `pa_4`, and `pa_5`, we include polymorphic
type declarations but no working definitions. You are to provide a working,
well-typed definition for each function that returns without error for valid
inputs. 

## Part B: Recursion and List processing

In this part you will provide implementations for a number of functions, each of
whose declaration in the source file is prefaced by a comment containing a
specification and sample calls/results. 

You may use any of the language constructs (pattern matching, `where` clauses,
guards, `let-in`/`if-else`/`case` expressions, lambdas, list comprehensions,
etc.). You may not, however, use any pre-defined functions other than those in
the following table:

| Category   | Functions                                |
| :--------- | :--------------------------------------- |
| Arithmetic | +, –, *, /, ^, div, mod                  |
| Relational | <, <=, >, >=, ==, /=                     |
| Boolean    | not, &&, \|\|                            |
| Character  | ord, chr, isLetter, toUpper              |
| Lists      | :, ++, !!, length, take, drop, replicate |
| Misc       | $, ., map, filter, succ                  |

The functions you will implement are listed below, for reference:

1. `listsFrom :: Enum a => a -> [[a]]`

2. `riffle :: Int -> [a] -> [a] -> [a]`
  
3. `ngrams :: Int -> [a] -> [[a]]`
   
4. `autokeyEncrypt :: Char -> String -> String`
   
5. `autokeyDecrypt :: Char -> String -> String`
   
6. `ttt_empty :: [[Int]]`
   
7. `ttt_place :: [[Int]] -> Int -> (Int,Int) -> [[Int]]`
   
8. `ttt_play :: [[Int]] -> Int -> [(Int,Int)] -> [[Int]]`
   
9.  `ttt_win :: [[Int]] -> Int -> Bool`
    
10. `ttt_winningMoves :: [[Int]] -> Int -> [(Int,Int)]`


## Testing and Scoring

We include tests for all the problems in this exam, but the tests ***are not
comprehensive***! They are included to help ensure you don’t alter the
names/types of functions, and to test for very basic functionality. Passing the
test(s) for a given problem does *not* guarantee you will receive full credit
for it.

You can run the test suite using the command `stack test`. Feel free to add your
own test cases to “test/MidtermSpec.hs” (we will replace them with our own when
grading).

Solutions will be scored based on functionality first, followed by a code review
to make sure you follow all rules specified in the problem descriptions. If your
solution fails to compile, it will earn 0 points; if it fails to pass a test,
there will be no partial credit for that test. If your solution passes all tests
but fails to follow the rules, deductions will be made as we see fit.

All functions are worth 4 points each, for a maximum of $15 \times 4 = 60$ points

## Submission

Submit your work by committing all your changes and pushing them to GitHub. The
command `git commit -am “Submitting midterm” ; git push` should do this.

After submitting, confirm that your changes to “src/Midterm.hs” have been
correctly pushed to GitHub by checking your repository website (its URL should
look like <https://github.com/cs340ppp/midterm-USERNAME>, where USERNAME is your GitHub
username).

You must push your solutions to GitHub by **Saturday, March 13, 2021, 12PM
Central Time**.
