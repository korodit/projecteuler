(load "math.lisp")
(load "input-helpers.lisp")

(defmacro input-filename (num) (concatenate 'string "inputs/input" (write-to-string num) ".txt"))
(defmacro fetch-input-string (num) `(get-file-contents ,`(input-filename ,num)))

(defparameter problem_solutions  #(

'((math::sum-mults-of-args-up-to 1000 3 5)
"Problem 1
If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
Find the sum of all the multiples of 3 or 5 below 1000.")

'((math::even-fibonacci-sum 4000000)
"Problem 2
Each new term in the Fibonacci sequence is generated by adding the previous two terms. By starting with 1 and 2, the first 10 terms will be:
1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...
By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms.
")

'((math::largest-prime-factor 600851475143)
"Problem 3
The prime factors of 13195 are 5, 7, 13 and 29.
What is the largest prime factor of the number 600851475143 ?
")

'((math::max-palindrome-prod-of-2-n-digit-nums 3)
"Problem 4
A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
Find the largest palindrome made from the product of two 3-digit numbers.
")

'((math::smallest-divisible-by-all-in-range 20)
"Problem 5
2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
")

'((math::square-of-sum-sum-of-square-diff 100)
"Problem 6
The sum of the squares of the first ten natural numbers is,
1^2 + 2^2 + ... + 10^2 = 385

The square of the sum of the first ten natural numbers is,
(1 + 2 + ... + 10)^2 = 55^2 = 3025
Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025 − 385 = 2640.
Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
")

'((math::nth-prime 10001)
"Problem 7
By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
What is the 10 001st prime number?
")

'((math::largest-n-digit-product-in-series 13 (fetch-input-string 8))
"Problem 8
The four adjacent digits in the 1000-digit number that have the greatest product are 9 × 9 × 8 × 9 = 5832.

(Number in input file 8)

Find the thirteen adjacent digits in the 1000-digit number that have the greatest product. What is the value of this product?
")

'((apply '* (math::pythagorean-triplets-with-sum-n 1000))
"Problem 9
A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
a^2 + b^2 = c^2

For example, 32 + 42 = 9 + 16 = 25 = 52.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.
")

'((reduce '+ (math::prime-list-range-by-division 2000000))
"Problem 10
The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
Find the sum of all the primes below two million.
")

'((math::matrix-max-product-of-n-consecutive-values (read-matrix-from-stream 'integer (make-string-input-stream (fetch-input-string 11))) 4)
"Problem 11
In the 20×20 grid below, four numbers along a diagonal line have been marked in red.

(Array in input file 11)

The product of these numbers is 26 × 63 × 78 × 14 = 1788696.
What is the greatest product of four adjacent numbers in the same direction (up, down, left, right, or diagonally) in the 20×20 grid?
")

'((math::first-triangular-number-with-at-least-n-divisors 500 :primes (math::prime-sieve-array 1000))
"Problem 12
The sequence of triangle numbers is generated by adding the natural numbers. So the 7th triangle number would be 1 + 2 + 3 + 4 + 5 + 6 + 7 = 28. The first ten terms would be:
1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
Let us list the factors of the first seven triangle numbers:

     1: 1
     3: 1,3
     6: 1,2,3,6
    10: 1,2,5,10
    15: 1,3,5,15
    21: 1,3,7,21
    28: 1,2,4,7,14,28

We can see that 28 is the first triangle number to have over five divisors.
What is the value of the first triangle number to have over five hundred divisors?
"
)

'((identity (parse-integer (subseq (write-to-string (apply '+ (map 'list #'parse-integer (split (fetch-input-string 13))))) 0 10)))
"Problem 13
Work out the first ten digits of the sum of the following one-hundred 50-digit numbers.

(Array in input file 13)
")

'((identity (math::largest-collatz-length-in-range 1000000))
"Problem 14
The following iterative sequence is defined for the set of positive integers:

n → n/2 (n is even)
n → 3n + 1 (n is odd)

Using the rule above and starting with 13, we generate the following sequence:
13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1

It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms.
Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.
Which starting number, under one million, produces the longest chain?

NOTE: Once the chain starts the terms are allowed to go above one million.
")

'((/ (math::factorial 40) (expt (math::factorial 20) 2))
"Problem 15
Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down, there are exactly 6 routes to the bottom right corner.
How many such routes are there through a 20×20 grid?

Solution: It takes 40 moves to reach the bottom right corner, 20 left and 20 down. Each path is uniquely described as a sequence
of down (D) and right (R) moves. Moreover, each path is uniquely described by the positions in the 40-move long path
at which we place D moves, since the rest are just filled with R moves. Therefore, our pool of objects are 40 positions,
and the places are 20 down moves, so we pick which 20 positions out of 40 are to be filled with D moves.
So it's (40 20) = 40!/(20!*(40-20)!) possible paths.
")

'((math::sum-of-digits (expt 2 1000))
"Problem 16
2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
What is the sum of the digits of the number 2^1000?
")

'((math::letters-in-range 1000)
"Problem 17
If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?
NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20 letters.
The use of \"and\" when writing out numbers is in compliance with British usage.
")

))

(defun euler_fun (num &key solution show-form show-doc)
    "Takes problem number and evaluates the answer or shows solution form, according to keys.
    Numbering of problems starts with 1."
    (setf solution t) ;; It is the default action anyway, if all else is nil it should be executed, keyword allows for code clarity
    (if (or (< num 1) (> num (length problem_solutions)))
        (format t "Invalid problem number~%")
        (let ((problem-info (eval (aref problem_solutions (- num 1)))))
        (cond
            (show-form (first problem-info))
            (show-doc (second problem-info))
            (solution (eval (first problem-info)))))))

(defun show_solved (&key export)
"Prints the presentations and found solutions of all the problems on terminal,or exports them to solved_problems.txt"
(let ((dest (if export (open "solved_problems.txt" :direction :output :if-exists :overwrite :if-does-not-exist :create) t)))
        (loop for i from 1 to (length problem_solutions) do (format dest "****************************************~%~%~a~%~%Solution: ~a~%~%****************************************~%" 
                                (euler_fun i :show-doc t) (euler_fun i :solution t)))
(if export (close dest))
))