(defpackage :math (:use :cl))

(in-package :math)

;; Gathering functions in a list for future features
(setf funlist (list 

(defun char-to-int (chr)
"    Takes a numerical character and returns the integer value it represents."
    (let ((result (- (char-int chr) (char-int #\0))))
        (if (<= 0 result 9)
            result
            (error "Non-arithmetic character given to function \"char-to-int\""))))

(defun range (start &optional limit &key (stp 1))
"    Range, limit non-inclusive if ascending sequence, inclusive if descending.
    Omitting the limit make the value of start as limit and start is 0.
    #range #numbers"
    (if (not (numberp start)) (error "Non-number input for range"))
    (if (not (numberp stp)) (error "Non-number input for range"))
    (if (zerop stp) (error "Zero step not allowed in range"))
    (if (and limit (not (numberp limit))) (error "Non-number input for range"))
    (if (not limit) (setf limit start start 0))
    (let ((comp (if (plusp stp) #'< #'>=)))
        (loop
            for i = start then (+ i stp)
            while (funcall comp i limit)
            collect i)))

(defun prime-sieve-raw-array (upper_bound &aux (arr
                                            (make-array (1+ upper_bound) :initial-element t)))
"    Performs Eratosthenes sieve on an array of suitable size
    to fit the upper search bound. Primes are marked with the t value,
    together with 0 and 1. Number of primes is also returned as value.
    !!!MEMORY CONSUMING
    #prime #range #sieve #array"
    (loop
        with num = 0
        for i from 2 to upper_bound
        when (aref arr i) 
        do (incf num)
        and 
        do (loop
                    for j = (* 2 i) then (+ j i)
                    while (<= j upper_bound)
                    do (setf (aref arr j) nil))
        finally (return (values arr num))))

(defun prime-sieve-list (upper_bound)
"    Creates a list with all the primes lesser or equal to upper bound.
    Uses prime-sieve-raw-array. !!!MEMORY CONSUMING.
    #prime #range #sieve #list"
    (multiple-value-bind (arr length) (prime-sieve-raw-array upper_bound)
        (loop
            for i from 2 to (1- (length arr))
            when (aref arr i) collect i)))

(defun prime-sieve-array (upper_bound)
"    Creates an array with all the primes lesser or equal to upper bound.
    Also returns the array length.
    Uses prime-sieve-raw-array. !!!MEMORY CONSUMING.
    #prime #range #sieve #array"
    (multiple-value-bind (arr length) (prime-sieve-raw-array upper_bound)
        (loop
            with new-arr = (make-array length :initial-element 0)
            and index = 0
            for i from 2 to (1- (length arr))
            when (aref arr i) do (progn (setf (aref new-arr index) i) (incf index))
            finally (return (values new-arr length)))))

(defun is-prime (n &aux (r (isqrt n)))
"    A faster prime checker, based on the problem 7 overview on project Euler site.
    Some useful facts:
    1 is not a prime.
    All primes except 2 are odd.
    All primes greater than 3 can be written in the form  6k+/-1.
    Any number n can have only one primefactor greater than sqrt n.
    The consequence for primality testing of a number n is: if we 
    cannot find a numberf less than or equal sqrt n that divides n 
    then n is prime: the only primefactor of n is n itself.
    #prime"
    (cond
        ((= n 1) nil)
        ((< n 4) t)
        ((zerop (mod n 2)) nil)
        ((< n 9) t)
        ((zerop (mod n 3)) nil)
        (t (loop
                ;; for r from (isqrt n) 
                for f = 5 then (+ f 6)
                while (<= f r)
                when (or (zerop (mod n f)) (zerop (mod n (+ f 2)))) do (return nil) 
                finally (return t)))))

(defun prime-list-range-by-division (limit1 &rest limit2 &aux 
                                                        (start (if (null limit2) 2 limit1))
                                                        (end (if (null limit2) limit1 limit2)))
"    Outputs a list with all the prime numbers within the given range, limits inclusive.
    If given one value, that value is the upper limit inclusive.
    #prime #list #range"
    (loop
        for i from start upto end
        when (is-prime i) collect i))

(defun first-n-primes-division-list (n &aux (cnt 0))
"    Outputs a list with the first n primes. Uses is-prime function in loop.
    #prime #first"
    (loop
        for i from 2
        when (is-prime i)
            do (incf cnt)
            and collect i
        while (< cnt n)))

(defun nth-prime-slow (n) 
"    Returns the nth prime, n given.
    #primes #nth"       
        (last (first-n-primes-division-list n)))

(defun nth-prime (n &aux (countt 1))
"    Returns the nth prime, n given.
    #primes #nth"  
    (loop
        ;; for count from 1
        for candidate = 3 then (+ candidate 2)
        when (is-prime candidate) do (incf countt)
        when (= countt n) do (return-from nth-prime candidate)))

(defun prime-factors-slow (x)
"    Finds all prime factors of x and returns them as a list
    #prime #factor"
    (delete-if-not #'(lambda (y) (= 0 (rem x y))) (prime-list-range-by-division (floor (sqrt x)))))

(defun divisor-power-and-rem (divided divisor already)
"    Takes a number to be divided and a divisor of it, and gives the power of the divisor that exists in the divided number,
    plus the result of the division between the divided and the divisor. Helper function of prime-factors-powers.
    #factor #divisor #divided #power"
    (if (/= 0 (rem divided divisor)) (values already divided) (divisor-power-and-rem (/ divided divisor) divisor (+ already 1))))

(defun prime-factors-powers (ceil &aux pow)
"    Returns the prime factors of the argmuent, and the power of each in the number.
    Result is in the form ((factor1 power1) (factor2 power2) ...)
    #prime #factor #power"
    (if (not (and (integerp ceil) (plusp ceil))) (error "Non positive integer given to function prime_factors"))
    (loop
        for i = 2 then (+ i 1)
        while (> ceil 1)
        when (zerop (rem ceil i)) 
            do (setf (values pow ceil) (divisor-power-and-rem ceil i 0))
            and collect (list i pow)))

(defun is-palindrome (str)
"    Takes a string and returns a boolean value on whether it's a palindrome or not
    #string #palindrome"
    (labels ((ip_rec (str start end)
                (if (>= start end) 
                    t
                    (if (eql (aref str start) (aref str end))
                        (ip_rec str (+ start 1) (- end 1))))))
        (ip_rec str 0 (- (length str) 1))))

(defun sum-mults-of-args-up-to (limit fst &rest nums &aux (anums (cons fst nums)))
"    Takes a limit number, and a list of numbers. Returns the sum of all the numbers from 1 up to and including limit-1, which are divided by one of the argument numbers.
    anums just concatenates the needed first number with the list of optional rest of the numbers.
    #sum #multiple"
    (loop for x from 1 to (- limit 1) 
        sum (if 
                (member-if 
                    #'(lambda (n) (= 0 (rem x n))) 
                    anums) 
                x 0)))

(defun fibonacci-test-sum (limit test)
"    Takes a limit and a test function, and produces the sum of all fibonacci numbers up to limit non-inclusive that satisfy the test given - test is a function
    #fibonacci #sequence #sum #test #filter"
    (loop
        for p2 = 0 then p1
        and p1 = 1 then (+ p1 p2)
        while (< p2 limit)
        when (funcall test p2) sum p2))

(defun even-fibonacci-sum (limit)
"    Sums the numbers of the Fibonacci sequence up to the limit non-inclusive that are even numbers.
    #fibonacci #sequence #sum #even"
    (fibonacci-test-sum limit #'evenp))

(defun largest-prime-factor-slow (x)
"     Takes x and finds its largest prime factor
    #largest #max #prime #factor"
    (let ((candidates (prime-factors-powers x)))
            ;; If no prime factors other than itself, the candidates list is empty!
            (if (not candidates)
                x
                (caar (last candidates)))))

(defun largest-prime-factor (x)
"    Takes x and finds its largest prime factor
    #largest #max #prime #factor"
    (labels
        ((recursive-helper (x current_check)
            (if (= x current_check)
                x
                (if (zerop (rem x current_check))
                    (recursive-helper (floor x current_check) current_check)
                    (recursive-helper x (1+ current_check))))))
        (recursive-helper x 2)))

(defun max-palindrome-prod-of-2-n-digit-nums (n &aux (start (expt 10 (- n 1))) (end (- (expt 10 n) 1)) (mult 0) (maxx 0))
"    Takes n and finds the largest number which is a product of two n-digit numbers and which is
    a palindrome
    #number #max #palindrome"
    (loop
        for i from start to end
        do (loop 
            for j from (max i (floor maxx i)) to end
            do (setf mult (* i j))
            when (and (> mult maxx) (is-palindrome (write-to-string mult))) do (setf maxx mult)))
    maxx)

(defun smallest-divisible-by-all-in-range (limit)
"    Takes limit and finds the smallest number which is divisible by all numbers up to limit
    #number #min #division"
    ;; max_power calculates the maximum power of mul that is at most equal with n
    (flet ((max_power (mul)
        (labels ((mp_rec (mul acc &aux (newacc (* acc mul)))
            (if (<= newacc limit) (mp_rec mul newacc) acc)))
            (mp_rec mul mul))))
        (reduce #'* (map 'list #'max_power (prime-list-range-by-division limit)))))

(defun square-of-sum-sum-of-square-diff-slow (limit &aux (num_sum (expt (floor (* limit (+ limit 1)) 2) 2)))
"    Takes a limit and finds the difference between the square of the sum up to limit
    and the sum of the squares up to limit
    #square #sum #difference"
    (loop
        for i from 1 to limit
        sum (* i i) into sq_sum
        finally (return (- num_sum sq_sum))))

(defun square-of-sum-sum-of-square-diff (n)
"     Takes a limit and finds the difference between the square of the sum up to limit
    and the sum of the squares up to limit

    Knowing that the sum of the first n natural numbers can be expressed as n(n+1)/2, 
    [and thus the square of the sum of the first n natural numbers can be expressed
    as n(n+1)(n)(n+1)/4 ] and that the sum of the first n square numbers is n(n+1)(2n+1)/6,
    it is quite easy to do this problem. Also, you can simplify the difference between these
    two to this: n(n+1)(3n+2)(n-1)/12
    #square #sum #difference"
    (identity (floor (* n (+ n 1) (+ (* 3 n) 2) (- n 1)) 12)))  ;; identity gets rid of second VALUE
    
(defun largest-n-digit-product-in-series (n series &aux (trans (map 'vector #'char-to-int series)) (len (length trans)))
"    Takes a number n and a string 'series' conmtaining a valid integer representation,
    and finds the largest product that can be created by multiplying any n adjacent digits contained in the series
    #sequence #product #multiply"
    (if (< len n) 
        (error "The series input must be longer than the number of digits of the product")
        (largest-n-digit-product-in-sequence n trans)))

(defun largest-n-digit-product-in-sequence (n seq &aux (len (length seq)))
"    Takes a number n and an integer sequence, and finds the largest product that can be 
    created by multiplying any n adjacent digits contained in the sequence.
    #sequence #product #multiply"
    (if (< len n) 
        0
        (loop for i from 0 to (- len n) maximize (reduce #'* (subseq seq i (+ i n))))))
        
(defun pythagorean-triplets-with-sum-n (n)
"    Takes a number n and finds a pythagorean triplet with sum n.
    Returns the triplet in a list, or nil if it does not exist.
    A Pythagorean triplet is a set of three natural numbers, a < b < c,
    for which a^2 + b^2 = c^2.
    #sum #pythagorean #triplet"
    (block nested-loops
        (loop for i from 1 to (floor n 3) do
            (loop for j from (1+ i) to (floor n 2) do
                (let ((k (- n i j)))
                    (if (= (* k k) (+ (* i i) (* j j)))
                        (return-from nested-loops (list i j k)))))
        finally (return nil))))

(defun matrix-slice (arr part &optional (num 0) &aux new-array (old-rows (array-dimension arr 0)) (old-columns (array-dimension arr 1)))
"    Creates a vector containing a slice of a given matrix.
    Num gives the index, e.g. the nth row, nth row,
    nth right diagonal, nth left diagonal.
    The right diagonals start from up left and go to down right.
    Right diagonal number 0 starts with the element (rows-1,0),
    then the starting element sequence continues up and then right.
    Left diagonals start from down left and go to up right.
    Left diagonal number 0 starts with the element (0,0), then
    the startign element sequence continues down and then right.
    Keywords for the type of slice are: :row, :column, :right-diag, :left-diag.
    #matrix #aray #slice"
    (case part
        (:row
            (setf new-array (make-array old-rows :initial-element 0))
            (loop for i from 0 to (- old-rows 1)
                do (setf (aref new-array i) (aref arr num i))))
        (:column
            (setf new-array (make-array old-columns :initial-element 0))
            (loop for i from 0 to (- old-columns 1)
                do (setf (aref new-array i) (aref arr i num))))
        (:right-diag
            (let* (
                    (srow (if (< num old-rows) (- old-rows num 1) 0))
                    (scolumn (if (< num old-rows) 0 (1+ (- num old-rows))))
                    (array-size (min (- old-rows srow) (- old-columns scolumn))))
                (setf new-array (make-array array-size :initial-element 0))
                (loop
                    for i from srow to (- old-rows 1)
                    for j from scolumn to (- old-columns 1)
                    for index = 0 then (1+ index)
                    do (setf (aref new-array index) (aref arr i j)))))
        (:left-diag
            (let* (
                    (srow (if (< num old-rows) num (- old-rows 1)))
                    (scolumn (if (< num old-rows) 0 (+ (- num old-rows) 1)))
                    (array-size (min (1+ srow) (- old-columns scolumn))))
                (setf new-array (make-array array-size :initial-element 0))
                (loop
                    for i from srow downto 0
                    for j from scolumn to (- old-columns 1)
                    for index = 0 then (1+ index)
                    do (setf (aref new-array index) (aref arr i j))))))
    new-array)

(defun number-of-matrix-slices (arr part &aux (rows (array-dimension arr 0)) (columns (array-dimension arr 1)))
"    Returns the number of columns,rows, or diagonals of a matrix respectively.
    Keywords are deliberately the same as the matrix-slice function.
    #matrix #array #slice $number #dimensions"
    (case part
        (:row rows)
        (:column columns)
        (:right-diag (- (+ rows columns) 1))
        (:left-diag (- (+ rows columns) 1))))

(defun matrix-max-product-of-n-consecutive-values (matrix n &aux (dimensions (array-dimensions matrix)))
"    Returns the maximum product of n consecutive elements in an integer matrix that are adjucent
    horizontally, vertically, or diagonically.
    #matrix #array #max #product #consecutive #adjucent"
    (loop
        for slice in (list :row :column :right-diag :left-diag)
        maximize (loop
                    for i from 0 to (- (number-of-matrix-slices matrix slice) 1)
                    maximize (largest-n-digit-product-in-sequence n (matrix-slice matrix slice i)))))

(defun divisors (n)
"    Creates a list all the integers that divide n,
    including 1 and itself.
    #divisors #division"
    (loop
        for i from 1 to n
        when (zerop (rem n i)) collect i))

(defun number-of-divisors (n &key primes (sqr (isqrt n)))
"    Finds the number of all integers that divide n,
    including 1 and itself.
    It can be optionally given an array of primes, the max of which
    is known to be larger than n's sqrt,
    for significantly faster results.
    #divisors #division #number"
    (if primes
        (loop
            with prime = 0 and result = 1 and pow = 0
            for i from 0
            while (and (> n 1) (<= i sqr))
            do (setf prime (aref primes i))
            when (zerop (rem n prime)) 
                do (setf (values pow n) (divisor-power-and-rem n prime 0))
                and do (setf result (* result (1+ pow)))
            finally (return result))
        (+ 
            (* 2 (loop
                for i from 1 to sqr
                count (zerop (rem n i))))
            (if (= n (* sqr sqr)) -1 0))))

(defun first-triangular-number-with-at-least-n-divisors (n &key primes)
"    Finds the first triangular number that has at least
    the number of divisors given as a parameter.
    We make use of the fact that the ith triangular number
    equals i*(i+1)/2, the fact that two consecutive numbers are
    co-prime, and thus the number of divisors of the above formula's
    result is the product of the divisors of i/2 and i+1, if i is even,
    or of the divisors of i and (i+1)/2, if i is odd.
    Optionally takes an array prime range for use by number-of-divisors function.
    #divisors #division #number #triangle #triangular"
    (loop 
        with part1 = 0 and part2 = 0
        for i from 1
        do (if (zerop (rem i 2))
            (setf part1 (/ i 2) part2 (1+ i))
            (setf part1 i part2 (/ (1+ i) 2)))
        when (> (* (number-of-divisors part1 :primes primes) (number-of-divisors part2 :primes primes)) n) return (/ (* i (1+ i)) 2)))

(defun largest-collatz-length-in-range (limit &aux (lengths (make-array (1+ limit) :initial-element 0)))
"    Prints the number at which the longest Collatz sequence starts, among
    all the numbers up to and including the given limit. Also returns the
    max length as the second value.
    #collatz #sequence #max"
    (setf (aref lengths 1) 1)
    (labels ((next-collatz (n)
                (if (evenp n) (floor n 2) (1+ (* 3 n))))
            (update-lengths (path currlen &aux (curr-index (car path)) (taill (cdr path)))
                (if path 
                    (progn (if (<= curr-index limit) (setf (aref lengths curr-index) currlen))
                            (update-lengths taill (1+ currlen)))))
            (traverse-collatz (cnum path &aux (cvalue (if (<= cnum limit) (aref lengths cnum) 0)))
                (if (zerop cvalue)
                    (traverse-collatz (next-collatz cnum) (cons cnum path))
                    (update-lengths path (1+ cvalue)))))
        (loop
            with maxx = 0 and maxxid = 0
            for i from (floor limit 2) to limit
            do (traverse-collatz i nil)
            when (> (aref lengths i) maxx) 
                do (setf maxxid i) 
                and do (setf maxx (aref lengths i))
            finally (return (values maxxid maxx)))))

(defun factorial (n)
"    Gives the factorial of n, n greater than or equal to zero.
    #factorial"
    (labels ((rec-helper (n acc)
                (if (zerop n) acc (rec-helper (1- n) (* acc n)))))
        (rec-helper n 1)))

(defun sum-of-digits (num)
"    Gives the sum of digits of a number in decimal form.
    #sum #digits"
    (apply '+ (map 'list #'char-to-int (write-to-string num))))


(defun letters (num &aux 
                        (letnum #(0 3 3 5 4 4 3 5 5 4 3 6 6 8 8 7  7 9 8 8))
                        (lettens #(0 0 6 6 5 5 5 7 6 6))
                        (total 0)
                        (hundreds (floor num 100))
                        (mod100 (mod num 100))
                        (tens (floor (mod num 100) 10))
                        (units (mod num 10)))
"Returns the number of letters used for a given number, from 0 to 1000.
Rules: 
If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?
Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20 letters.
The use of \"and\" when writing out numbers is in compliance with British usage."
    (if (= num 1000) (return-from letters 11))
    (if (zerop num) 4)
    (if (plusp hundreds)
        (incf total (+ 7 (aref letnum hundreds))))
    (if (and (plusp hundreds) (plusp mod100))
        (incf total 3))
    (if (< mod100 20)
        (incf total (aref letnum mod100))
        (incf total (+ (aref lettens tens) (aref letnum units))))
    total)

(defun letters-in-range (limit)
"Returns the sum of letters used for all numbers up to limit,
limit less or equal to 1000 for now due to 'letters' function
limitations."
    (loop for i from 1 to limit
        sum (letters i)))

))

(defun doclist (&key export)
"Prints the titles and documentations of all the functions on terminal, or exports them to math_funs.txt"
    (let ((dest (if export (open "math_funs.txt" :direction :output :if-exists :overwrite :if-does-not-exist :create) t)))
            (loop for f in funlist do (format dest "****************************************~%~%    --~a--~%~%~a~%~%****************************************~%" 
                                    f (documentation (symbol-function f) 'function)))
    (if export (close dest))))

(in-package :cl)
