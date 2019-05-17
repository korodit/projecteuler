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
    (if (/= 0 (rem divided divisor)) (values already divided) (times_divided (/ divided divisor) divisor (+ already 1))))

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
    (loop
        for i from 2 below (sqrt x)
        when (zerop (rem x i)) return (largest-prime-factor (/ x i))
        finally (return x)))

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
    and finds the largest product that can be created my multiplying any n adjacent digits contained in the series
    #sequence #product #multiply"
    (if (< len n) 
        (error "The series input must be longer than the number of digits of the product")
        (largest-n-digit-product-in-sequence n trans)))

(defun largest-n-digit-product-in-sequence (n seq &aux (len (length seq)))
"    Takes a number n and an integer sequence, and finds the largest product that can be 
    created my multiplying any n adjacent digits contained in the sequence.
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
))

(defun doclist (&key export)
"Prints the titles and documentations of all the functions on terminal, or exports them to math_funs.txt"
    (let ((dest (if export (open "math_funs.txt" :direction :output :if-exists :overwrite :if-does-not-exist :create) t)))
            (loop for f in funlist do (format dest "****************************************~%~%    --~a--~%~%~a~%~%****************************************~%" 
                                    f (documentation (symbol-function f) 'function)))
    (if export (close dest))))

(in-package :cl)
