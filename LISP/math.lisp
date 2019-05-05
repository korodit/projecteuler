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

(defun prime-list (limit)
"    Prime list up to limit non-inclusive in order
    #prime"
    (loop
        for nums = (range 2 limit) then (delete-if #'(lambda (x) (= 0 (rem x (car nums)))) (cdr nums))
        while nums
        collect (car nums)))

(defun first-n-primes (n)
"    Returns the list of the first n primes, n given. Can take optimizations.
    #prime #first"
     (labels ((relative-primes-in-list (listt)
                (loop
                        for nums = listt then (delete-if #'(lambda (x) (= 0 (rem x (car nums)))) (cdr nums))
                        while nums
                        collect (car nums))
                )
             (prime-list-with-known-beginning (beginning extension-start extension-end goal)
                (setf extension
                        (loop
                                ;;beginning, one extra element added at start of list as padding for loop
                                for b-prime in (cons 1 beginning)
                                for extension = (range extension-start extension-end) 
                                        then (delete-if #'(lambda (x) (= 0 (rem x b-prime))) extension)
                                finally (return (nconc beginning (relative-primes-in-list extension)))))
                (if (>= (length extension) goal) 
                        (subseq extension 0 goal) 
                        (prime-list-with-known-beginning extension extension-end (+ (* 2 goal) extension-end) n))))
        (prime-list-with-known-beginning () 2 (* n 2) n)))

(defun nth-prime (n) 
"    Returns the nth prime, n given.
    #primes #nth"       
        (last (first-n-primes n)))

(defun prime-factors-slow (x)
"    Finds all prime factors of x and returns them as a list
    #prime #factor"
    (delete-if-not #'(lambda (y) (= 0 (rem x y))) (prime-list (floor (sqrt x)))))

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
        (reduce #'* (map 'list #'max_power (prime-list (+ limit 1))))))

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
    (identity (floor (* n (+ n 1) (+ (* 3 n) 2) (- n 1)) 12)))
    
(defun largest-n-digit-product-in-series (n series &aux (trans (map 'vector #'char-to-int series)) (len (length trans)))
"    Takes a number n and a string 'series' conmtaining a valid integer representation,
    and finds the largest product that can be created my multiplying any n adjacent digits contained in the series
    #sequence #product #multiply"
    (if (< len n) 
        (error "The series input must be longer than the number of digits of the product")
        (loop for i from 0 to (- len n) maximize (reduce #'* (subseq trans i (+ i n))))))))

(defun doclist (&key export)
"Prints the titles and documentations of all the functions on terminal, or exports them to math_funs.txt"
    (let ((dest (if export (open "math_funs.txt" :direction :output :if-exists :overwrite :if-does-not-exist :create) t)))
            (loop for f in funlist do (format dest "****************************************~%~%    --~a--~%~%~a~%~%****************************************~%" 
                                    f (documentation (symbol-function f) 'function)))
    (if export (close dest))))

(in-package :cl)