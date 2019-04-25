(defpackage :math (:use :cl))

(in-package :math)

;; Gathering functions in a list for future features
(setf funlist (list 

(defun range (start &optional limit &key (stp 1))
"    --range--

    Range, limit non-inclusive if ascending sequence, inclusive if descending.
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


(defun prime_list (limit)
"    --prime_list--

    Prime list up to limit non-inclusive in order
    #prime"
    (loop
        for nums = (range 2 limit) then (delete-if #'(lambda (x) (= 0 (rem x (car nums)))) (cdr nums))
        while nums
        collect (car nums)))


(defun prime_factors_slow (x)
"    --prime_factors_slow--

    Finds all prime factors of x and returns them as a list
    #prime #factor"
    (delete-if-not #'(lambda (y) (= 0 (rem x y))) (prime_list (floor (sqrt x)))))

(defun times_divided (divided divisor already)
"    --times_divided--

    Takes a number to be divided and a divisor of it, and gives the power of the divisor that exists in the divided number,
    plus the result of the division between the divided and the divisor. Helper function of prime_factors_powers.
    #factor #divisor #divided #power"
    (if (/= 0 (rem divided divisor)) (values already divided) (times_divided (/ divided divisor) divisor (+ already 1))))

(defun prime_factors_powers (ceil &aux pow)
"    --prime_factors_powers--

    Result is in the form ((factor1 power1) (factor2 power2) ...)
    #prime #factor #power"
    (if (not (and (integerp ceil) (plusp ceil))) (error "Non positive integer given to function prime_factors"))
    (loop
        for i = 2 then (+ i 1)
        while (> ceil 1)
        when (zerop (rem ceil i)) 
            do (setf (values pow ceil) (times_divided ceil i 0))
            and collect (list i pow)))

(defun is_palindrome (str)
"    --prime_factors_powers--

    Takes a string and returns a boolean value on whether it's a palindrome or not
    #string #palindrome"
    (labels ((ip_rec (str start end)
                (if (>= start end) 
                    t
                    (if (eql (aref str start) (aref str end))
                        (ip_rec str (+ start 1) (- end 1))))))
        (ip_rec str 0 (- (length str) 1))))

(defun sum_mults (limit fst &rest nums &aux (anums (cons fst nums)))
"    --sum_mults--

    Takes a limit number, and a list of numbers. Returns the sum of all the numbers from 1 up to and including limit-1, which are divided by one of the argument numbers.
    anums just concatenates the needed first number with the list of optional rest of the numbers.
    #sum #multiple"
    (loop for x from 1 to (- limit 1) 
        sum (if 
                (member-if 
                    #'(lambda (n) (= 0 (rem x n))) 
                    anums) 
                x 0)))

(defun fib_sum_test (limit test)
"    --fib_sum_test--

    Takes a limit and a test function, and produces the sum of all fibonacci numbers up to limit non-inclusive that satisfy the test given - test is a function
    #fibonacci #sequence #sum #test #filter"
    (loop
        for p2 = 0 then p1
        and p1 = 1 then (+ p1 p2)
        while (< p2 limit)
        when (funcall test p2) sum p2))

(defun fib_sum_even (limit)
"    --fib_sum_even--

    Sums the numbers of the Fibonacci sequence up to the limit non-inclusive that are even numbers.
    #fibonacci #sequence #sum #even"
    (fib_sum_test limit #'evenp))

(defun largest_prime_factor_slow (x)
"    --largest_prime_factor_slow--

    Takes x and finds its largest prime factor
    #largest #max #prime #factor"
    (let ((candidates (prime_factors_powers x)))
            ;; If no prime factors other than itself, the candidates list is empty!
            (if (not candidates)
                x
                (caar (last candidates)))))

(defun largest_prime_factor (x)
"    --largest_prime_factor--

    Takes x and finds its largest prime factor
    #largest #max #prime #factor"
    (loop
        for i from 2 below (sqrt x)
        when (zerop (rem x i)) return (largest_prime_factor (/ x i))
        finally (return x)))

(defun max_palindrome_n_digits (n &aux (start (expt 10 (- n 1))) (end (- (expt 10 n) 1)) (mult 0) (maxx 0))
"    --max_palindrome_n_digits--

    Takes n and finds the largest number which is a product of two n-digit numbers and which is
    a palindrome
    #number #max #palindrome"
    (loop
        for i from start to end
        do (loop 
            for j from (max i (floor maxx i)) to end
            do (setf mult (* i j))
            when (and (> mult maxx) (is_palindrome (write-to-string mult))) do (setf maxx mult)))
    maxx)

(defun divisible_up_to (limit)
"    --divisible_up_to--

    Takes limit and finds the smallest number which is divisible by all numbers up to limit
    #number #min #division"
    ;; max_power calculates the maximum power of mul that is at most equal with n
    (flet ((max_power (mul)
        (labels ((mp_rec (mul acc &aux (newacc (* acc mul)))
            (if (<= newacc limit) (mp_rec mul newacc) acc)))
            (mp_rec mul mul))))
        (reduce #'* (map 'list #'max_power (prime_list (+ limit 1))))))

(defun sum_sqr_diff_slow (limit &aux (num_sum (expt (floor (* limit (+ limit 1)) 2) 2)))
"    --sum_sqr_diff_slow--

    Takes a limit and finds the difference between the square of the sum up to limit
    and the sum of the squares up to limit
    #square #sum #difference"
    (loop
        for i from 1 to limit
        sum (* i i) into sq_sum
        finally (return (- num_sum sq_sum))))

(defun sum_sqr_diff (n)
"    --sum_sqr_diff--

    Takes a limit and finds the difference between the square of the sum up to limit
    and the sum of the squares up to limit

    Knowing that the sum of the first n natural numbers can be expressed as n(n+1)/2, 
    [and thus the square of the sum of the first n natural numbers can be expressed
    as n(n+1)(n)(n+1)/4 ] and that the sum of the first n square numbers is n(n+1)(2n+1)/6,
    it is quite easy to do this problem. Also, you can simplify the difference between these
    two to this: n(n+1)(3n+2)(n-1)/12
    #square #sum #difference"
    (identity (floor (* n (+ n 1) (+ (* 3 n) 2) (- n 1)) 12))))) ;; identity gets rid of second VALUE

(defun doclist (&key export)
(let ((dest (if export (open "math_funs.txt" :direction :output :if-exists :overwrite :if-does-not-exist :create) t)))
        (loop for f in funlist do (format dest "****************************************~%~%~a~%~%****************************************~%" 
                                (documentation (symbol-function f) 'function)))
(if export (close dest))
))

(in-package :cl)