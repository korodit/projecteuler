;; General Utilities

;; Range, limit non-inclusive if ascending sequence, inclusive if descending
;; #range #numbers
(defun range (start &optional limit &key (stp 1))
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

;; Prime list up to limit non-inclusive in order
;; #prime
(defun prime_list (limit)
    (loop
        for nums = (range 2 limit) then (delete-if #'(lambda (x) (= 0 (rem x (car nums)))) (cdr nums))
        while nums
        collect (car nums)))

;; Finds all prime factors of x and returns them as a list
;; #prime #factor
(defun prime_factors_slow (x)
    (delete-if-not #'(lambda (y) (= 0 (rem x y))) (prime_list (floor (sqrt x)))))

;; Takes a number to be divided and a divisor of it and gives the power of the divisor that exists in the divided number
;; plus the divided number if the divisor is removed from it
;; #factor #divisor #divided #power
(defun times_divided (divided divisor already)
    (if (/= 0 (rem divided divisor)) (values already divided) (times_divided (/ divided divisor) divisor (+ already 1))))

;; Result is in the form ((factor1 power1) (factor2 power2) ...)
;; #prime #factor #power
(defun prime_factors_powers (ceil &aux pow)
    (if (not (and (integerp ceil) (plusp ceil))) (error "Non positive integer given to function prime_factors"))
    (loop
        for i = 2 then (+ i 1)
        while (> ceil 1)
        when (zerop (rem ceil i)) 
            do (multiple-value-setq (pow ceil) (times_divided ceil i 0))
            and collect (list i pow)))

;; Problem #1
;; Takes a limit number, and a list of numbers. Returns the sum of all the numbers from 1 up to and including limit-1, which are divided by one of the argument numbers
;; anums just concatenates the needed first number with the list of optional rest of the numbers.
;; #sum #multiple
(defun sum_mults (limit fst &rest nums &aux (anums (cons fst nums)))
    (loop for x from 1 to (- limit 1) 
        sum (if 
                (member-if 
                    #'(lambda (n) (= 0 (rem x n))) 
                    anums) 
                x 0)))

;; (sum_mults 1000 3 5)

;; Problem #2
;; Takes a limit and a test function, and produces the sum of all fibonacci numbers up to limit non-inclusive that satisfy the test given - test is a function
;; #fibonacci #sequence #sum
(defun fib_sum_test (limit test)
    (loop
        for p2 = 0 then p1
        and p1 = 1 then (+ p1 p2)
        while (< p2 limit)
        when (funcall test p2) sum p2))

(defun fib_sum_even (limit) (fib_sum_test limit #'evenp))

;; (fib_sum_even 4000000)

;; Problem #3
;; Takes x and finds its largest prime factor
;; #largest #max #prime #factor
(defun largest_prime_factor_slow (x)
    (let ((candidates (prime_factors_powers x)))
            ;; If no prime factors other than itself, the candidates list is empty!
            (if (not candidates)
                x
                (caar (last candidates)))))

(defun largest_prime_factor (x)
    (loop
        for i from 2 below (sqrt x)
        when (zerop (rem x i)) return (largest_prime_factor (/ x i))
        finally (return x)))

;; (largest_prime_factor 600851475143)
