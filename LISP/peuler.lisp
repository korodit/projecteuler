;; Problem 1
;; Takes a limit number, and a list of numbers. Returns the sum of all the numbers from 1 up to and including limit-1, which are divided by one of the argument numbers
;; anums just concatenates the needed first number with the list of optional rest of the numbers.
(defun sum_mults (limit fst &rest nums &aux (anums (cons fst nums)))
    (loop for x from 1 to (- limit 1) 
        sum (if 
                (member-if 
                    #'(lambda (n) (eq 0 (rem x n))) 
                    anums) 
                x 0)))

;; (sum_mults 1000 3 5)

;; Problem 2
;; Takes a limit and a test function, and produces the sum of all fibonacci numbers up to limit non-inclusive that satisfy the test given - test is a function
(defun fib_sum_test (limit test)
    (loop
        for p2 = 0 then p1
        and p1 = 1 then (+ p1 p2)
        while (< p2 limit)
        when (funcall test p2) sum p2))

(defun fib_sum_even (limit) (fib_sum_test limit #'evenp))

;; (fib_sum_even 4000000)
