(load "peuler.lisp")

(defun euler_fun (num &key evaluate show-form)
    "Takes problem number and evaluates the answer or shows solution form, according to keys.
    Numbering of problems starts with 1."
    (if (or (< num 1) (> num (length problem_solutions)))
        (format t "Invalid problem number~%")
        (let ((problem-info (aref problem_solutions (- num 1))))
        (if show-form
            (eval problem-info)
            (eval (eval problem-info))))))

(setf problem_solutions  #(
            ;; Problem #1
            '(math::sum_mults 1000 3 5)
            ;; Problem #2
            '(math::fib_sum_even 4000000)
            ;; Problem #3
            '(math::largest_prime_factor 600851475143)
            ;; Problem #4
            '(math::max_palindrome_n_digits 3)
            ;; Problem #5
            '(math::divisible_up_to 20)
            ;; Problem #6
            '(math::sum_sqr_diff 100))
            )