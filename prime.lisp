(defun prime-before-number (number)
  "求在number之前所有的素数"
  (let ((primes ()))
    (labels ((is-a-prime (num)
		 (loop for x from 2 to (- num 1)
		    do (if (= (mod num x) 0)
			   (return-from is-a-prime nil)))
		 num))
    (loop for x from 2 to number
	 do (if (is-a-prime x)
		(push x primes))))
    (sort primes #'<)))

(defmacro while (test &body body)
  `(do ()
       (,test)
     ,@body))

(defun prime-factorization (number)
  "质因数分解"
  (let ((primes-list (prime-before-number number))
	(prime-factorization-list ()))
    (let ((product number))
      (while (\= 1 product)
	(dolist (num primes-list)
	  (if (= 0 (mod product num))
	      (progn
		(push num prime-factorization-list)
		(setf product (/ product num))))))
      prime-factorization-list)))