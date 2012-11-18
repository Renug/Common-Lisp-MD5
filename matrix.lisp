(defpackage :com.matrix.lx
  (:use :common-lisp)
  (:export :make-matrix
	    :init-matrix
	    :print-matrix
	    :matrix-length
	    :matrix+
	    :matrix-
	    :matrix*))

(in-package :com.matrix.lx)
	    

(defun make-matrix (row col)
  (make-list row :initial-element (make-list col :initial-element 'ele)))

(defun init-matrix (matrix)
  (labels ((set-element-value (lst)
			      (if (listp lst)
				  (mapcar #'set-element-value lst)
				(setf lst (random 20)))))
  (set-element-value matrix)))

(defun print-matrix (matrix)
  (mapcar #'(lambda (x) (mapcar #'(lambda (x) (format t "~2d "x)) x) 
	      (format t "~%")) matrix))

(defun matrix-length (matrix)
    (values (length matrix) (length (elt matrix 0))))

(defun matrix+ (matrix1 matrix2)
    (labels ((row (lst1 lst2)
		  (mapcar #'(lambda (x y) (+ x y)) lst1 lst2))
	     (v (m1 m2)
		(mapcar #'row m1 m2)))
	    (v matrix1 matrix2)))

(defun matrix- (matrix1 matrix2)
    (labels ((row (lst1 lst2)
		  (mapcar #'(lambda (x y) (- x y)) lst1 lst2))
	     (v (m1 m2)
		(mapcar #'row m1 m2)))
	    (v matrix1 matrix2)))


(defun matrix* (matrix1 matrix2)
  (if (/= (nth-value 1 (matrix-length matrix1)) (nth-value 0 (matrix-length matrix2)))
      (progn (print "error")
	     (return-from matrix* nil)))
  (labels ((calc-value (lst-row lst-col)
		  (let ((val 0) (lst (mapcar #'(lambda (x y) (* x y)) lst-row lst-col)))
		    (dolist (temp lst val)
		      (setf val (+ val temp)))))
	   (row (pos)
		(copy-list (nth pos matrix1)))
	   (col (pos)
		(mapcar #'(lambda (lst) (nth pos lst)) matrix2))
	   (make-row (pos)
		     (let ((retval '()) (r (row pos)) (count (nth-value 1 (matrix-length matrix2))))
		       (dotimes (index  count (nreverse retval))
			 (push (calc-value r (col index)) retval)))))
	  (let ((retval '()) (count (nth-value 0 (matrix-length matrix1))))
	    (dotimes (index count (nreverse retval))
	      (push (make-row index) retval)))))
