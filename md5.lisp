(defpackage :liang.rannger.md5
  (:use :common-lisp)
  (:export :md5))

(defvar *r-list* '(7  12  17  22   7  12  17  22   7  12  17  22   7  12  17  22 
		   5   9  14  20   5   9  14  20   5   9  14  20   5   9  14  20
		   4  11  16  23   4  11  16  23   4  11  16  23   4  11  16  23
		   6  10  15  21   6  10  15  21   6  10  15  21   6  10  15  21))

(defvar *k-list* (loop for i from 0 to 63 collect 
		      (floor (* (expt 2 32) (abs (sin (+ i 1)))))))

(defun byte-to-dword (lst)
  (let ((result 0))
    (dolist (item lst result)
      (incf result (+ (* (expt 2 (* 8 (position item lst))) item))))))

(defun string-to-code-list (string)
  (loop for x across string collect (char-code x)))

(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

(defmacro nappend (lst1 lst2)
  (let ((lst1-sym (gensym))
	(lst2-sym (gensym)))
    `(let ((,lst2-sym ,lst2) (,lst1-sym ,lst1))
       (setf ,lst1 (append ,lst1-sym ,lst2-sym)))))

(defun left-rotate (num rotate-bit &optional (length-in-bit 32))
  (if (and (numberp num) (numberp rotate-bit))
	   (let ((number (byte-to-dword (reverse (loop for i from 0 to (1- (/ 32 8)) collect (ldb (byte 8 (* 8 i)) num))))))
	     (let ((right-part (ldb (byte rotate-bit (- length-in-bit rotate-bit)) number))
		   (left-part (ldb (byte (- length-in-bit rotate-bit) 0) number)))
	       (+ right-part (* (expt 2 rotate-bit) left-part))))))

(defun pre-processing (lst)
  (let ((ret-list (copy-list lst)))
    (nappend ret-list '(#x80))
    (while (or (/= (mod (length ret-list) 64) 56) (<= (length ret-list) 56))
      (nappend ret-list '(0)))
    (let ((len (* (length lst) 8)))
	  (nappend ret-list (loop for x from 0 to 7 
			       collect (ldb (byte 8 (* x 8)) len))))))

(defun combo-list (lst &optional (sublist-length 4))
	     (if (or (null lst) (< (length lst) sublist-length))
		 lst
		 (append (list (subseq lst 0 sublist-length)) (combo-list (subseq lst sublist-length) sublist-length))))

(defmacro 32integer (num)
  `(ldb (byte 32 0) ,num))



(defun md5 (string)
  (let ((512bit-list (combo-list (pre-processing (string-to-code-list string)) 64))
	(a #x67452301)
	(b #xEFCDAB89)
	(c #x98BADCFE)
	(d #x10325476)
	(f 0)
	(g 0)
	(h0 #x67452301)
	(h1 #xEFCDAB89)
	(h2 #x98BADCFE)
	(h3 #x10325476))
    (loop for lst in 512bit-list 
	 do(let ((dword-list (mapcar #'reverse  (combo-list lst))))
		  (dotimes (i 63)
		    (cond 
		      ((and (<= 0 i) (<= i 15))
		       (setf f (32integer (logior (logand b c) (logand (lognot b) d))))
		       (setf g i))
		      ((and (<= 16 i) (<= i 31))
		       (setf f (32integer (logior (logand d b) (logand (lognot d) c))))
		       (setf g (mod (1+ (* 5 i)) 16)))
		      ((and (<= 32 i) (<= i 47))
		       (setf f (32integer (logxor b c d)))
		       (setf g (mod (+ 5 (* 3 i)) 16)))
		      ((and (<= 48 i) (<= i 63))
		       (setf f (32integer (logxor c (logior b (lognot d)))))
		       (setf g (mod (* 7 i) 16))))
		    (let ((temp (32integer d)))
		      (setf d (32integer c))
		      (setf c (32integer b))
		      (setf b (32integer (+ b  (left-rotate 
					       (+ a f (elt *k-list* i) (byte-to-dword (elt dword-list g))) 
					       (elt *r-list* i)))))
		      (setf a (32integer temp))))
		  (setf h0 (32integer (+ h0 a)))
		  (setf h1 (32integer (+ h1 b)))
		  (setf h2 (32integer (+ h2 c)))
		  (setf h3 (32integer (+ h3 d)))))
    (format nil "~8x ~8x ~8x ~8x" h0 h1 h2 h3)))
	       
      
