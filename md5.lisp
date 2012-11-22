;(defpackage :liang.rannger.md5
;  (:use :common-lisp)
;  (:export :md5))

(defparameter *r-list* '(7  12  17  22   7  12  17  22   7  12  17  22   7  12  17  22 
			 5   9  14  20   5   9  14  20   5   9  14  20   5   9  14  20
			 4  11  16  23   4  11  16  23   4  11  16  23   4  11  16  23
			 6  10  15  21   6  10  15  21   6  10  15  21   6  10  15  21))

;(defvar *k-list* (loop for i from 0 to 63 collect 
;		      (floor (* (expt 2 32) (abs (sin (+ i 1)))))))

(defparameter *k-list* '(#xd76aa478 #xe8c7b756 #x242070db #xc1bdceee #xf57c0faf #x4787c62a #xa8304613 #xfd469501 #x698098d8
			 #x8b44f7af #xffff5bb1 #x895cd7be #x6b901122 #xfd987193 #xa679438e #x49b40821 #xf61e2562 #xc040b340
			 #x265e5a51 #xe9b6c7aa #xd62f105d #x2441453  #xd8a1e681 #xe7d3fbc8 #x21e1cde6 #xc33707d6 #xf4d50d87 
			 #x455a14ed #xa9e3e905 #xfcefa3f8 #x676f02d9 #x8d2a4c8a #xfffa3942 #x8771f681 #x6d9d6122 #xfde5380c
			 #xa4beea44 #x4bdecfa9 #xf6bb4b60 #xbebfbc70 #x289b7ec6 #xeaa127fa #xd4ef3085 #x4881d05 #xd9d4d039
			 #xe6db99e5 #x1fa27cf8 #xc4ac5665 #xf4292244 #x432aff97 #xab9423a7 #xfc93a039 #x655b59c3 #x8f0ccc92
			 #xffeff47d #x85845dd1 #x6fa87e4f #xfe2ce6e0 #xa3014314 #x4e0811a1 #xf7537e82 #xbd3af235 #x2ad7d2bb #xeb86d391))

(defun byte-to-dword (lst)
  (destructuring-bind (byte1 byte2 byte3 byte4) lst
    (logior (ash byte4 24) (ash byte3 16) (ash byte2 8) byte1)))

(defun string-to-code-list (string)
  (loop for x across string collect (char-code x)))

(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

(defun left-rotate (num rotate-bit &optional (length-in-bit 32))
  (if (and (numberp num) (numberp rotate-bit))
      (logior (ldb (byte length-in-bit 0) (ash num rotate-bit)) (ash num (- rotate-bit length-in-bit)))))

(defmacro nappend (lst1 lst2)
  (let ((lst1-sym (gensym))
	(lst2-sym (gensym)))
    `(let ((,lst2-sym ,lst2) (,lst1-sym ,lst1))
       (setf ,lst1 (append ,lst1-sym ,lst2-sym)))))

(defun pre-processing (lst)
  (let ((ret-list (copy-list lst)))
    (nappend ret-list '(#x80))
    (while (or (/= (mod (length ret-list) 64) 56) (<= (length ret-list) 56))
      (nappend ret-list '(0)))
    (append ret-list (loop for i from 0 to 7 collect (ldb (byte 8 (* i 8)) (* 8 (length lst)))))))

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
	 do(let ((dword-list (combo-list lst)))
	     (format t "~a" (length dword-list))
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
