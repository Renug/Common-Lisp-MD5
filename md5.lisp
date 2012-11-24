(defpackage :liang.rannger.md5
  (:use :common-lisp)
  (:export :md5))

(in-package :liang.rannger.md5)

(defparameter *r-list* '(7  12  17  22   7  12  17  22   7  12  17  22   7  12  17  22 
			 5   9  14  20   5   9  14  20   5   9  14  20   5   9  14  20
			 4  11  16  23   4  11  16  23   4  11  16  23   4  11  16  23
			 6  10  15  21   6  10  15  21   6  10  15  21   6  10  15  21))

(defparameter *k-list* (loop for i from 1 to 64 collect 
		      (truncate (* (expt 2 32) (abs (sin (float i 0.0d0)))))))

(defun byte-to-dword (lst)
  (destructuring-bind (byte1 byte2 byte3 byte4) lst
    (logior (ash byte4 24) (ash byte3 16) (ash byte2 8) byte1)))

(defun string-to-code-list (string)
  (loop for x across string collect (char-code x)))

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
    (loop while (/= (mod (length ret-list) 64) 56) 
       do (nappend ret-list '(0)))
    (append ret-list (loop for i from 0 to 7 collect (ldb (byte 8 (* i 8)) (* 8 (length lst)))))))

(defun combo-list (lst &optional (sublist-length 4))
	     (if (or (null lst) (< (length lst) sublist-length))
		 lst
		 (append (list (subseq lst 0 sublist-length)) (combo-list (subseq lst sublist-length) sublist-length))))

(defmacro 32integer+ (&rest args)
  `(ldb (byte 32 0) (+ ,@args)))

(defun dword-to-byte (num)
  (list (ldb (byte 8 0) num) 
	(ldb (byte 8 8) num)
	(ldb (byte 8 16) num)
	(ldb (byte 8 24) num)))


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
		  (dotimes (i 64)
		    (cond 
		      ((and (<= 0 i) (<= i 15))
		       (setf f (logior (logand b c) (logandc1 b d)))
		       (setf g i))
		      ((and (<= 16 i) (<= i 31))
		       (setf f (logior (logand d b) (logandc1 d c)))
		       (setf g (mod (1+ (* 5 i)) 16)))
		      ((and (<= 32 i) (<= i 47))
		       (setf f (logxor b c d))
		       (setf g (mod (+ 5 (* 3 i)) 16)))
		      ((and (<= 48 i) (<= i 63))
		       (setf f (logxor c (logorc2 b d)))
		       (setf g (mod (* 7 i) 16))))
		      (let ((temp d))
			(setf d c)
			(setf c b)
			(setf b (32integer+ b  (left-rotate 
						  (32integer (+ a f (elt *k-list* i) (byte-to-dword (elt dword-list g)))) 
						  (elt *r-list* i))))
			(setf a temp)))
		  (setf h0 (32integer+ h0 a))
		  (setf h1 (32integer+ h1 b))
		  (setf h2 (32integer+ h2 c))
		  (setf h3 (32integer+ h3 d))))
    (append (dword-to-byte h0) (dword-to-byte h1) (dword-to-byte h2) (dword-to-byte h3))))


