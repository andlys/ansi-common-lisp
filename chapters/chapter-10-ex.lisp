;; ex-1
(let ((x 'a)
      (y 'b)
      (z '(c d)))
 (list `(,z ,x z)
       `(x ,y ,@z)
       `((,@z ,x) z)))

;; ex-2
(defmacro iff (test then &optional else)
  `(let ((val ,test))
     (cond (val ,then)
	   (t ,else))))

(pprint (macroexpand-1
	   (iff (/= 2 2)
	       (oddp 2)
               (oddp 3))))
;; variable capture example
(iff (find 'a '(a b c))
     val ; captured from macro
     'nothing-found)
(iff (= 2 3)
     'success)
(iff (= 2 3)
     'success
     'failure)

;; ex-3
(defmacro nth-expr (n &rest args)
  `(eval (nth (- ,n 1) '(,@args)))) ; TODO rewrite this without eval function

(defun watch () (print 'evaluated!))
(let ((y 2))
  (macroexpand-1 '(nth-expr y (watch) (+ 1 3) 'value)))
(let ((x 2))
  (nth-expr x (watch) (+ 1 3) (watch) 'value))
(let ((n 2))
  (nth-expr n (/ 1 0) (+ 1 2) (/ 1 0)))
(do ((y 1 (incf y)))
    ((= y 6) 'done)
  (print (nth-expr y (+ 40 1) 'value (exp 2) (expt 2.713 2) (watch))))

;; ex-4
(defmacro ntimes (n &rest body)
  (if (<= n 0)
      nil
      `(progn (ntimes ,(1- n) ,@body)
	     ,@body)))

(pprint (macroexpand-1 '(ntimes 3 (print 'hello))))
(ntimes 5 (print "<br"))
(let ((n 10))
  (ntimes 5
    (print (incf n))))

;; ex-5
(defmacro n-of (n expr)
  (let ((i (gensym))
	(expr2 (gensym)))
    `(loop for ,i from 0 to ,n
	until (> (setf ,expr2 ,expr)
		 ,n)
	collect ,expr2))))

(let ((i 0)
      (n 4))
  (macroexpand-1 '(n-of n (incf i))))
(let ((i 0)
      (n 4))
  (n-of n (incf i)))

;; ex-6
;; TODO: redefine, this does not work
(defmacro ensure (vars &rest body)
  ;`(setf ,(car vars) 0)) ; works ok
  (let ((setter (gensym))
	(sym (gensym))
	`(let ((setter (loop for sym in ',vars
			     append (list sym ,sym))))
	   (print setter)))))

(let ((i 10)
      (j 15)
      (k 42))
  (macroexpand-1 '(ensure (i j) (setf i))))
(let ((i 10)
      (j 15)
      (k 42))
  (ensure (i j k) (setf i 0)) ; TODO correct the error 
  (format t "i: ~A; j: ~A; k: ~A" i j k))

(defmacro test1 (args)
  (let ((vals (gensym))
	(w (gensym)))
    `(let ((,vals (copy-list ,args)))
       (print (loop for elt in ,args
		 collect elt))
       (print (let ((,w (car ,args)))) ,w)
       (format t "3arg: ~A ~%" ,vals)
       )))

(defmacro test2 (args)
    `(let ((w (car ,args)))
       (print w)))

(defmacro test3 (args &rest body)
;  `(setf ,(car args) ,(cadr args)))
;   `(format t "~A is set to ~A ~%" (car '(,@args)) ,(car args)))
  (let ((restorer (gensym)))
  `(let ((,restorer (loop for sym in '(,@args)
	                  for val in (list ,@args)
		       collect `(setf ,sym ',val))))
     ,@body
     ;,(progn restorer))))
     (print ,restorer)
     ,(car (list restorer)))))
     

(let ((var 'some-symbol)
      (n 42))
  (macroexpand-1 (test3 (var n)
	 (setf var 'qwe)
	 (setf n 0))))
(let ((var 'some-symbol)
      (n 42))
  (macroexpand-1 '(test3 (var n)
	 (setf var 'qwe)
	 (setf n 0)))
  (values var n))
(let ((var 'some-symbol)
      (n 42))
  (test3 (var n)
	 (setf var 'qwe)
	 (setf n 0))
  (values var n))
;; ex-7
;; TODO find the reason why this definition is bad compared to the real push
;; The only difference I noticed so far is using setf vs setq...
(defmacro my-push (obj lst)
  `(setf ,lst (cons ,obj ,lst)))

lst
(defparameter lst '(b c))
(cons lst (cons lst nil))
(setf lst (cons lst lst))
(pprint (macroexpand-1 '(my-push 'a lst)))
(my-push 'a (cdr lst))
(push 'a (cdr lst))

;; ex-8
(defmacro my-double (x)
  `(setf ,x (* 2 ,x)))

(let ((x -21))
  (my-double x)
  x)
