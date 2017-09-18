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
  `(case ,n
     ,@(let ((i 0))
       (mapcar #'(lambda (expr)
		   `(,(incf i) ,expr))
	       args)))))

(defun watch () (print 'evaluated!))
(let ((y 2))
  (macroexpand-1 '(nth-expr y (watch) (+ 1 3) 'value)))
(let ((x 2))
  (nth-expr x (watch) (+ 1 3) (watch) 'value))
(let ((n 2))
  ;; this evaluates without any errors despite division by zero
  ;; the division is not evaluated because we are interested only
  ;; in n-th expression!
  (nth-expr n (/ 1 0) (+ 1 2) (/ 1 0)))

;; ex-4
(defmacro ntimes (n &rest body)
  (if (>= n 1)
      `(progn (ntimes ,(1- n) ,@body)
	      ,@body)))

(macroexpand-1 '(ntimes 3 (print 'hello)))
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
(defmacro save-vars (vars &rest body)
  `(let (,@(mapcar #'(lambda (var)
		       `(,var ,var))
		   vars))
     ,@body))

(let ((var 'some-symbol)
      (n 42)
      (fun (lambda (n) (+ n 1))))
  (macroexpand-1 '(save-vars (var n)
	 (setf var 'qwe)
	 (setf n 0))))
(let ((var 'some-symbol)
      (n 42)
      (fun (lambda (n) (+ n 1)))
      (unsaved 'possession))
  (save-vars (var n fun)
	 (setf var 'qwe)
	 (setf n 0)
	 (dotimes (n 5)
	   (incf n))
	 (setf fun 'hiking)
	 (setf unsaved 'redefined)
	 (format t "inside block of code: n = ~A; fun = ~A ~%" n fun))
   (values var n fun unsaved))

;; ex-7
;; TODO find the reason why this definition is bad compared to the real push
;; The only difference I noticed so far is using setf vs setq...
(defmacro my-push (obj lst)
  `(setf ,lst (cons ,obj ,lst)))

lst
(defparameter lst (list 'b 'c))
(cons lst (cons lst nil))
(setf lst (cons lst lst))
(macroexpand-1 '(my-push 'a lst))
(my-push 'a (cdr lst))
(push 'a (cdr lst))
(macroexpand-1 '(push 'a lst))
lst

;; ex-8
(defmacro my-double (x)
  `(setf ,x (* 2 ,x)))

(let ((x -21))
  (my-double x)
  x)
