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
	   (iff (= 2 2)
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
  (let ((g (gensym))) 
    `(let ((,g (1- ,n)))
       (eval (nth ,g ',args))))) ; TODO: make this work without eval function

(defun watch () (print 'evaluated!))
(let ((n 2))
  (macroexpand-1 '(nth-expr n (watch) (+ 2 3) 'value)))
(let ((n 2))
  (nth-expr n (watch) (+ 1 3) (watch) 'value))
(let ((n 2))
  (nth-expr n (/ 1 0) (+ 1 2) (/ 1 0)))
(do ((y 1 (incf y)))
    ((= y 6) 'done)
  (print (nth-expr y (+ 40 1) 'value (exp 2) (expt 2.713 2) (watch))))

;; ex-5
(defmacro n-of (n expr)
  (let ((tmp (gensym))
	(expr2 (gensym)))
    `(loop for ,tmp from 0 to ,n
	until (> (setf ,expr2 ,expr)
		 ,n)
	collect ,expr2))))
	   
(let ((i 0)
      (n 4))
  (macroexpand-1 '(n-of n (incf i))))
(let ((i 0)
      (n 4))
  (n-of n (incf i)))
;; ex-7
;; TODO find the reason why this definition is bad
(defmacro my-push (obj lst)
  `(setf ,lst (cons ,obj ,lst)))

(defparameter lst '(b c))
(cons lst (cons lst nil))
(setf lst (cons lst lst))
(pprint (macroexpand-1 '(my-push (cons 'a lst) lst)))

;; ex-8
(defmacro my-double (x)
  `(setf ,x (* 2 ,x)))

(let ((x -21))
  (my-double x)
  x)
