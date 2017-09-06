;

(labels ((add2 (x) (+ x 2)))
    (add2 5))

(defun philosoph (thing &optional property)
    (list thing 'is property))

(philosoph 'death)

(defun philosoph (thing &optional (property 'fun))
    (list thing 'is property))

(philosoph 'death)

(defun keylist (a &key x y z)
    (list a x y z))

(keylist 1 :y 2)

(defun map-int (fn n)
    (let (acc)
        (dotimes (i n)
            (let ((val (funcall fn i)))
                (push val acc)))
    (nreverse acc)))

(map-int #'identity 10)
(map-int #'(lambda (x) (random 100))
         10)

;;; generalized version of remove-if
(defun filter (fn lst)
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
        (if val (push val acc))))
    (nreverse acc)))

(filter #'(lambda (x)
            (and (evenp x) (+ x 10)))
        '(1 2 3 4 5 6 7))

(defun combiner (x)
    (typecase x
        (number #'+)
        (list   #'append)
        (t      #'list)))

(defun combine (&rest args)
    (apply (combiner (car args))
           args))

(combine 2 3)
(combine '(a b) '(c d))

(destructuring-bind (obj1 . rest) (list 'a 'b 'c 'd 'e)
    (values obj1 rest))

(funcall (constantly 42) '(1 2 3 4) 5 6 7)

(defun our-complement (func)
  #'(lambda (&rest args)
      (not (apply func args))))
(mapcar (our-complement #'oddp) '(1 2 3 4 5 6))

;; several closures that share a variable
(let ((counter 0))
  (defun my-reset ()
    (setf counter 0))
  (defun my-inc()
    (incf counter)))

(list (my-inc) (my-inc) (my-reset) (my-inc))

(mapcar (complement #'oddp) '(1 2 3 4 5 6))


;; compose function returns a function that calls sequentially a set of
;; functions on some data
;; works like reduce. implemented by reduce
;; (compose #'f1 #'f2 #'f3 data)
;; is equivalent to
;; (lambda (data) (f1 (f2 (f3 data))))
(defun compose (&rest funcs)
  #'(lambda (&rest args)
      (reduce #'(lambda (v f) (funcall f v))
              (cdr (reverse funcs))
              :initial-value (apply (car (reverse funcs)) args))))

(mapcar (compose #'list #'round #'sqrt)
        '(4 9 16 25))
(mapcar (compose #'oddp #'car #'list #'round #'sqrt)
        '(4 9 16 25))
(defun get-last (lst)
  (funcall (compose #'car #'reverse) lst))
(get-last '(a b c d e f))

;; curry function returns a function that calls some function to which
;; many (nested) arguments are applied
;; TODO
(defun curry (fn &rest args)
  #'(lambda (&rest args2)
      (apply fn (append args args2))))

(funcall (curry #'list 'a 'b 'c) 'd)

(defun disjoin (fn &rest fns)
  (if (null fns)
      fn
      (let ((disj (apply #'disjoin fns)))
        #'(lambda (&rest args)
            (or (apply fn args) (apply disj args))))))

(funcall (disjoin #'< #'=) 3 5)
(funcall (disjoin #'< #'=) 5 5)
(funcall (disjoin #'< #'=) 6 5)
(mapcar (disjoin #'integerp #'symbolp)
       '(a "a" 2 3 (a b c)))

(let ((*print-base* 16))
    (princ 32))

(defun foo (x) (+ x 1))
(compiled-function-p #'foo)

(compile 'foo)
