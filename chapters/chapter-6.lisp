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

(let ((*print-base* 16))
    (princ 32))

(defun foo (x) (+ x 1))
(compiled-function-p #'foo)

(compile 'foo)

