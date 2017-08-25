(defvar l (list 'a 'b 'c 'd 'e))

(car l)

(cdr l)

(car (cdr (cdr l)))

(third l)

(listp '(a b c))

(not (listp 1))

(and (member 'a l) t)

(defun is-even(n)
    (if (numberp n)
        (= (mod n 2) 0)
        nil))

(member 'f '('a 'b 'c 'd 'e 'f 6 7 8))

(member-if #'is-even '('a 'b 'c 'd 'e 'f))

(print (is-even 10))
(print (is-even 9))
(print (is-even 'a))

(null '())

(null '(a))

(defun our-member(obj lst)
    (if (null lst)
        nil
        (if (eql obj (car lst))
            lst
            (our-member obj (cdr lst)))))

(terpri)
(print (our-member 'a '(e c b a)))
(print (our-member 'b '(a b c)))
(print (our-member 'z '(a b c)))


(format t "~A plus ~A equals ~A.~%" 2 3 (+ 2 3))


(defun askem (string)
    (format t "~A~%" string)
    (let ((n (read)))
        (if (not (numberp n))
            (askem string)
            n)))

;(askem "How old are you?")


(defparameter *glob* 99)
(defconstant limit (+ *glob* 1))
(boundp `*glob*)

(setf lst (list 'a 'b 'c))
(setf (car lst) 'n)

(setf a 'aaa
      b 'bbb
      c 'ccc)


(defun my-reverse(lst)
    (let ((new-lst '()))
        (do ((n 0 (+ n 1)))
             ((> n (- (length lst) 1)) new-lst)
             (setf new-lst (cons (nth n lst) new-lst)))))

(print (my-reverse '(h e l l o)))


(function +)
#'+
(apply #'+ '(1 2 3))
(funcall #'+ 1 2 3)

(load "test.lisp")
(func 'a 'b)



(defvar sym (read))
(apply sym '(1 2 3 4 5))
; #'max
; #'+
; #'*
