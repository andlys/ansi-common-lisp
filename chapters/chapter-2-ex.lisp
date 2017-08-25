;access the third element of list
(defvar lst (list 'a 'b 'c 'd 'e))
(defvar x)
(setq x (third lst))
(print x)
(setq x (car (cdr (cdr lst))))
(print x)
(setq x (caddr lst))
(print x)

;;; ex-1.c
(list (and (listp 3) t) (+ 1 2))
;;; ex-2
(cons 'a '(b c))
(cons 'a (cons 'b '(c)))
(cons 'a (cons 'b (cons 'c nil)))

;;; ex-3
(defun my-fourth (lst) (cadddr lst))
(my-fourth '(1 2 3 4 5))

;;; ex-4
(defun bigger (a b)
    (if (> a b)
        a
        b))
(bigger 4 5)
(bigger -30 -20)

;;; ex-5a
;;; checks whether a list x is not empty and contains at least one NIL value
(defun enigma (x)
    (and (not (null x))
         (or (null (car x))
             (enigma (cdr x)))))
(enigma '())
(enigma '(a))
(enigma '(a b))
(enigma '(nil))
(enigma '(nil nil))
(enigma '(a nil))
(enigma '(nil a))
(enigma '(a b c nil))
;;; ex-5b
;;; returns index number of an object x from a list y
;;; if nothing found returns nil
(defun mystery (x y)
    (if (null y)
        nil
        (if (eql (car y) x)
            0
            (let ((z (mystery x (cdr y))))
                (and z (+ z 1))))))


;;; ex-6.a
(car (car (cdr '(a (b c) d))))
;;; ex-6.b
(or 13 (/ 1 0))
;;; ex-6.c
(apply #'list 1 nil)

;;; ex-7
(defun check (lst)
    (if (null lst)
        nil
        (or (listp (car lst))
            (check (cdr lst)))))
(check (list 'a 'b 'c))
(check nil)
(check (list 'a '(b) 'c))
(check (list '(a) 'b 'c))
(check (list 'a 'b '(c)))
;;; ex-8.a
(defun commas-rec (n)
    (when (> n 0)
        (format t ", ")
        (commas-rec (- n 1))))

(defun dots-iter (n)
    (loop for i from 1 to n do
        (format t ". ")))

(defun test (lim &rest fnames)
    (dolist (fun fnames)
        (dotimes (n lim)
            (print n)
            (funcall fun n))))

(test 5 #'dots-iter
        #'commas-rec)

;;; ex-9
(defun summit (lst)
    (let ((new (remove-if #'null lst)))
        (apply #'+ new)))
(summit '(nil 1 nil 2 nil 3 nil))
(defun summit (lst)
    (when (null lst) (return-from summit 0))
    (let ((x (car lst)))
        (if (null x)
            (summit (cdr lst))
            (+ x (summit (cdr lst))))))
(summit '(1))
(summit '(nil 1 nil 2 nil 3 nil))

