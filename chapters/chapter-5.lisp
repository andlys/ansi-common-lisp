(block head
    (format t "Here we go")
    (return-from head 'idea)
    (format t "We'll never see this."))

(defun our-member (obj lst)
    (if (atom lst)
        nil
        (if (eql obj (car lst))
            lst
            (our-member obj (cdr lst)))))

(defun our-member2 (obj lst)
    (cond ((atom lst) nil)
          ((eql obj (car lst)) lst)
          (t (our-member2 obj (cdr lst)))))

(defun month-length (mon)
    (case mon
        ((jam mar may jul aug oct dec) 31)
        ((apr jun sept nov) 30)
        (feb (if (leap-year) 29 28))
        (otherwise "unknown month")))
        
(defun leap-year () nil)

(month-length 'apr)

(dolist (x '(a b c d e) 'done)
    (format t "~A " x))

(dotimes (x 5 x)
    (format t "~A " x))

(defun factorial (n)
    (do ((j n (- j 1))
         (f 1 (* f j)))
        ((= j 0) f)))
(factorial 5)

(mapc #'(lambda (x y)
        (format t "~A ~A  " x y))
    '(hip flip slip)
    '(hop flop slop))

(get-decoded-time)

(values 'a nil (+ 2 4))

(multiple-value-bind (x y z) (values 1 2)
    (list x y z))

(multiple-value-bind (s m h day month year) (get-decoded-time)
    (format t "Today it's ~A-~A-~A ~%" year month day))

(multiple-value-call #'* (values 1 2 3 4))

(multiple-value-list (values 'a 'b 'c))

(defun super ()
    (catch 'abort
        (sub)
        (format t "We'll never see this.")))

(defun sub ()
    (throw 'abort 99))

(progn
    (error "Oos!")
    (format t "After the error."))


(defparameter x 1)
(catch 'abort
    (unwind-protect
        (throw 'abort 99)
        (setf x 2)))
x

(defparameter mon '(31 28 31 30 31 30 31 31 30 31 30 31))
(apply #'+ mon)

(defun my-sum (lst) (apply #'+ lst))

;; java
;int[] mon = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
;int total = 0;
;(for days : mon)
    ;total += days;

(defparameter nom (reverse mon))
(defparameter sums (maplist #'(lambda (x)
                                (apply #'+ x))
                            nom))

(defparameter month (make-array 13 :initial-element 0))
(do ((i 1 (+ i 1))
     (j 11 (- j 1)))
    ((= i 13) month)
    (setf (aref month i) (nth j sums)))

(defconstant yzero 2000)

(defun leap? (y)
    (and (zerop (mod y 4))
         (or (zerop (mod y 400))
             (not (zerop (mod y 100))))))

(defun date->num (d m y)
    (+ (- d 1) (month-num m y) (year-num y)))

(defun month-num (m y)
    (+ (svref month (- m 1))
        (if (and (> m 2) (leap? y)) 1 0)))

(defun year-num (y)
    (let ((d 0))
        (if (>= y yzero)
            (dotimes (i (- y yzero) d)
                (incf d (year-days (+ yzero i))))
            (dotimes (i (- yzero y) (- d))
                (incf d ((year-days (+ y i))))))))

(defun year-days (y) (if (leap? y) 366 365))


(mapcar #'leap? '(1904 1900 1600))


;(multiple-value-list (date+ 17 12 1997 60))

