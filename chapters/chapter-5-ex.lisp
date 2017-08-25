;;; ex-1a

(defparameter y '(a b c))
(let ((x (car y)))
    (cons x x))

(do ((x (car y)))
    (t (cons x x)))

;;; ex-1b
(defparameter x '(10 20 30))
(defparameter z 100)

(let* ((w (car x))
       (y (+ w z)))
    (cons w y))

(do* ((w (car x))
     (y (+ w z)))
    (t (cons w y)))

;;; ex-2

;;; old
(defun mystery (x y)
    (if (null y)
        nil
        (if (eql (car y) x)
            0
            (let ((z (mystery x (cdr y))))
                (and z (+ z 1))))))

;;; new
(defun mystery (obj lst)
    (cond ((null lst) nil)
          ((eql (car lst) obj) 0)
          (t (let ((z (mystery obj (cdr lst))))
                (and z (+ z 1))))))

(mystery 5 '(1 2 3))
(mystery 5 '(1 2 5))

;;; ex-3
(defun square (n)
    (cond ((and (<= n 5)
                (>= n 0)) n)
          (t (expt n 2))))

(square 4)
(square 5)
(square 6)
(square 7)
(square -4)

;;; ex-5
(defun precedes-iter (x v)
    (do ((i (- (length v) 1) (- i 1))
         (j (- (length v) 2) (- j 1))
         (res nil))
        ((< i 1) res)
        (when (eq x (aref v i))
            (setf res (adjoin (aref v j) res)))))

(defun precedes-rec (x v &key (res nil) (i (- (length v) 1)))
    (cond ((< i 1) res)
          ((eq x (aref v i))
           (precedes-rec x v
                :res (adjoin (aref v (- i 1)) res)
                :i (- i 1)))
          (t (precedes-rec x v
                :res res
                :i (- i 1)))))

(precedes-rec #\a "abracadabra")
(precedes-iter #\a "abracadabra")

;;; ex-6
(defun intersperse-iter (obj lst)
    (let (res)
        (dolist (elt lst)
            (setf res (append res (list elt) (list obj))))
        (butlast res)))

(defun intersperse-rec (obj lst)
    (if (consp (cdr lst))
        (cons (car lst)
            (cons obj
                  (intersperse-rec obj (cdr lst))))
        (cons (car lst) nil)))

(intersperse-rec '- '(a b c d))
(intersperse-iter '- '(a b c d))

;;; ex-7a
(defun diff-rec (lst)
    (cond ((not (consp (cdr lst))) t)
          ((/= 1 (abs (- (first lst) (second lst))))
                nil)
          (t (diff-rec (cdr lst)))))

(diff-rec '(3 4 5 6))
(diff-rec '(6 5 4 3))
(diff-rec '(3 4 5 7))
(diff-rec '(7 5 4 3))

;;; ex-7b
(defun diff-do (lst)
    (do ((tmp lst (cdr tmp)))
        ((not (consp (cdr tmp)))
            t)
        (if (/= 1 (abs (- (first tmp) (second tmp))))
            (return-from diff-do nil))))

(diff-do '(3 4 5 6))
(diff-do '(6 5 4 3))
(diff-do '(3 4 5 7))
(diff-do '(7 5 4 3))

;;; ex-7c
(defun diff-mapc (lst)
    (mapc #'(lambda (x y)
                    (when (/= 1 (abs (- x y)))
                        (return-from diff-mapc nil)))
          (butlast lst)
          (cdr lst))
    t)

(diff-mapc '(3 4 5 6))
(diff-mapc '(6 5 4 3))
(diff-mapc '(3 4 5 7))
(diff-mapc '(7 5 4 3))

;;; ex-8
(defun minmax (v &key (i 0) (min (aref v 0)) (max (aref v 0)))
    (if (>= i (length v))
        (values min max)
        (let ((elt (aref v i)))
            (when (> elt max)
                (setf max elt))
            (when (< elt min)
                (setf min elt))
            (minmax v :i (+ i 1) :min min :max max))))

(minmax #(6 3 7 2 8 1 3 -1 0))

