;;; ex-3
(defun len (&rest args) (length args))

(len)
(len nil)
(len 'a)
(len 'a 'b)
(len 'a 'b 'c)

;;; ex-6
;; closure technique used
;; description: function that takes one number and returns the greatest number
;; passed to it so far
(let (lst)
  (defun highest (n)
    (reduce #'max (push n lst))))

(highest 0)
(highest 1)
(highest -1)
(highest 3)
(highest 2)

;;; ex-7
;; closure technique used
;; description: function that takes one number and returns true if it is
;; greater than the last number passed to it
(let (last)
  (defun greater-than-last? (n)
    (prog1 (> n (if last last n))
         (setf last n))))

(greater-than-last? 0)
(greater-than-last? -1)
(greater-than-last? 4)
(greater-than-last? 6)
(greater-than-last? 6)
(greater-than-last? 5)
