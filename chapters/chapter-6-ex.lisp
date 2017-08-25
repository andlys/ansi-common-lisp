;;; ex-3
(defun len (&rest args) (length args))

(len)
(len nil)
(len 'a)
(len 'a 'b)
(len 'a 'b 'c)

