;;; ex-2B
(defun my-copy-list (lst)
    (reduce #'(lambda (a b) (cons a b))
            lst
            :from-end t
            :initial-value nil))

(my-copy-list '(a b c d))

;;; ex-2B
(defun my-reverse (lst)
    (reduce #'(lambda (a b) (append b (list a)))
            lst
            :from-end t
            :initial-value nil))

(my-reverse '(a b c d))

;;; ex-4
(load "ch4-tree.lisp")
(defparameter tree nil)
(dolist (n '(5 8 4 2 1 9 6 3))
    (setf tree (bst-insert n tree #'<)))

(defun run (tree)
    (if tree
        (let ((lst nil))
            (setf lst (append
                (run (node-right tree))
                (list (node-value tree))
                (run (node-left tree))))
            lst)))

(run tree)

;;; ex-6 A & B
(defun print-hash-entry (key value)
    (format t "~S -> ~S~%" key value))

(defun assoc2hash (assoc-list)
    (let ((res (make-hash-table :size (length assoc-list))))
        (dolist (pair assoc-list)
            (setf (gethash (first pair) res) (cdr pair)))
        res))

(defun hash2assoc (hash)
    (let ((tmp nil))
        (maphash #'(lambda (k v)
                 (setf tmp (append tmp (list (cons k v)))))
                 hash)
        tmp))

(defparameter assoc-list '(("Andrii" . 19) ("Bob" . 27) ("Alice" . 16)))
(defparameter ht (assoc2hash assoc-list))
(print assoc-list)
(terpri)
(maphash #'print-hash-entry ht)
(print (hash2assoc ht))


