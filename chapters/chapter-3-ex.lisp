;;; ex-2
(defun new-union (lst1 lst2)
    (let ((new nil))
        (dolist (elem lst1)
            (setf new (append new (list elem))))
        (if (not (car new)) (pop new))
        (dolist (elem lst2)
            (if (not (member elem new))
                (setf new (append new (list elem)))))
        new))

(print (new-union '(a b c) '(b a d)))

;;; ex-3
(defun occurrences (lst)
    (let ((new nil))
        (dolist (elem lst)
            (if (null (assoc elem new))
                (setf new (append 
                                new 
                                (list (cons elem 1))))
                (setf (cdr (assoc elem new))
                        (+ (cdr (assoc elem new)) 1))))
        (sort new #'(lambda (pair1 pair2)
                    (>= (cdr pair1) (cdr pair2))))))

(print (occurrences '(a b a d a c d c a)))

;;; ex-4
(member '(a) '((a) (b)) :test #'equal)

;;; ex-5
;(pos+ '(7 5 1 4))
(defun pos+itr (lst)
    (let ((new (copy-list lst)))
        (dotimes (n (length lst))
            (setf (nth n new) (+ (nth n new) n)))
        new))
(print (pos+itr '(7 5 1 4)))

(defun pos+rec (lst &optional depth)
    (when (null depth) (setq depth 0))
    (if (null lst)
        nil
        (cons (+ (car lst) depth)
                (pos+rec (cdr lst) (+ depth 1)))))
(print (pos+rec '(7 5 1 4)))

(defun pos+mpc (lst)
    (mapcar #'(lambda (elem)
                (+ elem (position elem lst)))
            lst))
(print (pos+mpc '(7 5 1 4)))
