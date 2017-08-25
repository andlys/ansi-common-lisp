(defparameter arr (make-array '(2 3) :initial-element nil))
(aref arr 0 0)
(setf (aref arr 0 0) 'b)
(setf *print-array* t)

(defparameter vec (make-array 4 :initial-element nil))

(vector "a" 'b 3)

(svref vec 0)

#| a multy-
line
comment
|#

;;;; Utilities for operations on sorted vector

;;; finds an element in a sorted vector
(defun bin-search (obj vec)
    (let ((len (length vec)))
        (if (zerop len)
            nil
            (finder obj vec 0 (- len 1)))))
(defun finder (obj vec start end)
    (format t "~A~%" (subseq vec start (+ end 1)))
    (let ((range (- end start)))
        (if (zerop range)
            nil
            (let ((mid (+ start (round (/ end 2)))))
                (let ((obj2 (aref vec mid)))
                    (if (> obj obj2)
                        (finder obj vec (+ mid 1) end)
                        (finder obj vec start (- mid 1))))))))
(bin-search 3 #(0 1 2 3 4 5 6 7 8 9))

(sort "elbow" #'char<)

(sort (list "bbb" "aaa" "ccc") #'string<)

(aref "abc" 1)

(char "abc" 1)

;; the first argument of 'format' operator defines whether string will go
;; to the output stream or to a resulting string object
(format nil "~A or ~A" "truth" "dare")

(identity "a")

(position #\a "fantasia")
(position #\a "fantasia" :start 3 :end 5)
(position #\a "fantasia" :from-end t)
(position 'a '((c d) (a b)))
(position 'a '((c d) (a b)) :key #'car)
(position 3 '(4 3 2 1) :test #'>)

(defun second-word (str)
    (let ((i (+ (position #\  str) 1)))
        (subseq str i (position #\  str :start i))))
(second-word "Form follows function")
(defun nth-word (n str)
    (let ((i 0))
        (loop
            (when (= n 1)
                (return-from nth-word (subseq str i (position #\  str :start i))))
            (decf n)
            (setq i (+ 1 (position #\  str :start i))))))
(nth-word 1 "Form follows function")

(position-if #'oddp '(2 3 4 5))

(defun fun (x) (setq x 42) (print x) )
(let ((a 1))
    (fun a)
    (print a))

(find #\a "cat")

(remove-duplicates "abracadabra")

(reduce #'intersection '((b r a d 's ) (b a d) (c a t)))

(defun tokens (str test start)
    (let ((p1 (position-if test str :start start)))
        (if p1
            (let ((p2 (position-if #'(lambda (chr)
                                            (not (funcall test chr)))
                                    str
                                    :start p1)))
                (cons (subseq str p1 p2) 
                    (if p2
                        (tokens str test p2)
                        nil)))
            nil)))
(tokens "ab12 3cde.f" #'alpha-char-p 0)

(defun constituent (chr)
    (and (graphic-char-p chr)
        (not (char= chr #\ ))))
(tokens "ab12 3cde.f
        gh" #'constituent 0)

(defun parse-date-my (date-str)
    (let ((lst (tokens date-str #'constituent 0)))
        (let ((mth (cadr lst)))
            (if (string-equal "Aug" mth)
                (setq mth 8))
            (list (first lst) mth (last lst)))))

(defun parse-date (str)
    (let ((toks (tokens str #'constituent 0)))
        (list (parse-integer (first toks))
              (parse-month   (second toks))
              (parse-integer (third toks)))))

(defconstant month-names
    #("jan" "feb" "mar" "apr" "may" "jun"
      "jul" "aug" "sep" "oct" "nov" "dec"))

(defun parse-month (str)
    (let ((p (position str month-names
                        :test #'string-equal)))
        (if p
            (+ p 1)
            nil)))

(parse-date "16 Aug 1980")
(parse-date-my "16 Aug 1980")

(defun read-integer (str)
    (if (every #'digit-char-p str)
        (let ((accum 0))
            (dotimes (pos (length str))
                (setf accum (+ (* accum 10)
                                (digit-char-p (char str pos)))))
            accum)
        nil))

(read-integer "42")

;;;; structures

;; note: every point structure is of type point, structure, atom, t
(defstruct point
    x
    y)

(defparameter p (make-point :x 0 :y 0))
(point-p p)
(point-p 42)
(point-x p)
(point-y p)
(copy-point p)
(setf (point-y p) 2)
p
(typep p 'point)

(defstruct polemic
    (type (progn
            (format t "What type of polemic was it? ")
            (read)))
    (effect nil))
(make-polemic)
;; input - scathing

(defstruct (point (:conc-name p)
                  (:print-function print-point))
    (x 0)
    (y 0))

(defun print-point (p stream depth)
    (format stream "#<~A,~A>" (px p) (py p)))
(make-point :x 42 :y -42)


;;;; BST (Binary Search Tree) is a tree in which, for some ordering function <, 
;;;; the left child of each element is < the element, and the element is < its
;;;; right child. 

(defparameter node (cons 'a
                         (cons '(b c) nil)))
(car node)
(cadr node)


(defstruct (node (:print-function
                    (lambda (node stream depth)
                        (format stream "#<~A>" (node-value node)))))
    value
    (left nil)
    (right nil))

(defun bst-insert (obj bst less-cmp)
    (if (null bst)
        (make-node :value obj)
        (let ((val (node-value bst)))
            (if (eql obj val)
                bst
                (if (funcall less-cmp obj val)
                    (make-node
                        :value val
                        :left  (bst-insert obj (node-left bst) less-cmp)
                        :right (node-right bst))
                    (make-node
                        :value val
                        :left  (node-left bst)
                        :right (bst-insert obj (node-right bst) less-cmp)))))))

(defun bst-find (obj bst less-cmp)
    (if (null bst)
        nil
        (let ((val (node-value bst)))
            (if (eql obj val)
                bst
                (if (funcall less-cmp obj val)
                    (bst-find obj (node-left bst) less-cmp)
                    (bst-find obj (node-right bst) less-cmp))))))

(defun bst-find-min (bst)
    (if (null bst)
        nil
        (let ((left (node-left bst)))
            (if (null left)
                bst
                (bst-find-min left)))))

(defun bst-find-max (bst)
    (if (null bst)
        nil
        (let ((right (node-right bst)))
            (if (null right)
                bst
                (bst-find-max right)))))

(defun bst-remove (obj bst less-cmp)
    (if (null bst)
        nil
        (let ((val (node-value bst)))
            (if (eql obj val)
                (percolate bst)
                (if (funcall less-cmp obj val)
                    (make-node
                        :value val
                        :left  (bst-remove obj (node-left bst) less-cmp)
                        :right (node-right bst))
                    (make-node
                        :value val
                        :left  (node-left bst)
                        :right (bst-remove obj (node-right bst) less-cmp)))))))

(defun percolate (bst)
    (cond ((null (node-left bst))
        (if (null (node-right bst))
            nil
            (rpec bst)))
        ((null (node-right bst)) (lperc bst))
        (t (if (zerop (random 2))
                (lperc bst)
                (rperc bst)))))

(defun rperc (bst)
    (make-node :value (node-value (node-right bst))
               :left  (node-left bst)
               :right (percolate (node-right bst))))

(defun lperc (bst)
    (make-node :value (node-value (node-left bst))
               :left  (percolate (node-left bst))
               :right (node-right bst)))

(defparameter tree nil)
(dolist (n '(5 8 4 2 1 9 6 3))
    (setf tree (bst-insert n tree #'<)))
(bst-find 12 tree #'<)
(bst-find 4 tree #'<)
(bst-find-min tree)
(bst-find-max tree)

(setf tree (bst-remove 2 tree #'<))
(bst-find 2 tree #'<)

(defun bst-traverse (fn bst)
    (when bst
        (bst-traverse fn (node-left bst))
        (funcall fn (node-value bst))
        (bst-traverse fn (node-right bst))))

(bst-traverse #'princ tree)


(defparameter ht (make-hash-table))
(gethash 'color ht)
(setf (gethash 'color ht) 'red)
(gethash 'color ht)

(defun our-member () ())
(defparameter bugs (make-hash-table))
(push "Doesn't take keyword arguments."
        (gethash #'our-member bugs))

(defparameter fruit (make-hash-table))
(setf (gethash 'apricot fruit) t) ; using hashtables as sets
(gethash 'apricot fruit)

(remhash 'apricot fruit) ; removes an entry from a hashtable

(setf (gethash 'shape ht) 'spherical
      (gethash 'size  ht) 'giant)

(maphash #'(lambda (k v)
                    (format t "~A -> ~A~%" k v))
        ht)

(make-hash-table :size 5)

(defparameter ht2 (make-hash-table :size 1 :test #'equal))
(setf (gethash '(ralph waldo emerson) ht2) t)

