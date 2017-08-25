;;; code from the book retyped here by me

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
