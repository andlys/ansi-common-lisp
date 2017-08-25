(nthcdr 3 '(a b c d e))
(reverse '(a b c))
(sort '(2 5 3 1 4) #'<)
(subseq '(a b c) 1)
(every #'oddp '(1 3 5))
(some #'evenp '(1 2 3))
(every #'> '(1 3 5) '(0 2 4))

(defun every-odd (lst) (every #'oddp lst))
(every-odd '(1 3 5 7 8))

;stacks
(defparameter lst (list 'a))
(push 'b lst)
(pop lst)

(cons 'a 'b)
(defparameter trans '((+ . "add") (* . "multiply")))
(assoc '+ trans)
'(node . (neigh bours)) ;???
(defun shortest-path (start end net)
    (bfs end (list (list start)) net))
(defun bfs (end queue net)
    (if (null queue)
        nil
        (let ((path (car queue)))
            (let ((node (car path)))
                (if (eql node end)
                    (reverse path)
                    (bfs end
                        (append (cdr queue)
                                (new-paths path node net))
                        net))))))
(defun new-paths (path node net)
    (mapcar #'(lambda (n)
                (cons n path))
            (cdr (assoc node net))))
(defparameter min-net '((a b c) (b c) (c d)))
(cdr (assoc 'a min-net))
(shortest-path 'a 'd min-net)

