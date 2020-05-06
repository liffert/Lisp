(defun is_exist (lst)
  (print "Item is exist")
  lst)

(defun associative-list-add (lst key value)
  (if (null (car (associative-list-get lst key)))
      (append lst (list (cons key value)))
      (progn (print "aaa") lst)))

(defun associative-list-get (lst key)
  (cond
    ((null lst) (list nil nil))
    ((eq (car (car lst)) key) (list (cdr (car lst)) t))
    (t (associative-list-get (cdr lst) key))))


(defun property-list-add (lst key value)
  (if (null (car (PROPERTY-LIST-GET lst key)))
      (append lst (list key value))
      (is_exist lst)))

(defun property-list-get (lst key )
  (cond
    ((null lst) (list nil nil))
    ((eq (car lst) key) (list (car (cdr lst)) t))
    (t (property-list-get (cdr (cdr lst)) key))))


(defun binary-tree-add (tree key value)
  (let ((Left (cadr tree))
	(Right (caddr tree))
	(entry (car tree)))
    (cond
      ((null tree) (list (cons key value) nil nil))
      ((string= key (car entry)) (is_exist tree))
      ((string< key (car entry)) (list entry (binary-tree-add Left key value) Right))
      ((string> key (car entry)) (list entry Left (binary-tree-add right  key value)))
      )))
(defun binary-tree-get (tree key)
  (let ((Left (cadr tree))
	(Right (caddr tree))
	(entry (car tree)))
    (cond
      ((null tree) (list nil nil))
      ((string= key (car entry)) (list (cdr entry) t))
      ((string< key (car entry)) (binary-tree-get Left key))
      ((string> key (car entry)) (binary-tree-get right key))
      )))


(defun main ()
 

  (let ((lst '())
	(tree '())
	(lstp '()))

    (print "Properti list")
    (setf lstp (property-list-add lstp :a 1))
    (setf lstp (property-list-add lstp :b 2))
    (print lstp)

    (print (PROPERTY-LIST-GET lstp :c))
    (print (property-list-get lstp :b))

    (print "Associative list")
    (setf lst (associative-list-add lst :a 1))
    (setf lst (associative-list-add lst :b 23))
    (setf lst (associative-list-add lst :c 2))
    (print lst)
    
    (print (associative-list-get lst :b))
    (print (associative-list-get lst :c))
    (print (associative-list-get lst :s))

    (print "binary tree")
    (setf tree (binary-tree-add tree "a" 1))
    (setf tree (binary-tree-add tree "b" 2))
    (setf tree (binary-tree-add tree "c" 3))
    (setf tree (binary-tree-add tree "aaa" 4))
    (print tree)

    (print (binary-tree-get tree "c"))
    (print (binary-tree-get tree "s")))
    
  "End")

