(defun associative-list-add (lst key value)
  (append lst (list (cons key value))))

(defun associative-list-get (lst key)
  (cond
    ((null lst) (list nil nil))
    ((eq (car (car lst)) key) (list (cdr (car lst)) t))
    (t (associative-list-get (cdr lst) key))))


(defun property-list-add (lst key value)
  (append lst (list key value)))

(defun property-list-get (lst key )
  (cond
    ((null lst) (list nil nil))
    ((eq (car lst) key) (list (car (cdr lst)) t))
    (t (property-list-get (cdr (cdr lst)) key))
    ))

(defun binary-tree-add (tree key value))

(defun binary-tree-get (tree key))

(defun main ()

  (let ((lst '()))
    (setf lst (property-list-add lst :a 1))
    (setf lst (property-list-add lst :b 2))
    (print lst)
    (print (PROPERTY-LIST-GET lst :c))
    (print (property-list-get lst :b)))

  (let ((lst '()))
    (setf lst (associative-list-add lst :a 1))
    (print lst)
    (setf lst (associative-list-add lst :b 23))
    (setf lst (associative-list-add lst :c 2))
    (print lst)
    (print (associative-list-get lst :b))
    (print (associative-list-get lst :c))
    (print (associative-list-get lst :s)))
  '("End"))

