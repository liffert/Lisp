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

(defvar lsta '())
(defvar lstp '())

(defun main ()
  (setf lsta '())
  (setf lstp '())
  (setf lstp (property-list-add lstp :a 1))
  (setf lstp (property-list-add LSTP :b 2))
  (print lstp)
  (setf lsta (associative-list-add lsta :a 1))
  (print lsta)
  (setf lsta (associative-list-add lsta :b 23))
  (setf lsta (associative-list-add lsta :c 2))
  (print lsta)
  (print (associative-list-get lsta :b))
  (print (associative-list-get lsta :c))
  (print (associative-list-get lsta :s))
  (PROPERTY-LIST-GET LSTP :b)
)

