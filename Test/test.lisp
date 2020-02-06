(defun Fuct (N)
  (if (or (= N 1) (= N 0)) 1
      (* N (Fuct (- N 1)))))

(defun rev (lst)
  (if (null lst) nil
      (append (rev (cdr lst)) (list (car lst)))))

(defun myLast (lst)
  (if (null (cdr lst)) lst
      (myLast (cdr lst))))
