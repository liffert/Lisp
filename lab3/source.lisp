(defun mcar (func lst)
  (if (null lst) nil
  (cons (funcall func (car lst)) (mcar func (cdr lst)))))

(defun filter (func lst)
  (cond ((null lst) nil)
	((funcall func (car lst)) (cons (car lst) (filter func (cdr lst))))
	(t (filter func (cdr lst)))))

(defun mred (func lst)
  (labels ((%internal-func (res lst)
	     (if (null lst)
		 res
		 (%internal-func (funcall func res (car lst)) (cdr lst)))))
    (%internal-func (car lst) (cdr lst))))
