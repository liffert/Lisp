(defun flatten (lst)
  (cond ((null lst) '())
	((listp (car lst)) (append (flatten (car lst)) (flatten (cdr lst))))
	(t (cons (car lst) (flatten (cdr lst))))))

(defun find-symbols (lst)
  (let
      ((signs '(+ - * / sin cos log expt)))
    (dolist (x lst)
      (unless (numberp x) (unless (find x signs) (return-from find-symbols nil))))
    t))

(defun differentiate (var expr)
  (labels ((%sum (arg1 arg2)
	     (cond
	       ((eq arg1 0) arg2)
	       ((eq arg2 0) arg1)
	       (t (list '+ arg1 arg2))))
	   (%minus (arg1 arg2)
	     (cond
	       ((eq arg1 0) (- arg2))
	       ((eq arg2 0) arg1)
	       (t (list '- arg1 arg2))))
	   (%prod (arg1 arg2)
	     (cond
	       ((or (eq arg1 0) (eq arg2 0)) 0)
	       ((eq arg2 1) arg1)
	       ((eq arg1 1) arg2)
	       (t (list '* arg1 arg2))))
	   (%div (arg1 arg2)
	     (list '/ arg1 arg2))
	   (%expt (num pow)
	     (cond
	       ((eq pow 1) num)
	       ((eq pow 0) 0)
	       (t (list 'expt num pow))))
	   (%sin (arg)
	     (list 'cos arg))
	   (%cos (arg)
	     (list '- (list 'sin arg)))
	   (%dexpt (var expr)
	     (let
		 ((arg1 (second expr))
		  (arg2 (third expr)))
	       (cond
		 ((or (numberp arg1)
		      (if (and (symbolp arg1)
			       (eq arg1 var))
			  nil
			  t))
		  (%prod (%expt arg1 arg2) (list 'log arg1)))
		 ((and (or (numberp arg2) (symbolp arg2))
		       (eq arg1 var)
		       (not (listp arg2)))
		  (cond
		    ((numberp arg2)
		     (%prod arg2 (%expt arg1 (1- arg2))))
		    ((eq arg2 var)
		     (%prod
		      (%expt arg1 arg2)
		      (differentiate var (%prod arg2 (list 'log arg1)))))
		    (t 
		      (%prod arg2 (%expt arg1 (list '1- arg2))))))
		 (t (cond
		      ((find var (flatten arg2))
			(%prod
			 (%expt arg1 arg2)
			 (differentiate var (%prod arg2 (list 'log arg1)))))
		      ((find-symbols (flatten arg2))
		       (%prod (eval arg2) (%expt arg1 (1- (eval arg2)))))
		      (t (%prod  arg2 (%expt arg1 (list '1- arg2)))))
		      )))))
  (cond
    ((numberp expr) 0)
    ((symbolp expr) (if (eq expr var) 1 0))
    (t (let
	   ((sign (first expr))
	    (arg1 (second expr))
	    (arg2 (third expr)))
	 (cond
	   ((eq '+ sign) (%sum (differentiate var arg1)
			       (differentiate var arg2)))
	   ((eq '- sign) (%minus (differentiate var arg1)
				 (differentiate var arg2)))
	   ((eq '* sign) (%sum (%prod (differentiate var arg1) arg2)
			       (%prod arg1 (differentiate var arg2))))
	   ((eq '/ sign) (%div (%minus (%prod (differentiate var arg1) arg2)
				       (%prod arg1 (differentiate var arg2)))
				       (%expt arg2 2)))
	   ((eq 'expt sign) (%dexpt var expr))
	   ((eq 'sin sign) (%prod
			    (%sin arg1)
			    (differentiate var arg1)))
	   ((eq 'cos sign) (%prod
			    (%cos arg1)
			    (differentiate var arg1)))
	   ((eq 'log sign) (%div 1 arg1))))))))
