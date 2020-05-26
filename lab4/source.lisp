(defclass associative-list ()
  ((lst :accessor container
	 :initarg :lst
	 :initform nil)))

(defclass binary-tree ()
  ((tree :accessor container
	 :initarg :tree
	 :initform nil)))

(defgeneric dict-add (dict key value))
(defgeneric dict-get (dict key))
(defgeneric dict-delete (dict key))


(defmethod dict-add ((dict associative-list) key value)
  (dolist (x (container dict))
    (when (eq (car x) key)
	      (let
		  ((prev (cdr x)))
		(setf (cdr x) value)
		(return-from dict-add (values prev t)))))
  (setf (container dict) (append (container dict) (list (cons key value)))))

(defmethod dict-get ((dict associative-list) key)
  (dolist (x (container dict))
    (when (eq (car x) key)
      (return-from dict-get (values (cdr x) t))))
  (values nil nil))

(defmethod dict-delete ((dict associative-list) key)
  (let
      ((prev nil))
    (setf (container dict) (remove-if (lambda (x)
					(when (eq (car x) key)
					  (setf prev x)
					  t))
				      (container dict)))
    (values (cdr prev) (not (null prev))))) 

(defmethod dict-add ((dict binary-tree) key value)
  (labels
      ((%add (tree key value)
	 (let
	     ((Left (cadr tree))
	      (Right (caddr tree))
	      (entry (car tree)))
	   (cond
	     ((null tree) (list (cons key value) nil nil))
	     ((string= key (car entry))
	      (let
		  ((prev (cdr entry)))
		(setf (cdr entry) value)
		(return-from dict-add (values prev t))))
	     ((string< key (car entry)) (list entry (%add Left key value) Right))
	     ((string> key (car entry)) (list entry Left (%add Right key value)))))))
    (setf (container dict) (%add (container dict) key value))
    (values nil nil)))

(defmethod dict-get ((dict binary-tree) key)
  (labels
      ((%get (tree key)
	 (let
	     ((Left (cadr tree))
	      (Right (caddr tree))
	      (entry (car tree)))
	   (cond
	     ((null tree) (values nil nil))
	     ((string= key (car entry)) (values (cdr entry) t))
	     ((string< key (car entry)) (%get Left key))
	     ((string> key (car entry)) (%get Right key))))))
    (%get (container dict) key)))

(defmethod dict-delete ((dict binary-tree) key)
  (let
      ((branches nil)
       (deleted nil))
    (labels
	((%del (tree key)
	   (let
	       ((Left (cadr tree))
		(Right (caddr tree))
		(entry (car tree)))
	     (cond
	       ((null tree) tree)
	       ((string= key (car entry))
		(setf branches (list Left Right))
		(setf deleted entry)
		(setf tree nil))
	       ((string< key (car entry)) (list entry (%del Left key) Right))
	       ((string> key (car entry)) (list entry left (%del Right key)))
	       )
	     )
	   )
	 (%add-branch (tree branch)
	   (let
	       ((Left (cadr tree))
		(Right (caddr tree))
		(entry (car tree))
		(key (caar branch)))
	     (cond
	       ((null tree) branch)
	       ((string< key (car entry)) (list entry (%add-branch Left branch) Right))
	       ((string> key (car entry)) (list entry Left (%add-branch Right branch)))
	       )
	       )
	   )
	 )
      (setf (container dict) (%del (container dict) key))
      (when (car branches)
	(setf (container dict) (%add-branch (container dict) (car branches))))
      (when (cdr branches)
	(setf (container dict) (%add-branch (container dict) (cadr branches))))
      (values (cdr deleted) (not (null deleted)))))
  )
