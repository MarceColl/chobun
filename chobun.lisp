(in-package :cl-user)
(defpackage chobun
  (:use :cl :bedrock)
  (:export :html :with-dynamic-html :with-layout :define-layout :*layouts*))
(in-package :chobun)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *layouts* (make-hash-table))
  (defparameter *html-stream* nil)
  (defparameter *inline-special-forms* '(if when unless mapcar))
  (defparameter *hoistable-special-forms* '(dolist dotimes))
  (defparameter *dynamic-html-gensym* (gensym "DYN-HTML"))

  (defun get-args-and-subtrees (tree-node)
    "From a given tree node return the node arguments and the subtrees"
    (let ((args nil)
	  (subtrees nil))
      (loop while (car tree-node)
	    do (let ((item (car tree-node)))
		 (if (is-keyword item)
		     (progn
		       (pushback (symbol-name item) args)
		       (pushback (cadr tree-node) args)
		       (setf tree-node (cddr tree-node)))
		     (progn
		       (pushback item subtrees)
		       (setf tree-node (cdr tree-node))))))
      (values args subtrees)))

  (defun parse-list-tree-node (tree-node)
    (let ((res nil))
      (macrolet ((push-res (c) `(pushback ,c res)))
	(let ((first-element (car tree-node))
	      (rest (cdr tree-node)))
	  (case (type-of first-element)
	    (keyword
	     (multiple-value-bind (args subtrees) (get-args-and-subtrees rest)
	       (pushback `(write-string ,(format nil "<~(~a~)~{ ~(~a~)=~s~}>" (symbol-name first-element) args) *html-stream*) res)
	       (dolist (next-tree-node subtrees)
		 (let ((items (parse-tree-node next-tree-node)))
		   (dolist (item items)
		     (pushback item res))))
	       (pushback `(write-string ,(format nil "</~(~a~)>" (symbol-name first-element)) *html-stream*) res)))
	    (symbol
	     (cond
	       ((eq first-element 'yield) (pushback 'yield res))
	       ((in first-element *hoistable-special-forms*)
		(push-res `(,first-element ,(cadr tree-node) ,@(parse-tree-node (caddr tree-node)))))
	       ((in first-element *inline-special-forms*)
		(push-res `(format *html-stream* "~a" (,first-element ,(cadr tree-node)
								      (progn ,@(parse-tree-node (caddr tree-node)))
								      (progn ,@(parse-tree-node (cadddr tree-node)))))))
	       (t (pushback `(format *html-stream* "~a" (maybe-eval-html (apply #',first-element (list ,@rest)))) res)))))))
      res))

  (defun parse-tree-node (tree-node)
    (let ((res nil))
      (when tree-node
	(cond
	  ((listp tree-node) (dolist (item (parse-list-tree-node tree-node))
			       (pushback item res)))
	  ((stringp tree-node) (pushback `(write-string ,tree-node *html-stream*) res))
	  ((symbolp tree-node) (pushback `(format *html-stream* "~a" ,tree-node) res)))
	res)))

  (defun parse-html (tree-node)
    (parse-tree-node tree-node))

  (defun optimize-html-codegen (c &key (for-layout nil))
    "Optimize the generated codegen"
    (let ((res nil)
	  (str "")
	  (format-args nil))
      (dolist (l c)
	(cond
	  ((and for-layout (eq 'yield l))
	   (pushback `(format *html-stream* ,str ,@format-args) res)
	   (setf str ""
		 format-args nil))
	  ((eq 'write-string (car l)) (setf str (concatenate 'string str (nth 1 l))))
	  ((eq 'format (car l))
	   (setf str (concatenate 'string str (nth 2 l)))
	   (pushback (nth 3 l) format-args))
	  (t (pushback `(format *html-stream* ,str ,@format-args) res)
	     (setf str ""
		   format-args nil)
	     (pushback l res))))
      (pushback `(format *html-stream* ,str ,@format-args) res)
      res)))
    
(defmacro html (tree)
  (let ((html-gen (optimize-html-codegen (parse-html tree))))
    `(progn
       (let ((*html-stream* (make-string-output-stream)))
	 (with-output-to-string (*html-stream*)
	   ,@html-gen)))))

(defmacro with-dynamic-html (&body body)
  `(let ((res (progn ,@body)))
     (cons *dynamic-html-gensym* (list res))))

(defun eval-html (tree)
  (let ((html-gen (optimize-html-codegen (parse-html tree))))
    (eval `(progn ,@html-gen))))

(defun maybe-eval-html (tree)
  (if (and (listp tree) (eq *dynamic-html-gensym* (car tree)))
    (with-output-to-string (*html-stream*)
      (dolist (subtree (cadr tree))
	(eval-html subtree)))
    tree))

;; Layouts

(defmacro define-layout (name args &body tree)
  (let ((html-gen (optimize-html-codegen (parse-html (car tree)) :for-layout t))
	(name (make-keyword name)))
    `(setf (gethash ',name *layouts*)
	   (lambda ,args (values ,(car html-gen) ,(cadr html-gen))))))

(defmacro with-layout (name args &body body)
  (let ((name (make-keyword name)))
    (with-gensyms (layout-func)
      `(let ((,layout-func (gethash ',name *layouts*))
	     (inner-html ,@body))
	 (multiple-value-bind (pre-html post-html) (apply ,layout-func (list ,@args))
	   (format nil "~a~a~a" pre-html inner-html post-html))))))
