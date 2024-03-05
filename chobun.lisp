(in-package :cl-user)
(defpackage chobun
  (:use :cl :bedrock)
  (:export :html :with-dynamic-html :with-layout :define-layout :*layouts*))
(in-package :chobun)

(defmacro define-step (name args &body body)
  (with-gensyms (item res)
    `(defun ,name ,args
      (let ((,res nil))
	(flet ((add-code (c) (pushback c ,res))
	       (add-codes (c) (dolist (,item c)
				(pushback ,item ,res))))
	  (declare (ignorable #'add-code #'add-codes))
	  ,@body)
	,res))))

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

  (define-step parse-tag-tree-node (first-element rest)
    (multiple-value-bind (args subtrees) (get-args-and-subtrees rest)
      (add-code `(write-string ,(format nil "<~(~a~)~{ ~(~a~)=~s~}>" (symbol-name first-element) args) *html-stream*))
      (dolist (next-tree-node subtrees)
	(add-codes (parse-tree-node next-tree-node)))
      (add-code `(write-string ,(format nil "</~(~a~)>" (symbol-name first-element)) *html-stream*))))

  (define-step parse-symbol-tree-node (first-element rest)
    (cond
      ((eq first-element 'yield) (add-code 'yield))
      ((in first-element *hoistable-special-forms*)
       (add-code `(,first-element ,(car rest) ,@(parse-tree-node (cadr rest)))))
      ((in first-element *inline-special-forms*)
       (add-code `(format *html-stream* "~a" (,first-element ,(car rest)
							     (progn ,@(parse-tree-node (cadr rest)))
							     (progn ,@(parse-tree-node (caddr rest)))))))
      (t (add-code `(format *html-stream* "~a" (maybe-eval-html (apply #',first-element (list ,@rest))))))))
    
  (define-step parse-list-tree-node (tree-node)
    (let ((first-element (car tree-node))
	  (rest (cdr tree-node)))
      (case (type-of first-element)
	(keyword (add-codes (parse-tag-tree-node first-element rest)))
	(symbol (add-codes (parse-symbol-tree-node first-element rest))))))

  (define-step parse-tree-node (tree-node)
    (when tree-node
      (cond
	((listp tree-node) (add-codes (parse-list-tree-node tree-node)))
	((stringp tree-node) (add-code `(write-string ,tree-node *html-stream*)))
	((symbolp tree-node) (add-code `(format *html-stream* "~a" ,tree-node))))))

  (defun parse-html (tree-node)
    (parse-tree-node tree-node))

  (defun optimize-html-codegen (c &key (for-layout nil))
    "Optimize the generated codegen"
    (let ((res nil)
	  (str "")
	  (format-args nil))
      (flet ((commit-curr-str ()
	       (pushback `(format *html-stream* ,str ,@format-args) res)
	       (setf str "" format-args nil))
	     (append-to-str (val) (setf str (concatenate 'string str val))))
	(dolist (l c)
	  (cond
	    ((and for-layout (eq 'yield l)) (commit-curr-str))
	    ((eq 'write-string (car l)) (append-to-str (nth 1 l)))
	    ((eq 'format (car l))
	     (append-to-str (nth 2 l))
	     (pushback (nth 3 l) format-args))
	    ((eq 'dolist (car l))
	     (commit-curr-str)
	     (pushback `(dolist ,(cadr l)
			  ,@(optimize-html-codegen (cddr l))) res))
	    (t (commit-curr-str) 
	       (pushback l res))))
	(commit-curr-str)
      res))))
    
(defmacro html (html-tree)
  "Generates code to print the given HTML-TREE.

Some examples first

# Basic static html
```lisp
(html
  (:div :class \"container\"
     (:h1 \"Title\")))
```

# Using lisp variables
```lisp
(let ((title \"Title\"))
  (html
    (:div :class \"container\"
       (:h1 title))))
```

# Functions
Functions that generate html using the HTML macro are easily composable by just calling
the function.

```lisp
(defun person (p)
  (html
    (:div :class \"person\"
       (:h2 (name p))
       (:p (description p)))))

(let ((p (get-person)))
   (html (:div (person p))))
```

# Control structures

Some of the lisp control structres are available inside the HTML macro.

```lisp
(html
  (:ul
    (dolist (p persons)
       (:li (person p)))))
```

# HTML-TREE
The HTML tree is a nested Lisp S-Expression, each nested list can start by either a keyword or a symbol.
If it's a keyword then it's interpreted as an HTML tag. For example (:div \"Hola\") is interpreted as \"<div>Hola</div>\".
If it's a symbol what happens depends on the symbol, if it's one of the supported Lisp control structures, in includes that
in the generated code, so it feels like using Lisp."
  (let ((html-gen (optimize-html-codegen (parse-html html-tree))))
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
