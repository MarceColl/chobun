(in-package :cl-user)
(defpackage chobun/test
  (:use :cl :bedrock :chobun :parachute :split-sequence))
(in-package :chobun/test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun to-kebab-case (str)
    (intern (format nil "~{~:@(~a~)~#[~:;-~]~}" (split-sequence #\Space str)))))

(defmacro html-test (desc html-tree expected-output &optional (vars nil))
  (let ((test-name (to-kebab-case desc)))
    `(define-test ,test-name
	 (let ,vars
	   (is string= ,expected-output (html ,html-tree))))))

(html-test "Single tag" (:div "Hola") "<div>Hola</div>")
(html-test "Nested tags" (:div (:span "Hola")) "<div><span>Hola</span></div>")
(html-test "Same level tags" (:div (:span "Hola") (:h1 "Title")) "<div><span>Hola</span><h1>Title</h1></div>")
(html-test "Tag properties" (:div :class "test" :id "wassup" (:span :id "inner" "Hola"))
	   "<div class=\"test\" id=\"wassup\"><span id=\"inner\">Hola</span></div>")
(html-test "Variables"
    (:div :class class-name :id "wassup" (:span :id "inner" value))
    "<div class=\"test\" id=\"wassup\"><span id=\"inner\">Hola</span></div>"
    ((class-name "test") (value "Hola")))
(html-test "dolist"
    (:div (dolist (i '(1 2 3 4))
	    (:span i)))
    "<div><span>1</span><span>2</span><span>3</span><span>4</span></div>")
(html-test "dotimes"
    (:div (dotimes (i 4)
	    (:span i)))
    "<div><span>0</span><span>1</span><span>2</span><span>3</span></div>")
(html-test "if"
    (:div (if t 
	      (:span "TRUE")
	      (:span "FALSE")))
    "<div><spanTRUE</span></div>")
(html-test "if inside dotimes"
    (:div (dotimes (i 6)
	    (:div (if (= (mod i 2) 0)
		      (:span (format nil "~a is EVEN" i))))))
    "<div><div><span>0 is EVEN</span></div><div></div><div><span>2 is EVEN</span></div><div></div><div><span>4 is EVEN</span></div><div></div></div>")
