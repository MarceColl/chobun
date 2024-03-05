# ChoBun 超文

An HTML toolset library.

This library provides an in-between between template libraries like Djoula and and HTML generator lib like spinneret.

## To install

ChoBun is in [ultralisp](https://ultralisp.org/), so follow the instructions there first to setup the distribution.

Then after that is done:

```lisp
(ql:quickload :chobun)
```

## Why ChoBun?

None of the current HTML libraries provided everything I needed,

* I wanted to generate HTML using common lisp, not html templates like djoula.
* I wanted the djoula layouts feature.
* I wanted to interperse common lisp features in the HTML.
* I wanted to be able to compose using normal functions.
* I wanted to be able to generate html dynamically if needed.
* I wanted the generated code to be very fast.

## How to use

The main macro you'll use is `html`. `html` generates efficient code to render an html tree.

```lisp
(html
  (:div :class "item" 
    (:h1 "Header")
    (:input :type "text" :placeholder "Name")))

=> "<div class=\"item\"><h1>Header</h1><input type=\"text\" placeholder=\"Name\"></input></div>"
```

You can use lisp values as you need in any place except the tag (we'll get to generating dynamic html later).

```lisp
(defun person (name description)
  (html
    (:div :class "person"
	  (:h2 name)
	  (:p description))))
	  
(person "Marce Coll" "A Lisp programmer")

=> "<div class=\"person\"><h2>Marce Coll</h2><p>A Lisp programmer</p></div>"
```

You can use functions to compose more complex html

```lisp
(html
  (:ul
    (:li (person "Person 1" "A Lisp programmer"))
	(:li (person "Person 2" "Another Lisp programmer"))))
	
=> "<ul><li><div class=\"person\"><h1>Person 1</h1><p>A Lisp programmer</p></div></li><li><div class=\"person\"><h1>Person 2</h1><p>Another Lisp programmer</p></div></li></ul>"
```

You can use some lisp control structures to make generating repeating or conditional html easily.

```lisp
(let ((persons '(("Person 1" . "A Lisp programmer") ("Person 2" . "Another Lisp Programmer"))))
  (html
    (:ul
     (dolist (person persons)
	   (:li (person (car person) (cdr person)))))))

=> "<ul><li><div class=\"person\"><h1>Person 1</h1><p>A Lisp programmer</p></div></li><li><div class=\"person\"><h1>Person 2</h1><p>Another Lisp programmer</p></div></li></ul>"
```

```lisp
(html
  (dotimes (i 6)
	(if (= (mod i 2) 0)
	    (:div (format nil "~a is even" i))
	    (:span (format nil "~a is odd" i)))))

=> "<div >0 is even</div><span >1 is odd</span><div >2 is even</div><span >3 is odd</span><div >4 is even</div><span >5 is odd</span>"
```

Currently the list of available structures is `if, when, unless, dolist, dotimes`.

If the control structures available are not enough for the html you need to generate, you can resort to a more low level approach by using the `with-dynamic-html` macro. The macro ensures that the `html` macro dynamically processes the return conses of the body as an html tree dynamically at runtime.

```lisp
(defun generate-dynamic-persons (persons)
  (with-dynamic-html
    (loop for person in persons
	  collect `(:li (:div :class "person" (:h1 ,(car person)) (:p ,(cdr person)))))))
	  
(let ((persons '(("Person 1" . "A Lisp programmer") ("Person 2" . "Another Lisp Programmer"))))
  (html (:ul (generate-dynamic-persons persons)))
  
=> "<ul><li><div class=\"person\"><h1>Person 1</h1><p>A Lisp programmer</p></div></li><li><div class=\"person\"><h1>Person 2</h1><p>Another Lisp programmer</p></div></li></ul>"
```
