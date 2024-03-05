(asdf:defsystem :chobun
  :depends-on (:bedrock)
  :components ((:file "chobun"))
  :in-order-to ((test-op (test-op :chobun/test))))

(asdf:defsystem :chobun/test
  :depends-on (:chobun :parachute :split-sequence)
  :components ((:module "t"
		:components ((:file "chobun"))))
  :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test-toplevel :chobun/test)))
