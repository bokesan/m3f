(defpackage :config-tests
  (:use :cl :fiveam :config))

(in-package :config-tests)

(def-suite config-tests)
(in-suite config-tests)

(defun assoc-has-key-p (a key)
  (some #'(lambda (list) (equalp (car list) key)) a))

(test all
      (is-true (assoc-has-key-p '(("a" "b") ("c" "d") ("e" "f")) "e"))
      
      (let* ((conf (read-config-file "test/resources/sample.conf"))
	     (section1 (gethash "section1" conf))
	     (many (gethash "many" conf)))
	(is (= 3 (hash-table-count conf)))
	(is (equal '((("foo" . "bar"))) (gethash "SeCtIoN2" conf)))
	(is (= 1 (length section1)))
	(is (= 2 (length many)))
	
	(is (string= "value1" (cdr (assoc "key1" (car section1) :test #'string=))))
	(is (string= "value2, also with spaces" (cdr (assoc "key2 with spaces" (car section1) :test #'string=))))

	(is-true (every #'(lambda (a) (assoc-has-key-p a "field1")) many))))
