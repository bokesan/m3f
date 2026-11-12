(defpackage :config-tests
  (:use :cl :fiveam :config))

(in-package :config-tests)

(def-suite config-tests)
(in-suite config-tests)

(test all
      (let* ((conf (read-config-file "test/resources/sample.conf"))
	     (section1 (gethash "section1" conf)))
	(is (= 2 (hash-table-count conf)))
	(is (equal '(("foo" . "bar")) (gethash "SeCtIoN2" conf)))
	(is (= 2 (length section1)))
	(is (string= "value1" (cdr (assoc "key1" section1 :test #'string=))))
	(is (string= "value2, also with spaces" (cdr (assoc "key2 with spaces" section1 :test #'string=))))))
