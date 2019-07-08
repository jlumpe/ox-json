;;; Test the test helper functions.


(ert-deftest test-encoded= ()
  (should (encoded= "[1,2]" "[1, 2]"))
  (should (encoded= "[1,2]" "[\n1,\n2\n]"))
  (should-not (encoded= "[1,2]" "[1,2,3]")))


(ert-deftest test-json-obj ()
  (let ((data-type-property (plist-get info :json-data-type-property))
        (obj1 (json-obj info "myobj" 'foo 1 'bar 2))
        (obj2 (json-obj info nil 'foo 1 'bar 2)))
    (should (hash-table-p obj1))
    (should (equal
              (sort (hash-table-keys obj1) #'string<)
              (sort (list "foo" "bar" data-type-property) #'string<)))
    (should (equal (gethash data-type-property obj1) "myobj"))
    (should (equal (gethash "foo" obj1) 1))
    (should (equal (gethash "bar" obj1) 2))
    ; 2nd variant shouldn't have data type key
    (should (equal
              (sort (hash-table-keys obj2) #'string<)
              '("bar" "foo")))))
