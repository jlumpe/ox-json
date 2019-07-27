;;; Test generic data encoder functions

(require 'ox-json)
(require 'ox-json-test-helpers)


;;; Error reporting
(ert-deftest test-error ()
  ; Strict option enabled
  (let ((errdata (should-error (ox-json--error info "foo %s" "bar"))))
    (should (string= (cadr errdata) "foo bar")))
  ; Strict option disabled:
  (decode-compare
    (ox-json--error info-nonstrict "foo %s" "bar")
    (json-obj info-nonstrict "error" 'message "foo bar")))


;;; Fixed-type scalar encoding functions

(ert-deftest test-encode-bool ()
  ; Non-strict (default)
  (should (string= (ox-json-encode-bool t info) "true"))
  (should (string= (ox-json-encode-bool nil info) "false"))
  (should (string= (ox-json-encode-bool 0 info) "true"))
  (should (string= (ox-json-encode-bool "foo" info) "true"))
  ; Strict
  (should (string= (ox-json-encode-bool t info t) "true"))
  (should (string= (ox-json-encode-bool nil info t) "false"))
  (should-error (ox-json-encode-bool 0 info t))
  (should-error (ox-json-encode-bool "foo" info t)))

(ert-deftest test-encode-string ()
  (should (string= (ox-json-encode-string "foo" info) "\"foo\""))
  (should (string= (ox-json-encode-string 'foo info) "\"foo\""))
  (should (string= (ox-json-encode-string t info) "\"t\""))
  (should (string= (ox-json-encode-string "foo \" \\" info) "\"foo \\\" \\\\\""))
  (should (string= (ox-json-encode-string nil info) "null"))
  (should-error (ox-json-encode-string 0 info)))

(ert-deftest test-encode-number ()
  (should (string= (ox-json-encode-number 0 info) "0"))
  (should (string= (ox-json-encode-number 1.5 info) "1.5"))
  (should (string= (ox-json-encode-number nil info) "null"))
  (should-error (ox-json-encode-number "foo" info)))

(ert-deftest test-encode-tag-string ()
  (decode-compare (ox-json-encode-tag-string "foo" info) ["foo"])
  (decode-compare (ox-json-encode-tag-string "foo:bar" info) ["foo" "bar"])
  (decode-compare (ox-json-encode-tag-string ":foo:bar:" info) ["foo" "bar"])
  (decode-compare (ox-json-encode-tag-string ":::foo::bar::::" info) ["foo" "bar"])
  (decode-compare (ox-json-encode-tag-string "" info) [])
  (decode-compare (ox-json-encode-tag-string ":" info) []))


;;; Variable-type encoding functions

(ert-deftest test-encode-auto ()
  (should (string= (ox-json-encode-auto nil info) "null"))
  (should (string= (ox-json-encode-auto 1 info) "1"))
  (should (string= (ox-json-encode-auto 1.5 info) "1.5"))
  (should (string= (ox-json-encode-auto t info) "true"))
  (should (string= (ox-json-encode-auto "foo" info) "\"foo\""))
  (should (string= (ox-json-encode-auto 'foo info) "\"foo\""))
  (decode-compare
    (ox-json-encode-auto '(1 t nil "foo") info)
    [1 t :json-null "foo"])
                                        ; TODO org node
  )

(ert-deftest test-encode-with-type ()
  ; Bool
  (should (string= (ox-json-encode-with-type 'bool nil info) "false"))
  (should (string= (ox-json-encode-with-type 'bool t info) "true"))
  (should (string= (ox-json-encode-with-type 'bool "foo" info) "true"))
  ; Bool (strict)
  (should (string= (ox-json-encode-with-type '(bool t) nil info) "false"))
  (should (string= (ox-json-encode-with-type '(bool t) t info) "true"))
  (should-error (ox-json-encode-with-type '(bool t) "foo" info))
  ; String
  (should (string= (ox-json-encode-with-type 'string "foo" info) "\"foo\""))
  (should (string= (ox-json-encode-with-type 'string nil info) "null"))
  (should-error (ox-json-encode-with-type 'string 0 info))
  ; Number
  (should (string= (ox-json-encode-with-type 'number 0 info) "0"))
  (should (string= (ox-json-encode-with-type 'number nil info) "null"))
  (should-error (ox-json-encode-with-type 'number "foo" info))
  ; Array
  (should (string= (ox-json-encode-with-type 'array nil info) "[]"))
  (should (encoded= (ox-json-encode-with-type 'array '(1 nil t) info) "[1, null, true]"))
  ; Array with item type
  (should (encoded=
            (ox-json-encode-with-type '(array bool) '(nil t) info)
            "[false, true]")))


;;; Encoding composite data types

(ert-deftest test-encode-array ()
  ; Auto item type
  (decode-compare
    (ox-json-encode-array '(1 t nil "foo" foo) info)
    [1 t :json-null "foo" "foo"])
  ; Empty
  (should (string= (ox-json-encode-array nil info) "[]"))
  ; Bool item type
  (decode-compare
    (ox-json-encode-array '(1 t nil "foo" foo) info 'bool)
    [t t :json-false t t])
  (decode-compare
    (ox-json-encode-array '(t nil) info '(bool t))
    [t :json-false])
  (should-error (ox-json-encode-array '(1 t nil "foo" foo) info '(bool t)))
  ; String item type
  (decode-compare
    (ox-json-encode-array '("foo" foo nil) info 'string)
    ["foo" "foo" :json-null])
  (should-error (ox-json-encode-array '(1 t nil "foo" foo) info 'string))
  ; Single/multi-line
  (should (string= (ox-json-encode-array '(1 2 3) info 'number) "[\n1,\n2,\n3\n]"))
  (should (string= (ox-json-encode-array '(1 2 3) info 'number t) "[1, 2, 3]")))

(ert-deftest test-encode-alist ()
  ; Auto value type
  (decode-compare
    (ox-json-encode-alist "mytype" '((a . 1) (:b . t) (c . "foo")) info)
    (json-obj info "mytype" :a 1 :b t :c "foo"))
  ; Empty
  (decode-compare
    (ox-json-encode-alist "mytype" nil info)
    (json-obj info "mytype"))
  ; Bool value type
  (decode-compare
    (ox-json-encode-alist "mytype" '((true . t) (true2 . 1) (false . nil)) info 'bool)
    (json-obj info "mytype" :true t :true2 t :false :json-false))
  (decode-compare
    (ox-json-encode-alist "mytype" '((true . t) (false . nil)) info '(bool t))
    (json-obj info "mytype" :true t :false :json-false))
  (should-error (ox-json-encode-alist "mytype" '((true . 1) (false . nil)) info '(bool t)))
  ; String value type
  (decode-compare
    (ox-json-encode-alist "mytype" '((string . "foo") (symbol . foo) (null . nil)) info 'string)
    (json-obj info "mytype" :string "foo" :symbol "foo" :null :json-null))
  ; No data type property
  (let ((info2 (org-combine-plists info '(:json-data-type-property nil))))
    (decode-compare
      (ox-json-encode-alist "ignored" '((foo . "bar")) info2)
      (json-obj info2 nil :foo "bar"))))


(ert-deftest test-encode-plist ()
  ; Auto value type
  (decode-compare
    (ox-json-encode-plist "mytype" '(:a 1 b t :c "foo") info)
    (json-obj info "mytype" :a 1 :b t :c "foo"))
  ; Empty
  (decode-compare
    (ox-json-encode-plist "mytype" nil info)
    (json-obj info "mytype"))
  ; Bool value type
  (decode-compare
    (ox-json-encode-plist "mytype" '(:true t :true2 1 :false nil) info 'bool)
    (json-obj info "mytype" :true t :true2 t :false :json-false))
  (decode-compare
    (ox-json-encode-plist "mytype" '(:true t :false nil) info '(bool t))
    (json-obj info "mytype" :true t :false :json-false))
  (should-error (ox-json-encode-plist "mytype" '(:true 1 :false nil) info '(bool t)))
  ; String value type
  (decode-compare
    (ox-json-encode-plist "mytype" '(:string "foo" :symbol foo :null nil) info 'string)
    (json-obj info "mytype" :string "foo" :symbol "foo" :null :json-null))
  ; No data type property
  (let ((info2 (org-combine-plists info '(:json-data-type-property nil))))
    (decode-compare
      (ox-json-encode-plist "ignored" '(:foo "bar") info2)
      (json-obj info2 nil :foo "bar"))))


;;; Changing default encoding functions

(ert-deftest test-override-type-exporter ()
  (let* ((numencoder (lambda (value info) (format "%S" (+ value 1))))
         (info2
           (org-combine-plists
             info
             `(:json-exporters (number ,numencoder)))))
    (should (equal (ox-json--get-type-encoder 'number info2) numencoder))
    (should (string= (ox-json-encode-with-type 'number 1 info2) "2"))
    (should (string= (ox-json-encode-with-type 'number 100 info2) "101"))
    (should (string= (ox-json-encode-number 100 info2) "100"))
    (should (encoded=
              (ox-json-encode-array '(1 10) info2 'number)
              "[2, 11]"))))
