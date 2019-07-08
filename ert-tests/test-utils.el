; Tests of utility code

(require 'ox-json)
(require 'ox-json-test-helpers)


(ert-deftest test-merge-alists ()
  "Test `org-json--merge-alists'."
  ; Complex case
  (should
    (equal
      (org-json--merge-alists
        '(
           (a . 1)
           (b . 2)
           (c . 3)
           (b . 10))
        '(
           (b . 99)
           (d . 4)
           (e . 5))
        '(
           (a . 102)
           (e . 234)
           (f . 6)))
      '(
         (a . 1)
         (b . 2)
         (c . 3)
         (d . 4)
         (e . 5)
         (f . 6))))
  ; Edge case - one  list (duplicates should be removed)
  (should
    (equal
      (org-json--merge-alists
        '((a . 1) (b . 2) (c . 3) (a . 100)))
      '((a . 1) (b . 2) (c . 3))))
  ; Edge case - merge no lists
  (should
    (equal
      (org-json--merge-alists)
      nil))
  )


(ert-deftest test-plist-to-alist ()
  (should (equal
    (org-json--plist-to-alist '(one 1 two 2 three 3))
    '((one . 1) (two . 2) (three . 3)))))


(ert-deftest test-plists-get ()
  (let ((pl1 '(:a 1 :b 2 :c 3))
        (pl2 '(:b 20 :d 40))
        (pl3 '(:b 200 :d 400 :f 500)))
    ; No plists
    (should (equal (org-json--plists-get :a) nil))
    ;; (should (equal (org-json--plists-get :a :default 123) 123))
    ; One plist
    (should (equal (org-json--plists-get :a pl1) 1))
    (should (equal (org-json--plists-get-default :a 123 pl1) 1))
    (should (equal (org-json--plists-get :d pl1) nil))
    (should (equal (org-json--plists-get-default :d 123 pl1) 123))
    ; Multiple
    (should (equal (org-json--plists-get :a pl1 pl2 pl3) 1))
    (should (equal (org-json--plists-get-default :a 123 pl1 pl2 pl3) 1))
    (should (equal (org-json--plists-get :b pl1 pl2 pl3) 2))
    (should (equal (org-json--plists-get :b pl2 pl3) 20))
    (should (equal (org-json--plists-get :c pl1 pl2 pl3) 3))
    (should (equal (org-json--plists-get :d pl1 pl2 pl3) 40))
    (should (equal (org-json--plists-get :f pl1 pl2 pl3) 500))
    (should (equal (org-json--plists-get :g pl1 pl2 pl3) nil))
    (should (equal (org-json--plists-get-default :g 123 pl1 pl2 pl3) 123))
    ))
