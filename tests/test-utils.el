; Tests of utility code

(require 'ox-json)
(require 'ox-json-test-helpers)


(ert-deftest test-merge-alists ()
  "Test `ox-json--merge-alists'."
  ; Complex case
  (should
    (equal
      (ox-json--merge-alists
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
      (ox-json--merge-alists
        '((a . 1) (b . 2) (c . 3) (a . 100)))
      '((a . 1) (b . 2) (c . 3))))
  ; Edge case - merge no lists
  (should
    (equal
      (ox-json--merge-alists)
      nil))
  )


(ert-deftest test-plist-to-alist ()
  (should (equal
    (ox-json--plist-to-alist '(one 1 two 2 three 3))
    '((one . 1) (two . 2) (three . 3)))))


(ert-deftest test-plists-get ()
  (let ((pl1 '(:a 1 :b 2 :c 3))
        (pl2 '(:b 20 :d 40))
        (pl3 '(:b 200 :d 400 :f 500)))
    ; No plists
    (should (equal (ox-json--plists-get :a) nil))
    ;; (should (equal (ox-json--plists-get :a :default 123) 123))
    ; One plist
    (should (equal (ox-json--plists-get :a pl1) 1))
    (should (equal (ox-json--plists-get-default :a 123 pl1) 1))
    (should (equal (ox-json--plists-get :d pl1) nil))
    (should (equal (ox-json--plists-get-default :d 123 pl1) 123))
    ; Multiple
    (should (equal (ox-json--plists-get :a pl1 pl2 pl3) 1))
    (should (equal (ox-json--plists-get-default :a 123 pl1 pl2 pl3) 1))
    (should (equal (ox-json--plists-get :b pl1 pl2 pl3) 2))
    (should (equal (ox-json--plists-get :b pl2 pl3) 20))
    (should (equal (ox-json--plists-get :c pl1 pl2 pl3) 3))
    (should (equal (ox-json--plists-get :d pl1 pl2 pl3) 40))
    (should (equal (ox-json--plists-get :f pl1 pl2 pl3) 500))
    (should (equal (ox-json--plists-get :g pl1 pl2 pl3) nil))
    (should (equal (ox-json--plists-get-default :g 123 pl1 pl2 pl3) 123))
    ))


(ert-deftest test-is-node ()
  (should (ox-json--is-node '(bold (:begin 1 :end 100) "foo")))
  (should (ox-json--is-node '(bold (:begin 1 :end 100))))
  (should-not (ox-json--is-node nil))
  (should-not (ox-json--is-node "string"))
  (should-not (ox-json--is-node '(bold)))
  (should-not (ox-json--is-node '(bold "foo")))
  ; Non-list cons was creating problems
  (should-not (ox-json--is-node '(foo . bar))))


(ert-deftest test-loop-plist ()
  (let ((plist '(:a 1 :b 2 :c 3 :d 4)))
    (should (equal '(:a :c)
              (ox-json--loop-plist (key value plist)
                if (oddp value)
                collect key)))))
