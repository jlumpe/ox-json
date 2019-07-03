; Tests of utility code

(require 'ox-json)


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
