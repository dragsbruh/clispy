(print
  (cat
    (cat
      "the number 10, relative to 5 is "
      (let x 10
        (if (gt x 5)
          "big"
          "small")))
    "\n"))

(rec fact (n)
  (if (le n 1)
    1
    (mult n (fact (sub n 1))))
  (print
    (cat
      (cat "the factorial of 10 is " (str (fact 10)))
      "\n")))

(rec fib (n)
  (if (le n 1)
    n
    (add (fib (sub n 1)) (fib (sub n 2))))
  (fib 10))

(rec even (n)
  (rec odd (n)
    (if (eq n 0)
      false
      (even (sub n 1)))
    (if (eq n 0)
      true
      (odd (sub n 1))))
  (even 5))
