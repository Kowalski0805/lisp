(defun func (a b c d temp1 temp2)
  (cond
    ((not (or (null c) (null temp2))) (cond
      ((= (car c) (car temp2)) (func a b (cdr c) d temp1 d))
      (t (func a b c d temp1 (cdr temp2)))
    ))
    ((not (null c))
      (func a b (cdr c) d (cons (car c) temp1) d)
    )
    ((not (null temp1)) (func a (cons (car temp1) b) c d (cdr temp1) a))
    ((not (or (null temp2) (null b))) (cond
      ((= (car b) (car temp2)) (cons (car b) (func a (cdr b) c d temp1 a)))
      (t (func a b c d temp1 (cdr temp2)))
    ))
    ((not (null b)) (func a (cdr b) c d temp1 a))
    (t nil)
  )
)

(print
  (func '(1 2 3 4 5 6 7 8)
    '(2 5 7 8 9 10 11 12)
    '(3 5 6 8 10 12 13 14)
    '(4 5 6 8 11 12 14 15)
    nil
    '(4 5 6 8 11 12 14 15)
  )
)
