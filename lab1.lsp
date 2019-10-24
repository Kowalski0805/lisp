((lambda (l1 l2 l3)
  (print (list (first l1) (first l2) (first l3)))
) '(T (U U1 U2) (U4 U6 U8)) '(4 6 (7 8 9)) '(78 89 90 67 45))

((lambda (l1 l2 l3)
  (print (cons
    (car l1)
    (cons
      (car l2)
      (cons
        (car l3)
        nil
      )
    )
  ))
) '(T (U U1 U2) (U4 U6 U8)) '(4 6 (7 8 9)) '(78 89 90 67 45))

(defun getelems (l1 l2 l3)
  (cons
    (cadr l1)
    (cons
      (caddr l2)
      (cons
        (cadddr l3)
        nil
      )
    )
  )
)

(defun getelemsnew (l1 l2 l3)
  (list
    (second l1)
    (third l2)
    (fourth l3)
  )
)

(defun getelemsnewnew (x1 x2 x3 l1 l2 l3)
  (list
    (nth (- x1 1) l1)
    (nth (- x2 1) l2)
    (nth (- x3 1) l3)
  )
)

(defun check2 (n)
  (cond
    ((>= n 2) (check2 (/ n 2)))
    ((= n 1) t)
    (t nil)
  )
)

(defun memb (a s)
  (cond
    ((null (atom a)) nil)
    ((null (listp s)) nil)
    ((null s) nil)
    ((equal a (car s)) t)
    (t (memb a (cdr s)))
  )
)

(defun unio (s1 s2)
  (cond
    ((null s1) s2)
    ((null s2) s1)
    ((memb (car s2) s1) (unio s1 (cdr s2)))
    (t (cons (car s2) (unio s1 (cdr s2))))
  )
)

(defun minus (s1 s2)
  (cond
    ((null s1) nil)
    ((null s2) s1)
    ((memb (car s1) s2) (minus (cdr s1) s2))
    (t (cons (car s1) (minus (cdr s1) s2)))
  )
)

(defun inter (s1 s2)
  (cond
    ((null s1) s1)
    ((null s2) nil)
    ((memb (car s1) s2) (cons (car s1) (inter (cdr s1) s2)))
    (t (inter (cdr s1) s2))
  )
)
