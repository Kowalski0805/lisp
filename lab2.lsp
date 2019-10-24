(defun cut (lst n)
  (cond
    ((<= n 0) nil)
    ((< (list-length lst) n) lst)
    (t (cut (reverse (cdr (reverse lst))) n))
  )
)

(defun sedgewick (i)
  (cond
    ((= (mod i '2) '0) (+ '1 (- (* '9 (expt '2 i)) (* '9 (expt '2 (/ i '2))))))
    (t (+ '1 (- (* '8 (expt '2 i)) (* '6 (expt '2 (/ (+ i '1) '2))))))
  )
)

; reverse in usage
(defun get-sedgewick (s i)
  (cond
    ((> (* 3 (sedgewick i)) s) nil)
    (t (cons (sedgewick i) (get-sedgewick s (+ i 1))))
  )
)

(defun back-compare (lst i step fn)
  (cond
    ((< i step) lst)
    ((> (nth i lst) (nth (- i step) lst)) (back-compare lst (- i step) step nil))
    (t (back-compare lst (- i step) step (rotatef (nth i lst) (nth (- i step) lst))))
  )
)

(defun move-group (lst i step)
  (cond
    ((>= i (list-length lst)) lst)
    (t (back-compare lst i step nil) (move-group lst (+ i 1) step))
  )
)

(defun move-iter (lst i steps)
  (cond
    ((null i) lst)
    (t (move-group lst i i) (move-iter lst (car steps) (cdr steps)))
  )
)

(defun get-iters (lst)
  (reverse (get-sedgewick (list-length lst) 0))
)

(defun shell-sort (lst)
  (move-iter lst (car (get-iters lst)) (cdr (get-iters lst)))
  lst
)

(defun cocktail-forward (lst i max fn)
  (cond
    ((>= i max) lst)
    ((< (nth i lst) (nth (+ i 1) lst)) (cocktail-forward lst (+ i 1) max nil))
    (t (cocktail-forward lst (+ i 1) max (rotatef (nth i lst) (nth (+ i 1) lst))))
  )
)

(defun cocktail-backward (lst i min fn)
  (cond
    ((<= i min) lst)
    ((> (nth i lst) (nth (- i 1) lst)) (cocktail-backward lst (- i 1) min nil))
    (t (cocktail-backward lst (- i 1) min (rotatef (nth i lst) (nth (- i 1) lst))))
  )
)

(defun cocktail-iter (lst i max)
  (cond
    ((>= i (/ (list-length lst) 2)) lst)
    (t
      (cocktail-forward lst i max nil)
      (cocktail-backward lst max i nil)
      (cocktail-iter lst (+ i 1) (- max 1))
    )
  )
)

(defun cocktail-sort (lst)
  (cocktail-iter lst 0 (- (list-length lst) 1))
  lst
)

(defun join (l1 l2)
  (cond
    ((and (< (first l1) (second l1)) (> (first l2) (second l2))) nil)
    ((< (first l1) (second l1)) (shell-sort (append l1 l2)))
    (t (reverse (shell-sort (append l1 l2))))
  )
)

(defun mod2 (lst)
  (cond
    ((null lst) nil)
    ((listp (car lst)) (cons (mod2 (car lst)) (mod2 (cdr lst))))
    ((numberp (car lst)) (cons (mod (car lst) 2) (mod2 (cdr lst))))
    (t (cons (car lst) (mod2 (cdr lst))))
  )
)
