(defvar *currname* "")
(defvar *curr* '())

(defun create ()
  (princ "Enter DB name: ")
  (setq db (read-line))
  (with-open-file
    (str (concatenate 'string "./db/" db)
      :direction :output
      :if-exists :supersede
      :if-does-not-exist :create
    )
  )
  (cli)
)

(defun connect ()
  (princ "Enter DB name: ")
  (setq db (read-line))
  (setq *currname* db)
  (setq *curr* '())
  (with-open-file
    (stream (concatenate 'string "./db/" db))
    (setq *curr*
      (read-from-string
        (concatenate 'string "(" (read-line stream nil) ")")
      )
    )
  )
  (cli)
)

(defun save ()
  (with-open-file
    (out (concatenate 'string "./db/" *currname*)
      :direction :output :if-exists :overwrite
    )
    (dolist (segment *curr*)
      (format out "~D " segment)
    )
    (format out "~%")
  )
  (cli)
)

(defun add ()
  (princ "Enter row: ")
  (setq row (read))
  (setq *curr* (cons row *curr*))
  (cli)
)

(defun modify ()
  (princ "Enter row id: ")
  (setq id (read))
  (princ "Enter modify key: ")
  (setq key (read))
  (princ "Enter modify value: ")
  (setq val (read))
  (rplacd (assoc key (cdr (assoc id *curr*))) val)
  (cli)
)

(defun deleterow ()
  (princ "Enter row id: ")
  (setq id (read))
  (setq *curr* (remove (assoc id *curr*) *curr*))
  (cli)
)

(defun listrow ()
  (pprint *curr*)
  (cli)
)

(defun searchrow ()
  (princ "Enter search key: ")
  (setq key (read))
  (princ "Enter search value: ")
  (setq val (read))
  (cond
    ((equal key 'id) (pprint (assoc val *curr*)))
    (t
      (pprint
        (remove-if-not
          #'(lambda (x) (equal (cdr (assoc key x)) val))
          *curr*
          :key #'cdr
        )
      )
    )
  )
  (cli)
)

(defun sortrow ()
  (princ "Enter sort key: ")
  (setq key (read))
  (setq curr (copy-seq *curr*))
  (cond
    ((equal key 'id) (pprint (sort curr #'< :key #'car)))
    (t
      (pprint
        (sort
          curr
          #'(lambda (a b)
            (cond
              ((equal (type-of a) 'fixnum) (< a b))
              (t (string-lessp a b))
            )
          )
          :key #'(lambda (x) (cdr (assoc key (cdr x))))
        )
      )
    )
  )
  (cli)
)

(defun help ()
  (terpri)
  (print "1 - create ")
  (print "2 - connect")
  (print "3 - save   ")
  (print "4 - add    ")
  (print "5 - modify ")
  (print "6 - delete ")
  (print "7 - list   ")
  (print "8 - search ")
  (print "9 - sort   ")
  (print "q - quit   ")
  (terpri)
)

(defun cli ()
  (help)
  (princ "> ")
  (setq key (read))
  (cond
    ((equal key 'q) (quit))
    ((equal key '1) (create))
    ((equal key '2) (connect))
    ((equal key '3) (save))
    ((equal key '4) (add))
    ((equal key '5) (modify))
    ((equal key '6) (deleterow))
    ((equal key '7) (listrow))
    ((equal key '8) (searchrow))
    ((equal key '9) (sortrow))
    (t (cli))
  )
)
