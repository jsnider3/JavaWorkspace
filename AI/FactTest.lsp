(defun test-mc ()
;; test for match-conditions
(format t "~%~%Test1:")
(setf *short-term-memory*
'((robbie has dark spots)
(robbie has hair)
(robbie has hoofs))
)
(setf conditions
'(
((? x) is a mammal)
)
)

(print (match-conditions conditions nil))
(format t "~%Expect to return: ~%~a"'(NIL NIL))
(format t "~%Test2:")
(setf *short-term-memory*
'((robbie has dark spots)
(robbie has hair)
(robbie has hoofs))
)
(setf conditions
'(
((? x) has hair)
)
)
(print (match-conditions conditions nil))
(format t "~%Expect to return: ~%~a" '(T (((X ROBBIE)))))
(format t "~%~%Test3:")
(setf *short-term-memory*
'((robbie has dark spots)
(robbie is a mammal)
(robbie has hair)
(robbie has hoofs))
)
(setf conditions
'(
((? x) is a mammal)
((? x) has hoofs)
)
)

(print (match-conditions conditions nil))
(format t "~%Expect to return: ~%~a" '(T (((X ROBBIE)))))
(format t "~%~%Test4:")
(setf *short-term-memory*
'((Marcus is a man)
(Marcus is a person)
(Cleopatra is a beautiful woman)
(Caesar is a ruler)
(Marcus tried to assassinate Caesar))
)
(setf conditions
'(
((? x) is a person)
((? y) is a ruler)
((? x) tried to assassinate (? y))
)
)
(print (match-conditions conditions nil))
(format t "~%Expect to return: ~%~a" '(T (((X MARCUS) (Y CAESAR)))))
(format t "~%~%Test5:")
(setf *short-term-memory*
'((Marcus is a person)
(Marcus is a man)
(Cleopatra is a beautiful woman)
(Caesar is a ruler)
(Marcus tried to assassinate Caesar)
(Peter is a person)
(Peter is a man)
(Cleopatra is a beautiful woman)
(Gary is a ruler)
(Peter tried to assassinate Gary))
)
(setf conditions
'(
((? x) is a person)
((? y) is a ruler)
((? x) tried to assassinate (? y))
)
)

(print (match-conditions conditions nil))
(format t "~%Expect to return: ~%~a" '(T (((X PETER) (Y GARY)) ((X MARCUS) (Y CAESAR)))))
(format t "~%~%Test6:")
(setf *short-term-memory*
'(((block A on block B))
((block B on block C))
((block C on block D)))
)
(setf conditions
'(
((block (? x) on block (? y)))
((block (? y) on block (? z)))
)
)
(print (match-conditions conditions nil))
(format t "~%Expect to return: ~%~a" '(T (((X B) (Y C) (Z D)) ((X A) (Y B) (Z C)))))
NIL
)
(test-mc)
