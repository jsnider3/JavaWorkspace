;Josh Snider
(setf *short-term-memory*
  '((Marcus is a man)
  (Cleopatra is a beautiful woman)
  (Caesar is a ruler)
  (Marcus tried to assassinate Caesar))
)

(setf *rule-base*
  '((not-loyal1
  if
    ((? x) is a person)
    ((? y) is a ruler)
    ((? x) tried to assassinate (? y))
  then
    ((? x) was not loyal to (? y)))
    (person1
  if 
    ((? x) is a man)
  then
    ((? x) is a person)) )
)

(defun update-stm (S)
  "If S is not in short-term memory add it and print \"I inferred: S\"
  if it is in short term memory print \"S is already known\""
  ;Iterate through the elements of short-term-memory.
  (dolist (x *short-term-memory*)
    (if (equal x S) ;If S matches the current element.
    ( ;Print "S is already known."
      return-from update-stm (if (format t "~S is already known." S) nil nil)
    )
    )
  )
  ;If you get to the end of the list
  ;Add S to short-term-memory
  (setf *short-term-memory* (cons S *short-term-memory*))
  ;Print "I inferred: S"
  (format t "I inferred: ~S" S)
)

(defun instantiate (S B)
  "Replace the variables in S with the bindings in B."
  ;If we've reached the base case where S is a 
  ;list of two elements with ? for the first one.
  (case (listp S); If S is just a symbol, return it.
    (nil (return-from instantiate S))
  )
  (cond
    ((equal B nil) (return-from instantiate S));If we have nothing to bind, return S.
    ((equal (listp S) nil)(return-from instantiate S));Added this a second time while debugging.
    ((equal (first S)'?) (return-from instantiate (if(equal(assoc (second S) B) nil)S(second(assoc (second S) B)))))
  )
  (case (listp S)
    (t (let ((result(make-sequence 'list (list-length S))))
      ;In haskell this would be (map instantiate S (replicate (length S) B))
      (map-into result #'instantiate S (make-list (list-length S) :initial-element B))
      (return-from instantiate (remove nil result))
      )
    )
  )
  ;If we're not at the base case.
    ;Map instantiate onto each element of S.
)

(defun test-instantiate ()
  (print (instantiate '(A B C) ( ) ))
  (print (instantiate '(A B C) '((X A) (Y B))))
  (print (instantiate '(A (? X) B (? X) C) '((X A) (Y B))))
  (print (instantiate '(A ((? X) (B (? X)) D) C) '((X A) (Y B))))
  (print (instantiate '((A) ((? X) (B (? Y)) D) C) '((X A) (Y B))))
  (print (instantiate '(A (((? Y)) (B (? X)) D) C) '((X A) (Y B) (Z C))))
  (print (instantiate '(A ((B)) (((? Z) (B (? X)) D)) C) '((X A) (Y B))))
  (print (instantiate '(A ((? Y)) (((? Z) (B (? X)) D)) C) '((X A) (Y B) (Z C))))
  (print (instantiate '(((? U) ((B))) (((? Z) (B (? X)) D)) C) '((X A) (Y B))))
  (instantiate '(((? U) ((B))) (((? Z) (B (? X)) D)) C) () )
)
