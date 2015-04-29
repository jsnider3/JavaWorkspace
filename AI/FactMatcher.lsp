;Josh Snider
;My match-conditions requires match5, test-binds, completeSolution,
;invalidSolution, get-pos-truths, and instantiate to work properly.

(setf *short-term-memory*
  '((Marcus is a person)
  (Marcus is a man)
  (Cleopatra is a beautiful woman)
  (Caesar is a ruler)
  (Marcus tried to assassinate Caesar)
  (Peter is a person)
  (Peter is a man)
  (Gary is a ruler)
  (Peter tried to assassinate Gary))
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
  ;Test if it's an element of the list and behave accordingly.
  (cond
    ((find S *short-term-memory* :test #'equal) (format t "~S is already known." S))
    (t (setf *short-term-memory* (cons S *short-term-memory*))(format t "I inferred: ~S" S))
  )
)

(defun instantiate (S B)
  "Replace the variables in S with the bindings in B."
  (cond
    ((equal B nil) S);If we have nothing to bind, return S.
    ((equal (listp S)nil)S);If we're at an atom, return it.
    ((equal (first S)'?) (if(equal(assoc (second S) B) nil)S(second(assoc (second S) B))))
    (t (let ((result(make-sequence 'list (list-length S))));If we're not at a base case.
      ;In haskell this would be map instantiate S (replicate (length S) B))
      (map-into result #'instantiate S (make-list (list-length S) :initial-element B))
      ;Map instantiate onto each element of S.
      (remove nil result)
      )
    )
  )
)

(defun get-pos-truths (conditions)(let ((binds nil))
    (cons t binds)
    (dolist (k conditions);Get every possible true thing.
      (dolist (p *short-term-memory*)
        (cond 
          ((not(equal(match5 k p nil) '(nil nil))) 
            (let ((templist (first(rest(match5 k p nil)))))
              (dolist (x templist)(push x binds))
            )
          )
        )
      )
    )
    (setf binds (remove-duplicates binds :test #'equal))
    (sort binds (lambda (x y)(string< (symbol-name(first x))(symbol-name(first y)))))
    )
)   

(defun invalidSolution (conditions)(let ((setdif (set-difference conditions *short-term-memory* :test 'equal)))
    ;(format t "Invalid Solution ~a setdif? ~a ~%" conditions setdif)
    (let((containsNoVar (not(equal setdif nil))))
      (dolist (dif setdif)
      ;If dif doesn't contain a variable, return true.
      (dolist (el dif)
        (cond ((and(listp el)(equal(first el)'?))( setf containsNoVar nil)))
      )
      )
      containsNoVar
    )
  )
)
(defun completeSolution (conditions)
  (let ((setdif (set-difference conditions *short-term-memory* :test 'equal)))
    (equal setdif nil)
  )
)
(defun match-conditions (conditions all-bindings)
  ;Steps
  ;1)Sub in all-bindings in the conditions.
  ;1b)If one of the results isn't in *short-term-memory*, return (nil nil).
  ;1c)If everything is in *short-term-memory and there are no variables left return (t all-bindings)
  ;2)Generate every possible truth.
  ;3)Generate every possible truth set.
  ;4a)If one of the truth sets doesn't deal with all of the variables return (nil nil)
  ;4b)Go through all the truth-sets, collecting the ones that work out.
  (setf conditions (instantiate conditions all-bindings))
  (cond
    ((invalidSolution conditions) (return-from match-conditions (list nil nil)))
    ((completeSolution conditions) (return-from match-conditions (list t all-bindings)))
  )
  (let ((binds (get-pos-truths conditions)))
    (setf result (list(list(first binds))))
    (setf binds (rest binds))
    (dolist (b binds)
      (setf binds (remove b binds))
      (cond
        (result
          (let ((result_copy (copy-list result)))
          (dolist (r result)
            (cond
              ;If we've found the spot to add b, first one handles the edge case where there's only one thing in results, second handles everything else.
              ((and(equal (first b)(first(first r)))(equal(list-length r) 1))(setf result(push (cons b(first result))(rest result))))
              ((equal (first b)(first(first r))) (setf result (list(remove nil(cons (remove r result :test 'equal) (cons b r))))))
            )
            (if (not(equal result_copy result))(return nil) t);If we've added b to the results, move to the next b.
          )
          (if (equal result_copy result)(setf result (cons (list b) result))t));If b is a binding for a new variable.
        ) 
        (t (setf result (list(list b))));This line is unreachable, but in previous versions it handled the case where result was nil.
      )
    )
    (setf cartesian (first result))
    (setf result (rest result))
    ;For each set in results.
    (dolist (cur_set result)
      (let ((accum nil))
      ;For each thing in cartesian.
      (dolist (cart cartesian)
        ;For each thing in the current set.
        (dolist (cur_thing cur_set)
          (push (cons cart (list cur_thing)) accum)
        )
      )
      (setf cartesian accum)
      )
    )
    (cond ((and(not(invalidSolution (instantiate conditions (first cartesian))))(equal (set-difference conditions *short-term-memory* :test 'equal) nil)) (return-from match-conditions (list nil nil))))
    ;for every possible solution
    (let ((result (list nil nil)))
      (dolist (posSol cartesian)
          (cond ((first (match-conditions conditions posSol)) (setf result (cons t(push posSol (rest result))))))
          ;call match-conditions conditions posSol
      )
      (cond ((first result) (setf result (cons t (list(remove nil (rest result)))))))
      result
    )
  )
)

(defun match5 (p s binds)
  (let ( (temp nil) )
  (cond 
    ((atom p) (cond (
      (equal p s) (list t binds))
      (t (list nil nil))))
    ((equal (first p) '?) 
      (setf temp (test-binds (second p) s binds)) 
      (cond 
        (temp (list t temp))
        (t (list nil nil))))
    ((atom s) (list nil nil))
    (t 
      (setf temp (match5 (first p) (first s) binds))
      (cond 
        ((first temp)(match5 (rest p) (rest s) (second temp)))
        (t (list nil nil))))))
)

(defun test-binds (x v binds)
;; returns nil or the binds updated by the addition of the pair (x v)
  (let ( (y nil) )
    (setf y (assoc x binds))
    (cond 
      (y (cond 
        ((equal (second y) v) binds)
        (t nil)))
      (t (setf binds (append binds (list (list x v)))))
    )
  )
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
