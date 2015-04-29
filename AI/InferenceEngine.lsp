;(defvar *short-term-memory*
;'((PAM IS A PARENT OF BOB)
;(BOB IS A PARENT OF ANN)
;(ANN IS A PARENT OF TOM)
;(TOM IS A PARENT OF LIZ)))

;(defvar *rule-base*
;'((PREDECESSOR1
;IF
;((? X) IS A PARENT OF (? Y))
;THEN
;((? X) IS A PREDECESSOR OF (? Y)))
;(PREDECESSOR2
;IF
;((? X) IS A PARENT OF (? Y))
;((? Y) IS A PREDECESSOR OF (? Z))
;THEN
;((? X) IS A PREDECESSOR OF (? Z)))
;))

(defvar *short-term-memory*
'((Alice is in the same bipartite set as Bob)
(Bob is in the same bipartite set as Charlie)
(David is in the same bipartite set as Charlie)
(David is connected to Eric)
(Eric is connected to Fred)
)
)

(defvar *rule-base* 
 
 '((SYMMETRIC1
 IF 
 ((? X) is in the same bipartite set as (? Y)) 
 THEN 
 ((? Y) is in the same bipartite set as (? X)) 
 )
 
 (TRANSITIVE1
 IF 
 ((? X) is in the same bipartite set as (? Y))
 ((? Y) is in the same bipartite set as (? Z))  
 THEN 
 ((? X) is in the same bipartite set as (? Z)) 
 )
 
 (DEF1
 IF 
 ((? X) is connected to (? Y)) 
 THEN 
 ((? X) is in the opposing bipartite set of (? Y)) )
 
 (SYMMETRIC2
 IF 
 ((? X) is in the opposing bipartite set of (? Y)) 
 THEN 
 ((? Y) is in the opposing bipartite set of (? X)) )
 
 (TRANSITIVE2
 IF 
 ((? X) is in the same bipartite set as (? Y))
 ((? Y) is in the opposing bipartite set of (? Z))  
 THEN 
 ((? X) is in the opposing bipartite set of (? Z))
 )
 
 (TRANSITIVE3
 IF 
 ((? X) is in the opposing bipartite set of (? Y))
 ((? Y) is in the opposing bipartite set of (? Z))  
 THEN 
 ((? X) is in the same bipartite set as (? Z))
 )
) 
)

(defun fc-production-system ()
  (setf len (length *short-term-memory*))
  ;try all of the rules.
  (dolist (rule *rule-base*)
    (try-rule rule)
  )
  (cond   ;If we've discovered new stuff, print and continue.
      ((equal len (length *short-term-memory*))   (format t "There is nothing else to be inferred.~%"))
      (t (format t "I am trying the rules again.~%") (fc-production-system)));else print message and end
  t ;return true.
)

(defun try-rule (rule)
  ;Break the rule down into its condition and conclusion.
  (setf rule-condition (get-condition rule))
  (setf rule-conclusion (get-conclusion rule))
  ;Get a list of all the bindings that work.
  (setf pos-bindings(second(match-conditions rule-condition nil)))
  ;Try adding all of the bindings to knowledge base.
  (dolist (binding pos-bindings)
    (update-stm (first(instantiate rule-conclusion binding)))
  )
)

(defun get-condition (rule)
  ;Get the part of the rule after the if and before the then.
  (subseq rule (+(position 'if rule :test #'equal)1) (position 'then rule :test #'equal))
)

(defun get-conclusion (rule)
  ;Get everything after the then.
  (subseq rule (+(position 'then rule :test #'equal)1))
)

(defun update-stm (S)
  "If S is not in short-term memory add it and print \"I inferred: S\"
  if it is in short term memory print \"S is already known\""
  ;Test if it's an element of the list and behave accordingly.
  (cond
  ((not (member S *short-term-memory* :test #'equal)) (push S *short-term-memory*)(format t "I inferred: ~S~%" S))
  )
)

(defun instantiate (pattern bindings)
  (cond ((atom pattern) pattern)
    ((equal (first pattern) '?)
      (cond ((second (assoc (second pattern) bindings)))
        (t pattern)))
    (t  (cons (instantiate (first pattern) bindings)
        (instantiate (rest pattern) bindings)))))

(defun match-conditions (conditions all-bindings)
;; returns (t updated-bindings) if matching between conditions and the facts
;; in the short term memory was successful
;; and (nil nil) otherwise
(let ((result-test))
  (cond((endp conditions)
  ; all the conditions have been tested
  ; all-bindings contain the binds of all successful matchings
    (list t all-bindings))
    (t (setf result-test (test-one-cond(first conditions) all-bindings))
      (setf all-bindings (second result-test))
      (cond((first result-test)
        (match-conditions (rest conditions) all-bindings))
        (t (list nil nil))
      )
    )
  )
)
)

(defun test-one-cond (pattern all-bindings)
;;returns (t updated-bindings) if matching was successful
;;  and (nil nil) otherwise
  (let ((success  nil)(new-bindings ())(match-result nil))
    (cond (all-bindings
      ;there are binding lists
      ;each will be used in turn
        (dolist (bds all-bindings)
          (setf match-result
            (match-pattern-to-assertions pattern bds))
            ;returns (t updated bindings) or (nil nil)
          (cond ((first match-result)
              (setf success t)
              (setf new-bindings
                (append new-bindings
                  (second match-result))))))
        (list success new-bindings))
      (t
      ;there are no binding variables
        (match-pattern-to-assertions pattern ())))))

(defun match-pattern-to-assertions (pattern binding)
;; returns (success all-bindings)
(let ((success nil)(all-bindings ())(match-result ()))
  (dolist (assertion *short-term-memory*)
    (setf match-result (match pattern assertion binding))
    ;returns (t bindings) or (nil nil)
    (cond ((first match-result)(setf success t)
                  (push (second match-result)
                    all-bindings))))
  (list success all-bindings)
))

(defun match (p s binds)
(let ( (temp nil) )
  (cond   ((atom p)   (cond ((equal p s) (list t binds))
              (t (list nil nil))))
      ((equal (first p) '?)   (setf temp (test-binds (second p) s binds)) 
                  (cond (temp (list t temp))
                      (t (list nil nil))))
      ((atom s)         (list nil nil))
      (t 
      ; both p and s are lists
            (setf temp (match (first p) (first s) binds))
            ; temp = (flag binds)
            (cond ((first temp)
              ; (first p) and (first s) match
                  (match (rest p) (rest s) (second temp)))
              (t (list nil nil)))))))

(defun test-binds (x v binds)
;; returns nil or the binds updated by the addition of the pair (x v)
(let ( (y nil) )
  (setf y (assoc x binds))
  (cond (y (cond ((equal (second y) v)  binds)
  (t                    nil)))
    (t (setf binds (append binds (list (list x v))))))))  
