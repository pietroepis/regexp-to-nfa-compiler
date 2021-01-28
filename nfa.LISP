;;;; Pietro Epis 845045
;;;; Michele Milesi 844682

;;;; -*- Mode: Lisp -*-

;;;; nfa.lisp --

;;;; is-regexp(RE) 
;;;; is-regexp(RE) is true if RE represents a valid Regular Expression

(defun is-regexp (RE) 
  (when RE
    (cond ((atom RE) T)
          ((and (atom (first RE))
                (=(length RE) 1))
           T)
          
          ((or (eql (first RE) 'or)
               (eql (first RE) 'seq))
           (and (is-regexp (second RE))
                (is-regexp(append (list (first RE)) (cdr (cdr RE))))))
          
          ((or (eql (first RE) 'star)
                    (eql (first RE) 'plus))
           (and (= (length (cdr RE)) 1)
                (is-regexp (rest RE))))

          (T T)))) 

;;;; nfa-regexp-comp(RE) checks whether RE is a valid
;;;; Regular Expression, creates initial and final states and
;;;; then calls build-nfa

(defun nfa-regexp-comp (RE)
  (if (is-regexp RE) 
      (list '(initial 1) '(final 2) (car (build-nfa RE 1 2 2)))
    NIL))

;;;; build-nfa(RE Initial Final Counter) actually creates states and
;;;; transitions of the automata

(defun build-nfa (RE initial final counter)
  (let ((n_initial (+ counter 1))
        (n_final (+ counter 2))
        (x (+ counter 2)))
    
    (if (listp RE) 
        (cond ((eq (car RE) 'star) 
               (let ((sub_exp (build-nfa (car (cdr RE)) n_initial n_final x)))
               (cons (append (list (list 'delta initial 'epsilon final) 
                             (list 'delta initial 'epsilon n_initial)
                             (list 'delta n_final 'epsilon final)
                             (list 'delta n_final 'epsilon n_initial))
                             (car sub_exp))
                     (cdr sub_exp))))
          
              ((eq (car RE) 'plus)
               (build-nfa (append (list 'seq) 
                                  (cdr RE) 
                                  (list (append (list 'star) 
                                                (cdr RE)))) 
                          initial 
                          final
                          counter))

              ((eq (car RE) 'seq)
               (let ((sub_exp (build-nfa (car (cdr RE)) n_initial n_final x))) 
                  (if (> (length RE) 2)
                      (let ((sub_exp2 (build-nfa (append (list (car RE)) 
                                                         (cdr (cdr RE))) 
                                                 n_final
                                                 final
                                                 (cdr sub_exp))))
                        (cons (append (list (list 'delta initial 'epsilon n_initial))
                                      (car sub_exp)
                                      (car sub_exp2))
                              (cdr sub_exp2)))               
                    (cons (append (list (list 'delta initial 'epsilon n_initial))
                                  (car sub_exp)
                                  (list (list 'delta n_final 'epsilon  final)))
                          (cdr sub_exp)))))

              
              ((eq (car RE) 'or)
               (let ((sub_exp (build-nfa (car (cdr RE)) n_initial n_final x)))
                 (if (> (length RE) 2)
                     (let ((sub_exp2 (build-nfa (append (list (car RE))
                                                        (cdr (cdr RE)))
                                                initial
                                                final
                                                (cdr sub_exp))))
                       (cons (append (list (list 'delta initial 'epsilon n_initial))
                                     (car sub_exp)
                                     (car sub_exp2)
                                     (list (list 'delta n_final 'epsilon final)))
                             (cdr sub_exp2)))

                   (cons (append (list (list 'delta initial 'epsilon n_initial))
                           (car sub_exp)
                           (list (list 'delta n_final 'epsilon final)))
                         (cdr sub_exp)))))

              (T (cons (list (list 'delta initial RE final))
                       counter)))
    
      (when (atom RE)
        (cons (list (list 'delta initial RE final))
              counter)))))

;;;; nfa-test(nfa input) checks whether the automata accepts the Input or not

(defun nfa-test (nfa input)
  (if (is-nfa nfa)
      (nfa-accept nfa input (second (car nfa)) 0)
    (print "This is not a NFA")))

;;;; nfa-accept(nfa input state i_delta) traverses the automata in relation to Input.

(defun nfa-accept (nfa input state i_delta)
  (when (listp input)
    (let ((delta (nth i_delta (third nfa))) )
      (cond ((and (= state 2) (null input)) T)
            ((= (length (third nfa)) i_delta) NIL)
            ((= (second delta) state)
             (if (and (equal (third delta) 'epsilon)
                      (nfa-accept nfa input (fourth delta) 0))
                 T
               (if (and (equal (third delta) (first input))
                        (nfa-accept nfa (rest input) (fourth delta) 0))
                   T
                 (nfa-accept nfa input state (1+ i_delta)))))
            (T (nfa-accept nfa input state (1+ i_delta)))))))
  

;;;; is-nfa(nfa) checks if nfa is a nfa compiled by nfa-regexp-comp 

(defun is-nfa (nfa)
  (when (listp nfa)
    (and (equal (first (first nfa)) 'initial)
         (= (length (first nfa)) 2)
         (= (length (second nfa)) 2)
         (equal (first (second nfa)) 'final)
         (is-deltas (third nfa)))))

;;;; is-deltas(deltas) checks if every element in the list
;;;; respects the structure of a delta

(defun is-deltas (deltas)
  (when (listp deltas)
    (if (= (length deltas) 1)
        (and (= (length (first deltas)) 4)
             (equal (first (first deltas)) 'delta))
      (and (is-deltas (list (first deltas)))
           (is-deltas (rest deltas))))))
      
;;;; end of file -- nfa.lisp




        
         