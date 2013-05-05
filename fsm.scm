(load "pmatch.scm")
(load "test-check.scm")

;;; A deterministic finite automaton interpreter written in Scheme,
;;; created during the 8th miniKanren Hangout on 5 May 2013:
;;;
;;; https://www.youtube.com/watch?v=ux6St9r-KmM

;;; A deterministic finite automaton (DFA) can be represented as a
;;; 5-tuple (as described on p. 35 of Michael Sipser's 'Introduction
;;; to the Theory of Computation, third edition')
;;;
;;; (Q Sigma delta q0 F)
;;;
;;; Here we give the 5-tuple for the DFA example in the Wikipedia article:
;;; http://en.wikipedia.org/wiki/Deterministic_finite_automaton
;;;
;;; 'Q' is the set of states: {S0 S1 S2}
;;; 'Sigma' is alphabet (binary digits in this case): {0 1}
;;; 'delta' is the transition function, of type    delta: Q x Sigma -> Q
;;; 'q0' is the starting state: S0
;;; 'F' is the set of accepting states: {S0}
;;;
;;; We can represent the transition function 'delta' as a table:
;;;
;;;     |  0    1
;;; --------------
;;; S0 |  S0   S1
;;; S1 |  S2   S0
;;; S2 |  S1   S2
;;;
;;; The table shows that when in state S1, reading a 0 results in a
;;; transition to state S2, while reading a 1 results in a transition
;;; to state S0.

(define fsm
  (lambda (machine str)
    (pmatch machine
      [(,Q ,Sigma ,delta ,q0 ,F) 
       ;;; sanity check code goes here       
       (letrec ([fsm (lambda (str state)
                       (cond
                         [(null? str)
                          (if (memq state F)
                              'accept
                              'reject)]
                         [else
                          (let ([a (car str)]
                                [d (cdr str)])
                            (let ([state (cdr (assoc (list state a) delta))])
                              (fsm d state)))]))])
         (fsm str q0))])))

;;; tests
(define wiki-dfa '((S0 S1 S2)
                   (0 1)
                   (((S0 0) . S0)
                    ((S0 1) . S1)
                    ((S1 0) . S2)
                    ((S1 1) . S0)
                    ((S2 0) . S1)
                    ((S2 1) . S2))
                   S0
                   (S0)))

(test "fsm-1"
  (fsm wiki-dfa '(0 1 1))
   'accept)

(test "fsm-2"
  (fsm wiki-dfa '(0 1 1 1))
  'reject)

(test "fsm-3"
  (fsm wiki-dfa '(1))
  'reject)
