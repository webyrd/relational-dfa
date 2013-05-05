(load "mk.scm")
(load "test-check.scm")

;;; A relational deterministic finite automaton interpreter written in
;;; miniKanren, created during the 8th miniKanren Hangout on 5 May
;;; 2013:
;;;
;;; https://www.youtube.com/watch?v=ux6St9r-KmM

;;; Please see 'fsm.scm' for a description of the 5-tuple representing
;;; the machine.

(define fsmo
  (lambda (machine str out)
    (fresh (Q Sigma delta q0 F)
      (== `(,Q ,Sigma ,delta ,q0 ,F) machine)
      (letrec ([fsmo (lambda (str state out)
                       (conde
                         [(== '() str)
                          (accept/rejecto state F out)]
                         [(fresh (a d state^)
                            (== `(,a . ,d) str)
                            (lookupo `(,state ,a) delta state^)
                            (fsmo d state^ out))]))])
         (fsmo str q0 out)))))

(define lookupo
  (lambda (state/a delta state^)
    (fresh (rest)
      (conde
        [(== `((,state/a . ,state^) . ,rest) delta)]
        [(fresh (s^/a^ _)
           (== `((,s^/a^ . ,_) . ,rest) delta)
           (=/= s^/a^ state/a)
           (lookupo state/a rest state^))]))))

(define accept/rejecto
  (lambda (state F out)
    (conde
      [(== '() F) (== 'reject out)]
      [(fresh (a d)
         (== `(,a . ,d) F)
         (conde
           [(== state a) (== 'accept out)]
           [(=/= state a) (accept/rejecto state d out)]))])))

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

(test "accept/rejecto-1"
  (run* (q) (accept/rejecto 'S0 '(S0) q))
  '(accept))

(test "accept/rejecto-2"
  (run* (q) (accept/rejecto 'S1 '(S0) q))
  '(reject))

(test "accept/rejecto-3"
  (let ((F '(S0)))
    (run* (q) (fresh (s v) (accept/rejecto s F v) (== `(,s ,v) q))))
  '((S0 accept)
    ((_.0 reject) (=/= ((_.0 S0))))))


(test "lookupo-1"
  (let ((delta '(((S0 0) . S0)
                 ((S0 1) . S1)
                 ((S1 0) . S2)
                 ((S1 1) . S0)
                 ((S2 0) . S1)
                 ((S2 1) . S2))))
    (run* (q) (lookupo '(S1 0) delta q)))
  '(S2))

(test "lookupo-2"
  (let ((delta '(((S0 0) . S0)
                 ((S0 1) . S1)
                 ((S1 0) . S2)
                 ((S1 1) . S0)
                 ((S2 0) . S1)
                 ((S2 1) . S2))))
    (run* (q) (fresh (pr s) (lookupo pr delta s) (== `(,pr ,s) q))))
  '(((S0 0) S0)
    ((S0 1) S1)
    ((S1 0) S2)
    ((S1 1) S0)
    ((S2 0) S1)
    ((S2 1) S2)))

(test "fsmo-0"
  (run 10 (q) (fresh (machine str out) (fsmo machine str out) (== `(,machine ,str ,out) q)))
  '(((_.0 _.1 _.2 _.3 ()) () reject)
    ((_.0 _.1 _.2 _.3 (_.3 . _.4)) () accept)
    (((_.0 _.1 _.2 _.3 (_.4)) () reject) (=/= ((_.3 _.4))))
    (((_.0 _.1 _.2 _.3 (_.4 _.3 . _.5)) () accept) (=/= ((_.3 _.4))))
    ((_.0 _.1 (((_.2 _.3) . _.4) . _.5) _.2 ()) (_.3) reject)
    (((_.0 _.1 _.2 _.3 (_.4 _.5)) () reject) (=/= ((_.3 _.4)) ((_.3 _.5))))
    (((_.0 _.1 _.2 _.3 (_.4 _.5 _.3 . _.6)) () accept) (=/= ((_.3 _.4)) ((_.3 _.5))))
    (((_.0 _.1 _.2 _.3 (_.4 _.5 _.6)) () reject) (=/= ((_.3 _.4)) ((_.3 _.5)) ((_.3 _.6))))
    (((_.0 _.1 _.2 _.3 (_.4 _.5 _.6 _.3 . _.7)) () accept) (=/= ((_.3 _.4)) ((_.3 _.5)) ((_.3 _.6))))
    (((_.0 _.1 _.2 _.3 (_.4 _.5 _.6 _.7)) () reject) (=/= ((_.3 _.4)) ((_.3 _.5)) ((_.3 _.6)) ((_.3 _.7))))))

(test "fsmo-1"
  (run 1 (q) (fsmo wiki-dfa '(0 1 1) q))
  '(accept))

(test "fsmo-2"
  (run 1 (q) (fsmo wiki-dfa '(0 1 1 1) q))
  '(reject))

(test "fsmo-3"
  (run 1 (q) (fsmo wiki-dfa '(1) q))
  '(reject))

(test "fsmo-4"
  (run 10 (q) (fsmo wiki-dfa q 'accept))
  '(()
    (0)
    (0 0)
    (0 0 0)
    (1 1)
    (1 1 0)
    (0 0 0 0)
    (0 1 1)
    (1 1 0 0)
    (0 1 1 0)))

(test "fsmo-5"
  (run 10 (q) (fsmo wiki-dfa q 'reject))
  '((1)
    (0 1)
    (1 0)
    (0 0 1)
    (0 1 0)
    (1 0 0)
    (1 0 1)
    (1 1 1)
    (1 0 0 0)
    (0 0 0 1)))

(test "fsmo-6"
  (run* (q)
    (fresh (dfa)
      (==
       `((S0 S1 S2)
         (0 1)
         (((S0 0) . S0)
          ((S0 1) . S1)
          ((S1 0) . S2)
          ((S1 1) . S0)
          ((S2 0) . S1)
          ((S2 1) . S2))
         S0
         (S0))
       dfa)
      (fsmo dfa '(0 1 1) 'accept)))
  '(_.0))

(test "fsmo-7"
  (run 10 (q)
    (fresh (dfa start-state str)
      (==
       `((S0 S1 S2)
         (0 1)
         (((S0 0) . S0)
          ((S0 1) . S1)
          ((S1 0) . S2)
          ((S1 1) . S0)
          ((S2 0) . S1)
          ((S2 1) . S2))
         ,start-state
         (S0))
       dfa)
      (fsmo dfa str 'accept)
      (== `(,start-state ,str) q)))
  '((S0 ())
    (S0 (0))
    (S0 (0 0))
    (S0 (0 0 0))
    (S1 (1))    
    (S0 (1 1))
    (S0 (0 0 0 0))
    (S0 (0 1 1))
    (S0 (1 1 0))
    (S1 (1 0))))

(test "fsmo-8"
  (run 10 (q)
    (fresh (dfa)
      (==
       `((S0 S1 S2)
         (0 1)
         ,q
         S0
         (S0))
       dfa)
      (fsmo dfa '(0 1 1) 'accept)))
  '((((S0 0) . S0)
     ((S0 1) . S0)
     .
     _.0)

    ((((S0 0) . _.0)
      ((_.0 1) . _.1)
      ((_.1 1) . S0) . _.2)
     (=/= ((_.0 _.1))))
    
    (((S0 1) . S0) ((S0 0) . S0) . _.0)
    
    ((((S0 0) . _.0) ((_.1 1) . S0) ((_.0 1) . _.1) . _.2) (=/= ((_.0 _.1))))
    
    ((((S0 0) . _.0) ((_.0 1) . _.1) (_.2 . _.3) ((_.1 1) . S0) . _.4) (=/= ((_.0 _.1)) ((_.2 (_.1 1)))))
    
    ((((S0 0) . _.0) ((_.0 1) . _.1) (_.2 . _.3) (_.4 . _.5) ((_.1 1) . S0) . _.6) (=/= ((_.0 _.1)) ((_.2 (_.1 1))) ((_.4 (_.1 1)))))
    
    ((((_.0 1) . _.1) ((S0 0) . _.0) ((_.1 1) . S0) . _.2) (=/= ((_.0 _.1))))
    
    ((((S0 0) . S0) (_.0 . _.1) ((S0 1) . S0) . _.2) (=/= ((_.0 (S0 1)))))
    
    ((((S0 0) . _.0) ((_.0 1) . _.1) (_.2 . _.3) (_.4 . _.5) (_.6 . _.7) ((_.1 1) . S0) . _.8) (=/= ((_.0 _.1)) ((_.2 (_.1 1))) ((_.4 (_.1 1))) ((_.6 (_.1 1)))))
    
    ((((S0 1) . S0) (_.0 . _.1) ((S0 0) . S0) . _.2) (=/= ((_.0 (S0 0)))))))
