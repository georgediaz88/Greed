; Greed (Dice Game)
; Written by George Diaz
; Student at DePaul University
; June 3rd 2010
;
; This is a Human vs PC(1) game.
;
; Object of the game is for the first player
; hit 10,000 points and solely win over the other by rolling dice
; This game starts with the (start) function located at the bottom.
;---------------------------------------------------------------------------------------------------------------------------------------------------------

;;*total1* = points for player
;;*pc* = pc's points

(defparameter *total1* 0)
(defparameter *pc* 0)
(defparameter *count* 0)

;;Checks when player reaches 10000 points
;;if so gives PC a chance to top high score
(defun check-score ()
  (cond
   ((>= *count* 1)
      (princ " "))
   ((>= *total1* 10000)
      (and (setf *count* (1+ *count*)) (last-chance-pc)))
    (t (computer-go))))

;;Checks when PC reaches 10000 points
;;if so gives other player a chance to top high score
(defun check-pc-score ()
   (cond
   ((>= *count* 1)
      (princ " "))
   ((>= *pc* 10000)
      (and (setf *count* (1+ *count*)) (last-chance-player)))
    (t (start))))

;;Decides if player wins or not
(defun last-chance-player()
  (and (princ "

You have last chance to beat PC's score. Press 1 for the big throw! ")
  (setf input1 (read))
  (setf lthr2 (throw-six))
  (princ (format nil "
You've just thrown and you rolled: ~A" lthr2))
  (evaluate lthr2)
  (if (>= *total1* *pc*)
      (last-chance-pc)
    '(SORRY PC Wins!!! Better Bring your A game next time!))))

;;Decides if PC wins or not
(defun last-chance-pc()
  (and (princ "

PC's last chance to beat your score.")
  (setf lthr (throw-six))
  (princ (format nil "
PC rolled: ~A " lthr))
  (computer-scoring lthr)
  (if (>= *pc* *total1*)
      (last-chance-player)
    '(YOU WIN!!!!!!!!!!!!!!!!!!!!))))

;;This function prints points for PC at that state
(defun print-pc ()
  (format nil "
Computer has ~A points." *pc*))

;;Computes points for a single number
(defun one (lis2)
 (cond
 ( (equal lis2 1) 100)
 ( (equal lis2 5) 50)
 (t 0)))

;; Computes points for two dice
(defun two (tlist)
 (+(one (first tlist)) (one (second tlist))))

;; Computes points for a roll of 3 dice
(defun three (tlist)
  (cond
  ((and (equal (count (first tlist) tlist) 3)(equal (first tlist) 1))
   (* 1000 (first tlist)))
  ((equal (count (first tlist) tlist) 3)
   (* 100 (first tlist)))
   (t (+ (one (first tlist)) (one (second tlist)) (one (third tlist))))))

;; checks if number or list and sends to appropriate function
;; to compute points for player
(defun score (lis)
 (cond
  ((numberp lis) (one lis))
  ((equal (length lis) 3) (three lis))
  ((equal (length lis) 2) (two lis))
  (t (evaluate lis)) ))

;;----------------------------------------------------------------------------------------------------------------------------------------------------------

;; This function computes points for the player.
;; We sort the list first to evaluate what the user rolled easier and compute points.
(defun evaluate(lis)

  (setf newlis (sort lis #'<))

  (cond

   ((equal newlis '(1 2 3 4 5 6))
    (and (setf *total1* (+ *total1* 1200))
         (princ (format nil " BOOM a Straight!! & an extra roll! Totaled ~A points." *total1*))
         (start) ))

   ((and (equal(count (first newlis) newlis) 2 ) (equal (count (third newlis) newlis) 2)
                (equal(count (fifth newlis) newlis) 2))
    (and  (setf *total1* (+ *total1* 800)) (princ (format nil "
Nice! 3 Pairs & an extra roll! You have ~A points." *total1*)) (start)))

   ((and (equal (first newlis) 1) (equal (count (first newlis) newlis) 6))
    (and (setf *total1* (+ *total1* (* 1000 8))) (princ (format nil "
Wow! Killer 1's & an extra roll! You have ~A points." *total1*))(start)))

   ((equal (count (first newlis) newlis) 6)
    (and  (setf *total1* (+ *total1* (* (* (first newlis) 100) 8)))
         (princ (format nil "
Amazing! 6 of a Kind & an extra roll! You have ~A points." *total1*))(start)))

;;cases for 5 of a kind :

   ((and (equal (first newlis) 1) (equal (count (first newlis) newlis) 5))
            (and (princ "
BANG 5 of a Kind with 1's. Press 1 to compile total score or 0
to see what you can do with the other dice! ")
    (setf input (read))
    (if (equal input 1)
    (and (setf *total1* (+ *total1* (* (first newlis) 4000) (one (sixth newlis))))
         (princ (format nil "You have ~A points." *total1*)))

      (and (setf trial (list (throw-die)))
    (princ (format nil "ROLLED: ~A . " trial))
           (setf *total1* (+ *total1* (one (car trial)) (* (first newlis) 4000)))
           (princ (format nil " You have ~A points." *total1*))))
    (check-score)))

   ((equal (count (first newlis) newlis) 5)
            (and (princ "
5 of a Kind!!!! Press 1 to compile total score or 0
to see what you can do with the other dice! ")
    (setf input (read))
    (if (equal input 1)
    (and (setf *total1* (+ *total1* (* (first newlis) 400) (one (sixth newlis))))
         (princ (format nil "You have ~A points." *total1*)))

      (and (setf trial (list (throw-die)))
    (princ (format nil "ROLLED: ~A . " trial))
           (setf *total1* (+ *total1* (one (car trial)) (* (first newlis) 400)))
           (princ (format nil " You have ~A points." *total1*))))
    (check-score)))

   ((equal (count (second newlis) newlis) 5)
            (and (princ "
5 of a Kind!!!! Press 1 to compile total score or 0
to see what you can do with the other dice! ")
    (setf input (read))
    (if (equal input 1)
    (and (setf *total1* (+ *total1* (* (second newlis) 400) (one (first newlis))))
         (princ (format nil "You have ~A points." *total1*)))

      (and (setf trial (list (throw-die)))
    (princ (format nil "ROLLED: ~A . " trial))
           (setf *total1* (+ *total1* (one (car trial)) (* (second newlis) 400)))
           (princ (format nil " You have ~A points." *total1*))))
    (check-score)))

 ;;cases for 4 of a kind :

   ((and (equal (count (first newlis) newlis) 4) (equal (first newlis) 1))
            (and (princ "
BAM! 4 of a Kind with 1's. Press 1 to compile total score or 0
to see what you can do with the other 2 dice! ")
    (setf input (read))
    (if (equal input 1)
    (and (setf *total1* (+ *total1* (* (first newlis) 2000) (one (fifth newlis))
                           (one (sixth newlis))))
         (princ (format nil "You have ~A points." *total1*)))

      (and (setf trial (list (throw-die) (throw-die)))
    (princ (format nil "ROLLED: ~A . " trial))
           (setf *total1* (+ *total1* (two trial) (* (first newlis) 2000)))
           (princ (format nil " You have ~A points." *total1*))))
    (check-score)))

   ((equal (count (first newlis) newlis) 4)
        (and (princ "
4 of a Kind. Press 1 to compile total score or 0
to see what you can do with the other 2 dice! ")
    (setf input (read))
    (if (equal input 1)
    (and (setf *total1* (+ *total1* (* (first newlis) 200) (one (fifth newlis))
                           (one (sixth newlis))))
         (princ (format nil "You have ~A points." *total1*)))

      (and (setf trial (list (throw-die) (throw-die)))
    (princ (format nil "ROLLED: ~A . " trial))
           (setf *total1* (+ *total1* (two trial) (* (first newlis) 200)))
           (princ (format nil " You have ~A points." *total1*))))
    (check-score)))

   ((equal (count (second newlis) newlis) 4)
            (and (princ "
4 of a Kind. Press 1 to compile total score or 0
to see what you can do with the other 2 dice! ")
    (setf input (read))
    (if (equal input 1)
    (and (setf *total1* (+ *total1* (* (second newlis) 200) (one (first newlis))
                           (one (sixth newlis))))
         (princ (format nil "You have ~A points." *total1*)))

      (and (setf trial (list (throw-die) (throw-die)))
    (princ (format nil "ROLLED: ~A . " trial))
           (setf *total1* (+ *total1* (two trial) (* (second newlis) 200)))
           (princ (format nil " You have ~A points." *total1*))))
    (check-score)))

   ((equal (count (third newlis) newlis) 4)
            (and (princ "
4 of a Kind. Press 1 to compile total score or 0
to see what you can do with the other 2 dice! ")
    (setf input (read))
    (if (equal input 1)
    (and (setf *total1* (+ *total1* (* (third newlis) 200) (one (first newlis))
                           (one (second newlis))))
         (princ (format nil "You have ~A points." *total1*)))

      (and (setf trial (list (throw-die) (throw-die)))
    (princ (format nil "ROLLED: ~A . " trial))
           (setf *total1* (+ *total1* (two trial) (* (third newlis) 200)))
           (princ (format nil " You have ~A points." *total1*))))
    (check-score)))

;;cases for 3 of a kind :

   ((and (equal (count (first newlis) newlis) 3) (equal (count (fourth newlis) newlis) 3))
    (and (setf *total1* (+ *total1* (+ (* (first newlis) 100) (* (fourth newlis) 100))))
         (princ (format nil " (2) 3 of a Kinds. You have ~A points." *total1*))(check-score)))

   ((and (equal (count (first newlis) newlis) 3) (equal (first newlis) 1))

    (and (princ "
3 of a Kind!! =) Press 1 to compile total score or 0
to see what you can do with the other 3 dice! ")
    (setf input (read))
    (if (equal input 1)
    (and (setf *total1* (+ *total1* (* (first newlis) 1000) (one (fourth newlis))
                           (one (fifth newlis)) (one (sixth newlis))))
         (princ (format nil "3 of a Kind. You have ~A points." *total1*)))

      (and (setf trial (list (throw-die) (throw-die) (throw-die)))
           (princ (format nil "ROLLED: ~A . " trial))
           (setf *total1* (+ *total1* (three trial) 1000))
           (princ (format nil " You have ~A points." *total1*)))) (check-score)))

   ((equal (count (first newlis) newlis) 3)

    (and (princ "
3 of a Kind!! =) Press 1 to compile total score or 0
to see what you can do with the other 3 dice! ")
    (setf input (read))
    (if (equal input 1)
    (and (setf *total1* (+ *total1* (* (first newlis) 100) (one (fourth newlis))
                           (one (fifth newlis)) (one (sixth newlis))))
         (princ (format nil "3 of a Kind. You have ~A points." *total1*)))

      (and (setf trial (list (throw-die) (throw-die) (throw-die)))
    (princ (format nil "ROLLED: ~A . " trial))
           (setf *total1* (+ *total1* (three trial) (* (first newlis) 100)))
           (princ (format nil " You have ~A points." *total1*))))
    (check-score)))

   ((equal (count (second newlis) newlis) 3)
   (and (princ "
3 of a Kind!! =) Press 1 to compile total score or 0
to see what you can do with the other 3 dice! ")
    (setf input (read))
    (if (equal input 1)
    (and (setf *total1* (+ *total1* (* (second newlis) 100) (one (first newlis))
                           (one (fifth newlis)) (one (sixth newlis))))
         (princ (format nil "3 of a Kind. You have ~A points." *total1*)))

      (and (setf trial (list (throw-die) (throw-die) (throw-die)))
    (princ (format nil "ROLLED: ~A . " trial))
           (setf *total1* (+ *total1* (three trial) (* (second newlis) 100)))
           (princ (format nil " You have ~A points." *total1*))))
     (check-score)))

   ((equal (count (third newlis) newlis) 3)
   (and (princ "
3 of a Kind!! =) Press 1 to compile total score or 0
to see what you can do with the other 3 dice! ")
    (setf input (read))
    (if (equal input 1)
    (and (setf *total1* (+ *total1* (* (third newlis) 100) (one (first newlis))
                           (one (second newlis)) (one (sixth newlis))))
         (princ (format nil "3 of a Kind. You have ~A points." *total1*)))

      (and (setf trial (list (throw-die) (throw-die) (throw-die)))
    (princ (format nil "ROLLED: ~A . " trial))
           (setf *total1* (+ *total1* (three trial) (* (third newlis) 100)))
           (princ (format nil " You have ~A points." *total1*))))

    (check-score)))

   ((equal (count (fourth newlis) newlis) 3)
   (and (princ "
3 of a Kind!! =) Press 1 to compile total score or 0
to see what you can do with the other 3 dice! ")
    (setf input (read))
    (if (equal input 1)
    (and (setf *total1* (+ *total1* (* (fourth newlis) 100) (one (first newlis))
                           (one (second newlis)) (one (third newlis))))
         (princ (format nil " 3 of a Kind. You have ~A points." *total1*)))

      (and (setf trial (list (throw-die) (throw-die) (throw-die)))
    (princ (format nil "ROLLED: ~A . " trial))
           (setf *total1* (+ *total1* (three trial) (* (fourth newlis) 100)))
           (princ (format nil " You have ~A points." *total1*))))
        (check-score)))

;;Case for rolling a 1 or 5

   ((or (equal (first newlis) 1) (>= (count 5 newlis) 1))
   (and (princ "
Press 1 to keep score (1-100pts 5-50pts) or 0
for a fresh roll and chance at better points! ")
    (setf input (read))
      (if (equal input 1)
        (and (setf *total1* (+ *total1* (one (first newlis)) (one (second newlis)) (one (third newlis))
                               (one (fourth newlis)) (one (fifth newlis)) (one (sixth newlis))))
            (princ (format nil "You have ~A points." *total1*))(check-score))

        (start2))))

   (t (and (princ " Nothing scored. Computer's turn")
           (computer-go)))

       ))
;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;; This is the basic scoring the Computer does
;; on it's own.

(defun computer-scoring (lis)

  (setf newlis (sort lis #'<))

  (cond

   ((equal newlis '(1 2 3 4 5 6))
    (and (setf *pc* (+ *pc* 1200)) (princ " UGH OH STRAIGHT!! & an extra roll! " )
    (princ (print-pc)) (computer-go)))

   ((and (equal(count (first newlis) newlis) 2 ) (equal (count (third newlis) newlis) 2)
                (equal(count (fifth newlis) newlis) 2))
    (and  (setf *pc* (+ *pc* 800)) (princ " 3 Pairs! & an extra roll. ")
    (princ (print-pc)) (computer-go)))

   ((and (equal (first newlis) 1) (equal (count (first newlis) newlis) 6))
    (and (setf *pc* (+ *pc* (* 1000 8))) (princ " DAGGER 6 1's & an extra roll. ")
    (princ (print-pc))(computer-go)))

   ((equal (count (first newlis) newlis) 6)
    (and  (setf *pc* (+ *pc* (* (* (first newlis) 100) 8)))
         (princ " Wow, 6 of a kind! & an extra roll." )
    (princ (print-pc))(computer-go)))

;;Cases for 5 of a kind:

   ((and (equal (first newlis) 1) (equal (count (first newlis) newlis) 5))
   (and (princ "
5 of a Kind with 1's. ")
        (setf chance (random 2))
        (if (equal chance 0)
        (and (princ "Computer Decided to roll extra die")
             (setf trial (list (throw-die)))
             (princ (format nil "
PC ROLLED: ~A . " trial))
           (setf *pc* (+ *pc* (one (car trial)) (* (first newlis) 4000)))
             (princ (format nil " PC has ~A points." *pc*)))
         (and (setf *pc* (+ *pc* (* 1000 4) (one (sixth newlis)))) (princ (print-pc)))) (check-pc-score)))


   ((equal (count (first newlis) newlis) 5)
    (and (princ "
5 of a Kind. ")
        (setf chance (random 2))
        (if (equal chance 0)
        (and (princ "Computer Decided to roll extra die")
             (setf trial (list (throw-die)))
             (princ (format nil "
PC ROLLED: ~A . " trial))
           (setf *pc* (+ *pc* (one (car trial)) (* (first newlis) 400)))
             (princ (format nil " PC has ~A points." *pc*)))
          (and (setf *pc* (+ *pc* (* (first newlis) 400) (one (sixth newlis)))) (princ (print-pc))))
         (check-pc-score)))

   ((equal (count (second newlis) newlis) 5)
    (and (princ "
5 of a Kind. ")
        (setf chance (random 2))
        (if (equal chance 0)
        (and (princ "Computer Decided to roll extra die")
             (setf trial (list (throw-die)))
             (princ (format nil "
PC ROLLED: ~A . " trial))
           (setf *pc* (+ *pc* (one (car trial)) (* (second newlis) 400)))
             (princ (format nil " PC has ~A points." *pc*)))
          (and (setf *pc* (+ *pc* (* (second newlis) 400) (one (first newlis)))) (princ (print-pc))))
         (check-pc-score)))

;;Case for 4 of a Kind:

   ((and (equal (count (first newlis) newlis) 4) (equal (first newlis) 1))
    (and (princ "
4 of a Kind with 1's. That hurts. ")
        (setf chance (random 2))
        (if (equal chance 0)
        (and (princ "Computer Decided to roll the other two dice")
             (setf trial (list (throw-die)(throw-die)))
             (princ (format nil "
PC ROLLED: ~A . " trial))
           (setf *pc* (+ *pc* (two trial) (* (first newlis) 2000)))
             (princ (format nil " PC has ~A points." *pc*)))
          (and (setf *pc* (+ *pc* (* 1000 2) (one (fifth newlis))(one (sixth newlis)))) (princ (print-pc))))
          (check-pc-score)))

   ((equal (count (first newlis) newlis) 4)
    (and (princ "
4 of a Kind. ")
        (setf chance (random 2))
        (if (equal chance 0)
        (and (princ "Computer Decided to roll the other two dice")
             (setf trial (list (throw-die)(throw-die)))
             (princ (format nil "
PC ROLLED: ~A . " trial))
           (setf *pc* (+ *pc* (two trial) (* (first newlis) 200)))
             (princ (format nil " PC has ~A points." *pc*)))
          (and (setf *pc* (+ *pc* (* (first newlis) 200) (one (fifth newlis))(one (sixth newlis))))
               (princ (print-pc))))
          (check-pc-score)))

   ((equal (count (second newlis) newlis) 4)
     (and (princ "
4 of a Kind. ")
        (setf chance (random 2))
        (if (equal chance 0)
        (and (princ "Computer Decided to roll the other two dice")
             (setf trial (list (throw-die)(throw-die)))
             (princ (format nil "
PC ROLLED: ~A . " trial))
           (setf *pc* (+ *pc* (two trial) (* (second newlis) 200)))
             (princ (format nil " PC has ~A points." *pc*)))
          (and (setf *pc* (+ *pc* (* (second newlis) 200) (one (first newlis))(one (sixth newlis))))
               (princ (print-pc))))
          (check-pc-score)))


   ((equal (count (third newlis) newlis) 4)
    (and (princ "
4 of a Kind. ")
        (setf chance (random 2))
        (if (equal chance 0)
        (and (princ "Computer Decided to roll the other two dice")
             (setf trial (list (throw-die)(throw-die)))
             (princ (format nil "
PC ROLLED: ~A . " trial))
           (setf *pc* (+ *pc* (two trial) (* (third newlis) 200)))
             (princ (format nil " PC has ~A points." *pc*)))
          (and (setf *pc* (+ *pc* (* (third newlis) 200) (one (first newlis))(one (second newlis))))
               (princ (print-pc))))
          (check-pc-score)))

;;case for triples

   ((and (equal (count (first newlis) newlis) 3) (equal (first newlis) 1))
    (and (princ "
3 of a Kind with 1's. ")
        (setf chance (random 2))
        (if (equal chance 0)
        (and (princ "Computer Decided to roll the other three dice")
             (setf trial (list (throw-die)(throw-die)(throw-die)))
             (princ (format nil "
PC ROLLED: ~A . " trial))
           (setf *pc* (+ *pc* (three trial) (* (first newlis) 1000)))
             (princ (format nil " PC has ~A points." *pc*)))
          (and (setf *pc* (+ *pc* (* (first newlis) 1000) (one (fourth newlis))(one (fifth newlis))
                             (one (sixth newlis))))
               (princ (print-pc))))
          (check-pc-score)))

   ((equal (count (first newlis) newlis) 3)
    (and (princ "
3 of a Kind. ")
        (setf chance (random 2))
        (if (equal chance 0)
        (and (princ "Computer Decided to roll the other three dice")
             (setf trial (list (throw-die)(throw-die)(throw-die)))
             (princ (format nil "
PC ROLLED: ~A . " trial))
           (setf *pc* (+ *pc* (three trial) (* (first newlis) 100)))
             (princ (format nil " PC has ~A points." *pc*)))
          (and (setf *pc* (+ *pc* (* (first newlis) 100) (one (fourth newlis))(one (fifth newlis))
                             (one (sixth newlis))))
               (princ (print-pc))))
          (check-pc-score)))

   ((equal (count (second newlis) newlis) 3)
   (and (princ "
3 of a Kind. ")
        (setf chance (random 2))
        (if (equal chance 0)
        (and (princ "Computer Decided to roll the other three dice")
             (setf trial (list (throw-die)(throw-die)(throw-die)))
             (princ (format nil "
PC ROLLED: ~A . " trial))
           (setf *pc* (+ *pc* (three trial) (* (second newlis) 100)))
             (princ (format nil " PC has ~A points." *pc*)))
          (and (setf *pc* (+ *pc* (* (second newlis) 100) (one (first newlis))(one (fifth newlis))
                             (one (sixth newlis))))
               (princ (print-pc))))
          (check-pc-score)))

   ((equal (count (third newlis) newlis) 3)
   (and (princ "
3 of a Kind. ")
        (setf chance (random 2))
        (if (equal chance 0)
        (and (princ "Computer Decided to roll the other three dice")
             (setf trial (list (throw-die)(throw-die)(throw-die)))
             (princ (format nil "
PC ROLLED: ~A . " trial))
           (setf *pc* (+ *pc* (three trial) (* (third newlis) 100)))
             (princ (format nil " PC has ~A points." *pc*)))
          (and (setf *pc* (+ *pc* (* (third newlis) 100) (one (first newlis))(one (second newlis))
                             (one (sixth newlis))))
               (princ (print-pc))))
          (check-pc-score)))

   ((equal (count (fourth newlis) newlis) 3)
   (and (princ "
3 of a Kind. ")
        (setf chance (random 2))
        (if (equal chance 0)
        (and (princ "Computer Decided to roll the other three dice")
             (setf trial (list (throw-die)(throw-die)(throw-die)))
             (princ (format nil "
PC ROLLED: ~A . " trial))
           (setf *pc* (+ *pc* (three trial) (* (fourth newlis) 100)))
             (princ (format nil " PC has ~A points." *pc*)))
          (and (setf *pc* (+ *pc* (* (fourth newlis) 100) (one (first newlis))(one (second newlis))
                             (one (third newlis))))
               (princ (print-pc))))
        (check-pc-score)))

;;Case for 1 or 5 in which you can reroll or keep score!

   ((or (equal (first newlis) 1) (>= (count 5 newlis) 1))
    (setf chance (random 1))
    (if (equal chance 0)
        (and (princ "Computer Decided to roll again")
             (computer-go))
      (and (setf *pc* (+ *pc* (one (first newlis)) (one (second newlis)) (one (third newlis))
                         (one (fourth newlis)) (one (fifth newlis)) (one (sixth newlis))))
          (princ (print-pc))(check-pc-score))))

   ( t (and (princ "No Points out of that roll") (start)))

         ))
;-----------------------------------------------------------------------------------------------------------------------------------------------------------
;;This is the computer playing!
(defun computer-go ()

  (setf thr (throw-six))
  (princ "
----------------------------------------------------------------------------------------------")
  (princ (format nil "
Computer Rolled:  ~A " thr))

  (computer-scoring thr) )

;; throw-die() generates a number for a thrown die
(defun throw-die()
  (+ (random 6) 1))

; throw-six shows you the numbers you rolled on a fresh throw of 6 dice
(defun throw-six()
  (list (throw-die) (throw-die) (throw-die) (throw-die) (throw-die) (throw-die)))

;; Code executes here and gives the user the chance to continue in the game
;; by typing (start) anytime
(defun start()

  (princ "

Go ahead and Press 1 to throw dice. ")

  (setf newinput (read))

  (if (equal newinput 1)
      (and (princ "
Rolled:  ") (setf initial (throw-six)) (princ initial) (continue1 initial))
    '(Quitting? Okay well see you next time))
    )

;;start2 is a nice way to call start in the middle of the game without much typing
(defun start2 ()
  (princ "
You Rolled : ")
  (and (setf freshthr (throw-six)) (princ freshthr) (continue1 freshthr)))

;;continue calls score to check the type of x and then computes its points
(defun continue1 (x)

 (score x))