;; main Module

"(deftemplate user
    (slot income (default 0))
)"

(deftemplate question 

    (slot text)
    (slot type)
    (slot ident))

(deftemplate answer 

    (slot ident)
    (slot text))

(deftemplate recommendation 
    (slot song)
    (slot artist))

(deffacts question-data 

    "The questions the system can ask."
      (question (ident pop) (type yes-no)
            	(text " Do you like Pop?"))
      (question (ident rnb) (type yes-no)
            	(text " Do you like RnB?"))
      (question (ident rap) (type yes-no)
            	(text " Do you like rap?"))
      (question (ident classical) (type yes-no)
            	(text " Do you like classical?"))
      (question (ident hiphop) (type yes-no)
            	(text " Do you like Hip-Hop?"))
      (question (ident punk) (type yes-no)
	            (text " Do you like punk?"))
      (question (ident metal) (type yes-no)
	            (text " Do you like metal?"))
	  )

(defglobal ?*crlf* = "
")

;; ask Module

(defmodule ask)

(deffunction is-of-type (?answer ?type)

    "Check that the answer has the right form"
    (if (eq ?type yes-no) then
        (return (or (eq ?answer yes) (eq ?answer no)))
    elif (eq ?type number) then
        (return (numberp ?answer))
    else (return (> (str-length ?answer) 0))))

(deffunction ask-user (?question ?type)

  "Ask a question, and return the answer"
  (bind ?answer "")
  (while (not (is-of-type ?answer ?type)) do
         (printout t ?question " ")
         (if (eq ?type yes-no) then
           (printout t "(yes or no) "))
         (bind ?answer (read)))
  (return ?answer))

   
(defrule ask::ask-question-by-id 

  "Given the identifier of a question, ask it and assert the answer"
  (declare (auto-focus TRUE))
  (MAIN::question (ident ?id) (text ?text) (type ?type))
  (not (MAIN::answer (ident ?id)))
  ?ask <- (MAIN::ask ?id)
  =>
  (bind ?answer (ask-user ?text ?type))
  (assert (answer (ident ?id) (text ?answer)))
  (retract ?ask)
  (return))

;; interview Module
(defmodule interview)

(defrule request-punk 

  =>
  (assert (ask punk)))

(defrule request-pop 

  =>
  (assert (ask pop)))


(defrule request-rap 

  =>
  (assert (ask rap)))

(defrule request-hiphop 

  =>
  (assert (ask hiphop)))

(defrule request-metal 

  =>
  (assert (ask metal)))


(defrule request-classical 

  =>
  (assert (ask classical)))

(defrule request-rnb 

  =>
  (assert (ask rnb)))


;; startup Module

(defmodule startup)

(defrule print-banner 

    =>
    (printout t "Please enter your name and then press enter key> ")
    (bind ?name (read))
    (printout t crlf " " crlf)
    (printout t " Hello, " ?name "." crlf)
    (printout t " Welcome to the Music Recommender" crlf)
    (printout t " Please answer the following questions and we will recommend you a song." crlf)
    (printout t " " crlf crlf))

;; recommend Module

(defmodule recommend)

(defrule song-rap-hiphop-metal 

    (answer (ident rap) (text yes))
    (answer (ident hiphop) (text yes))
    (answer (ident metal) (text yes))
    =>
    (assert
        (recommendation (song "Shape of you") (artist "Ed Sheeran"))))

(defrule song-pop-punk 

    (answer (ident pop) (text yes))
    (answer (ident punk) (text yes))
    =>
    (assert
        (recommendation (song "Boulevard of Broken Dreams") (artist "Green Day"))))



(defrule song-pop-rap-metal 

    (answer (ident pop) (text yes))
    (answer (ident rap) (text yes))
    (answer (ident metal) (text yes))
    =>
    (assert
        (recommendation (song "Toy Soldiers") (artist "Eminem"))))


(defrule song-hiphop-metal 

    (answer (ident hiphop) (text yes))
    (answer (ident metal) (text yes))
    =>
    (assert
        (recommendation (song "Paris") (artist "Chainsmokers"))))

(defrule song-rap-metal 

    (answer (ident rap) (text yes))
    (answer (ident metal) (text yes))
    =>
    (assert
        (recommendation (song "Closer") (artist "Chainsmokers"))))

(defrule song-rap-metal-rnb 

    (answer (ident rap) (text yes))
    (answer (ident metal) (text yes))
    (answer (ident rnb) (text yes))
    =>
    (assert
        (recommendation (song "Hotline Bling") (artist "Drake"))))

(defrule song-classical-rnb 


    (answer (ident classical) (text yes))
    (answer (ident rnb) (text yes))
    =>
    (assert
        (recommendation (song "BlankSpace") (artist "Taylor Swift"))))

;; report Module

(defmodule report)

(defrule sort-and-print 
    ?r1 <- (recommendation (song ?f1) (artist ?e))
    (not (recommendation (song ?f2&:(< (str-compare ?f2 ?f1) 0))))
    =>
    (printout t crlf " " crlf)
    (printout t " You can listen " ?f1 " song" crlf)
    (printout t " Artist: "  crlf ?e crlf crlf)
    (retract ?r1))

;; run Module

(deffunction run-system () 

    (reset)
    (focus startup interview recommend report)
    (run))

(while TRUE
    (run-system))