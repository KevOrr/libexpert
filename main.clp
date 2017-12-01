;;;; Kevin Orr and Elijah Malaby
;;; Expert system for providing recommendations for languages+libraries which
;;; satisfy the needs and preferences of the user.
;;;
;;; Call (run) and the prompts will explain the information required

;; Class for tracking language data
(defclass language (is-a USER)
  (slot lang-name)
  (slot flexibility)
  (slot speed)
  (slot safety)
  (slot score (default 0))
  (slot viable (default TRUE))
  (multislot stdlib-features))

;; Class for tracking library data
(defclass library (is-a USER)
  (multislot languages) ; Languages which support this library
  (multislot features) ; Features provided by this library
  (slot lib-name))

(definstances langs
  (python of language (lang-name "Python") (speed 1) (safety 2) (flexibility 3)
          (stdlib-features file-io strings threads datetime cffi oop))
  (c of language (lang-name "C") (speed 2) (safety 1) (flexibility 1)
     (stdlib-features file-io strings threads datetime fast-math))
  (javascript of language (lang-name "Javascript") (speed 1) (safety 2) (flexibility 3)
              (stdlib-features strings webscripting file-io oop))
  (dj of language (lang-name "DJ") (speed 1) (safety 3) (flexibility 1)
      (stdlib-features oop))
  (clojure of language (lang-name "Clojure") (speed 2) (safety 3) (flexibility 3)
           (stdlib-features immutability strings file-io threads datetime oop)))

(definstances libs
  (opengl of library (lib-name "OpenGL") (languages [c] [python])
          (features gpgpu 3d-rendering))
  (ajax of library (lib-name "AJAX") (languages [javascript])
        (features networking))
  (numpy of library (lib-name "Numpy") (languages [python])
         (features fast-math)))

; Hack for getting access to a slot by symbol
(defmessage-handler language get (?place)
  (dynamic-get ?place))

; messages for updating language score
(defmessage-handler language better ()
  (bind ?self:score (+ ?self:score 1)))
(defmessage-handler language worse ()
  (bind ?self:score (- ?self:score 1)))

;;; Updates the scores of languages based on preferences
(defrule update-for-preferences
  (input-finished)
  (preference ?more ?less)
  ?o <- (object (is-a language))
 =>
  (bind ?more (send ?o get ?more))
  (bind ?less (send ?o get ?less))
  (if (< ?less ?more)
   then
     (send ?o better))
  (if (< ?more ?less)
   then
     (send ?o worse)))

;;; Updates viability based of project requirements
(defrule requirement-test
  (input-finished)
  (requirement ?req)
  ?o <- (object (is-a language) (name ?on) (stdlib-features $?f))
  (not (or (test (member ?req ?f))
           (exists (object (is-a library)
                           (languages $? ?on $?)
                           (features $? ?req $?)))))
 =>
  (send ?o put-viable FALSE))

;;; Handles being out of any languages to output
(defrule out-of-stuff
  (output-stage)
  (not (exists (object (is-a language) (viable TRUE))))
 =>
  (printout t "No languages in the database satisfy these requirements" crlf))

;;; Comparison function for languages by score
(deffunction better-score (?a ?b)
  (< (send ?a get-score) (send ?b get-score)))

;;; Handles output when there are viable languages
(defrule check-done
  (output-stage)
  (exists (object (is-a language) (viable TRUE)))
 =>
  (bind ?os (find-all-instances ((?o language)) (send ?o get-viable)))
  (bind ?os (sort better-score ?os))
  (assert (to-display (nth 1 ?os) (nth 2 ?os) (nth 3 ?os)))
  (assert (disp-start)))

;;; Controller for iterating over the languages to display
(defrule display-manager
  ?list <- (to-display ?l&~nil $?ls)
  ?flag <- (disp-start)
 =>
  (retract ?list)
  (assert (to-display $?ls))
  (retract ?flag)
  (assert (display ?l))
  (printout t (send ?l get-lang-name) " satisfies the requirements:" crlf))

;;; Controller for resetting the display-manager
(defrule end-display (declare (salience -1))
  ?lang <- (display ?)
 =>
  (retract ?lang)
  (assert (disp-start)))

;;; Displays requirements handled by stdlib
(defrule display-stdlib
  (display ?on)
  (requirement ?f)
  ?o <- (object (is-a language) (name ?on) (stdlib-features $? ?f $?)) 
 =>
  (printout t ?f " is satisfied by the standard library." crlf))
;;; Displays requirements handled by libraries
(defrule display-lib
  (display ?on)
  (requirement ?f)
  ?l <- (object (is-a library) (languages $? ?on $?) (features $? ?f $?))
 =>
  (printout t ?f " is satisfied by " (send ?l get-lib-name) "." crlf))

(deffunction union (?a ?b)
  (progn$ (?v ?b)
    (if (not (member ?v ?a))
     then
       (bind ?a (create$ ?a ?v))))
  ?a)

;;; Main function for the user interface
(deffunction interface ()
  (printout t "Please enter any required project features on one line separated by spaces:" crlf)
  (bind ?features (create$))
  (do-for-all-instances
      ((?lang language))
      TRUE
    (bind ?features (union ?features (send ?lang get-stdlib-features))))
  (do-for-all-instances
      ((?lib library))
      TRUE
    (bind ?features (union ?features (send ?lib get-features))))
  (printout t "(availible features: " (implode$ ?features) ")" crlf)
  (progn$ (?r (explode$ (readline)))
    (assert (requirement ?r)))
  (printout t "Enter language preferences (ie. speed over safety) on separate lines (ex. \"speed safety\")" crlf)
  (printout t "Enter \"done\" on its own line when finished." crlf)
  (printout t "(Availible values to prefer: speed safety flexibility)" crlf)
  (bind ?done FALSE)
  (while (not ?done)
    (bind ?a (readline))
    (if (eq ?a "done")
     then
       (bind ?done TRUE)
     else
       (assert (preference (explode$ ?a))))))

;;; Controls the overall state
(defrule start
 =>
  (interface)
  (assert (input-finished)))
(defrule processing-done
  (input-finished)
 =>
  (assert (output-stage)))
    

