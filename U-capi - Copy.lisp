;; *********************************** U-capi.lisp ********************************
;;
;;My general CAPI UTILITY FUNCTIONS


(defun my-select-font ()
  "In U-capi.lisp"
  (CAPI:PROMPT-FOR-FONT "Select Font" )
  )


(defun my-select-color ()
  "In U-capi.lisp"
  (CAPI:PROMPT-FOR-COLOR "Select Color")
  )

;;CENTERING PANES AND WINDOWS
;; ALSO USE :best-x :center and :best-y :center
;;
(defun center-window (element)
  "From LispHUG, Nick Levine for centering a CAPI element OR a window on a screen-see def below"
  (let ((interface  (capi:element-interface element)))
    ;;Camille said to use for centering window on screen must use following:
    ;;  (capi:top-level-interface element) instead
    (multiple-value-bind (i-left i-top i-width i-height)
        (capi:top-level-interface-geometry interface)
      (declare (ignore i-left i-top))
      Multiple-value-bind (s-left s-top s-width s-height)
      (capi:screen-internal-geometry (capi:element-screen interface))
      (let ((s-center-x(+ s-left (/ s-width 2)))
            (s-center-y (+ s-top (/ s-height 2))))
        (capi:set-top-level-interface-geometry interface
                                               :x (- s-center-x (/ i-width 2))
                                               :y (- s-center-y (/ i-height 2)))))))



;;NONSELECTED&SELECTED-BUTTONS
;;
;;ddd
(defun nonselected&selected-buttons (selected-buttons all-selected-buttons)
  "In U-CAPI, from a list of selected buttons eg (\"item2\") and list of all-button-selections eg (\"item2\" \"item2\" \"item3\"), RETURNS (values nonsel-items nonsel-item-nums  sel-item-nums). num begins at 1."
  (let
      ((nonsel-item-nums)
       (nonsel-items)
       (sel-item-nums)
       )
    (loop
     for button in all-selected-buttons
     for n from 1 to (list-length all-selected-buttons)
     do
     (cond
      ((member button selected-buttons :test 'string-equal)
       (setf sel-item-nums (append sel-item-nums (list n))))
      (t
       (setf nonsel-item-nums (append nonsel-item-nums (list n))
             nonsel-items (append nonsel-items (list button)))))
     ;;end loop
     )
    (values nonsel-items nonsel-item-nums  sel-item-nums)
    ;;end let, nonselected&selected-buttons
    ))
;;TEST
;;  (nonselected&selected-buttons '("item2")'("item1"  "item2" "item3"))
      
    





#|
xx CAPI:PROMPT-FOR-COLOR 
Lambda List: (MESSAGE &KEY COLOR COLORS PANE-ARGS POPUP-ARGS OWNER)
xxCAPI:PROMPT-FOR-CONFIRMATION 
xxxCAPI:PROMPT-FOR-DIRECTORY 
xxxCAPI:PROMPT-FOR-FILE 
CAPI::PROMPT-FOR-FILE-GET-CANONICAL-PATH 
xxCAPI:PROMPT-FOR-FILES 
xxCAPI:PROMPT-FOR-FONT 
Lambda List: (CAPI::MESSAGE &KEY CAPI::FONT CAPI::OWNER)

(CAPI::*DEFAULT-PROMPT-LIST-BORDER
Global Variables
* CAPI::*DEFAULT-PROMPT-LIST-HEIGHT-IN-CHARACTERS
* CAPI::*DEFAULT-PROMPT-LIST-MIN-HEIGHT-IN-CHARACTERS
* CAPI::*DEFAULT-PROMPT-LIST-MIN-WIDTH-IN-CHARACTERS
* CAPI::*PROMPT-FOR-FILE-ERROR-ON-BAD-ARG
* CAPI::CALL-PROMPT-WITH-LIST-CALLBACK 
Functions
CAPI::EXIT-RANDOM-TYPEOUT-PROMPT-INFO
xx CAPI::MAKE-CHOICE-FOR-PROMPT-WITH-LIST
xx CAPI:PROMPT-FOR-COLOR 
xxCAPI:PROMPT-FOR-CONFIRMATION 
xxxCAPI:PROMPT-FOR-DIRECTORY 
xxxCAPI:PROMPT-FOR-FILE 
CAPI::PROMPT-FOR-FILE-GET-CANONICAL-PATH 
xxCAPI:PROMPT-FOR-FILES 
xxCAPI:PROMPT-FOR-FONT 
xxCAPI:PROMPT-FOR-FORM 
CAPI:PROMPT-FOR-FORMS 
CAPI:PROMPT-FOR-INTEGER 
CAPI:PROMPT-FOR-ITEMS-FROM-LIST 
CAPI::PROMPT-FOR-LABEL 
CAPI:PROMPT-FOR-NUMBER 
CAPI:PROMPT-FOR-STRING 
CAPI:PROMPT-FOR-SYMBOL 
CAPI:PROMPT-FOR-VALUE 
CAPI::PROMPT-INFO 
CAPI:PROMPT-WITH-LIST 
CAPI:PROMPT-WITH-LIST-NON-FOCUS 
CAPI:PROMPT-WITH-MESSAGE 
CAPI-INTERNALS:CAPI-PROMPT-FOR-COLOR 
CAPI-INTERNALS:CAPI-PROMPT-FOR-DIRECTORY 
CAPI-INTERNALS:CAPI-PROMPT-FOR-FILE 
CAPI-INTERNALS:CAPI-PROMPT-FOR-FONT 
CAPI-LIBRARY:LIBRARY-PROMPT-FOR-COLOR 
CAPI-LIBRARY:LIBRARY-PROMPT-FOR-DIRECTORY 
CAPI-LIBRARY:LIBRARY-PROMPT-FOR-FILE 
CAPI-LIBRARY:LIBRARY-PROMPT-FOR-FILES 
CAPI-LIBRARY:LIBRARY-PROMPT-FOR-FONT 
CAPI-TOOLKIT:INTERFACE-PROMPT-FOR-FILE 
CAPI-TOOLKIT::INTERFACE-PROMPT-FOR-PACKAGE 
CAPI-TOOLKIT::PROMPT-ARGS 
CAPI-TOOLKIT::PROMPT-FOR-ALTERNATIVE-CLASS 
CAPI-TOOLKIT::PROMPT-FOR-ALTERNATIVE-SYMBOL 
CAPI-TOOLKIT::PROMPT-FOR-ALTERNATIVES 
CAPI-TOOLKIT::PROMPT-FOR-HISTORY 
CAPI-TOOLKIT:PROMPT-FOR-PACKAGE 
CAPI-WIN32-LIB::PROMPT-FOR-FILE-ARGS 
CAPI-WIN32-LIB::REPRESENTATION-PROMPT-FOR-COLOR 
CAPI-WIN32-LIB::WW-PROMPT-FOR-DIRECTORY 
CONDITIONS::PROMPT-FOR-FUNCTION
|#

