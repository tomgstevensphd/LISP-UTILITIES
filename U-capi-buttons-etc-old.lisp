;;**********************U-capi-buttons-etc.lisp **************************;;
;;

#|(capi:define-interface radio-buttons ()
  ;;sets the button/item data values
  ((data :initform (list 1 2 3)))
  (:layouts
   (panel
    capi:column-layout)))|#

;;MAKE-MY-BUTTON-PANEL
;;
;;ddd
(defun make-my-button-panel (button-layout-name
                                              parent-layout-name                                              
                                               interface-name 
                                               button-text-list  button-data-list            
                                               &key   (close-interface-on-selection-p NIL)
                                                (button-layout-type 
                                                (quote (quote capi:column-layout)))
                                               (parent-layout-type
                                                (quote (quote capi:row-layout)))
                                               button-arglist
                                               button-layout-arglist
                                               parent-layout-arglist
                                               )    ;;&rest button-arglist) caused error
  "Makes a button 'button-layout-name' which is a customized row or column layout of buttons that allows more options than the standard radio-button or check-button panels. Uses radio buttons as the buttons. All regular initargs to the layout is accessed via button-layout-arglist, and all radio-button initargs thru the &rest list. Starts with NO DEFAULT PRESELECTED BUTTON-a major change over capi. The arg parent-layout-name is the interface layout of which the button layout is an item in the layout-description.  Use to create buttons and add to interfaces. Returns a  layout. NOTE: THESE FUNCTIONS MUST BE USED AS A PACKAGE. 2 arg lists MUST be in form of  '(list \"A\" \"B\" \"C\").  REQUIRES a pre-defined INTERFACE  a layout named button-layout-name (can be within another empty layout parent-layout-name). The arglists must be preceded by double quotes."
  (let
      ((button-layout-inst)       
       (global-selected-values-list (my-make-symbol 
                                     (format nil "*~A-selected-values-list" button-layout-name)))
        (buttons-list)
       )
;;SSS BUTTONS WON'T DESELECT IF CHOOSE MORE THAN ONE--OK IF USE DESTROY INTERFACE, BUT NOT OTHERWISE!!
   ;;Make the SELECTION CALLBACK
    (eval 
     `(defun my-button-panel-selection-callback (item interface)  
        "In U-capi-buttons-etc.lisp, MUST use with my-button-panels.  MODIFY THIS CALLBACK to get data or actions from the button selection."
        (let
            ((button-value)
             (button-data)
             (selected-text)
             (selected-data)
             (selected-button)
             )
          ;;AAA
          (with-slots (,button-layout-name ,parent-layout-name) interface
            (afout 'out (format nil "button-layout-name= ~A~%item= ~A~%,(capi:layout-description ,button-layout-name)= ~A~%parent-layout-name= ~A~%(capi:layout-description parent-layout-name)= ~A~%",button-layout-name  item ,parent-layout-name  (capi:layout-description ,button-layout-name) (capi:layout-description ,parent-layout-name)  ))
            (loop
             for button in (capi:layout-description ,button-layout-name)
             unless (eq item button) ;;  (or (equal item button) (null item))
             do 
             (setf (capi:item-selected button) nil)
             (afout 'out (format nil "button-layout-name= ~A~%item= ~A~%button= ~A (capi:item-selected button)= ~A" button-layout-name item button (capi:item-selected button)))
             ;;end loop
             )
            ;;added
            (setf selected-text (capi:item-text item)
                  selected-data (capi:item-data item)
                  selected-button item)

            (setf ,global-selected-values-list 
                  (list selected-text selected-data  selected-button))

            (if ,close-interface-on-selection-p
                (capi:destroy interface))
            ))
     ;;end defun callback,eval
     ))

    ;; Make button function
    (defun make-a-button (text data)
       (apply #'make-instance 'capi:radio-button  
                   :text (format nil "~A" text)
                     :data data
                     :selected nil
                     :selection-callback 'my-button-panel-selection-callback
                     :callback-type :item-interface
              button-arglist)
       )

    ;;Make the :BEFORE METHOD TO MAKE THE BUTTONS, BUTTON-LAYOUT, AND PARENT LAYOUT
    ;; (setf *mytestvar
    (eval
    `(defmethod capi:interface-display :before ((self  ,interface-name))
        (let
            ((button-list)
             )
        (with-slots (,button-layout-name ,parent-layout-name) self
 ;;BBB
          ;;make the button-list
          (setf  button-list 
                 (loop 
                  for data in  ,button-data-list
                  for text in  ,button-text-list
                  collect (make-a-button text data)))

          ;;make button-layout-inst and put into the layout
          (setf ,button-layout-name
                (apply  #'make-instance  ,button-layout-type
                         :description  buttons-list ,button-layout-arglist))
          ;;put into layout
          (setf (capi:pane-layout self) button-layout-inst)

          ;;make the parent-layout-name instance
          (setf ,parent-layout-name
                (apply  #'make-instance   ,parent-layout-type
                         :description  (list button-layout-inst)  ,parent-layout-arglist))
          ;;put the parent-layout into the interface
                (setf (capi:pane-layout self) ,parent-layout-name))    

          (with-slots ( ,parent-layout-name) self
    (setf (capi:layout-description ,parent-layout-name) button-list))
          

          ;;now make the button-panel-pane
#|          (setf  button-layout-inst         ;;  ,button-layout-name
                 (apply  #'make-instance  ,button-layout-type
                         :description  buttons-list ,button-layout-arglist))
                      ;;   :description  (quote ,buttons-list) ,button-layout-arglist))
          (capi:apply-in-pane-process  ,parent-layout-name ;; ,button-layout-name
                                       #'(setf capi:layout-description)
                                       (list button-layout-inst)  ,parent-layout-name)
          ;;end with-slots,defmethod, eval|#
          )))

    #|(eval
     `(defmethod capi:interface-display :before ((interface  ,interface-name))
        (let
            ((button-list)
             )
        (with-slots (,button-layout-name ,parent-layout-name) interface    
          (setf  button-list 
                 (loop 
                  for data in  ,button-data-list
                  for text in  ,button-text-list
                  collect (make-a-button text data)))
          (setf (capi:layout-description ,button-layout-name)
                button-list)    

          ;;now make the button-panel-pane
#|          (setf  button-layout-inst         ;;  ,button-layout-name
                 (apply  #'make-instance  ,button-layout-type
                         :description  buttons-list ,button-layout-arglist))
                      ;;   :description  (quote ,buttons-list) ,button-layout-arglist))
          (capi:apply-in-pane-process  ,parent-layout-name ;; ,button-layout-name
                                       #'(setf capi:layout-description)
                                       (list button-layout-inst)  ,parent-layout-name)
          ;;end with-slots,defmethod, eval|#
          ))))|#
    ;;end defun make-my-button-panel
    ))
;;TEST
(defun testmb ()
  (setf out nil)
  (let
      ((x)
       )   
    (capi:define-interface test-radio-buttons ()
      ;;sets the button/item data values
      ()    ;;was (data :initform (list 1 2 3))
      (:layouts
#|       (row-layout1
        capi:row-layout
        '(my-button-panel-1)
        )|#
#|       (my-button-panel-1
        capi:column-layout
        ()
        )|#
       )
      (:default-initargs
       :title "TEST INTERFACE"
       :visible-min-width 500
       :visible-min-height 400
       :background :yellow
       :internal-border 30
       ))
    (defun button-press-callback (data interface)
      (setf *test-result-data data)
      (capi:destroy interface)
      )

    ;;SSS CHANGE -PANEL2 BACK 
    (make-my-button-panel 'my-button-panel-1  (quote row-layout1)
                          'test-radio-buttons 
                          '(list "A" "B" "C")
                          '(list 10 20 30) 
                          :close-interface-on-selection-p nil
                         :button-layout-arglist  (quote (quote (:visible-min-width 200
                                                          :visible-min-height 300
                                                          :background :green
                                                          :title "My Test Buttons"
                                                          :title-gap 60  ;;no effect?
                                                     ;;no effect    :x 0  :y 0
                                                          :internal-border 2 ;;important
                                                          :border t
                                                          :title-adjust :center
                                                  ;;no effect        :title-position :bottom
                                                         #| :font won't work :font (gp:find-best-font pane                                                                      (gp:make-font-description 
                                    :family "Times New Roman" :weight :normal
                                    :slant :italic :size 12))|#
#|                                                             :font (gp:make-font-description 
                                                                 :family nil
                                                                 :weight :normal  :size 11)|#
                                                          )))
                          :button-arglist (quote   (                        
                                                   :title "Press a button"
                                                   :title-gap 50  ;;gap betw end of title and button
                                                   :background :light-blue
                                                     :internal-border 0 ;;no effect??
                                                   :visible-min-width 100
                                                   :visible-min-height 40
                                                   ))                        

                          ;;end make-my-button-panel
                          )
    (capi:display (setf *mb-testinst (make-instance 'test-radio-buttons)))      
    ))
;;TEST RESULT
;;  CL-USER 10 > *my-button-panel-1-selected-values-list
;; ("B" 20 #<CAPI:RADIO-BUTTON "B" 27AA618F>)
    ;;test
    ;; *my-button-panel-1-selected-values-list = ("C" 30 #<CAPI:RADIO-BUTTON "C" 29845217>)
;; (capi:display (make-instance 'test-radio-buttons))
;;  (capi:display *mb-testinst)
;;  (capi-internals:capi-object-plist *mb-testinst) = nil
;;  (with-slots (row-layout1) *mb-testinst (capi:layout-description row-layout1))
;;CL-USER 98 > *mytestvar
#|(DEFMETHOD CAPI:INTERFACE-DISPLAY :BEFORE ((INTERFACE TEST-RADIO-BUTTONS)) (WITH-SLOTS (MY-BUTTON-PANEL-1) INTERFACE (SETF MY-BUTTON-PANEL-1 (LOOP FOR DATA IN # FOR TEXT IN # COLLECT #)) (CAPI:APPLY-IN-PANE-PROCESS MY-BUTTON-PANEL-1 (FUNCTION #) (LIST MY-BUTTON-PANEL-1) MY-BUTTON-PANEL-1))) 
 =  #<STANDARD-METHOD CAPI:INTERFACE-DISPLAY (:BEFORE) (TEST-RADIO-BUTTONS) 200B1B37>|#



;; works with bugs
(defun make-my-button-panel2 (button-panel-name ROW-LAYOUT  ;;not used
                              interface-name 
                                  button-text-list  button-data-list            
                                  &key   (close-interface-on-selection-p T)
                                  (layout-type 'capi:column-layout)
                                  layout-arglist
                                  button-arglist)  ;;&rest button-arglist) caused error
  "Makes a button 'button-panel-name' which is a customized row or column layout of buttons that allows more options than the standard radio-button or check-button panels. Uses radio buttons as the buttons. All regular initargs to the layouts are accessed via layout-arglist, and all radio-button initargs thru the &rest list. Starts with NO DEFAULT PRESELECTED BUTTON-a major change over capi. Use to create buttons and add to interfaces. RETURNS A  LAYOUT. NOTE: THESE FUNCTIONS MUST BE USED AS A PACKAGE. 2 arg lists MUST be in form of  '(list "A" "B" "C").  REQUIRES a pre-defined INTERFACE  a layout named button-panel-name (can be within another layout), but must have its own separate empty slot."
  (let
      ((layout-inst)       
       (global-selected-values-list (my-make-symbol 
                                     (format nil "*~A-selected-values-list" button-panel-name)))
       )
;;  (apply #'plot plot-data)

   (eval 
    `(defun my-button-panel-selection-callback2 (item interface)  
  "In U-capi-buttons-etc.lisp, MUST use with my-button-panels.  MODIFY THIS CALLBACK to get data or actions from the button selection."
  (let
      ((button-value)
       (button-data)
       )
;;aaa
  (with-slots (,button-panel-name) interface
    (loop
     for button in (capi:layout-description ,button-panel-name)
        unless (eq item button)
        do
        (setf (capi:item-selected button) nil)
        (afout 'out (format nil "button= ~A (capi:item-selected button)= ~A" button (capi:item-selected button)))
        ;;end loop
        )
    ;;added
    (setf selected-text (capi:item-text item)
            selected-data (capi:item-data item)
            selected-button item)

    (setf ,global-selected-values-list 
          (list selected-text selected-data  selected-button))

    (if ,close-interface-on-selection-p
        (capi:destroy interface))
    ;;end callback
    )))
    ;;end defun,eval
    )

   (defun make-a-button2 (text data)
     (make-instance 'capi:radio-button 
                    :text (format nil "~A" text)
                    :data data
                    :selected nil
                    :selection-callback 'my-button-panel-selection-callback2
                    :callback-type :item-interface))

  ;; (setf *mytestvar
  (eval
  `(defmethod capi:interface-display :before ((interface  ,interface-name))
    (with-slots (,button-panel-name) interface
      
    (setf  buttons-list ;; ,button-panel-name 
          (loop 
           for data in  ,button-data-list
           for text in  ,button-text-list
                collect (make-a-button2 text data)))
    ;;
        (capi:apply-in-pane-process ,button-panel-name
                                  #'(setf capi:layout-description)
                                         buttons-list  ;;(list ,button-panel-name)                                       
                                         ,button-panel-name))
    ;;end defmethod, eval
       ))

    (setf  layout-inst (apply #'make-instance  layout-type  layout-arglist))
    ;;now make the layout
  ))
;;test2
(defun testmb2 ()
  (setf out nil)
  (let
      ((x)
       )   
    (capi:define-interface test-radio-buttons2 ()
      ;;sets the button/item data values
      ()    ;;was (data :initform (list 1 2 3))
      (:layouts
       (row-layout1
        capi:row-layout
        '(my-button-panel-1)
        )
       (my-button-panel-1
        capi:column-layout
        ()
        )
       )
      (:default-initargs
       :title "TEST INTERFACE"
       :visible-min-width 500
       :visible-min-height 400
       :background :yellow
       :internal-border 30
       ))

    ;;SSS CHANGE -PANEL2 BACK 
    (make-my-button-panel2 'my-button-panel-1 'row-layout1 'test-radio-buttons 
                          '(list "A" "B" "C")
                          '(list 10 20 30) 
                          :close-interface-on-selection-p nil
#|                          :layout-arglist  (quote (quote (:visible-min-width 200
                                                          :visible-min-height 300
                                                          :background :green
                                                          :title "My Test Buttons"
                                                          :title-gap 60  ;;no effect?
                                                     ;;no effect    :x 0  :y 0
                                                          :internal-border 2 ;;important
                                                          :border t
                                                          :title-adjust :center
                                                  ;;no effect        :title-position :bottom
                                                         #| :font won't work :font (gp:find-best-font pane                                                                      (gp:make-font-description 
                                    :family "Times New Roman" :weight :normal
                                    :slant :italic :size 12))|#
#|                                                             :font (gp:make-font-description 
                                                                 :family nil
                                                                 :weight :normal  :size 11)|#
                                                          )))|#
                          :button-arglist (quote   (                        
                                                   :title "Press a button"
                                                   :title-gap 50  ;;gap betw end of title and button
                                                   :background :light-blue
                                                     :internal-border 0 ;;no effect??
                                                   :visible-min-width 100
                                                   :visible-min-height 40
                                                   ))                        

                          ;;end make-my-button-panel
                          )
    (capi:display (setf *mb-testinst2 (make-instance 'test-radio-buttons2)))      
    ))

 
 #| LAYOUT INITARGS--I MIGHT USE
:ADJUST
:AUTOMATIC-RESIZE
:BACKGROUND
:BORDER
:BUTTONS
:CHILDREN
:COLOR-REQUIREMENTS
:COLUMNS
:DEFAULT-X
:DEFAULT-Y
:DESCRIPTION
:ENABLED
:FILTER
:FIXED-SIZE
:FLAG
:FONT
:GAP
:HAS-TITLE-COLUMN-P
:INITIAL-CONSTRAINTS
:MESSAGE
:MESSAGE-ARGS
:MESSAGE-FONT
:MESSAGE-GAP
:NAME
:PLIST
:PREPROCESS-DESCRIPTION
:RATIOS
:ROWS
:TITLE
:TITLE-ADJUST
:TITLE-ARGS
:TITLE-FONT
:TITLE-GAP
:TITLE-POSITION
:VISIBLE-BORDER
:VISIBLE-HEIGHT
:VISIBLE-MAX-HEIGHT
:VISIBLE-MAX-WIDTH
:VISIBLE-MIN-HEIGHT
:VISIBLE-MIN-WIDTH
:WIDGET-NAME
:WINDOW-STYLES
:X
:X-ADJUST
:X-GAP
:X-RATIOS
:X-UNIFORM-SIZE-P
:Y
:Y-ADJUST
:Y-GAP
:Y-RATIOS
:Y-UNIFORM-SIZE-P
|#
#|  COMMON RADIO-BUTTON INITARGS
:ACTION-CALLBACK
:ALTERNATE-CALLBACK
:ALTERNATIVE-ACTION-CALLBACK
:BACKGROUND
:BEZEL-STYLE
:BUTTON-GROUP
:BUTTONS
:CALLBACK
:CALLBACK-TYPE
:CANCEL-P
:COLLECTION
:COLOR-REQUIREMENTS
:DATA
:DATA-FUNCTION
:ENABLED
:EXTEND-CALLBACK
:FILTER
:FONT
:HELP-KEY
:HORIZONTAL-SCROLL
:IMAGE
:INDICATOR
:INITIAL-CONSTRAINTS
:INTERACTION
:INTERFACE
:INTERNAL-BORDER
:KEY-FUNCTION
:MAX-HEIGHT
:MAX-WIDTH
:MNEMONIC
:MNEMONIC-ESCAPE
:MNEMONIC-TEXT
:MNEMONIC-TITLE
:NAME
:PRESS-CALLBACK
:PRINT-FUNCTION
:RETRACT-CALLBACK
:SELECTED
:SELECTED-IMAGE
:SELECTION-CALLBACK
:TAKE-FOCUS
:TEXT
:TEXT-ALIGNMENT
:TITLE
:TITLE-ADJUST
:TITLE-ARGS
:TITLE-FONT
:TITLE-GAP
:TITLE-POSITION
:VISIBLE-BORDER
:VISIBLE-HEIGHT
:VISIBLE-MAX-HEIGHT
:VISIBLE-MAX-WIDTH
:VISIBLE-MIN-HEIGHT
:VISIBLE-MIN-WIDTH
:VISIBLE-WIDTH
:WIDGET-NAME
:WINDOW-STYLES
:X
:Y
|#

#| 
  I am converting a java questionnaire I wrote to lisp and a crucial   | element is the capi:radio-button-panel or a substitute that looks almost   just like it (not a   | list panel). It is very important that users do not see any checked   | buttons before they make their own choices-NO DEFAULT SELECTION.
 
  The only way is to use the CAPI:CHECK-BUTTON-PANEL like this.
|# 
#|
   (defun force-selection (pane data)
      (setf (capi:choice-selected-items pane) (list data)))
   
   (capi:contain
     (make-instance 'capi:check-button-panel
      :interaction :multiple-selection
      :items '("one" "two" "three")
      :selected-item nil
      :callback-type :collection-data
      :retract-callback 'force-selection
      :selection-callback 'force-selection))
|#


#|
(defun make-radio-button-panel (ans-instruction-text answer-array-list)
  (make-instance 'capi:radio-button-panel
                 :items answer-array-list
                 ;;'(1 2 3 4 5 6 7 8 9 10 11) 
                 :layout-class 'capi:column-layout
                 :layout-args (list :adjust :center :x 25 :y 25
                                    :y-gap *answer-panel-y-gap
                                    :x-gap 20                                                                       
                                    :internal-border 25)
                 :font (gp:make-font-description 
                        :family *answer-pane-font-face
                        :weight :normal  :size *answer-font-size)
                 :visible-border T
                 :visible-min-height *answer-pane-height
                 :visible-max-height *answer-pane-height
                 :visible-min-width *answer-pane-width
                 ;; :visible-max-width *answer-pane-width
                 :background *answer-pane-background
                ;; :selected-items nil  ;; :none
                 :title ans-instruction-text 
                 :title-adjust  :left
                 ;;   :title-args
                 :title-font (gp:make-font-description 
                              :family *answer-pane-font-face
                              :weight :normal  :size *answer-font-size)
                 :title-gap 15
                 :title-position :top
                 :mnemonics nil         ;;only works for each item'(:none)
                 :callback-type :data-interface
                 :selected nil                 
                ;;doesn't work capi:choice-selected-item nil  must use setf outside init-args
                 ;;:selection :none  ;;:nothing works
                 :selection-callback 'single-selection-callback
                 )
  )
;;
;;My version which can allow modifications, etc.

;;MY MODIFIED VERSION OF ABOVE
(capi:define-interface my-button-panel ()
  ((data :initform (list 1 2 3)))
  (:layouts
   (panel
    capi:column-layout)))

(setf *button-list nil
      *item-value-list nil
      *item-data-list nil)
(defun switch-other-buttons-off (item interface)
  (let
      ((button-value)
       (button-data)
       )
  (with-slots (panel) interface
    (loop for button in (capi:layout-description panel)
        unless (eq item button)
        do (setf (capi:item-selected button) nil))
    ;;added
   ;; (setf button-value (capi:item-selected button))
    ;;end added
    (setf *button-list (append *button-list (list item))
                               button-value (capi:item-text item)
                               *item-value-list (append *item-value-list  (list button-value))
                               button-data (capi:item-data item)
                                 *item-data-list (append *item-data-list  (list button-data)))                          
    )))
;;works, creates non-selected button "panel" then after pushing each button, results
#|CL-USER 35 > *button-list
(#<CAPI:RADIO-BUTTON "three" 2010A28B> #<CAPI:RADIO-BUTTON "two" 2010A56F> #<CAPI:RADIO-BUTTON "one" 2010A84F>)
CL-USER 44 > *item-value-list = ("three" "two" "one")
CL-USER 47 > *item-data-list = (3 2 1) |#


(defun make-a-button (i)
  (make-instance 'capi:radio-button 
                 :text (format nil "~R" i)
                 :data i
                 :selected nil
                 :selection-callback 'switch-other-buttons-off
                 :callback-type :item-interface))

(defmethod capi:interface-display :before ((self radio-buttons))
  (with-slots (data panel) self
    (setf (capi:layout-description panel)
          (loop for i in data
                collect (make-a-button i)))))

(capi:display (make-instance 'radio-buttons))
|#




#|
;;XXX
;;CREATING A BUTTON PANEL FROM FUNCTION (WITH NO DEFAULT SELECTION)
;;IF INSIDE A FUNCTION CREATING AN INTERFACE INSTANCE,
      (with-slots (answer-button-panel)  q-frame-inst
        ;;MAKE THE  BUTTON PANEL
      (setf answer-button-panel (make-radio-button-panel ans-instruction-text answer-array-list))
      ;;Put the BUTTON IN THE ALREADY CREATED FRAME
      (capi:apply-in-pane-process answer-column-layout
                                  #'(setf capi:layout-description)
                                          (list answer-button-panel)                                            
                                          answer-column-layout)
      ;;This SETS PREVENTS A DEFAULT SELECTION from being checked
      (capi:apply-in-pane-process answer-column-layout
                                  #'(setf CAPI:CHOICE-SELECTED-ITEM)
                                          (list nil)                                            
                                          answer-button-panel)


It is very important that users do not see any checked buttons before
 they make their own choices—NO DEFAULT SELECTION.
(defun test ()
  (let ((panel (make-instance 'capi:radio-button-panel :items '("He" "She" "They"))))
    (setf (capi:choice-selected-item panel) nil)
    (capi:contain panel)))
|#

#|

;;MAKE-RADIO-BUTTON-PANEL
;;
;;ddd
(defun make-radio-button-panel (ans-instruction-text answer-array-list)
  (make-instance 'capi:radio-button-panel
                 :items answer-array-list
                 ;;'(1 2 3 4 5 6 7 8 9 10 11) 
                 :layout-class 'capi:column-layout
                 :layout-args (list :adjust :center :x 25 :y 25
                                    :y-gap *answer-panel-y-gap
                                    :x-gap 20                                                                       
                                    :internal-border 25)
                 :font (gp:make-font-description 
                        :family *answer-pane-font-face
                        :weight :normal  :size *answer-font-size)
                 :visible-border T
                 :visible-min-height *answer-pane-height
                 :visible-max-height *answer-pane-height
                 :visible-min-width *answer-pane-width
                 ;; :visible-max-width *answer-pane-width
                 :background *answer-pane-background
                ;; :selected-items nil  ;; :none
                 :title ans-instruction-text 
                 :title-adjust  :left
                 ;;   :title-args
                 :title-font (gp:make-font-description 
                              :family *answer-pane-font-face
                              :weight :normal  :size *answer-font-size)
                 :title-gap 15
                 :title-position :top
                 :mnemonics nil         ;;only works for each item'(:none)
                 :callback-type :data-interface
                ;;doesn't work capi:choice-selected-item nil  must use setf outside init-args
                 ;;:selection :none  ;;:nothing works
                 :selection-callback 'single-selection-callback
                 )
  )
|#

;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/buttons:buttons.lisp,v 1.16.1.1 2011/08/24 13:26:20 davef Exp $" -*-

;;----------------------------------------------------------------------------
;;
;; examples/capi/buttons/buttons.lisp
;;
;; This example demonstrates the use of buttons and button-panels in the
;; CAPI.
;; To try it, compile and load this file and then execute:
;;
;;      (CL-USER::TEST-BUTTONS)
;;
;; This display two interfaces. One is the BUTTON-TEST, and demonstarte
;; the callbacks. The other one is an IMAGE-BUTTON-EXAMPLE, and demostrates
;; using images in buttons. 

;;----------------------------------------------------------------------------
;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
;;----------------------------------------------------------------------------


(in-package "CL-USER")


;;----------------------------------------------------------------------------
;; Define the interface button-test
;;----------------------------------------------------------------------------
#|
(capi:define-interface button-test ()
  ()
  (:panes
   (push-button
    capi:push-button
    :text "Push"
    :data :push-button
    :callback-type :data-interface
    :selection-callback 'button-selection-callback
    :extend-callback    'button-extend-callback
    :retract-callback   'button-retract-callback
    ;; PUSH-BUTTON has alternate callback that can be invoked
    ;; by selecting with CONTROL (on Windows and GTK) or Command
    ;; (on Cocoa)
    :alternate-callback 'button-alternate-callback
    )
   (check-button
    capi:check-button
    :text "Check"
    :data :check-button
    :callback-type :data-interface
    :selection-callback 'button-selection-callback
    :extend-callback    'button-extend-callback
    :retract-callback   'button-retract-callback)
   (radio-button
    capi:radio-button
    :text "Radio"
    :data :radio-button
    :callback-type :data-interface
    :selection-callback 'button-selection-callback
    :extend-callback    'button-extend-callback
    :retract-callback   'button-retract-callback)
   (push-button-panel
    capi:push-button-panel
    :items '("push 1" "push 2" "push 3")
    :callback-type :data-interface
    :selection-callback 'button-selection-callback
    :extend-callback    'button-extend-callback
    :retract-callback   'button-retract-callback)
   (check-button-panel
    capi:check-button-panel
    :items '("check 1" "check 2" "check 3")
    :callback-type :data-interface
    :selection-callback 'button-selection-callback
    :extend-callback    'button-extend-callback
    :retract-callback   'button-retract-callback)
   (radio-button-panel
    capi:radio-button-panel
    :items '("Radio 1" "Radio 2" "Radio 3")
    :callback-type :data-interface
    :selection-callback 'button-selection-callback
    :extend-callback    'button-extend-callback
    :retract-callback   'button-retract-callback)
   (op
    capi:collector-pane
    :initial-constraints '(:visible-min-width (character 25)))
    )
  (:layouts
   (row
    capi:row-layout
    '(push-button radio-button check-button)
    :y-adjust :centre)
   (default-layout
    capi:column-layout
    '(row push-button-panel check-button-panel radio-button-panel op)))
  (:default-initargs
   :layout 'default-layout
   :title "Button Test"))
|#

;;----------------------------------------------------------------------------
;; A generic callback
;;----------------------------------------------------------------------------

(defun button-callback (type data interface)
  (format (capi:collector-pane-stream(slot-value interface 'op))
          "~S ~a~%" data type))

(defun button-selection-callback (&rest args)
  (apply 'button-callback "selected" args))

(defun button-extend-callback (&rest args)
  (apply 'button-callback "extended" args))

(defun button-retract-callback (&rest args)
  (apply 'button-callback "retracted" args))

(defun button-alternate-callback (&rest args)
  (apply 'button-callback "alternate selected" args))

;;----------------------------------------------------------------------------
;; Image Buttons
;;----------------------------------------------------------------------------

;; These forms were derived via something like:
;; (let ((external-image (gp:read-external-image <file>)))
;;  (gp:compress-external-image external-image)
;;  (setf (gp:external-image-transparent-color-index external-image) 0)
;;  (make-load-form external-image))
;; where <file> contains the DIB format representation of an image.

#|
(defparameter *dont-button*
  (make-instance 
   'gp:external-image
   :data
   #(66 77 66 2 0 0 0 0 0 0 66 0 0 0 40 0 0 0 32 0 0 0 32 0 0 0 1 0 4 0 2 0
        0 0 54 1 0 0 109 11 0 0 109 11 0 0 3 0 0 0 3 0 0 0 0 0 0 0 255 0 0 0 0 255
        0 0 32 0 0 0 32 0 0 0 32 0 0 0 32 0 0 0 32 0 0 0 11 0 0 4 34 32 5 0 2 34
        10 0 0 0 10 0 0 4 34 32 6 0 4 34 8 0 0 0 11 0 0 4 34 32 4 0 0 4 34 32 9 0
        0 0 11 0 0 4 34 32 4 0 0 4 34 32 9 0 0 0 12 0 0 10 34 32 0 34 32 0 10 0 0 0
        2 1 10 17 0 10 34 33 17 34 33 0 7 17 0 3 0 0 0 0 2 0 11 17 0 8 34 33 34 33 9 17
        2 0 0 0 13 0 7 34 12 0 0 0 10 0 0 4 32 0 5 34 13 0 0 0 10 0 0 4 34 0 4 34
        14 0 0 0 10 0 0 4 34 0 4 34 4 0 2 34 8 0 0 0 11 0 0 12 34 2 34 32 0 34 9 0
        0 0 11 0 0 12 34 2 34 32 2 32 9 0 0 0 12 0 6 34 0 4 2 32 10 0 0 0 12 0 8 34
        12 0 0 0 13 0 6 34 13 0 0 0 14 0 4 34 14 0 0 0 15 0 0 4 34 32 13 0 0 0 15 0
        4 34 13 0 0 0 14 0 6 34 12 0 0 0 14 0 6 34 12 0 0 0 15 0 4 34 13 0 0 0 16 0
        2 34 14 0 0 0 32 0 0 0 32 0 0 0 32 0 0 0 32 0 0 0 0 1)
   :transparent-color-index 0))

(defparameter *do-button*
  (make-instance 
   'gp:external-image
   :data
   #(66 77 66 2 0 0 0 0 0 0 66 0 0 0 40 0 0 0 32 0 0 0 32 0 0 0 1 0 4 0 2 0
        0 0 14 1 0 0 109 11 0 0 109 11 0 0 3 0 0 0 3 0 0 0 0 0 0 0 255 0 0 0 0 0
        255 0 32 0 0 0 32 0 0 0 32 0 0 0 2 0 29 17 1 0 0 0 2 0 29 17 1 0 0 0 32 0
        0 0 12 0 0 8 34 32 34 32 12 0 0 0 12 0 0 8 34 32 34 32 12 0 0 0 12 0 0 8 34 32
        34 32 12 0 0 0 12 0 0 8 34 32 34 32 12 0 0 0 12 0 0 8 34 32 34 32 12 0 0 0 12 0
        0 8 34 32 34 32 12 0 0 0 12 0 0 8 34 32 34 32 12 0 0 0 12 0 0 8 34 32 34 32 12 0
        0 0 12 0 0 8 34 32 34 32 12 0 0 0 10 0 11 34 11 0 0 0 10 0 11 34 11 0 0 0 10 0
        11 34 11 0 0 0 10 0 11 34 11 0 0 0 10 0 11 34 11 0 0 0 10 0 11 34 11 0 0 0 10 0
        11 34 11 0 0 0 10 0 11 34 11 0 0 0 11 0 9 34 12 0 0 0 14 0 0 4 34 32 14 0 0 0
        13 0 5 34 14 0 0 0 13 0 5 34 14 0 0 0 13 0 5 34 14 0 0 0 14 0 0 4 34 32 14 0
        0 0 32 0 0 0 32 0 0 0 32 0 0 0 0 1) :transparent-color-index 0))

(defparameter *dont-dis-button*
  (make-instance 
   'gp:external-image
   :data
   #(66 77 86 2 0 0 0 0 0 0 86 0 0 0 40 0 0 0 32 0 0 0 32 0 0 0 1 0 4 0 2 0
        0 0 132 1 0 0 109 11 0 0 109 11 0 0 8 0 0 0 8 0 0 0 0 0 0 0 126 84 169 0 169 169
        169 0 212 169 169 0 169 255 169 0 169 89 174 0 84 174 174 0 126 92 177 0 32 0 0 0 32 0 0 0 32 0
        0 0 32 0 0 0 11 0 0 4 85 80 5 0 2 85 10 0 0 0 10 0 0 14 84 68 80 0 5 68 85 0
        8 0 0 0 9 0 0 6 84 4 80 0 4 0 0 6 84 4 69 0 7 0 0 0 10 0 0 14 84 4 80 0
        84 4 85 0 8 0 0 0 10 0 0 14 84 4 80 0 84 4 80 0 8 0 0 0 11 102 0 12 116 4 102 100
        4 22 6 102 0 3 0 6 0 0 2 99 10 51 0 10 64 67 51 64 67 0 7 51 0 3 96 0 0 0 2 6
        11 51 0 8 64 67 64 67 9 51 2 96 0 0 2 0 11 102 0 8 64 4 0 69 10 102 1 0 0 0 9 0
        0 12 84 82 84 0 4 80 11 0 0 0 9 0 0 16 84 69 84 0 69 0 5 80 7 0 0 0 9 0 0 16
        84 5 84 0 69 0 84 69 7 0 0 0 10 0 0 14 80 69 64 4 80 84 69 0 8 0 0 0 10 0 0 14
        84 5 64 4 85 68 80 0 8 0 0 0 11 0 0 12 80 64 0 69 68 80 9 0 0 0 11 0 2 84 6 0
        2 69 11 0 0 0 12 0 2 84 4 0 2 69 12 0 0 0 13 0 0 6 84 0 69 0 13 0 0 0 14 0
        0 6 84 4 80 0 12 0 0 0 14 0 0 6 84 0 69 0 12 0 0 0 13 0 2 84 4 0 2 69 11 0
        0 0 13 0 2 84 4 0 2 69 11 0 0 0 14 0 0 6 84 0 69 0 12 0 0 0 15 0 0 4 84 69
        13 0 0 0 16 0 2 85 14 0 0 0 32 0 0 0 32 0 0 0 32 0 0 0 0 1) :transparent-color-index 0))

(defparameter *do-dis-button*
  (make-instance
   'gp:external-image
   :data
   #(66 77 78 2 0 0 0 0 0 0 78 0 0 0 40 0 0 0 32 0 0 0 32 0 0 0 1 0 4 0 2 0
        0 0 106 1 0 0 109 11 0 0 109 11 0 0 6 0 0 0 6 0 0 0 0 0 0 0 255 255 116 0 227 135
        135 0 230 230 138 0 161 207 207 0 135 135 227 0 32 0 0 0 32 0 0 0 2 0 29 68 1 0 0 0 2 4
        29 34 1 64 0 0 2 4 29 34 1 64 0 0 2 0 29 68 1 0 0 0 11 0 0 10 53 85 53 85 48 0
        11 0 0 0 11 0 0 10 53 5 53 5 48 0 11 0 0 0 11 0 0 10 53 5 53 5 48 0 11 0 0 0
        11 0 0 10 53 5 53 5 48 0 11 0 0 0 11 0 0 10 53 5 53 5 48 0 11 0 0 0 11 0 0 10
        53 5 53 5 48 0 11 0 0 0 11 0 0 10 53 5 53 5 48 0 11 0 0 0 11 0 0 10 53 5 53 5
        48 0 11 0 0 0 10 0 0 12 51 80 83 80 83 48 10 0 0 0 9 0 0 14 53 80 0 80 0 85 48 0
        9 0 0 0 9 0 2 53 9 0 2 83 10 0 0 0 9 0 2 53 9 0 2 83 10 0 0 0 9 0 2 53
        9 0 2 83 10 0 0 0 9 0 2 53 9 0 2 83 10 0 0 0 9 0 2 53 9 0 2 83 10 0 0 0
        9 0 2 53 9 0 2 83 10 0 0 0 9 0 2 53 9 0 2 83 10 0 0 0 10 0 0 12 53 85 0 5
        85 48 10 0 0 0 11 0 0 10 51 53 5 19 48 0 11 0 0 0 12 0 0 8 53 0 5 48 12 0 0 0
        12 0 0 8 53 0 5 48 12 0 0 0 12 0 0 8 53 0 5 48 12 0 0 0 13 0 0 6 53 85 48 0
        13 0 0 0 14 0 0 4 51 48 14 0 0 0 32 0 0 0 32 0 0 0 0 1) :transparent-color-index 0))



;;------------------------------------------------------------

;; Register the images in the image translation table.
;; From henceforth they can be referenced via the ids

(mapc #'(lambda (id image)
          (gp:register-image-translation id image))
      '(:do :dont :do-disabled :dont-disabled)
      (list *do-button* *dont-button* *do-dis-button* *dont-dis-button*))

;; This shows the effects of button classes with and without indicators.
;; Note that :indicator, :disabled-images, :selected-disabled-images are ignored on Windows.
;; The images in the buttons are used shared resources.

(capi:define-interface image-button-example ()
  ()
  (:panes 
   (buttons1 capi:radio-button-panel
             :items '(1 2)
             :images (list :do :do)
             :disabled-images (list :do-disabled :do-disabled)              
             :selected-images (list :dont :dont)
             :selected-disabled-images (list :dont-disabled :dont-disabled) 
             :indicator T)                                                  
   (buttons2 capi:radio-button-panel
             :items '(1 2)
             :images (list :do :do)
             :disabled-images (list :do-disabled :do-disabled)              
             :selected-images (list :dont :dont)
             :selected-disabled-images (list :dont-disabled :dont-disabled) 
             :indicator nil)                                                
   (buttons3 capi:check-button-panel
             :items '(1 2)
             :images (list :do :do)
             :disabled-images (list :do-disabled :do-disabled)              
             :selected-images (list :dont :dont)
             :selected-disabled-images (list :dont-disabled :dont-disabled) 
             :indicator T)                                                  
   (buttons4 capi:check-button-panel
             :items '(1 2)
             :images (list :do :do)
             :disabled-images (list :do-disabled :do-disabled)
             :selected-images (list :dont :dont)
             :selected-disabled-images (list :dont-disabled :dont-disabled)
             :indicator nil)
   (disabler capi:radio-button-panel :items '("Enable" "Disable")
             :callback-type :none
             :layout-class 'capi:column-layout
             :selection-callback #'(lambda ()
                                     (mapc #'(lambda (b)
                                               (setf (capi:simple-pane-enabled b) (not (capi:simple-pane-enabled b))))
                                           (list buttons1 buttons2 buttons3 buttons4))))
   (image-explainer capi:output-pane
                    :visible-min-height 160
                    :visible-max-height t
                    :title "Key to images"
                    :title-position :frame
                    :visible-border nil
                    :visible-min-width 180
                    :display-callback 
                    'display-images-with-titles))
  (:layouts 
   (one capi:column-layout '(buttons1 buttons2) :title "Radio" :title-position :frame)
   (two capi:column-layout '(buttons3 buttons4) :title "Check" :title-position :frame)
   (interactive-layout  capi:column-layout  '(one two disabler) )
   (main  capi:row-layout  '(interactive-layout image-explainer) :default t))
  (:default-initargs
   :title "Image Button Example 2"))

(defun display-image-with-title (pane top-y title image)
  (gp:draw-string pane title 50 (+ top-y 30))
  (gp:draw-image pane (gp:load-image pane image) 10 (+ top-y 5)))

(defun display-images-with-titles (pane x y width height)
  (declare (ignore x y width height))
  (display-image-with-title pane 0 "Default image" :do)
  (display-image-with-title pane 40 "Selected image" :dont)
  (if (member (capi:default-library) '(:GTK :Cocoa))
      (gp:draw-string pane "Automatic disabling" 10 110)
    (progn 
      (display-image-with-title pane 80 "Default disabled" :do-disabled)
      (display-image-with-title pane 120 "Selcted disabled" :dont-disabled))))
;;----------------------------------------------------------------------------
;; Test button-test
;;----------------------------------------------------------------------------

(defun test-buttons ()
  (let(( first (capi:display (make-instance 'button-test))))
    (multiple-value-bind (x y width height)
        (capi:top-level-interface-geometry first)
      (declare (ignore x width height))
      (let ((second (make-instance 'image-button-example)))
        (capi:display second)
        (capi:set-top-level-interface-geometry second :y (+ y 300))))))

|#

;;xxx --------------------- STARTING WITH NO BUTTONS CHECKED -----------

#|  Thomas Stevens wrote on Sat, 19 Apr 2014 00:41:37 +0000 04:41:
#| 
  | I am converting a java questionnaire I wrote to lisp and a crucial   | element is the capi:radio-button-panel or a substitute that looks almost   just like it (not a   | list panel). It is very important that users do not see any checked   | buttons before they make their own choices-NO DEFAULT SELECTION.
 
  The only way is to use the CAPI:CHECK-BUTTON-PANEL like this.
|# 
  (defun force-selection (pane data)
    (setf (capi:choice-selected-items pane) (list data)))
 
  (capi:contain
   (make-instance 'capi:check-button-panel
    :interaction :multiple-selection
    :items '("one" "two" "three")
    :selected-item nil
    :callback-type :collection-data
    :retract-callback 'force-selection
    :selection-callback 'force-selection))

   (defun force-selection (pane data)
      (setf (capi:choice-selected-items pane) (list data)))
   
    (capi:contain
     (make-instance 'capi:check-button-panel
      :interaction :multiple-selection
      :items '("one" "two" "three")
      :selected-item nil
      :callback-type :collection-data
      :retract-callback 'force-selection
      :selection-callback 'force-selection))
|#
#|You can also do it like this, with all of the buttons unselected initially, and imitating radio-button-panel's :single-selection interaction subsequently by making their callbacks deselect the other
buttons:|#

;;HERE HE DOESN'T USE RADIO-BUTTON PANEL, BUT MAKES HIS OWN!
;;  PROBLEM WITH :SELECTION-CALLBACK ALREADY BEING DEFINED
;; I solved problem?? below
#|(capi:define-interface radio-buttons ()
  ;;sets the button/item data values
  ((data :initform (list 1 2 3)))
  (:layouts
   (panel
    capi:column-layout)))

(defun switch-other-buttons-off (item interface)
  (with-slots (panel) interface
    (loop for button in (capi:layout-description panel)
        unless (eq item button)
        do (setf (capi:item-selected button) nil))))

(defun make-a-button (i)
  (make-instance 'capi:radio-button 
                 :text (format nil "~R" i)
                 :data i
                 :selected nil
                 :selection-callback 'switch-other-buttons-off
                 :callback-type :item-interface))

(defmethod capi:interface-display :before ((self radio-buttons))
  (with-slots (data panel) self
    (setf (capi:layout-description panel)
          (loop for i in data
                collect (make-a-button i)))))

(capi:display (make-instance 'radio-buttons))
|#
#|
;;MY MODIFIED VERSION OF ABOVE
(capi:define-interface my-radio-buttons-interface ()
  ((data :initform (list 1 2 3)))
  (:layouts
   (panel
    capi:column-layout)))

(setf *button-list nil
      *item-value-list nil
      *item-data-list nil)
(defun switch-other-buttons-off (item interface)
  (let
      ((button-value)
       (button-data)
       )
  (with-slots (panel) interface
    (loop for button in (capi:layout-description panel)
        unless (eq item button)
        do (setf (capi:item-selected button) nil))
    ;;added
   ;; (setf button-value (capi:item-selected button))
    ;;end added
    (setf *button-list (append *button-list (list item))
                               button-value (capi:item-text item)
                               *item-value-list (append *item-value-list  (list button-value))
                               button-data (capi:item-data item)
                                 *item-data-list (append *item-data-list  (list button-data)))                          
    )))
|#
;;works, creates non-selected button "panel" then after pushing each button, results
#|CL-USER 35 > *button-list
(#<CAPI:RADIO-BUTTON "three" 2010A28B> #<CAPI:RADIO-BUTTON "two" 2010A56F> #<CAPI:RADIO-BUTTON "one" 2010A84F>)
CL-USER 44 > *item-value-list = ("three" "two" "one")
CL-USER 47 > *item-data-list = (3 2 1) |#


#|(defun make-a-button (i)
  (make-instance 'capi:radio-button 
                 :text (format nil "~R" i)
                 :data i
                 :selected nil
                 :selection-callback 'switch-other-buttons-off
                 :callback-type :item-interface))

(defmethod capi:interface-display :before ((self radio-buttons))
  (with-slots (data panel) self
    (setf (capi:layout-description panel)
          (loop for i in data
                collect (make-a-button i)))))

(capi:display (make-instance 'radio-buttons))
|#

#| This also seems to work on Windows and Cocoa, but probably should not since radio-button-panel is a :single-selection choice. Does not nullify initial selections on GTK+, so definitely not portable.
(let ((rbp (make-instance 'capi:radio-button-panel
                          :items (list 1 2 3) 
                          :print-function (lambda (i) 
                                            (format nil "~R" i))
                          :layout-class 'capi:column-layout)))
  (setf (capi:choice-selection rbp) nil)
  (capi:contain rbp))
|#
#|

--
Dave Fox
LispWorks Ltd
http://www.lispworks.com/

Registered Office: St John's Innovation Centre, Cowley Road, Cambridge CB4 0WS Registered in England: No. 5114963 EC VAT ID: GB 833329531|#





