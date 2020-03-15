;;******************************** U-debug.lisp *************************
;;
;; FUNCTIONS FOR DEBUGGING

;;



(in-package "COMMON-LISP-USER")


;;FOUT -- A SUBSTITUTE FOR (FORMAT T USING WINDOWS
;;works
;;ddd
(defun tfout ()  
  (fout "test output 1") ;; "test-output1" "Fout OUTPUT PANES" )
    (fout "test output 1" "test-output2" "Fout OUTPUT PANES" )
  )

;;AFOUT
;;works
;;ddd
(defun afout (fout-var text)
  "creates a list of texts that are appended by afout onto the var fout-var--even if it's unbound"
  (cond
   ((not (boundp fout-var))
      (set fout-var text))
   (t
    (set fout-var (format nil "~A~%~%~A~%" (eval fout-var) text)))
   ))

;;works
#|(afout 'thyz "this text")
(afout 'thyz "this text2")|#

;;FOUT -- REPLACES (FORMAT NIL IN MY FUNCTIONS)
;;  USE AFOUT to create texts that are added together 
;; then FOUT TO CREATE 2 EDITABLE WINDOWS FOR OUTPUT
;;
;;ddd
(defun fout (pane1-text &optional pane2-text win-title)
  "in U-debug.lisp, An OUTPUT WINDOW for format based debugging--if I can
get it to work"
  (let
      ((fout-instance)
       )
    (if (null pane2-text) (setf pane2-text ""))
    (if (null win-title) (setf win-title ""))
    ;;make the instance and display it
    (capi:display
     (setf fout-instance (make-instance 'output-window)))
    ;;set the slots
     (with-slots (outpane1 outpane2 title-pane) fout-instance
         ;;write the text to rich text windows
         (setf (capi:editor-pane-text outpane1) pane1-text)
         (setf (capi:editor-pane-text outpane2) pane2-text)
     ;;   (setf (capi:rich-text-pane-text outpane1) pane1-text)
     ;;    (setf (capi:rich-text-pane-text outpane2) pane2-text)
          (setf (capi:rich-text-pane-text title-pane) win-title)        
     ;;eown't work  (setf (capi:interface-title instance-name) "Fout OUTPUT PANES")
        )
    ))

   

;;OUTPUT-WINDOW INFERFACE
;;
;;ddd
(capi:define-interface output-window ()
  ()
   (:panes
   (title-pane
    capi:rich-text-pane
    :max-height 20
    :accepts-focus-p t
 ;;   :automatic-resize t
    :enabled t)
   (outpane1
    capi:editor-pane)
   (outpane2
    capi:editor-pane)

#| used editor-pane instead of rich-text-pane to make it searchable with C-F
  (outpane1
    capi:rich-text-pane
    :accepts-focus-p t
 ;;   :automatic-resize t
    :enabled t)
   (outpane2
    capi:rich-text-pane
    :accepts-focus-p t
 ;;   :automatic-resize t
    :enabled t)
|#
   (option-pane-1
    capi:option-pane
    :items '("Option-Pane-1" "Item" "Another Item")
    :selection 0)
   (radio-button-panel-1
    capi:radio-button-panel
    :items '("Radio-Button-Panel-1" "Button 2" "Button 3")
    :max-width t
    :max-height t))
  (:layouts
   (column-layout-1
    capi:column-layout
    '(title-pane :divider outpane1 :divider outpane2 :divider option-pane-1 radio-button-panel-1)))
#| (:menu-bar menu-1 menu-2)
  (:menus
   (menu-2
    "Menu-2"
    :items  ("item1" "item2"))
   (menu-1
    "Menu-1"
     :items ( "itemA" "itemB")))|#
  (:default-initargs
   :min-height 400
   :min-width 500
   :layout 'column-layout-1
   :default-height 600
   :default-width 300
   ))


;;************************* hhh  HELP *************************

#|
(make-instance 'menu
:title "Foo"
:items '("One" "Two" "Three" "Four")
:callback 'test-callback)|#