;;******************************* U-file-interfaces.lisp *********************************

;;EXPLORE-DIRS-INTERFACE
;;
;;ORIGINAL--INCLUDES LIST-PANELS
;;ddd
(capi:define-interface EXPLORE-DIRS-INTERFACE  ()
  ((current-list-panel
    :initarg :current-list-panel
    :accessor current-list-panel
    :initform NIL
    :documentation  "Current current-list-panel selection made in")
   (current-selection
    :initarg :current-selection
    :accessor current-selection
    :initform NIL
    :documentation  "Current selection in item list")
   (selection
    :initarg :selection
    :accessor selection
    :initform NIL
    :documentation  "Final selection in item list")
   (drive-syms
    :initarg :drive-syms
    :accessor drive-syms
    :initform NIL
    :documentation  "drive-syms")
   (drive-symlists
    :initarg :drive-symlists
    :accessor drive-symlists
    :initform NIL
    :documentation  "drive-symlists")
   (drive-names
    :initarg :drive-names
    :accessor drive-names
    :initform NIL
    :documentation  "drive-names")
   (current-drive-symlist
    :initarg :current-drive-symlist
    :accessor current-drive-symlist
    :initform NIL
    :documentation  "current-drive-symlist")
   (current-drive-sym
    :initarg :current-drive-sym
    :accessor current-drive-sym
    :initform NIL
    :documentation  "current-drive-sym")
   (panel-1-selected-path
    :initarg :panel-1-selected-path
    :accessor panel-1-selected-path
    :initform NIL
    :documentation  "panel-1-selected-path")
   (panel-2-selected-path
    :initarg :panel-2-selected-path
    :accessor panel-2-selected-path
    :initform NIL
    :documentation  "panel-2-selected-path")
   (panel-3-selected-path
    :initarg :panel-3-selected-path
    :accessor panel-3-selected-path
    :initform NIL
    :documentation  "panel-3-selected-path")
   (panel-4-selected-path
    :initarg :panel-4-selected-path
    :accessor panel-4-selected-path
    :initform NIL
    :documentation  "panel-4-selected-path")
   (panel-5-selected-path
    :initarg :panel-5-selected-path
    :accessor panel-5-selected-path
    :initform NIL
    :documentation  "panel-5-selected-path")
   (panel-6-selected-path
    :initarg :panel-6-selected-path
    :accessor panel-6-selected-path
    :initform NIL
    :documentation  "panel-6-selected-path")

   (panel-1-subdir-info
    :initarg :panel-1-subdir-info
    :accessor panel-1-subdir-info
    :initform NIL
    :documentation  "panel-1-subdir-info")
   (panel-2-subdir-info
    :initarg :panel-2-subdir-info
    :accessor panel-2-subdir-info
    :initform NIL
    :documentation  "panel-2-subdir-info")
   (panel-3-subdir-info
    :initarg :panel-3-subdir-info
    :accessor panel-3-subdir-info
    :initform NIL
    :documentation  "panel-3-subdir-info")
   (panel-4-subdir-info
    :initarg :panel-4-subdir-info
    :accessor panel-4-subdir-info
    :initform NIL
    :documentation  "panel-4-subdir-info")
   (panel-5-subdir-info
    :initarg :panel-5-subdir-info
    :accessor panel-5-subdir-info
    :initform NIL
    :documentation  "panel-5-subdir-info")
   (panel-6-subdir-info
    :initarg :panel-6-subdir-info
    :accessor panel-6-subdir-info
    :initform NIL
    :documentation  "panel-6-subdir-info")
   (current-subdir-info
    :initarg :current-subdir-info
    :accessor current-subdir-info
    :initform NIL
    :documentation  "Current subdir-info")
   )
  ;;EDITOR-PANES
  (:panes
   (editor-pane-1
    capi:rich-text-pane
    :visible-max-height 20
    )
   (path-title-pane
    capi:title-pane
    :title "Full PATH of SELECTED ITEM==>  "
    :title-font (gp:make-font-description :family "Times"
                                          :size 12  :weight :bold  :slant :roman)
    :visible-max-height 20
    :font (gp:make-font-description :family "Times"
                                    :size 12  :weight :bold  :slant :roman)
    ) 
   (misc-data-title-pane
    capi:title-pane
    :title "DATA FILE CREATION DATE==>  "
    :title-font (gp:make-font-description :family "Times"
                                          :size 12  :weight :bold  :slant :roman)
    :visible-max-height 20
    :font (gp:make-font-description :family "Times"
                                    :size 12  :weight :bold  :slant :roman)
    )
   (editor-pane-2
    capi:rich-text-pane
    :visible-max-height 20
    )
   ;; DIR-LIST-PANELS
   (dir-list-panel-1
    capi:multi-column-list-panel
    :columns '((:title "Name")(:title "Size"))
    :items '(("")(""))
    :selection-callback 'tomex-list-panel-callback
    :callback-type  :element
    ;; :selection 0
    ;;  :action-callback 'set-source-callback-1
    ;; :external-max-width 60
    :SCROLL-IF-NOT-VISIBLE-P T
    )
   (dir-list-panel-2
    capi:multi-column-list-panel
    :columns '((:title "Name")(:title "Size" :adjust :right :gap 15)(:title "Date"))
    :items '(("")("")) ;; '(("D:\\xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" 11111 "2016-11-2") ("E:\\" 2222 "2016-10-2") ( "F:\\"  33333 "2016-11-5"))
    ;; :selection 0
    :selection-callback 'tomex-list-panel-callback
    :callback-type  :element
    ;;  :action-callback 'set-source-callback-2
    :SCROLL-IF-NOT-VISIBLE-P T
    )
   (dir-list-panel-3
    capi:multi-column-list-panel
    :columns '((:title "Name")(:title "Size" :adjust :right :gap 15)(:title "Date"))
    :items '(("")("")) ;;'(("D:\\xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" 11111 "2016-11-2") ("E:\\" 2222 "2016-10-2") ( "F:\\"  33333 "2016-11-5"))
    ;; :selection 0
    :selection-callback 'tomex-list-panel-callback
    :callback-type  :element
    ;;   :action-callback 'set-source-callback-3
    :SCROLL-IF-NOT-VISIBLE-P T
    )
   (dir-list-panel-4
    capi:multi-column-list-panel
    :columns '((:title "Name")(:title "Size" :adjust :right :gap 15)(:title "Date"))
    :items '(("")("")) ;;'() ;;'(("D:\\xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" 11111 "2016-11-2") ("E:\\" 2222 "2016-10-2") ( "F:\\"  33333 "2016-11-5"))
    ;; :selection 0
    :selection-callback 'tomex-list-panel-callback
    :callback-type  :element
    ;;  :action-callback 'set-source-callback-4
    :SCROLL-IF-NOT-VISIBLE-P T
    )
   (dir-list-panel-5
    capi:multi-column-list-panel
    :columns '((:title "Name")(:title "Size" :adjust :right :gap 15)(:title "Date"))
    :items '(("")("")) ;;'() ;;'(("D:\\xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" 11111 "2016-11-2") ("E:\\" 2222 "2016-10-2") ( "F:\\"  33333 "2016-11-5"))
    ;; :selection 0
    :selection-callback 'tomex-list-panel-callback
    :callback-type  :element
    ;;  :action-callback 'set-source-callback-5
    :SCROLL-IF-NOT-VISIBLE-P T
    )
   (dir-list-panel-6
    capi:multi-column-list-panel
    :columns '((:title "Name")(:title "Size" :adjust :right :gap 15)(:title "Date"))
    :items '(("")("")) ;;'(("D:\\xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" 11111 "2016-11-2") ("E:\\" 2222 "2016-10-2") ( "F:\\"  33333 "2016-11-5"))
    
    :SCROLL-IF-NOT-VISIBLE-P T
    ;; :selection 0
    :selection-callback 'tomex-list-panel-callback
    :callback-type  :element
    ;;   :action-callback 'set-source-callback-6
    )
   (file-list-panel-1
    capi:multi-column-list-panel
    :columns '((:title "Name")(:title "Size" :adjust :right :gap 15)(:title "Date"))
    :items '(("")("")) ;;'(("D:\\xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" 11111 "2016-11-2") ("E:\\" 2222 "2016-10-2") ( "F:\\"  33333 "2016-11-5"))
    ;; :selection 0   
    :SCROLL-IF-NOT-VISIBLE-P T
    )
   (file-list-panel-2
    capi:multi-column-list-panel
    :columns '((:title "Name")(:title "Size" :adjust :right :gap 15)(:title "Date"))
    :items '(("")("")) ;;'(("D:\\xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" 11111 "2016-11-2") ("E:\\" 2222 "2016-10-2") ( "F:\\"  33333 "2016-11-5"))
    ;; :selection 0
    
    :SCROLL-IF-NOT-VISIBLE-P T
    )
   (file-list-panel-3
    capi:multi-column-list-panel
    :columns '((:title "Name")(:title "Size" :adjust :right :gap 15)(:title "Date"))
    :items '(("")("")) ;;'(("D:\\xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" 11111 "2016-11-2") ("E:\\" 2222 "2016-10-2") ( "F:\\"  33333 "2016-11-5"))
    ;; :selection 0
    )
   (file-list-panel-4
    capi:multi-column-list-panel
    :columns '((:title "Name")(:title "Size" :adjust :right :gap 15)(:title "Date"))
    :items '(("")("")) ;;'(("D:\\xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" 11111 "2016-11-2") ("E:\\" 2222 "2016-10-2") ( "F:\\"  33333 "2016-11-5"))
    ;; :selection 0
    
    :SCROLL-IF-NOT-VISIBLE-P T
    )
   (file-list-panel-5
    capi:multi-column-list-panel
    :columns '((:title "Name")(:title "Size" :adjust :right :gap 15)(:title "Date"))
    :items '(("")("")) ;;'(("D:\\xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" 11111 "2016-11-2") ("E:\\" 2222 "2016-10-2") ( "F:\\"  33333 "2016-11-5"))
    ;; :selection 0    
    :SCROLL-IF-NOT-VISIBLE-P T
    )
   (file-list-panel-6
    capi:multi-column-list-panel
    :columns '((:title "Name")(:title "Size" :adjust :right :gap 15)(:title "Date"))
    :items '(("")("")) ;;'(("D:\\xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" 11111 "2016-11-2") ("E:\\" 2222 "2016-10-2") ( "F:\\"  33333 "2016-11-5"))
    ;; :selection 0    
    :SCROLL-IF-NOT-VISIBLE-P T
    )

   ;;OUTPUT PANE
   (output-pane-1
    capi:output-pane
    :visible-max-height 40
    )

   ;;BUTTON PANES
   (find-drives-push-button
    capi:push-button
    :text "Find PREVIOUS & NEW Drives"
    :callback-type :data-interface
    :selection-callback 'find-tomex-previous&new-drives-callback
    :max-width t
    :visible-min-height 30
    :max-height t
    )
   (save-drive-info-push-button
    capi:push-button
    :text "  SAVE CURRENT DRIVE INFO TO FILES  "
    :callback-type :data-interface
    :selection-callback 'save-drive-data-to-files-callback
    :max-width t
    :visible-min-height 30
    :max-height t
    )
   ;;END PANES
   )
  (:layouts
   (column-layout-1
    capi:column-layout
    '(editor-pane-1 :SEPARATOR path-title-pane misc-data-title-pane  :SEPARATOR editor-pane-2 dir-row-layout  file-row-layout output-pane-1 push-button-row-layout))
   (dir-row-layout
    capi:row-layout
    '(dir-list-panel-1 dir-list-panel-2 dir-list-panel-3 dir-list-panel-4 dir-list-panel-5 dir-list-panel-6)
    )
   (file-row-layout
    capi:row-layout
    '(file-list-panel-1 file-list-panel-2 file-list-panel-3 file-list-panel-4 file-list-panel-5 file-list-panel-6)
    )
   (push-button-row-layout
    capi:row-layout
    '( find-drives-push-button save-drive-info-push-button) 
    :visible-min-height 30
    )
   ;;END LAYOUTS
   )
  (:menu-bar menu-1)
  (:menus
   (menu-1
    "Menu-1"
    ("Item-1"
     "Item-2"
     "Item-3")))
  (:default-initargs
   :best-height 780
   :best-width 1360
   :layout 'column-layout-1
   :title *tomex-interface-title)
  ;;END EXPLORE-DIRS-INTERFACE
  )
;;TEST


;; (example-edit-file "capi/choice/multi-column-list-panels")

;;MAKE-EXPLORE-DIRS-INSTANCE
;;
;;ddd
(defun make-explore-dirs-instance ()
  (let
      ((inst (make-instance 'EXPLORE-DIRS-INTERFACE))
       )
   (capi:display inst)

    ;;end let,make-explore-dirs-instance
    ))
;;TEST
;; (make-explore-dirs-instance)



;;sssss
;;SCAN-TOMEX-DRIVE-POPUP-INTERFACE
;;
;;ddd
(capi:define-interface scan-tomex-drive-popup-interface ()
  ((current-selection
    :initarg :current-selection
    :accessor current-selection
    :initform NIL
    :documentation  "Current selection in item list")
   (new-drive-syms
    :initarg :new-drive-syms
    :accessor new-drive-syms
    :initform NIL
    :documentation  "new-drive-syms")
   (new-drive-symlists
    :initarg :new-drive-symlists
    :accessor new-drive-symlists
    :initform NIL
    :documentation  "new-drive-symlists=( (symlist last-leveln) ) to be scanned")
   ;;found-drive-syms
   (found-drive-syms
    :initarg :new-drive-syms
    :accessor found-drive-syms
    :initform NIL
    :documentation  "found-drive-syms")
   ;;end slots
   )

  ;;PANES
  (:panes
   (selected-previous-title-pane1
    capi:title-pane
    :title "     Selected previous drive NAME=>  "
    :title-font (gp:make-font-description :family "Times"
                                          :size 12  :weight :bold  :slant :roman)
    :visible-min-height 20
    :visible-max-width 35
    :font (gp:make-font-description :family "Times"
                                    :size 12  :weight :bold  :slant :roman)
    )
   (selected-previous-editor-pane1
    capi:rich-text-pane
    :visible-max-height 20
    :visible-max-width 120
    )
   (selected-previous-title-pane2
    capi:title-pane
    :title "     Previous drive OLD PATH=>  "
    :title-font (gp:make-font-description :family "Times"
                                          :size 12  :weight :bold  :slant :roman)
    :visible-min-height 20
    :visible-max-width 35
    :font (gp:make-font-description :family "Times"
                                    :size 12  :weight :bold  :slant :roman)
    :scroll-if-not-visible-p T
    )

   (selected-previous-editor-pane2
    capi:rich-text-pane
    :visible-max-height 20
    :visible-max-width 140
    )
  
   (scan-directions-title-pane
    capi:title-pane
    :title "1. SELECT from 1A OR 1B below--NOT BOTH:   "
    :title-font  (gp:make-font-description :family "Times"
                                           :size 12  :weight :bold  :slant :roman)
    :visible-min-height 25
    )

   (previous-drive-list-panel
    capi:multi-column-list-panel
    :columns '((:title "Name")(:title "Size"))
    :items '(("")(""))
    :title "1A. May SELECT to RE-SCAN DRIVE (Select one below if want to re-scan):"
    :selection NIL
    :selection-callback 'previous-drive-popup-callback
    :title-font (gp:make-font-description :family "Times"
                                          :size 12  :weight :bold  :slant :roman)
    :external-max-width 200
    :visible-max-width 200
    :SCROLL-IF-NOT-VISIBLE-P T
    )

   (new-drive-list-panel1
    capi:multi-column-list-panel
    :columns '((:title "Name")(:title "Size"))
    :items '(("")(""))
    :title "OR 1B1. May SELECT NEW DRIVE TO SCAN (below) INSTEAD:"
    :selection NIL
    :callback-type :data-interface
    :selection-callback 'tomex-new-drive-list-panel1-callback
    :title-font (gp:make-font-description :family "Times"
                                          :size 12  :weight :bold  :slant :roman)
    :external-max-width 70
    :visible-max-width 120
    :SCROLL-IF-NOT-VISIBLE-P T
    )
   (new-drive-list-panel2
    capi:multi-column-list-panel
    :columns '((:title "Name")(:title "Size"))
    :items '(("")(""))
    :title "OR 1B1. May SELECT NEW DRIVE TO SCAN (below) INSTEAD:"
    :selection NIL
    :title-font (gp:make-font-description :family "Times"
                                          :size 12  :weight :bold  :slant :roman)
    ;; :internal-max-width 100
    :visible-max-width 200
    :SCROLL-IF-NOT-VISIBLE-P T
    )
   (path-title-pane
    capi:title-pane
    :title "Full PATH of SELECTED NEW DRIVE=> "
    :title-font (gp:make-font-description :family "Times"
                                          :size 12  :weight :bold  :slant :roman)
    :visible-min-height 20
    :font (gp:make-font-description :family "Times"
                                    :size 12  :weight :bold  :slant :roman)
    )
   (drive-sym-text-input-pane
    capi:text-input-pane
    :title "1B2. TYPE NEW DRIVE NAME HERE:  "
    :title-font (gp:make-font-description :family "Times"
                                          :size 12  :weight :bold  :slant :roman)
    :visible-max-width 200
    )
   (drive-location-text-input-pane
    capi:text-input-pane
    :title "1B3. TYPE NEW DRIVE LOCATION INFO HERE:  "
    :title-font (gp:make-font-description :family "Times"
                                          :size 12  :weight :bold  :slant :roman)
    :visible-max-width 200
    )
   (scan-level-title-pane
    capi:title-pane
    :title "2. SELECT NESTED DRIVE LEVEL BELOW [1-6; 4= Default]:  "
    :title-font (gp:make-font-description :family "Times"
                                          :size 12  :weight :bold  :slant :roman)
    :visible-max-height 20
    :font (gp:make-font-description :family "Times"
                                    :size 12  :weight :bold  :slant :roman)
    )
   (last-leveln-input-choice
    capi:text-input-choice
    :items '("1" "2" "3" "4" "5" "6")
    :visible-min-height 20
    :visible-max-width 30
    :selection 2)   
   (message-editor-pane
    capi:rich-text-pane
    :visible-max-height 40
    :visible-max-width 700
    )
   ;;BUTTONS
   (add-to-scan-push-button
    capi:push-button
    :text " ADD to scan LIST "
    :callback 'tomex-new-drive-add-to-scan-button-callback
    :callback-type :ITEM-interface
    )
   (scan-push-button
    capi:push-button
    :text " SCAN NOW "
    :callback 'tomex-new-drive-scan-button-callback
    :callback-type :ITEM-interface
    )
   (scan-save-push-button
    capi:push-button
    :text " SCAN & SAVE-RESULTS NOW "
    :callback 'tomex-new-drive-scan-button-callback
    :callback-type :ITEM-interface
    )
   (rescan-push-button
    capi:push-button
    :text "RESCAN NOWfrom TOP window"
    :callback 'tomex-new-drive-scan-button-callback
    :callback-type :ITEM-interface
    )
   (save-push-button
    capi:push-button
    :text "  SAVE-RESULTS NOW "
    :callback 'tomex-new-drive-list-panel1-callback
    :callback-type :ITEM-interface
    )
   (close-push-button
    capi:push-button
    :text "  CLOSE WINDOW "
    :callback 'tomex-new-drive-close-callback
    :callback-type :item-interface
    )
   #|(push-button-panel-1
    capi:push-button-panel
    :items *tomex-popup-button-list ;; ( " ADD to scan LIST "  " SCAN NOW "  " SCAN & SAVE-RESULTS NOW "  "  CLOSE  ")
    ;;"Find Drives" "PreScan Drive" "Scan Drive" "Save Info" "Save Config"  "Find All Drive Info")
    :max-width t
    :max-height t
    :callback-type :data-interface
    :selection-callback 'scan-tomex-drive--button-popup-callback
    :max-height 30)|#

   ;;END :PANES
   )

  ;;LAYOUTS
  (:layouts
   (column-layout-1
    capi:column-layout
    '(previous-drive-row-layout    :SEPARATOR scan-directions-title-pane previous-drive-list-panel  new-drive-row-layout path-title-pane drive-sym-text-input-pane drive-location-text-input-pane :SEPARATOR last-leveln-input-choice  :SEPARATOR message-editor-pane   :SEPARATOR scan-button-row-layout ))
   (previous-drive-row-layout
    capi:row-layout
    '(selected-previous-title-pane1 selected-previous-editor-pane1 selected-previous-title-pane2 selected-previous-editor-pane2) )
   (new-drive-row-layout
    capi:row-layout
    '(new-drive-list-panel1 new-drive-list-panel2) )
   (scan-button-row-layout
    capi:row-layout
    '(add-to-scan-push-button scan-push-button scan-save-push-button rescan-push-button save-push-button close-push-button)
    )

   ;;end LAYOUTS
   )  ;;editor-pane-1 
  (:default-initargs
   :visible-min-height 600
   :visible-min-width 900
   :layout 'column-layout-1
   :internal-border  20
   :background :orange
   :title "SELECT SCAN DRIVE NAME & LEVEL")
  ;;END, SCAN-TOMEX-DRIVE-POPUP-INTERFACE
  )








;;MY-MULTI-LIST-PANEL-INTERFACE
;;
;;ddd
(capi:define-interface MY-MULTI-LIST-PANEL-INTERFACE ()
  ()
  (:panes
   (editor-pane-1
    capi:rich-text-pane
    :visible-max-height 40
    )
   (editor-pane-2
    capi:rich-text-pane
    :visible-max-height 40
    )
   (directories-list-panel-1
    capi:multi-column-list-panel
    :items '(("DRIVE" "One" "Two" "Three" "Four") ("DRIVE" "One" "Two" "Three" "Four") ("DRIVE" "One" "Two" "Three" "Four")("DRIVE" "One" "Two" "Three" "Four")("DRIVE" "One" "Two" "Three" "Four"))
    :selection 0
    :columns '((:title "DRIVE") (:title "1 DIRS")(:title "1 DIRS")(:title "3 DIRS")(:title "3 DIRS"))
    )
 (files-list-panel-1
    capi:multi-column-list-panel
    :items '(("FILE" "One" "Two" "Three" "Four") ("FILE" "One" "Two" "Three" "Four") ("FILE" "One" "Two" "Three" "Four")("FILE" "One" "Two" "Three" "Four")("FILE" "One" "Two" "Three" "Four"))
    :selection 0
    :columns '((:title "DRIVE" ) (:title "1 FILES" :adjust :left :gap 10)(:title "2 FILES" :adjust :left :gap 10)(:title "3 FILES" :adjust :left :gap 10)(:title "4 FILES" :adjust :right))    
    )
   (output-pane-1
    capi:output-pane
    :visible-max-height 40
    )
   (push-button-panel-1
    capi:push-button-panel
    :items '("Push-Button-Panel-1" "Button 2" "Button 3")
    :max-width t
    :max-height t
  )
  ;;END PANES
   )
  (:layouts
   (column-layout-1
    capi:column-layout
    '(editor-pane-1 editor-pane-2 directories-list-panel-1 files-list-panel-1 output-pane-1 button-row-layout ))
   (button-row-layout
    capi:row-layout
    '(   )
    :visible-min-height 30
    )
   ;;END LAYOUTS
   )
  (:menu-bar menu-1)
  (:menus
   (menu-1
    "Menu-1"
    ("Item-1"
     "Item-2"
     "Item-3")))
  (:default-initargs
   :best-height 700
   :best-width 900
   :layout 'column-layout-1
   :title "MULTI-LIST-PANEL")
  ;;end MY-MULTI-LIST-PANEL-INTERFACE
  )


;;MAKE-MY-MULTI-LIST-PANEL-INTSTANCE
;;
;;ddd
(defun make-my-multi-list-panel-intstance ()
  (let
      ((inst (make-instance 'MY-MULTI-LIST-PANEL-INTERFACE))
       )
   (capi:display inst)

    ;;end let,make-my-multi-list-panel-intstance
    ))
;;TEST
;; (make-my-multi-list-panel-intstance)





;;MY-SELECT-DIR
;;
;;ddd
(defun my-select-dir (interface message show-text-p &key set-global-var-p  if-does-not-exist  pathname 
                                    file-package-is-directory pane-args popup-args owner )
  "U-Files-Interface.lisp. (when set-global-var-p (setf *my-select-dir-result dir))"
  (let ((dir)
        )
    (setf dir (capi:prompt-for-directory message :if-does-not-exist if-does-not-exist
                                         :pathname pathname :file-package-is-directory file-package-is-directory
                                          :pane-args pane-args  :popup-args popup-args
                                          :owner owner))
   (if show-text-p (show-text (format nil "dir= ~A~%" dir) 40 t))

   (when set-global-var-p
     (setf *my-select-dir-result dir))
   ;;end let, my-select-dir
    ))
;;TEST
;;   (my-select-dir 'select-dir-interface "TEST MESSAGE" T :set-global-var-p T)
;; works= #P"C:/3-TS/LISP PROJECTS TS/ANDYCARES/A-HELP/"




;;SELECT-DIR-INTERFACE
;;ddd
(capi:define-interface select-dir-interface ()
  ((current-selection
    :initarg :current-selection
    :accessor current-selection
    :initform NIL
    :documentation  "Current selection in item list")
   (selection
    :initarg :selection
    :accessor selection
    :initform NIL
    :documentation  "Final selection in item list")
    )
  (:panes
   (rich-text-pane-1
    capi:rich-text-pane
    :default-background :white
    :text "Select the Directory or File"
    :toolbar-title "Selector"
    :accepts-focus-p t
    :visible-max-height 40
    :background :yellow
    :font (gp:make-font-description :size 12)
    :foreground :red)
   (multi-column-list-panel-1
    capi:multi-column-list-panel
    :columns '((:title "Drive, Directory, or File NAME"
                :adjust :left
                :gap 5
                :width (character 70)) 
               (:title "Drive, Directory, or File?"
                :adjust :left
                :gap 5
                :width (character 20))
               (:title "More Information"
                :adjust :left
                :gap 5
                :width (character 30)))
               :items '(()) ;; '(("c:\\temp" "Directory" ".....") ("c::\\thisfile.txt" "File" ".....") ("Item" "Three" "....."))
               :selection 0
               :accepts-focus-p nil
               :background :white
               :font (gp:make-font-description :size 12)
               :foreground :black
               ;; FIX THIS LATER?  :internal-border '( t t)
               :SCROLL-IF-NOT-VISIBLE-P t
               
               :visible-border t
      ;;sss START HERE TO RETURN A VALUE FOR SELECTED FILE-DIR
             ;;not needed  :selection-callback 'mclp-header-callback
             ;;not needed :callback-type  :element
               )
   (push-button-panel-1
    capi:push-button-panel
    :items                 ;;was '("Select ")
    (list (make-instance 'capi:push-button
                         :text "Select"
                         :callback 'select-dir-file-callback
                         :callback-type :interface))
    :max-width t
    :max-height t)
   )
  (:layouts
   (column-layout-1
    capi:column-layout
    '(rich-text-pane-1 multi-column-list-panel-1 :divider push-button-panel-1)
    )
   ;;end layouts
   )
  (:menu-bar menu-1)
  (:menus
   (drive-data
    "DRIVE DATA"
    ("Find ALL drive DATA"
     "Find all DRIVES"
     "Make new data on selected list"
     "Make all NEW drive data")
    :callback-type :selection-callback
    :callback 'tomex-drive-data-callback
    )
   (saved-lists
    "SAVED LISTS"
    ("Create NEW (watch?) list"
     "Get SAVED LISTS"
     "Select list"
     "Add selected ITEM to selected LIST"
     "Delete item from list")
        :callback-type :selection-callback
    :callback 'tomex-saved-lists-callback
    )
   (settings
    "SETTINGS"
    ("Num dir levels"
     "Default saved-list dir"
     "Default settings-db dir"
     "SAVE SETTINGS (to file)")
    :callback-type :selection-callback
    :callback 'tomex-settings-callback )
   ;;end menus
   )
  (:default-initargs
   :best-height 400
   :best-width 800
   :best-x 30
   :best-y 30
   :enabled t
   :layout 'column-layout-1
   :title "Select Directory/Folder"
   :title-font (gp:make-font-description :size 13)))




;;SELECT-DIR-FILE-CALLBACK
;;
;;ddd
(defun select-dir-file-callback (interface)
  "In U-file-interfaces, sets instance selection slot-value to '(dir dir-or-file  info), and a global variable *select-dir-file-callback-result to same."
  (let
      ((selected)
       )
    (with-slots (multi-column-list-panel-1  selection) interface
      (setf selected (capi:choice-selected-item multi-column-list-panel-1))

      (show-text (format nil "Interface= ~A~%SELECTION= ~A" interface selected) 30 t)

      (setf (slot-value interface 'selection) selected
            *select-dir-file-callback-result selected)
      ;;end with-slots
      )
    ;;end let, select-dir-file-callback
    ))





(defun set-selection-value (interface)
  
  )

#| not needed
(defun mclp-header-callback (item interface )
  (declare (ignorable interface))
  (with-slots (current-selection) interface
    (setf  (slot-value interface 'current-selection ) item)
      )
  ;;(capi:display-message "current-selection=  ~a" item)
  ;;end mclp-header-callback
  )|#
;;TEST
;;  (mclp-header-callback

;;ttt --------------------------------------------------- TEST ----------------------------------------------------------
(defun select-directory-GUI (dir-items)
  (let
      ((inst (make-instance 'select-dir-interface))
       )
    (declare
     (special *select-dir-file-callback-result
      ))
    (setf *select-dir-file-callback-result nil)

  (capi:display inst)

  (with-slots (multi-column-list-panel-1 current-selection) inst
       (capi:apply-in-pane-process inst 
                 #'(setf capi:collection-items) dir-items  multi-column-list-panel-1 )
       (capi:apply-in-pane-process inst 
                 #'(setf capi:choice-selection) NIL  multi-column-list-panel-1 )
       ;;(setf (slot-value inst 'current-selection) (car dir-items))
       )
 ;;sssss
       
 ;;end let, select-directory-GUI
 ))
;;TEST
;;  (select-directory-GUI '(("c:/temp/"  "Directory"  "na")("c:/3-TS/" "directory" "na")))






;; hhh ------------------------------------------- HELP ---------------------------------------------
;;   
#|
MULTI-COLUMN-LIST-PANEL Class
xxx
Summary A list panel with multiple columns of text.
Package capi
Superclasses list-panel
345
INITARGS
 :COLUMN-FUNCTION A function of one argument. The default is
identity.
:ITEM-PRINT-FUNCTIONS A function of one argument, or a list of such
functions.
:COLUMNS A list of column specifications.
:HEADER-ARGS A plist of keywords and values.
:AUTO-RESET-COLUMN-WIDTHS A boolean. The default is t.

DESCRIPTION 
The class multi-column-list-panel is a list panel which
displays multiple columns of text. The columns can each
have a title.
Note that this is a subclass of list-panel, and hence of
choice, and inherits the behavior of those classes.
EACH ITEM IN A MULTI-COLUMN-LIST-PANEL IS DISPLAYED IN A
LINE OF MULTIPLE OBJECTS. The corresponding objects of each
line are aligned in a column.
The column-function generates the objects for each item. It
should take an item as its single argument and return a list of
objects to be displayed. The default column-function is identity,
which works if each item is a list.

ARGUMENT DETAILS
The ITEM-PRINT-FUNCTIONS argument determines how to
calculate the text to display for each element. If item-printfunctions
is a single function, it is called on each object, and
must return a string. Otherwise item-print-functions should be
a sequence of length no less than than the number of
columns. The text to display for each object is the result
(again, a string) of calling the corresponding element of itemprint-
functions on that object.
The COLUMNS argument specifies the number of columns, and
whether the columns have titles and callbacks on these titles.
346
EACH ELEMENT OF COLUMNS IS A SPECIFICATION FOR A COLUMN. Each
COLUMN SPECIFICATION is a plist of keyword and values, where
the ALLOWED KEYWORDS are as follows:
:TITLE Specifies the title to use for the column. If
any of the columns has a title, a header
object is created which displays the titles.
The values of the :title keywords are
passed as the items of the header, unless
header-args specifies :items.
:ADJUST Specifies how to adjust the column. The
value can be one of :right, :left, or :center.
:WIDTH Specifies a fixed width of the column.
:DEFAULT-WIDTH
Specifies the default initial width of the
column. The user can resize it. If :width is
supplied it overrides :default-width.
:VISIBLE-MIN-WIDTH
Minimum width of the column.
:GAP Specifies an additional gap alongside the
text in the column. :GAP is not supported
consistently across platforms (see Notes
below).
The values of :width, :visible-min-width and :gap are
interpreted as standard geometric hints. See element for
information about these hints.
columns should indicate how many columns to display. At a
minimum the value needs to be (() ()) for two columns
without any titles header-args is a plist of initargs passed to the header which
displays the titles of the columns. The header object is a collection.
The following collection initargs are useful to
pass in header-args:
347
:SELECTION-CALLBACK The callback for clicking on the header.
:CALLBACK-TYPE Defines the arguments of the selection-callback.
:ITEMS The items of the header object. Note that
:items overrides :title if that is supplied
in columns.
:PRINT-FUNCTION
Controls how each of items is printed,
providing the title of each column.
header-args may also contain the 
keyword :ALIGNMENTS. The
value should be a list of alignment keywords, each of which
is interpreted like 
an :ADJUST value in columns. The alignment is applied to the title only.
If auto-reset-column-widths is true, then the widths of the columns
are recomputed when the items of the multi-columnlist-
panel are set.
Notes 1. Similiar and enhanced functionality is provided by listview.
2. On Microsoft WINDOWS, :WIDTH In a column specification
does not actually make the column width be fixed,
though it DOES SUPPLY THE INITIAL WIDTH.
3. On Microsoft WINDOWS, :GAP In a column specification
adds the gap on BOTH SIDES OF THE TEXT. On Motif it adds
the gap only on the right side of the text. On GTK+ and
Cocoa :gap is ignored.
Example This example uses the columns initarg:
1 CAPI Reference Entries
348
|#
#|
(capi:contain
 (make-instance
  'capi:multi-column-list-panel
  :visible-min-width 300
  :visible-min-height :text-height
  :columns '((:title "Fruits"
              :adjust :right
              :width (character 15))
             (:title "Vegetables"
              :adjust :left
              :visible-min-width (character 30)))
  :items '(("Apple" "Artichoke")
           ("Pomegranate" "Pumkpin"))))
|#
#|This example uses header-args to add callbacks and independent
alignment on the titles:|#
#|
(capi:contain
 (make-instance
  'capi:multi-column-list-panel
  :visible-min-width 300
  :visible-min-height :text-height
  :columns '((:adjust :right
              :width (character 15))
             (:adjust :left
              :visible-min-width (character 30)))
  :header-args '(:items ( "Fruits" "Vegetables")
                 :selection-callback
                 mclp-header-callback
                 :alignments (:left :right))
  :items '(("Apple" "Artichoke")
           ("Pomegranate" "Pumkpin"))))
|#
#|This example uses column-function to implement a primitive
process browser:
349|#
(defun get-process-elements (process)
  (list (mp:process-name process)
        (mp:process-whostate process)
        (mp:process-priority process)))
#|
(capi:contain
 (make-instance
  'capi:multi-column-list-panel
  :visible-min-width '(character 70)
  :visible-min-height '(character 15)
  :items (mp:list-all-processes)
  :columns '((:title "Name" :adjust :left
              :visible-min-width (character 30))
             (:title "State" :adjust :center
              :visible-min-width (character 20))
             (:title "Priority" :adjust :center
              :visible-min-width (character 12)))
  :column-function 'get-process-elements))
|#
#|See also collection
list-panel
list-view|#



#|
column-layout Class
Summary The column-layout lays its children out in a column.
Package capi
Superclasses grid-layout
Initargs :ratios The size ratios between the layout’s
children.
:adjust The horizontal adjustment for each child.
:gap The gap between each child.
:uniform-size-p
If t, each child in the column has the same
height.
1 CAPI Reference Entries
74
Accessors layout-ratios
Description The column-layout lays its children out by inheriting the
behavior from grid-layout. The description is a list of the
layout’s children, and the layout also translates the initargs
ratios, adjust, gap and uniform-size-p into the grid-layout’s
equivalent initargs y-ratios, x-adjust, y-gap and
y-uniform-size-p.
description may also contain the keywords :divider and
:separator which automatically create a divider or separator
as a child of the column-layout. The user can move a
divider, but cannot move a separator.
When specifying :ratios in a row with :divider or :separator,
you should use nil to specify that the divider or separator
is given its minimum size, as in the example below.
Compatibility
note
*layout-divider-default-size* and column-layoutdivider
are not supported in LispWorks 4.4 and later.
|#
#|
EXAMPLE 
(capi:contain (make-instance
               'capi:column-layout
               :description
               (list
                (make-instance 'capi:push-button
                               :text "Press me")
                "Title"
                (make-instance 'capi:list-panel
                               :items '(1 2 3)))))
75
(setq column (capi:contain
              (make-instance
               'capi:column-layout
               :description
               (list
                (make-instance 'capi:push-button
                               :text "Press me")
                "Title:"
                (make-instance 'capi:list-panel
                               :items '(1 2 3)))
               :adjust :center)))
(capi:apply-in-pane-process
 column #'(setf capi:layout-x-adjust) :right column)

(capi:apply-in-pane-process
 column #'(setf capi:layout-x-adjust) :left column)

(capi:apply-in-pane-process
 column #'(setf capi:layout-x-adjust) :center column)

(flet ((make-list-panel (x y)
         (make-instance
          'capi:list-panel
          :items
          (loop for i below x
                collect i)
          :selection
          (loop for i below x by y
                collect i)
          :interaction
          :multiple-selection)))

  (capi:contain
   (make-instance
    'capi:column-layout
    :description
    (list
     (make-list-panel 100 5)
     :divider
     (make-list-panel 100 10))
    :ratios '(1 nil 2))))
|#
#|
See also row-layout
1 CAPI Reference Entries
76
component-name Function
Summary Gets and sets the component-name of an ole-control-pane.
Package capi
Signature component-name pane => name
(setf component-name) name pane => name
Description The function component-name accesses the component-name
of an ole-control-pane.
When the ole-control-pane is created, it automatically
opens the component and inserts it.
If (setf component-name) is called on a pane that is
already created, any existing component is closed, and the
new component is opened and inserted. (setf componentname)
also sets the pane’s user-component to nil.
Notes component-name is implemented only in LispWorks for Windows.
Load the functionality by (require "embed").
Example See the example in
examples/com/ole/simple-container/doc-viewerpair.
lisp
|#