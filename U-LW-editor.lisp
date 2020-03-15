;;************************ U-lw-editor.lisp *************************
;;
;;

;;==>CHANGE CURSOR TO BLOCK RED, PRE-LETTER
(SETF CAPI::*PC-CURSOR-STYLES* 
      '(CAPI:*EDITOR-CURSOR-ACTIVE-STYLE*  :LEFT-BAR CAPI:*EDITOR-CURSOR-DRAG-STYLE* :LEFT-BAR CAPI:*EDITOR-CURSOR-INACTIVE-STYLE* :INVISIBLE CAPI:*ECHO-AREA-CURSOR-INACTIVE-STYLE* :INVISIBLE CAPI:*EDITOR-CURSOR-COLOR* :RED))

(setf CAPI:*EDITOR-CURSOR-ACTIVE-STYLE*  :LEFT-BAR
      CAPI:*EDITOR-CURSOR-COLOR* :RED)





;;MY-EDIT-PROJECT (see end for older version w/ diff RESULTS values)
;;2018
;;ddd
(defun my-edit-project (dirnames &key omit-files return-found-paths-p
                                 use-my-edit-files-p load-p (incl-filetypes '("lisp"))
                                 omit-filetypes )
  "U-LW-editor Opens all files in all dirs except those matching filenames in omit-files (no dir specified)   RETURNS (values all-found-files total-n-files found-dir-subdir-files all-found-paths)"
  (let
      ((n-files)
       (all-found-files)
       (all-found-paths)
       (found-dir-subdir-files)
       (total-n-files 0)
       )
    (loop
     for dir in dirnames
     do
     (let*
         ((x)
          )
       (multiple-value-bind (found-files n-files found-dirs-files found-paths)
           (my-edit-dir-files dir :omit-files omit-files :load-p load-p
                                                          :incl-filetypes  incl-filetypes 
                                                          :omit-filetypes omit-filetypes
                              :return-found-paths-p return-found-paths-p
                              :use-my-edit-files-p use-my-edit-files-p)
         (setf all-found-files (append all-found-files (list found-files))
               found-dir-subdir-files (append found-dir-subdir-files 
                                              (list found-dirs-files))
               all-found-paths (append all-found-paths (list found-paths))
               total-n-files (+ total-n-files n-files))     
         ;;end let,mvb,loop
         )))
    (values all-found-files total-n-files found-dir-subdir-files all-found-paths)
    ;;end let, my-edit-project
    ))
;;TEST
;; (my-edit-project *actr-edit-subdirs :omit-files *actr-edit-omit-files)



;;MY-EDIT-DIR-FILES
;;2018
;;
(defun my-edit-dir-files (dirname  &key  omit-files return-found-paths-p
                                         (incl-filetypes '("lisp")) omit-filetypes
                                   (omit-lw-bus-p T) use-my-edit-files-p load-p 
                                   external-format check-function)
  "U-LW-editor,  Opens buffers for each file, but not windows if   RETURNS (values found-files n-files found-dirs-files found-paths)     When omit-files, omits files that have filenames ONLY matching omit-files filenames (no dir info).  FOUND-DIRS-FILES= (dir file-list) for each dir. Note: can't use in .LISPWORKS "
  (let
      ((found-pathnames) 
       (found-paths)
       (found-dirs-files)
       (n-files)
       )
    (setf dirname (my-delete-last-spaces dirname :delete-strs '("/" "\\")
                                         :delete-chars nil))
    (multiple-value-bind  (filenamestrs file-pathnamestrs subdir-namestrs
                                        file-paths subdir-paths host-str)
        (list-directory-contents dirname  :incl-filetypes  incl-filetypes 
                                                          :omit-filetypes omit-filetypes)
      (when omit-files
        (setf filenamestrs (delete-items-from-list omit-files filenamestrs)))

      (cond
       (use-my-edit-files-p
         (multiple-value-setq ( found-files n-files found-paths )
             (my-edit-files filenamestrs :dir dirname 
                            :return-found-paths-p  return-found-paths-p)))
       ;;new: uses my-open-buffers, works better if NOT ON .LISPWORKS 
       ;;causes error of can't use until multi-processing enabled.
       (t 
        (multiple-value-setq ( found-files  found-paths n-files)
            (my-open-buffers file-pathnamestrs :load-p load-p
                             :check-function check-function 
                             :external-format external-format))
          (setf found-dirs-files  (list dirname found-files))))
      (values found-files n-files found-dirs-files found-paths n-files)
      ;;end mvbs,let, my-edit-dir-files
      )))
;;TEST
;;(my-edit-dir-files  "C:\\3-TS\\LISP PROJECTS TS\\ACT-R TS\\support\\")



;;MY-OPEN-BUFFERS
;;2019
;;ddd
(defun my-open-buffers (filenames  &key load-p open-new-windows-p 
                                   check-function (external-format :default) )
  "U-lw-editor uses editor:find-file-buffer to open buffers. RETURNS: (values new-buffers paths n-files).  Return a buffer associated with the file Pathname, reading the file into a new buffer if necessary.  The second value is T if we created a buffer, NIL otherwise.  If the file has already been read, we check to see if the file has been modified on disk since it was read, giving the user various recovery options."
  (let*
      ((paths)
       (new-buffers)
       (n-files (list-length filenames))
       )    
    (loop
     for filename in filenames
     do
     (multiple-value-bind (new-buffer path)
         (my-open-buffer filename  :load-p load-p
                         :open-new-window-p open-new-windows-p
                         :check-function check-function
                         :external-format external-format)
       (setf new-buffers (append new-buffers (list new-buffer))
             paths (append paths (list path)))
     ;;end let, loop
     ))
    (values new-buffers paths n-files)
    ))
;;TEST
;; (my-open-buffers *my-lw-init-files*)


;;MY-OPEN-BUFFER
;;2019
;;ddd
(defun my-open-buffer (filename   &key load-p  open-new-window-p
                                  check-function  (external-format :default) )
  "U-lw-editor uses editor:find-file-buffer to open buffers.RETURNS: (values new-buffer path) 
Return a buffer associated with the file Pathname, reading the file into a new buffer if necessary.  The second value is T if we created a buffer, NIL otherwise.  If the file has already been read, we check to see if the file has been modified on disk since it was read, giving the user various recovery options."
  (let*
      ((path (pathname filename))
       (new-buffer (editor:find-file-buffer path check-function external-format))
       (filename (pathname-name path))
       (type (pathname-type path))
       (buffer-name (format nil "~A.~A" filename type))
       )
     (when load-p
         (compile-file filename :load T))
     (when open-new-window-p
       (make-buffer-window buffer-name   :goto-buffer-p T
             :if-exists-open-p T  :same-window-p T :temporary-p NIL ))
    (values new-buffer path)
    ))
;;TEST
;; (my-open-buffer "C:/3-TS/LISP PROJECTS TS/MyUtilities/U-BASIC-functions.lisp")
;; works = #<EDITOR:BUFFER U-BASIC-functions.lisp>    #P"C:/3-TS/LISP PROJECTS TS/MyUtilities/U-BASIC-functions.lisp"
;; (my-open-buffer "C:/3-TS/LISP PROJECTS TS/MyUtilities/U-lists.lisp" :open-new-window-p T)
;; works=> creates new buffer from filename, then opens that buffer in a new window
;; (my-open-buffer "C:/3-TS/LISP PROJECTS TS/1-load-actr.lisp")


;;MAKE-BUFFER-WINDOW
;;2019
;;ddd
(defun make-buffer-window (buffer-name  &key  (goto-buffer-p T) 
                                        (if-exists-open-p T)
                                        (same-window-p t)
                                        temporary-p )
  "U-LW-editor makes a new buffer window.   RETURNS new-buffer 
   NOTE: Use my-open-buffer instead to open/eval existing files first.  "
  (let
      ((new-buffer (editor:make-buffer buffer-name :contents if-exists-open-p
                                       :temporary temporary-p ))
       )
  (when goto-buffer-p 
    (editor:goto-buffer new-buffer same-window-p))
    new-buffer
    ;;end let, make-buffer-window
    ))
;;TEST
;; (make-buffer-window "new-buffer-EMPTY") 
;;works=> makes a new buffer="new-buffer-EMPTY", opens it, makes primary.
;; (make-buffer-window "new-buffer-XX" :goto-buffer-p NIL)
;; works=> makes new buffer, but NOT open a window for it
;; (make-buffer-window "new-buffer-XX") 
;; works=> takes existing buffer (with contents) and opens it and makes it primary
;; (make-buffer-window  "C:/3-TS/LISP PROJECTS TS/MyUtilities/U-lists.lisp")







;;*************** OLDER, WORKS, BUT NOT AS WELL AS OPEN-BUFFER(S) *********
;;MY-EDIT-FILES
;;2018
;;ddd
(defun my-edit-files (filenames &key dir filenames=paths-p return-found-paths-p
                                (file-exts '("lisp" "txt")))
  "U-LW-editor,USE OPEN-BUFFERS INSTEAD. Makes buffers for all files in paths. If dir then sets paths to all .lisp files in dir. When filenames=paths-p, saves converting to real paths. RETURNS (values  found-files n-files found-paths)  If RETURN-FOUND-PATHS-P, returns entire fpaths of found-files--otherwise 'Can return found-paths' FILE EXT MUST BE IN FILE-EXTS (unless it is NIL). Note: can't use in .LISPWORKS "
  (let
      ((found-paths)   
       (found-files)
       (n-files (list-length filenames))
       (all-buffers)
       (all-windows)
       )
    (when dir
     (setf dir (delete-final-string '("\\" "/") dir)))

    (loop
     for file in filenames
     do
     (let
         ((path)
          (pathname)
          (cur-buffer)
          (cur-window)
          (file-ext)
          (p)
          )
       (declare (ignore p))
       (cond
        (dir
         (setf pathname (format nil "~A/~A" dir file)
               path (pathname pathname)))
        (filenames=paths-p 
         (setf path file
               pathname (namestring file)))
        (t (setf pathname file
                 path (pathname file))))
       ;;(break "path")                   
       ;;NOTE wfind opens buffer in STARTING/CALLING window--displacing contents
       ;; Each new editor:wfind-file-command brings new file into EXISTING buffer.
       ;; Therefoe with wfind- (unlike find- ) don't need to delete any buffers.(causes error)
       ;;FILE EXT MUST BE IN FILE-EXTS (unless it is NIL)
       (setf file-ext (pathname-type path))
       (when (or (null file-exts)
                  (member file-ext file-exts :test 'string-equal))
          (editor:wfind-file-command p path)
           (setf cur-buffer (editor:current-buffer)
                 all-buffers (append all-buffers (list cur-buffer))
                 found-files (append found-files (list file))
                 found-paths (append found-paths (list path)))
           ;;end when
           )
              ;;(break "file")
         ;;end let,loop
         ))
    ;;222
    (setf n-files (list-length found-files))
    (unless return-found-paths-p
      (setf found-paths "Can return found-paths"))
    (values  found-files n-files found-paths)
    ))
;; (my-edit-files '("conflict-tree.lisp" "p-star-cmd.lisp" "p-star-cmd.lisp~" )   :dir "C:/3-TS/LISP PROJECTS TS/ACT-R TS/commands")



;;DELETE-BUFFER-WINDOWS
;;2018
;;ddd
(defun delete-buffer-windows (buffers &key (omit-n 1) )
  "U-LW-editor, Deletes buffer windows in editor, but keeps buffers. Only works when function called in an editor window. Some problems--finish?"
  (let*
      ((p)
       (delete-buffers )
       (n-buffers (list-length buffers))
       (rest-buffers )
       (windows)
       )
    ;;When buffers not NIL or '(NIL)
    (when (and buffers (car buffers))
    (cond
     ((> (- n-buffers omit-n) 0)
      (setf delete-buffers (nthcdr  omit-n buffers)
            rest-buffers (butlast buffers (- n-buffers omit-n))))
     (t (setf rest-buffers (list (car buffers))
              delete-buffer (cdr buffers))))
    (loop
     for buffer in delete-buffers
     do
     (let
         ((buffer-window)
          (cur-window)
          (cur-buffer)
          (p)
          )
       (declare (ignore p))
       ;;111
       (when buffer
;;         (setf (editor:current-buffer) buffer)
            (setf  cur-buffer (editor:current-buffer))
         ;;(editor:use-buffer buffer)
        ;; (setf (editor:current-window) (editor::find-window-for-buffer cur-buffer))
         (setf (editor:current-window) (editor::find-window-for-buffer buffer))
         ;;above causes error: Setting current window for the wrong process Background execute 1 : #<EDITOR::WM-WINDOW "motor-compilation.lisp" 2320B4E3>.

     ;;SSSSS DON'T USE DELETE-BUFFERS? DELETE ONE BUFFER AT A TIME WHEN THE NEW ONE IS CREATED??

         ;;IF RUN IN LISTENER, CAUSES ERROR: 
         ;; Error: Setting current window for the wrong process CAPI Execution Listener 1 : #<EDITOR::WM-WINDOW "p-star-cmd.lisp" 271F962B>.
         (setf cur-window (editor:current-window)
               windows (append windows (list cur-window)))
         (break "cur-window=? cur-buffer")
         #|           ;; (editor:current-window) can be set with setf
         (setf buffer-window (editor::find-window-for-buffer buffer))
                  (break "buffer-window")|#
         (editor:delete-window-command p)
         ;;end when
         )
         
       #|       (when buffer
         ;;(editor::set-current-buffer buffer)
         ;;closes windows not buffers  (editor:delete-window-command p)     
         (break "cur-window")
         (setf buffer-window (editor::find-window-for-buffer buffer))
               ;;cur-window (editor::set-current-window buffer-window))
         (editor:delete-window-command p))|#
       ;;end let, loop
       ))
    (when (and rest-buffers (car rest-buffers)) ;;note (when  '(nil) 7) = 7
      (editor::set-current-buffer (car rest-buffers)))
    ;;end initial when
    )
    windows
    ;;end let, delete-buffer-windows
    ))
;;TEST
;; (delete-buffer-windows buffers)
;; WORKS within my-find-files


;;FDF
;;
;;ddd
(defun  fdf  (dspec-sym)
   "In U-LW-editor, finds a definition for dspec-sym in any buffer?"
   (editor:find-source-for-dspec-command t dspec-sym :same-window nil)
   )

;;zzz me --------------  UNDEFINE BUFFER DEFS (CNTR-8) -------------------------
;;WORKS
#|
(editor:defcommand "Undefine Buffer Defs" (p)
   "Undefines all definitions in current buffer. Make sure have right one."     
       "Undefine current buffer defs"
    (declare (ignore p))
    (editor:undefine-buffer-command  p)
    ) 
  (editor:bind-key "Undefine Buffer Defs" "Control-8" :global :pc)

;;zzz ME ------------- FIND SOURCE  (CNTR-9) (asks in echo area) ------------------
;;WORKS
(editor:defcommand "Find Source" (p)
   "Moves cursor to function definition as in Find Source in debugger."     
       "Find Source"
    (declare (ignore p))
    (editor:find-source-command p) ;; NIL nil  :same-window nil)
    ) 
  (editor:bind-key "Find Source" "Control-9" :global :pc)
 


(editor:defcommand "Find Next Mismatched PARENS" (p)
   "Moves cursor to next mismatched parens."     
       "Find Mismatched Parens"
    (declare (ignore p))
    (EDITOR::FIND-NEXT-MISMATCHED-PARENTHESIS p) ;; NIL nil  :same-window nil)
    ) 
  (editor:bind-key "Find Next Mismatched PARENS" "Control-7" :global :pc)
|#

;;SSS doesn't work, fix?
#|(defun udb ()
  "In U-LW-editor. Undefines all definitions in current buffer"
  (editor:undefine-buffer-command T )
  )|#
#|(editor:defcommand "Undefine Buffer Defs" (p)
   "Undefines all definitions in current buffer. Make sure have right one."     
       "Undefine current buffer defs"
    (declare (ignore p))
    (editor:undefine-buffer-command  p)
    ) 
  (editor:bind-key "Undefine Buffer Defs" "Control-8" :global :pc)|#


#| SSS doesn't work, fix?
(defun fds (symbol)
   "In U-LW-editor, finds a definition for a SYMBOL in any buffer?"
   (editor:find-source-command t symbol t :same-window nil)
   )|#
  

;;from Dave Fox, LW
;;There is no built-in way to open more than one file at  > a time.
;;  If you like that, you could put it in your .lispworks file.
;; zzz "Find Multiple Files" from Dave Fox, LW

;; COPIED FROM .LISPWORKS
#| 
  (editor:defcommand "Find Multiple Files" (p)
      "Prompt for multiple files, and open each like WFind File command."
       "Find multiple files"
    (declare (ignore p))
    (loop for file in (capi:prompt-for-files
                       "Select multiple files to open: "
                       :filter "*.lisp;*.lsp"
                       :filters '("Lisp Source Files" "*.lisp;*.lsp" "All files" "*.*"))
          do (editor:wfind-file-command nil file)))
 
  (editor:bind-key "Find Multiple Files" "Control-i" :global :pc)
|#
;;ZZZ FROM MARTIN SIMMONS, LW
;; FOR CLIPBOARD RING
  (editor:bind-key "Rotate Kill Ring" "Control-j" :global :pc) 


;;
;;LIST-FILE-OBJECTS
;;
;;ddd
(defun list-file-objects (file-prename object-type-list &key (pathroot "C:\\3-TS\\LISP PROJECTS TS\\")(dir "shaq")(ext ".lisp") return-object-list-string-p )
  "In U-lw-editor.lisp, creates a window and prints a list of object-type items in it, Uses READ-LINE. Use list-file-nested-lists for complex DB PList objects."
  (let*
      ((pathname (format nil "~A~A\\~A~A" pathroot dir file-prename ext))
       (file-name (format nil "~A defclass file" pathroot))
       (object-list-file-inst (make-instance 'file-list-frame))
       (eof)
       (object-list-string (format nil "In pathname: ~A~% OBJECT-LIST=> ~A~%" pathname  object-type-list))
       )
    ;;object-type loop
    (loop
     for object-type in object-type-list
     with object-string
     ;;  with object-list-string
     do
     (with-open-file (s-input pathname :direction :input) 
       (setf object-string  (format nil "~A" object-type)
             object-list-string (format nil "~A~%~%>>>>>  OBJECT-TYPE: ~A  <<<<<~%" object-list-string object-string))
       (loop
        for n from 0 to 3000
        with line
        with token
        with rest-string
        do
        (multiple-value-setq (line eof)
            (read-line s-input nil 'eof ))
        (cond
         (eof
          (return))
         (t (multiple-value-setq (token rest-string)
              (match-first-token object-string line  :delimiter-list '(#\(   #\space)))
                                                       ;;was '( "\(" " ")))
          (cond
           (token
            (setf object-list-string (format nil "~A~%~A" object-list-string line))
            )
           (t nil))
          ))
        ;;end inner loop, with-open-file (reopen on next loop)
        ))      
     ;;end outer loop
     )
    ;;display the file
    (capi:display object-list-file-inst)

    #|      (with-slots (answer-button-panel)  q-frame-inst
        ;;MAKE THE  BUTTON PANEL
      (setf answer-button-panel (make-radio-button-panel ans-instruction-text answer-array-list))
      ;;Put the BUTTON IN THE ALREADY CREATED FRAME
        (capi:apply-in-pane-process quest-rich-text-pane
                   #'(setf capi:rich-text-pane-text) question-text-formated quest-rich-text-pane |#
    ;;print to the pane
    (with-slots (text-pane-1) object-list-file-inst
      (capi:apply-in-pane-process  text-pane-1
                                   #'(setf capi:rich-text-pane-text)   object-list-string text-pane-1)  
      (setf (capi:interface-title  object-list-file-inst)
                                           (format nil ">> ~A CONTENTS"  file-prename))         
               
      ;;end let,with-open-file list-my-defclasses
      ;;end with-slots
      )
    (if return-object-list-string-p
        object-list-string)
    ;;end let, list-file-objects
    ))
;;TEST
;; (list-file-objects "SHAQ-new-scales" '("my-defclass"))


;;LIST-ALL-FILE-OBJECTS
;;
;;ddd
(defun list-all-file-objects (filename-no-ext )
  "In U-LW-editor, lists defparameter, defun, my-defclass, my-make-instance, and first-order nested-list keys"
  (list-file-objects filename-no-ext '( defparameter my-defclass my-make-instance defun))
 )
;;TEST 
;;  (list-all-file-objects "SHAQ-new-scales")


;;LIST-MY-DEFCLASSES
;;
;;ddd
(defun list-my-defclasses (filename-no-ext)
  "In U-LW-editor, lists my-defclass and my-make-instance objects"
  (filename-no-ext (list "my-defclass" "my-make-instance"))
 )

;;LIST-SCALECLASSES
;;
;;ddd
(defun list-scaleclasses ()
  "In U-LW-editor"
   (list-ALL-file-objects "SHAQ-new-scales") ;; (list "my-defclass"))
       )
;; (list-scaleclasses)


;;LIST-NESTED-OBJECTS  (lists ALL DEFPARAMETER OBJECTS)
;;
;;ddd
(defun list-file-nested-lists (file-prename  &key object-sym  (pathroot "C:\\TOM\\LISP PROJECTS TS\\")(dir "shaq")(ext ".lisp") return-object-list-string-p add-defpar-newlines-p )
  "In U-lw-editor.lisp, creates a window and prints a list of DEFPARMETER items in it. Can list nested-list items horizontally or vertically using ADD-DEFPAR-NEWLINES-P. RETURNS the list. if return-object-list-string-p. If OBJECT-SYM, only finds keys for it. Uses READ."
  (let*
      ((pathname (format nil "~A~A\\~A~A" pathroot dir file-prename ext))
       (file-name (format nil "~A defclass file" pathroot))
       (object-list-file-inst (make-instance 'file-list-frame))
       (eof)
       (object-list-string (format nil "In pathname: ~A~% OBJECT-LIST=> ~A~%" pathname  "LISP OBJECTS--ESP   DEFPARAMETER NESTED-LISTS"))
       )
    ;;object-type loop
#|    (loop
     for object-type in object-type-list
     with object-string
     ;;  with object-list-string
     do|#
     (with-open-file (s-input pathname :direction :input) 
       (setf  object-list-string (format nil "~A~%~%>>>>>  ALL LISP OBJECTS  <<<<<~%" object-list-string ))
       (loop
        for n from 0 to 3000
        with object
        with length-object
        with object-1st 
        with object-2nd 
        with defpar-list
        with defpar-list-string
        do
#|        (multiple-value-setq (object eof)
            (read s-input  nil  'eof ))|#
        (setf object (read s-input  nil  'eof )) ;;returns 'eof if reaches end
        (if (listp object )(setf  length-object (list-length object)))
        (setf out1 (format nil "object= ~A~%" object ))
        (cond
        ((equal object 'eof)
          (return))
         ((and (listp object)(> length-object 2)  (listp  (third object))
               (equal (car object) 'defparameter)
               (if object-sym (equal object-sym (second object)) T)   )
          (setf object-list-string (format nil "~A
 -----------------------------------------------------------------------------" object-list-string))
            (setf  defpar-list (third object))
           ;; (setf out2 (format nil "~A~%~%~A" out2 defpar-list))
            (cond
             ((> (list-length defpar-list) 1)
              (setf  defpar-list-string
                     (find-keys-in-nested-lists defpar-list
                                                :sublist-newline-p add-defpar-newlines-p)))
             (t (setf defpar-list-string (format nil "~A" defpar-list))))  
            (setf object-list-string 
                  (format nil "~A~%~A" object-list-string defpar-list-string))
            ;;end and clause
            )
           ((listp object)
            (setf object-1st (first object))
            (if (> (list-length object) 1)
                (setf object-2nd (second object))
              (setf object-2nd ""))
            (setf  object-list-string (format nil "~A~%  (~A    ~A)" object-list-string object-1st object-2nd))
            ;;end listp clause
            )
           (t   (setf  object-list-string (format nil "~A~%  ~A " object-list-string object))))
        ;;end loop, with-open-file
        ))
    ;;display the frame
    (capi:display object-list-file-inst)

    #|      (with-slots (answer-button-panel)  q-frame-inst
        ;;MAKE THE  BUTTON PANEL
      (setf answer-button-panel (make-radio-button-panel ans-instruction-text answer-array-list))
      ;;Put the BUTTON IN THE ALREADY CREATED FRAME
        (capi:apply-in-pane-process quest-rich-text-pane
                   #'(setf capi:rich-text-pane-text) question-text-formated quest-rich-text-pane |#
    ;;print to the frame
    (with-slots (text-pane-1) object-list-file-inst
      (capi:apply-in-pane-process  text-pane-1
                                   #'(setf capi:rich-text-pane-text)   object-list-string text-pane-1)      (setf (capi:interface-title object-list-file-inst) 
                                       (format nil ">> ~A CONTENTS" file-prename))
                                                                                                                  

               
      ;;end let,with-open-file list-my-defclasses
      ;;end with-slots
      )
    (if return-object-list-string-p
        object-list-string)
    ;;end let, list-file-objects
    ))
;;TEST
;;  (list-file-nested-lists "shaq-new-scales" :add-defpar-newlines-p nil )
;;  WORKS  EG ( MY-DEFCLASS  ST2SOCINTIMNOFAMSCALE
;;  (list-file-nested-lists "SHAQ-all-scale-and-question-var-lists" :add-defpar-newlines-p nil )


           
;;FILE-LIST-FRAME
;;
;;ddd
(capi:define-interface file-list-frame ()
  ()
  (:panes
#|   (output-pane-1
    capi:output-pane)
   (collector-pane-1
    capi:collector-pane)|#
   (text-pane-1
    capi:rich-text-pane
   :visible-min-height 700
   :visible-min-width 350
   :enabled T
    )
   (radio-button-panel-1
    capi:radio-button-panel
    :items '("Radio-Button-Panel-1" "Button 2" "Button 3")
    :max-width t
    :max-height t))
  (:layouts
   (column-layout-1
    capi:column-layout
    '(text-pane-1 ;;output-pane-1 collector-pane-1  
                  radio-button-panel-1)))
  (:menu-bar menu-1)
  (:menus
   (menu-2
    "Menu-2"
    ())
   (menu-1
    "Menu-1"
    (menu-2
     "Item-1"
     "Item-2"
     "Item-3")))
  (:default-initargs
       :x 10
    :y 60
   :best-height nil
   :best-width nil
   :layout 'column-layout-1
   :title "File List Frame"))





;;GET-EDITOR-BUFFER-INFO
;;2019
;;ddd
(defun get-editor-buffer-info (buffer &key return-details-p)                       
  "U-LW-editor,  RETURNS (values  buffer-filename buffer-pathname modified-p).. INPUT buffer can be a buffer name string or a buffer object. If , returns. GET-DETAILS-P, RETURNS (values  buffer-filename buffer-pathname 
                      modified-p filename filetype buffer-path buffer modified-tick).
   INPUT: buffer object OR buffer-name  Note: BUFFER-NAME = buffer-filename."
     (let*
         ((buffer-path )
          (buffer-pathname) ;;(namestring buffer-path))
          (buffer-filename) ;; (pathname-name buffer-path))
          (modified-tick)
          (modified-p)
          )    
       (cond
        ((stringp buffer)
         (setf buffer (editor:get-buffer buffer)))
        (t nil))
       
       (when buffer
         (setf buffer-path (editor:buffer-pathname buffer)))
       (when buffer-path
         (setf buffer-pathname (namestring buffer-path)
               buffer-filename (file-namestring buffer-path)
               filename (pathname-name buffer-path)
               filetype (pathname-type buffer-path)
               modified-tick (editor::buffer-modified-tick  buffer)
               modified-p (> modified-tick 1))
         ;;(break "inside get info")
         (cond
          ((null return-details-p)
           (values  buffer-filename buffer-pathname modified-p))
          (t (values  buffer-filename buffer-pathname 
                      modified-p filename filetype  buffer-path buffer modified-tick)))
         ;;end when
         )
    ;;end let, get-editor-buffer-info
  ))
;;TEST
;; (get-editor-buffer-info "U-files.lisp")
;; works = "U-Files.lisp"   "C:\\3-TS\\LISP PROJECTS TS\\MyUtilities\\U-Files.lisp"  NIL
;;;; (get-editor-buffer-info "U-lists.lisp" :return-details-p T)
;; works = p T)   "U-lists.lisp"  "C:\\3-TS\\LISP PROJECTS TS\\MyUtilities\\U-lists.lisp"  NIL  "U-lists"  "lisp"  #P"C:/3-TS/LISP PROJECTS TS/MyUtilities/U-lists.lisp"  #<EDITOR:BUFFER U-lists.lisp>  1


;;TEST ON BUFFER OBJECT--NOT STRING
;;;; (progn (multiple-value-bind ( buffer-filename buffer-pathname modified-p filename filetype  buffer-path buffer modified-tick) (get-editor-buffer-info "U-lists.lisp" :return-details-p T) (get-editor-buffer-info buffer :return-details-p T)))
;; WORKS= "U-lists.lisp"  "C:\\3-TS\\LISP PROJECTS TS\\MyUtilities\\U-lists.lisp"   T   "U-lists"  "lisp"  #P"C:/3-TS/LISP PROJECTS TS/MyUtilities/U-lists.lisp"  #<EDITOR:BUFFER U-lists.lisp>  1001


;;SAVED vs UNSAVED; MODIFIED VS UNMODIFIED
;; NOT MODIFIED U-files = 1
;; MODIFIED SAVED & NOT-SAVED
;; ;; (get-editor-buffer-info "U-files-backup.lisp") 
;; WHEN NOT SAVED =  4521, WHEN SAVED = 4521
;; (get-editor-buffer-info "U-LW-editor.lisp")
;; MODIFIED, SAVED OR NOT = 1800




;;BUFFER-MODIFIED-P
;;2019
;;ddd
(defun buffer-modified-p (buffer)
  "U-LW-editor RETURNS (values modified-p modified-tick buffer-name buffer) INPUT: filename or buffer"
  (let*
      ((buffer-name (cond ((stringp buffer)
                      (setf buffer-name buffer
                            buffer (editor:get-buffer buffer))
                      buffer-name)   (t buffer)))
       (modified-tick   (editor::buffer-modified-tick  buffer))
       (modified-p (> modified-tick 1))
       )
   (values modified-p modified-tick buffer-name buffer)
   ;;end let, buffer-modified-p
   ))
;;TEST
;; (buffer-modified-p "U-System.lisp")
;; result = T   1544   "U-System.lisp"  #<EDITOR:BUFFER U-System.lisp>
;; (buffer-modified-p "U-Arrays.lisp")
;; works = NIL  1  "U-Arrays.lisp"   #<EDITOR:BUFFER U-Arrays.lisp>



;;GET-EDITOR-BUFFERS-INFO
;;2019
;;ddd
(defun get-editor-buffers-info (&key return-details-p
                                     (return-modified-buffer-names-p T))
  "U-LW-editor,  RETURNS (values  buffer-filenames buffer-pathnames n-buffers modified-buffer-names buffer-paths buffers) if RETURN-DETAILS-P   for ALL BUFFERS IN EDITOR. Otherwise, returns (values  buffer-filenames buffer-pathnames n-buffers modified-buffer-names). If return-modified-buffer-names-p, return-modified-buffer-names is list."
  (let*
      ((buffers editor:*buffer-list*)
      ;;doesn't give buffer names (buffer-names editor:*buffer-names*)
      (buffer-paths)
      (buffer-pathnames)
      (buffer-filenames)
      (n-buffers)
      (modified-buffer-names)
       )
    (loop
     for buffer in buffers
     do     
     (multiple-value-bind (buffer-filename buffer-pathname modified-p
                                           buffer-path buffer)
         (get-editor-buffer-info buffer :return-details-p return-details-p)
       (when buffer-filename
         (setf buffer-pathnames (append buffer-pathnames (list buffer-pathname))
               buffer-filenames (append buffer-filenames (list buffer-filename))
               buffer-paths (append buffer-paths (list buffer-path))  
               buffers (append buffers (list buffer)))
         (when (and modified-p return-modified-buffer-names-p)
           (setf modified-buffer-names (append modified-buffer-names 
                                               (list buffer-filename))))
         ;;end mvb,when,loop
         )))
    (setf n-buffers (list-length buffer-filenames))
    (cond
     ((null return-details-p)
    (values  buffer-filenames buffer-pathnames n-buffers modified-buffer-names))
     (t (values  buffer-filenames buffer-pathnames n-buffers modified-buffer-names buffer-paths buffers)))
    ;;end let, get-editor-buffers-info
  ))
;;TEST
;; (get-editor-buffers-info :return-details-p T)
;; works= ("U-LW-editor.lisp" "U-files-backup.lisp" "1-CODE-TEMPLATES.lisp" "U-Files.lisp" "H-Editor.lisp" "U-System.lisp" "U-capi-input-interfaces.lisp" "0 MY-LW-INIT.lisp" "1-KEY BINDS ETC.lisp" "1-LW 7-1 NOTES.lisp" "U-fonts.lisp" "U-symbol-info.lisp" "U-function-plotter.lisp" "U-Arrays.lisp" "U-clos.lisp" "U-IDE-tools.lisp" "U-lists.lisp" "U-debug.lisp" "U-sequences.lisp" "U-tstring.lisp" "U-BASIC-functions.lisp")
#|  ("C:\\3-TS\\LISP PROJECTS TS\\MyUtilities\\U-files-backup.lisp" "C:\\3-TS\\LISP PROJECTS TS\\MyUtilities\\U-LW-editor.lisp" "C:\\3-TS\\LISP PROJECTS TS\\H-HELP\\H-Editor.lisp" "C:\\3-TS\\LISP PROJECTS TS\\MyUtilities\\U-System.lisp"....ETC...... "C:\\3-TS\\LISP PROJECTS TS\\MyUtilities\\U-Files.lisp" "C:\\3-TS\\LISP PROJECTS TS\\MyUtilities\\U-BASIC-functions.lisp")
n-buffers= 21
modified-buffer-names= ("U-LW-editor.lisp" "U-files-backup.lisp" "U-System.lisp" "U-capi-input-interfaces.lisp")
(#P"C:/3-TS/LISP PROJECTS TS/MyUtilities/U-LW-editor.lisp" #P"C:/3-TS/LISP PROJECTS TS/MyUtilities/U-files-backup.lisp" #P"C:/3-TS/LISP PROJECTS TS/1-CODE-TEMPLATES.lisp" #P"C:/3-TS/LISP PROJECTS TS/MyUtilities/U-Files.lisp" ... ETC ...   #P"C:/3-TS/LISP PROJECTS TS/MyUtilities/U-BASIC-functions.lisp")
(#<EDITOR:BUFFER U-LW-editor.lisp> #<EDITOR:BUFFER CAPI interactive-pane 26> #<EDITOR:BUFFER CAPI editor-pane 59> #<EDITOR:BUFFER CAPI editor-pane 17> #<EDITOR:BUFFER U-files-backup.lisp> #<EDITOR:BUFFER CAPI editor-pane 58> #<EDITOR:BUFFER CAPI interactive-pane 25> #<EDITOR:BUFFER CAPI interactive-pane 24> #<EDITOR:BUFFER CAPI interactive-pane 23> #<EDITOR:BUFFER CAPI editor-pane 53> #<EDITOR:BUFFER CAPI interactive-pane 21>  .... ETC ...  #<EDITOR:BUFFER CAPI editor-pane 11> #<EDITOR:BUFFER CAPI editor-pane 10> #<EDITOR:BUFFER CAPI editor-pane 9> #<EDITOR:BUFFER CAPI editor-pane #<EDITOR:BUFFER U-function-plotter.lisp> #<EDITOR:BUFFER U-Arrays.lisp> #<EDITOR:BUFFER U-clos.lisp> #<EDITOR:BUFFER U-IDE-tools.lisp> #<EDITOR:BUFFER U-lists.lisp> #<EDITOR:BUFFER U-debug.lisp> #<EDITOR:BUFFER U-sequences.lisp> #<EDITOR:BUFFER U-tstring.lisp> #<EDITOR:BUFFER U-BASIC-functions.lisp>) |#


;;********************************OLD FROM U-editor.lisp **********************
;;
;; Utilities to change editor behavior etc


;;***>>> MOST FUNCTIONS ARE IN U-LW-editor.lisp


(SETF CAPI::*PC-CURSOR-STYLES* 
      '(CAPI:*EDITOR-CURSOR-ACTIVE-STYLE*  :LEFT-BAR CAPI:*EDITOR-CURSOR-DRAG-STYLE* :LEFT-BAR CAPI:*EDITOR-CURSOR-INACTIVE-STYLE* :INVISIBLE CAPI:*ECHO-AREA-CURSOR-INACTIVE-STYLE* :INVISIBLE CAPI:*EDITOR-CURSOR-COLOR* :RED))

(setf CAPI:*EDITOR-CURSOR-ACTIVE-STYLE*  :LEFT-BAR
      CAPI:*EDITOR-CURSOR-COLOR* :RED)

#|
6.3 PROGRAMMING THE EDITOR   xxx
The editor functions described in this section can be combined and provided with arguments to CREATE NEW COMMANDS.

Existing editor commands can also be used in the creation of new commands.

xxx EVERY EDITOR COMMAND documented in this manual is NAMED BY A STRING COMMAND which CAN BE USED TO INVOKE THE COMMAND INTERACTIVELY, 
but there is also associated with this 
A STANDARD LISP FUNCTION (the "command function")
 named by a SYMBOL EXPORTED FROM THE EDITOR PACKAGE. 
You can use this symbol to call the command programmatically. 
For example, the editor command "FORWARD CHARACTER"
              is referred to by EDITOR:FORWARD-CHARACTER-COMMAND.

The FIRST ARGUMENT OF ANY COMMAND FUNCTION IS THE PREFIX ARGUMENT P,
 and this must therefore be included in any programmatic call, even if the prefix argument is ignored. Some commands have additional optional arguments. For
example to insert 42 #\! characters, you would call
(editor:self-insert-command 42 #\!)
Details of these optional arguments are provided in the command descriptions
throughout this manual.
See editor:defcommand for the details of how to create new commands.
Note: code which modifies the contents of a capi:editor-pane (for example a
displayed editor buffer) must be run only in the interface process of that pane.
The following sections describe editor functions that are not interactive editor
commands.

6.3.1 CALLING EDITOR FUNCTIONS  xxx
All editor commands and some other editor functions expect to be called within
a dynamic context that includes settings for the current buffer and current window. This happens automatically when using the editor interactively.

You can set up the context IN A CAPI APPLICATION by using the function
CAPI:CALL-EDITOR (see the CAPI User Guide and Reference Manual).

You can also use the following function TO CALL EDITOR COMMANDS AND FUNCTIONS.

EDITOR:PROCESS-CHARACTER FUNCTION  xxx

EDITOR:PROCESS-CHARACTER CHAR WINDOW
Processes char in a dynamic context where the current window is windowand the current buffer is the buffer currently displayed in window.6 Advanced Features
214
The CHAR can be one of the following:
 A STRING, NAMING AN EDITOR COMMAND to invoke.
 A LIST OF THE FORM (FUNCTION . ARGS), which causes function to be called with args. The ITEMS IN ARGS ARE NOT EVALUATED.

 A FUNCTION OR SYMBOL, which is CALLED WITH NIL AS ITS ARGUMENT (LIKE A
COMMAND FUNCTION WOULD BE IF THERE IS NO PREFIX ARGUMENT).

 A CHARACTER OR SYSTEM:GESTURE-SPEC OBJECT, which is treated as if it has been TYPED ON THE KEYBOARD.

THERE IS NO RETURN VALUE. The processing may happen in another thread, so may not have competed before this function returns.

6.3.2 DEFINING COMMANDS xxx

EDITOR:DEFCOMMAND MACRO

DEFCOMMAND NAME LAMBDA-LIST COMMAND-DOC FUNCTION-DOC &BODY FORMS => command-function
Defines a new editor command. name is a usually string naming the new
editor command which can invoked in the editor via Extended Command,
and command-function is a symbol naming the new command function
which can be called programmatically. The command-function symbol is
interned in the current package.
lambda-list is the lambda list of the new command, which must have at
least one argument which is usually denoted p, the prefix argument.
command-doc and function-doc should be strings giving detailed and brief
descriptions of the new command respectively.
forms is the Lisp code for the command.
The name of the command must be a string, while the name of the associated command function must be a symbol. There are two ways in which
name can be supplied. Most simply, name is given as a string, and the string
is taken to be the name of the editor command. The symbol naming the
command function is computed from that string: spaces are replaced with
hyphens and alphabetic characters are uppercased, but otherwise the sym6.3 Programmin

6.3.3.2 BUFFER OPERATIONS
editor:*buffer-list* Variable
Contains a list of all the buffers in the editor.

editor:current-buffer Function
editor:current-buffer
Returns the current buffer.
editor:buffer-name Function
editor:buffer-name buffer
Returns the name of buffer.
editor:window-buffer Function
editor:window-buffer window
Returns the buffer currently associated with window.
editor:buffers-start Function
editor:buffers-start buffer
Returns the starting point of buffer.
editor:buffers-end Function
editor:buffers-end buffer6.3 Programming the editor
221
Returns the end point of buffer.
editor:buffer-pointeditor:buffer-point buffer
Returns the current point in buffer.
editor:use-buffer Macro
editor:use-buffer buffer &body forms
Makes buffer the current buffer during the evaluation of forms.
editor:buffer-from-name Function
editor:buffer-from-name name
Returns the buffer called name (which should be a string). If there is no
buffer with that name, nil is returned.
editor:make-buffer Function
make-buffer name &key modes contents temporary base-name name-pattern
Creates or returns an existing buffer.
name should be a string or nil.
modes should be a list of strings naming modes. The first mode must be a
major mode, and the rest minor modes. The default value of modes is the
value of default-modes.
base-name should be a string or nil. If name and temporary are both nil
then ba

editor:make-buffer Function
make-buffer name &key modes contents temporary base-name name-pattern
Creates or returns an existing buffer.
name should be a string or nil.
modes should be a list of strings naming modes. The first mode must be a
major mode, and the rest minor modes. The default value of modes is the
value of default-modes.
base-name should be a string or nil. If name and temporary are both nil
then base-name must be a string.
contents should be a string, nil or t (default value nil).
temporary is a boolean (default value nil).
name-pattern should be a string (default value "~a<~a>").
When name is non-nil, it is the name of the buffer. If there is already a
buffer with this name which is not temporary and the temporary argument6 Advanced Features
222
is nil, make-buffer returns that buffer. Before doing so, it sets its contents
to contents unless contents is t. When contents is nil, the buffer is made
empty.
If name is nil or temporary is non-nil or a buffer with the name cannot be
found, then a new buffer is made and returned. The buffer's contents is set
to contents if contents is a string, and otherwise the buffer is made empty.
The name of the buffer is set to name if name is non-nil.
|#
#|
porary is a boolean (default value nil).
name-pattern should be a string (default value "~a<~a>").
When name is non-nil, it is the name of the buffer. If there is already a
buffer with this name which is not temporary and the temporary argument6 Advanced Features
222
is nil, make-buffer returns that buffer. Before doing so, it sets its contents
to contents unless contents is t. When contents is nil, the buffer is made
empty.
If name is nil or temporary is non-nil or a buffer with the name cannot be
found, then a new buffer is made and returned. The buffer's contents is set
to contents if contents is a string, and otherwise the buffer is made empty.
The name of the buffer is set to name if name is non-nil.
If temporary is nil, the buffer is added to the internal tables of the editor. If
name is non-nil, it is used. Otherwise make-buffer tries to use base-name. If
there is already a buffer with this name, it constructs another name by
(format nil name-pattern base-name n)
with different integers n until it constructs an unused name, which it uses
as the buffers name.
If temporary is non-nil, the buffer is not added to the internal tables. It is
also marked as temporary, which mainly means that it does not have autosave and backup files, and avoids calling general hooks when it is modified.
Notes:
Using :temporary t gives you a buffer that is 'yours', that is the editor
does not do anything with it except in response to explicit calls from your
code. Except when actually editing files, this is the most useful way of
using buffers in most cases.
capi:editor-pane with the :buffer :temp initarg uses
(make-buffer ... :temporary t)
editor:goto-buffer Function
editor:goto-buffer buffer in-same-window
Makes buffer the current buffer. If buffer is currently being shown in a window then the cursor is moved there. If buffer is not currently in a window
and in-same-window is non-nil then it is shown in the current window, otherwise a new window is created for it|#



#|Go Back Editor Command
Arguments: None
Key sequence: Ctrl+X C
Takes you back to the most recently recorded location. If a prefix argument
count is supplied, it takes you back count locations in the location history. If
count is negative, it takes you forward again count locations in the history,
provided that no more locations have been recorded since you last went
back.
Select Go Back Editor Command
Arguments: None
Key sequence: Ctrl+X M
Takes you back to a previously recorded location, which you select from a
list.
Any prefix argument is ignored.
Go Forward Editor Command
Arguments: None
Key sequence: Ctrl+X P
Takes you back to the next location in the ring of recorded locations. If a
prefix argument count is supplied, it takes you forward count locations in
the location history. If count is negative, it takes you back count locations in
the history
|#

#|230
6.3.8 Files
editor:find-file-buffer Function
editor:find-file-buffer pathname &optional check-function
Returns a buffer associated with the file pathname, reading the file into a
new buffer if necessary. The second value returned is T if a new buffer is
created, and nil otherwise. If the file already exists in a buffer, its consistency is first checked by means of check-function. If no value is supplied for
check-function, editor:check-disk-version-consistent is used.
editor:fast-save-all-buffers Function
editor:fast-save-all-buffers &optional ask
Saves all modified buffers which are associated with a file. If ask is non-nil
then confirmation is asked for before saving each buffer. If ask is not set, all
buffers are saved without further prompting.
Unlike the editor command Save All Files this function can be run
without any window interaction. It is thus suitable for use in code which
does not intend to allow the user to leave any buffers unsaved, and from
the console if it is necessary to save buffers without re-entering the full
window system.
editor:check-disk-version-consistent Function
editor:check-disk-version-consistent pathname buffer
Checks that the date of the file pathname is not more recent than the last
time buffer was saved. If pathname is more recent, the user is prompted on
how to proceed. Returns t if there is no need to read the file from disk and
nil if it should be read from disk.
editor:buffer-pathname Function
editor:buffer-pathname buffer6.3 Programming the editor
231
Returns the pathname of the file associated with buffer. If no file is associated with buffer, nil is returned
|#
#|
6.4 EDITOR SOURCE CODE -- page 197 eduser-manual
The section does not apply to LispWorks Personal Edition.
LispWorks comes with source code for the editor, which you can refer to when
adding editor extensions.
6.4.1 Contents
The directory lib/6-1-0-0/src/editor/ contains most of the source files of the LispWorks editor. Some low-level source code is not distributed.
6.4.2 Source location
To enable location of editor definitions by Find Source and related commands, configure LispWorks as described under "Finding source code" in the LispWorks User Guide and Reference Manual.
6.4.3 Guidelines for use of the editor source code
Some care is needed when working with the supplied editor source code, to ensure that you do not compromise the IDE or introduce a dependancy on a particular release of LispWorks.
In particular please note:
 The editor source code may not match the compiled code in the Lisp-
Works image exactly, for example if editor patches have been loaded.
 Modifications to the EDITOR package definition are not allowed.
6 Advanced Features
198
 Redefining existing definitions is not recommended. It is better to define a new command to do what you want. If you find a bug or have a useful extension to an existing definition then please let us know.
 Do not rely on the expansion of exported macros.
 If you use any internal (that is, not exported) EDITOR symbols, please tell
us, so we can consider how to support your requirements. In addition,
some internal macros have been removed from the LispWorks image and
these should not be used.
|#

#|
CAPI::*PC-CURSOR-STYLES* = 
(CAPI:*EDITOR-CURSOR-ACTIVE-STYLE* :CARET CAPI:*EDITOR-CURSOR-DRAG-STYLE* :CARET CAPI:*EDITOR-CURSOR-INACTIVE-STYLE* :INVISIBLE CAPI:*ECHO-AREA-CURSOR-INACTIVE-STYLE* :INVISIBLE CAPI:*EDITOR-CURSOR-COLOR* NIL)

CAPI::*EMACS-CURSOR-STYLES* =
(CAPI:*EDITOR-CURSOR-ACTIVE-STYLE* :INVERSE CAPI:*EDITOR-CURSOR-DRAG-STYLE* :LEFT-BAR CAPI:*EDITOR-CURSOR-INACTIVE-STYLE* :OUTLINE CAPI:*ECHO-AREA-CURSOR-INACTIVE-STYLE* :INVISIBLE CAPI:*EDITOR-CURSOR-COLOR* NIL)
|#


