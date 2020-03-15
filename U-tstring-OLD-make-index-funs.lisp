;;**************************** U-tstring-OLD-make-index-funs.lisp *********************************
;;revised for LW 2013

;;******************* TSTRING UTILITIES FOR GWLISP *****************
;;
;;
;;SEE FILE \GT-UTIL\T-STRING.LSP FOR MORE TRANS-LISP RELATED FUNCTIONS
;;
;;includes append-list
;;
;;(load "c:In U-tstring.lisp .lsp")

;;(in-package 'gw) 
(in-package "CL-USER")


;;ddd
(defun my-equal (item1 item2)
  "In U-tstring.lisp, items may be any combo of string, number, or symbol types.  Tests to see if are case-insensitive equal. DOES NOT TEST LISTS."
  (let
      ((item1-str)
       (item2-str)
       )
    (cond 
     ;;if either is a list, return nil, doesn't test lists
     ((or (listp item1)(listp item2))
      NIL)
     (t
      (cond
       ((or (numberp item1)(characterp item1))
        (setf item1-str (format nil "~A" item1)))
       (t (setf item1-str (string item1))))
      (cond
       ((or (numberp item2)(characterp item2))
        (setf item2-str (format nil "~A" item2)))
       (t (setf item2-str (string item2))))

      ;; (afout 'out (format nil "~S = ~S" item1-str  item2-str ))
      (string-equal item1-str item2-str)
      ))
    ))
;;test
;; (my-equal 'this "this") = T
;; (my-equal 3 "3") = T
;; (my-equal 3 "4") = NIL
;; (my-equal #\c "c") = T
;; (my-equal #\c "C") = T
;; (my-equal #\c "d") = NIL
;; (characterp #\c)   = T
;; (characterp  "c")  = NIL   
;; (my-equal 3 "this")   = NIL
;; (my-equal '(this list) "this")



;;MY-MAKE-SYMBOL WORKS EFFICIENTLY
;;
;;ddd
(defun my-make-symbol (string)
   "In MyUtilities\\U-tstring.lisp, If string is string, converts to a symbol, if string is really a symbol, just returns it. ALSO
 converts a string of a list to a real list of symbols."
   (let
      ((new-symbol)
       )
  (cond
   ((stringp string)
      (setf new-symbol (read (make-string-input-stream string) NIL 'EOF-FOUND)))
   (t (setf new-symbol string)))
    new-symbol))

;;works
#|
(defun testmms ()
  "successfully sets this1 to 77, returns 77, x is a memory location?"
  (let
      ((x)
       )
   (setf x  (my-make-symbol "this1"))
   (set x 77)))
|#

#|was (defun my-make-symbol (string)
  (list "gwt-util\\tstring")
  (if (stringp string)
      (setf **new-symbol** (read (make-string-input-stream string) NIL 'EOF-FOUND))
    string))|#

;;MY-MAKE-LIST-OF-STRINGS
;;
;;ddd
(defun my-make-list-of-strings (objects &key list-each-item-p)
  "In U-tstring, INPUTS a symbol or object or list of symbols/objects.  RETURNS a list of strings of those symbols.  If list-each-item-p, puts each new string inside a separate list."
  (let
      ((object-str)
       (single-object-list)
       (new-list)
       )
    (cond
     ((listp objects)
      (loop
       for object in objects
       do
       (cond
         (list-each-item-p
          (setf new-list (append new-list (list (list (format nil "~A" object))))))
         (t (setf new-list (append new-list (list (format nil "~A" object))))))
       ;;end loop, listp
      ))
     (t (cond
         (list-each-item-p
          (setf new-list (list (list (format nil "~A" objects)))))
         (t (setf new-list (list (format nil "~A" objects)))))))

    ;;end let, my-make-list-of-strings
    new-list
    ))
;;TEST
;;  (my-make-list-of-strings 'this) = ("THIS")
;;  (my-make-list-of-strings "this is a test") = ("this is a test")
;;  (my-make-list-of-strings 'this :list-each-item-p t) = (("THIS"))
;;  (my-make-list-of-strings '(this is a test)) = ("THIS" "IS" "A" "TEST")
;;  (my-make-list-of-strings '(this is a test)  :list-each-item-p t) = (("THIS") ("IS") ("A") ("TEST"))
;;  (get-PC-inputs  'testpc) = ((IN1 0.2 PC1 FB) (IN2 0.7 FB) (IN3 0.5 SENS1) (IN4 0.1 SENS2))  
;; (my-make-list-of-strings '((IN1 0.2 PC1 FB) (IN2 0.7 FB) (IN3 0.5 SENS1) (IN4 0.1 SENS2)))
;;  works= ("(IN1 0.2 PC1 FB)" "(IN2 0.7 FB)" "(IN3 0.5 SENS1)" "(IN4 0.1 SENS2)")
;; (my-make-list-of-strings '((IN1 0.2 PC1 FB) (IN2 0.7 FB) (IN3 0.5 SENS1) (IN4 0.1 SENS2))   :list-each-item-p T)
;; works= (("(IN1 0.2 PC1 FB)") ("(IN2 0.7 FB)") ("(IN3 0.5 SENS1)") ("(IN4 0.1 SENS2)"))


;;SET-STRING WORKS EFFICIENTLY
;;
;;(set-string "this" 99) this
;;
(defun set-string (string value)
  "In U-tstring.lisp "
    (set (my-make-symbol string) value))


(defun append-list (listname item &optional list)
  "In U-tstring"
  (if (null list)
      (set listname (append (eval listname) (list item)))
      (set listname (append list (list item))))
      )	

;;MAKE-NEW-INDEX-SYMBOLS
;;
;; SSS START HERE -- INTEGRATE MAKE-NEW-INDEX-SYMBOL HERE
;;  THESE 3 MAKE FUNCTIONS ARE ONLY USED IN ART--SO CAN CHANGE
;;  FOR ART AS MUCH AS WANT (I THINK)
;;ddd mmm
(defun make-new-index-symbol-types (symbol-spec-lists
                                    &key make-sublists-for-each-dim-p)
                                 ;;   &key make-lower-cell-arrays  (index-i-begin 1) (index-j-begin 1))
  "In U-tstring.lisp, processes list of SPEC-LISTS to make a list of new symbols. Each symbol-spec-list (N-SYMBOLS (PREFIX (spec-sublist) betw-symbol end)) is used to create a new symbol The SPEC-SUBLIST is a list of  (BEGIN-NUM INCREMENT-NUM) used to create the array index values. The last 2 can be empty. n-symbol-types is number of  symbols in list.  Eg. symbol-spec-list= (,nInputs (\"Input\" ((0 1)) \"\" \"\")) or (,nInputs (\"Wup\" ((0 1)(0 0)) \"\" \"\"))."
  (let*    
      ((n-symbol-types  (length symbol-spec-lists))
       (n-symbols 1)
       (index-i-end)
       (index-j-end)
       ;;outputs
       (new-symbols-list)
       (new-symbol-type)
       (new-symbol-type-list)
       (new-symbol-type-spec-list)
       (new-symbol-type-symbol-string-list)
       (new-symbols-type-list-of-lists)
       (new-symbol-type-spec-list-of-lists)
       (new-symbol-type-symbol-string-list-of-lists)
       )
    (loop
     for index-spec-list in symbol-spec-lists
     for symbol-n from 1 to  n-symbol-types
     ;;    with n-symbols = (car index-spec-list)
     do
     ;;(afout 'out (format nil "index-spec-list= ~A~%" index-spec-list))
     ;;eg SingleIndex (,nInputs ("Input" ((1 1)) "" ""))
     ;;eg DoubleIndex (,nOutputs ("Wup" ((1 1)(1 1)) "" ""))
     (setf n-symbols (car index-spec-list))
     (multiple-value-setq (new-symbol-type new-symbols-list 
                                           new-symbol-type-spec-list 
                                           new-symbol-type-symbol-string-list)
         (make-new-index-symbols  n-symbols (second index-spec-list)
                                  :make-sublists-for-each-dim-p make-sublists-for-each-dim-p))

     (setf  new-symbol-type-list (append new-symbol-type-list 
                                         (list new-symbol-type))
           new-symbols-type-list-of-lists (append new-symbols-type-list-of-lists
                                                  (list new-symbols-list))
           new-symbol-type-spec-list-of-lists (append new-symbol-type-spec-list-of-lists
                                                      (list new-symbol-type-spec-list))
           new-symbol-type-symbol-string-list-of-lists 
           (append new-symbol-type-symbol-string-list-of-lists 
                   (list new-symbol-type-symbol-string-list)))
     ;;end loop
     )
    (values new-symbol-type-list  new-symbols-type-list-of-lists
            new-symbol-type-spec-list-of-lists 
            new-symbol-type-symbol-string-list-of-lists)
    ))
;;TEST
;;  (testmst 5 3)
;; creates cell types, cells, and arrays for EACH CELL (not each cell type).
#|(defun testmst (nInputs nOutputs)
  (setf out nil)
  (let 
      ((symbol-spec-lists
        `((,nInputs ("Input" ((0 1)) "" ""))
          (,nInputs ("X-Activity" ((0 1)) "" ""))
          (,nInputs ("V" ((0 1)) "" ""))
          (,nInputs ("R" ((0 1)) "" ""))
          (,nInputs ("U" ((0 1)) "" ""))
          (,nInputs ("Q" ((0 1)) "" ""))
          (,nInputs ("P" ((0 1)) "" ""))
          ;;NOTE: FOR CONNECTIONS TO SECOND FIELD, THE NUMBER OF DIMS = N CELLS IN FIELD 2
          (,nInputs ("Wup" ((0 1)(1 0 )(2 0)) "" "")) ;;for 3 cells in field 2
          (,nInputs ("Wdn" ((0 1)(1 0)(2 0)) "" "")) 
          (,nOutputs ("Y-Outputs" ((0 1)) "" ""))
          ;;others
          (,nInputs ("Temp" ((0 1)) "" ""))
          (1 ("reset-Val" ((0 1)) "" ""))
          (,nOutputs ("reset" ((0 1)) "" ""))
          (,nOutputs ("reset-cnt" ((0 1)) "" ""))
          (,nOutputs ("n-cats" ((0 1)) "" ""))
          (,nOutputs ("Temp2" ((0 1)) "" ""))
          ))
       ;;end let vars
         )

  (make-new-index-symbol-types  symbol-spec-lists)
  ))|#
;;works results=>
#|
 new-symbol-type-list  new-symbols-type-list-of-lists
            new-symbol-type-spec-list-of-lists 
            new-symbol-type-symbol-string-list-of-lists)
;;new-symbol-type-list=
(INPUTS X-ACTIVITY V R U Q P WUP WDN Y-OUTPUTS TEMP RESET-VAL RESET RESET-CNT N-CATS TEMP2)
;;;; new-symbols-type-list-of-lists= 
((INPUTS0 INPUTS1 INPUTS2 INPUTS3 INPUTS4) (X-ACTIVITY0 X-ACTIVITY1 X-ACTIVITY2 X-ACTIVITY3 X-ACTIVITY4) (V0 V1 V2 V3 V4) (R0 R1 R2 R3 R4) (U0 U1 U2 U3 U4) (Q0 Q1 Q2 Q3 Q4) (P0 P1 P2 P3 P4) (WUP00 WUP10 WUP20 WUP30 WUP40) (WDN01 WDN11 WDN21 WDN31 WDN41) (Y-OUTPUTS0 Y-OUTPUTS1 Y-OUTPUTS2) (TEMP0 TEMP1 TEMP2 TEMP3 TEMP4) (RESET-VAL0) (RESET0 RESET1 RESET2) (RESET-CNT0 RESET-CNT1 RESET-CNT2) (N-CATS0 N-CATS1 N-CATS2) (TEMP20 TEMP21 TEMP22))
;; new-symbol-type-spec-list-of-lists= 
((("Inputs" ((0 1)) "" "") ("Inputs" ((1 1)) "" "") ("Inputs" ((2 1)) "" "") ("Inputs" ((3 1)) "" "") ("Inputs" ((4 1)) "" "")) (("X-Activity" ((0 1)) "" "") ("X-Activity" ((1 1)) "" "") ("X-Activity" ((2 1)) "" "") ("X-Activity" ((3 1)) "" "") ("X-Activity" ((4 1)) "" "")) (("V" ((0 1)) "" "") ("V" ((1 1)) "" "") ("V" ((2 1)) "" "") ("V" ((3 1)) "" "") ("V" ((4 1)) "" "")) (("R" ((0 1)) "" "") ("R" ((1 1)) "" "") ("R" ((2 1)) "" "") ("R" ((3 1)) "" "") ("R" ((4 1)) "" "")) (("U" ((0 1)) "" "") ("U" ((1 1)) "" "") ("U" ((2 1)) "" "") ("U" ((3 1)) "" "") ("U" ((4 1)) "" "")) (("Q" ((0 1)) "" "") ("Q" ((1 1)) "" "") ("Q" ((2 1)) "" "") ("Q" ((3 1)) "" "") ("Q" ((4 1)) "" "")) (("P" ((0 1)) "" "") ("P" ((1 1)) "" "") ("P" ((2 1)) "" "") ("P" ((3 1)) "" "") ("P" ((4 1)) "" "")) (("Wup" ((0 1) (0 0)) "" "") ("Wup" ((1 1) (0 0)) "" "") ("Wup" ((2 1) (0 0)) "" "") ("Wup" ((3 1) (0 0)) "" "") ("Wup" ((4 1) (0 0)) "" "")) (("Wdn" ((0 1) (1 0)) "" "") ("Wdn" ((1 1) (1 0)) "" "") ("Wdn" ((2 1) (1 0)) "" "") ("Wdn" ((3 1) (1 0)) "" "") ("Wdn" ((4 1) (1 0)) "" "")) (("Y-Outputs" ((0 1)) "" "") ("Y-Outputs" ((1 1)) "" "") ("Y-Outputs" ((2 1)) "" "")) (("Temp" ((0 1)) "" "") ("Temp" ((1 1)) "" "") ("Temp" ((2 1)) "" "") ("Temp" ((3 1)) "" "") ("Temp" ((4 1)) "" "")) (("reset-Val" ((0 1)) "" "")) (("reset" ((0 1)) "" "") ("reset" ((1 1)) "" "") ("reset" ((2 1)) "" "")) (("reset-cnt" ((0 1)) "" "") ("reset-cnt" ((1 1)) "" "") ("reset-cnt" ((2 1)) "" "")) (("n-cats" ((0 1)) "" "") ("n-cats" ((1 1)) "" "") ("n-cats" ((2 1)) "" "")) (("Temp2" ((0 1)) "" "") ("Temp2" ((1 1)) "" "") ("Temp2" ((2 1)) "" "")))
;;new-symbol-type-symbol-string-list-of-lists=
(("Inputs0" "Inputs1" "Inputs2" "Inputs3" "Inputs4") ("X-Activity0" "X-Activity1" "X-Activity2" "X-Activity3" "X-Activity4") ("V0" "V1" "V2" "V3" "V4") ("R0" "R1" "R2" "R3" "R4") ("U0" "U1" "U2" "U3" "U4") ("Q0" "Q1" "Q2" "Q3" "Q4") ("P0" "P1" "P2" "P3" "P4") ("Wup00" "Wup10" "Wup20" "Wup30" "Wup40") ("Wdn01" "Wdn11" "Wdn21" "Wdn31" "Wdn41") ("Y-Outputs0" "Y-Outputs1" "Y-Outputs2") ("Temp0" "Temp1" "Temp2" "Temp3" "Temp4") ("reset-Val0") ("reset0" "reset1" "reset2") ("reset-cnt0" "reset-cnt1" "reset-cnt2") ("n-cats0" "n-cats1" "n-cats2") ("Temp20" "Temp21" "Temp22"))
|#



#|(defun testns ()
 (setf out nil)
  (let
      ((x)
       )
    (make-new-symbols "Prefix" 7  '((0 1)(1 7))
                                        :hyphen-p t  :end-string "Ending")
    ))|#


;;MAKE-NEW-INDEX-SYMBOLS
;;
;;mmm
;;ddd
(defun make-new-index-symbols (n-symbols index-spec-list 
                                         &key  no-type-symbol  make-sublists-for-each-dim-p
                                         restart-numbers-each-dim-p)
  "In U-tstring.lisp, Makes a series of  n-symbols related symbols in a list. Each spec-list  (prefix (spec-sublist) betw-symbol end). The last 2 can be empty.The SPEC-SUBLIST is a list of  (begin-num increment-num) used to create the array index values. See make-new-index-symbol also. Makes number of indexes = lengthh of spec-sublist.  Returns (values  new-symbols-list new-index-spec-list-of-lists new-symbol-string-list)"
  (let*
      ((new-symbol)              ;;eg index-spec-list=  ("varx" ((1 1)(1 1)(1 1)) "-" "end")
       (var-root (car index-spec-list))  ;;eg "varx"
       (betw-index-str (third index-spec-list))   ;;eg: "-"
       (end-index-str (fourth index-spec-list))    ;;eg:  "end"
       (index-specs (second index-spec-list))  ;;eg ((1 1)(1 1)(1 1))
       (first-index-spec (car index-specs))   ;;eg (1 1)
       (n-dims (length index-specs))          ;;eg  3
       (new-symbol-type)       
       (new-index-specs index-specs)  ;;needed on first cycle
       (new-index-spec-list  index-spec-list) ;;why start with this??
       ( new-dim-symbols-list)
       (new-dim-index-spec-list)
       (new-dim-symbol-string-list)
       (new-symbol-string)
       (new-symbols-list)
       (new-index-spec-list-of-lists)
       (new-symbol-string-list)
       )
    ;;first make the new symbol-type symbol that the list is set to
    (unless no-type-symbol
      (setf new-symbol-type
            (my-make-symbol
             (format nil "~A~A~A" var-root betw-index-str end-index-str))))                                      
    (cond
     ;;TO MAKE CONTINUOUS NON-NESTED LIST OF DIM LISTS
     ((null make-sublists-for-each-dim-p)      
      (loop
       for n-sym from 0  to  (- n-symbols 1)
       do
       (setf new-index-spec-list-of-lists
             (append  new-index-spec-list-of-lists  (list new-index-spec-list)))

       (multiple-value-setq (new-symbol new-index-spec-list new-symbol-string)
           (make-new-index-symbol new-index-spec-list))

       ;;(afout 'out (format nil "new-symbol= ~A~% new-index-spec-list= ~A~%  new-symbol-string= ~A~%" new-symbol new-index-spec-list new-symbol-string))
     
       (setf new-symbols-list (append new-symbols-list (list new-symbol))      
             new-symbol-string-list (append new-symbol-string-list (list new-symbol-string)))
       (set new-symbol-type new-symbols-list)

       ;;end loop, make-sublists-for-each-dim-p      
       ))
     ;;TO MAKE SUBLISTS FOR EACH DIM
     ;;SSS START HERE
     (t
      (loop              ;;eg index-spec-list ("varx" ((1 1)(1 1)(1 1)) "-" "end")
       for n-dim from 1 to n-dims  ;;eg n-dims= 3 in eg below
       for index-spec  in index-specs  ;;eg index-specs=  ((1 1)(1 1)(1 1))
       do
       ;;REPLACE CURRENT DIM SPEC AFTER FIRST RUN
       (when  (> n-dim 1)
         (setf current-digit (car index-spec)
               incr-digit (second index-spec))
         (setf new-digit (+ current-digit incr-digit)
               new-spec (list new-digit incr-digit)
               new-index-specs (replace-list index-specs (- n-dim 1) new-spec)))

         ;;when want to restart indexe numbers with each dim
         (when restart-numbers-each-dim-p
           (setf  new-index-specs (replace-list new-index-specs 0 first-index-spec)))          

         ;;make the new new-index-spec-list to be used in loop below
         (setf new-index-spec-list (list var-root new-index-specs betw-index-str end-index-str))
         ;;MAKE EACH SYMBOL, ETC IN THE CURRENT DIM
       (loop
        for n-sym from 1 to  n-symbols
        do
        (setf new-dim-index-spec-list
              (append  new-dim-index-spec-list  (list new-index-spec-list)))

        ;;MAKE THE NEW-SYMBOL, ETC
        (multiple-value-setq (new-symbol new-index-spec-list new-symbol-string)
            (make-new-index-symbol new-index-spec-list :change-only-first-index-p T))

        ;;(afout 'out (format nil "new-symbol= ~A~% new-index-spec-list= ~A~%  new-symbol-string= ~A~%" new-symbol new-index-spec-list new-symbol-string))

        (setf new-dim-symbols-list (append new-dim-symbols-list    (list new-symbol))
              new-dim-symbol-string-list 
              (append new-dim-symbol-string-list (list new-symbol-string)))

        ;;end inner loop
        )
       (setf new-index-spec-list-of-lists
             (append  new-index-spec-list-of-lists  (list new-dim-index-spec-list)))

       (setf new-symbols-list (append new-symbols-list (list new-dim-symbols-list))      
             new-symbol-string-list
             (append new-symbol-string-list (list new-dim-symbol-string-list)))

       ;;reinitialize the dim variables
       (setf new-dim-symbols-list nil
             new-dim-index-spec-list nil
             new-dim-symbol-string-list  nil)

       ;;end outer loop     
       )
      (set new-symbol-type new-symbols-list)
      ;;end , make-sublists-for-each-dim-p = T, cond 
      ))
    (values new-symbol-type new-symbols-list new-index-spec-list-of-lists new-symbol-string-list)
    ))
;;TEST
;; ART2 MOST REALISTIC TEST IS TESTNS4  BELOW:
#|(defun testns4 ()
  (setf out nil)
  ( make-new-index-symbols 5  '("wXX" ((1 1)(1 1)) "" "") :make-sublists-for-each-dim-p T  ))
  (testns4) |#
#| RETURNS= WXX
((WXX11 WXX21 WXX31 WXX41 WXX51) (WXX12 WXX22 WXX32 WXX42 WXX52))
((("wXX" ((1 1) (1 1)) "" "") ("wXX" ((2 1) (1 1)) "" "") ("wXX" ((3 1) (1 1)) "" "") ("wXX" ((4 1) (1 1)) "" "") ("wXX" ((5 1) (1 1)) "" "")) (("wXX" ((1 1) (2 1)) "" "") ("wXX" ((2 1) (2 1)) "" "") ("wXX" ((3 1) (2 1)) "" "") ("wXX" ((4 1) (2 1)) "" "") ("wXX" ((5 1) (2 1)) "" "")))
(("wXX11" "wXX21" "wXX31" "wXX41" "wXX51") ("wXX12" "wXX22" "wXX32" "wXX42" "wXX52"))|#
;; ( make-new-index-symbols 5  '("wXX" ((1 1)(1 1) (1 1)) "" "") :make-sublists-for-each-dim-p T  )



#|(defun testns2 ()
  (setf out nil)
  ( make-new-index-symbols 5  '("pre" ((0 1)(1 3)(2 5)) "-" "end")  ))|#
;;works, returns=>
#|PRE-END
(PRE0-1-2END PRE1-4-7END PRE2-7-12END PRE3-10-17END PRE4-13-22END)
(("pre" ((0 1) (1 3) (2 5)) "-" "end") ("pre" ((1 1) (4 3) (7 5)) "-" "end") ("pre" ((2 1) (7 3) (12 5)) "-" "end") ("pre" ((3 1) (10 3) (17 5)) "-" "end") ("pre" ((4 1) (13 3) (22 5)) "-" "end"))
("pre0-1-2end" "pre1-4-7end" "pre2-7-12end" "pre3-10-17end" "pre4-13-22end")
 pre-end returns=>
(PRE0-1-2END PRE1-4-7END PRE2-7-12END PRE3-10-17END PRE4-13-22END)
|#
#|(defun testns3 ()
  (setf out nil)
  ( make-new-index-symbols 5  '("pre" ((0 1)(1 3)(2 5)) "-" "end") :make-sublists-for-each-dim-p T  ))
  (testns3) |#
;;returns=> 
#|PRE-END
((PRE0-1-2END PRE1-4-7END PRE2-7-12END) (PRE3-10-17END PRE4-13-22END PRE5-16-27END) (PRE6-19-32END PRE7-22-37END PRE8-25-42END) (PRE9-28-47END PRE10-31-52END PRE11-34-57END) (PRE12-37-62END PRE13-40-67END PRE14-43-72END))
((("pre" ((0 1) (1 3) (2 5)) "-" "end") ("pre" ((1 1) (4 3) (7 5)) "-" "end") ("pre" ((2 1) (7 3) (12 5)) "-" "end")) (("pre" ((3 1) (10 3) (17 5)) "-" "end") ("pre" ((4 1) (13 3) (22 5)) "-" "end") ("pre" ((5 1) (16 3) (27 5)) "-" "end")) (("pre" ((6 1) (19 3) (32 5)) "-" "end") ("pre" ((7 1) (22 3) (37 5)) "-" "end") ("pre" ((8 1) (25 3) (42 5)) "-" "end")) (("pre" ((9 1) (28 3) (47 5)) "-" "end") ("pre" ((10 1) (31 3) (52 5)) "-" "end") ("pre" ((11 1) (34 3) (57 5)) "-" "end")) (("pre" ((12 1) (37 3) (62 5)) "-" "end") ("pre" ((13 1) (40 3) (67 5)) "-" "end") ("pre" ((14 1) (43 3) (72 5)) "-" "end")))
(("pre0-1-2end" "pre1-4-7end" "pre2-7-12end") ("pre3-10-17end" "pre4-13-22end" "pre5-16-27end") ("pre6-19-32end" "pre7-22-37end" "pre8-25-42end") ("pre9-28-47end" "pre10-31-52end" "pre11-34-57end") ("pre12-37-62end" "pre13-40-67end" "pre14-43-72end"))|#



;;MAKE-NEW-INDEX-SYMBOL
;;
;;ddd
(defun make-new-index-symbol (index-spec-list &key change-only-first-index-p)
  ;;&key   ;;end  betw-symbol)
  "In U-tstring.lisp, Makes ONE symbol.  Each spec-list  (prefix (spec-sublist) betw-symbol end). The last 2 can be empty.The spec-sublist is a list of  (begin-num increment-num) used to create the array index values. CHANGE-ONLY-FIRST-INDEX-P is used for ART2 so can change other indexes from outside this function. RETURNS (values new-symbol new-index-spec-list new-symbol-string). "
  (cond
   ((listp index-spec-list)
    (let*
        ((prefix (car index-spec-list))
         (new-symbol)
         (spec-sublist (second index-spec-list ))
         (betw-symbol)
         (end)
         ;; (next-digit 0)
         (digit-n -1)
         (current-digit 0)
         (incr-digit 0)
         (new-digit 0)
         (new-spec-sublist spec-sublist)
         ;;do so have record of original spec-list
         (new-index-spec-list  index-spec-list)
         (length-spec-list (length index-spec-list))
         (new-spec)
         (new-symbol-string)
         )
      ;;set optional parameters
      (if (> length-spec-list 2) (setf betw-symbol (third index-spec-list))
        (setf betw-symbol ""))
      (if (> length-spec-list 3) (setf end (fourth index-spec-list))
        (setf end ""))
      (cond
       (change-only-first-index-p
        ;;ONLY USE THE FIRST DIGIT/SUBLIST--rest must be changed from outside of function
        (loop
         for index-specs in  spec-sublist
         for n from 1 to (length spec-sublist)
         do
         (setf current-digit (car index-specs)
               incr-digit (second index-specs))
         (cond
          ;;only change specs for first index/sublist
          ((= n 1)          
           (incf digit-n)
           (setf new-digit (+ current-digit incr-digit)
                 new-spec (list new-digit incr-digit)
                 new-spec-sublist (replace-list new-spec-sublist digit-n new-spec)))
          (t nil))
         (cond 
          ((= n 1)
           (setf new-symbol (format nil "~A~A" prefix  current-digit) ))
          (t 
           (setf new-symbol (format nil "~A~A~A" 
                                    new-symbol betw-symbol current-digit))))
         ;;end loop, change-only-first-index-p
         ))
       (t 
        ;;process the index items in sublist --caused changing ALL sublists,
        (dolist (spec-sublist spec-sublist)
          (incf digit-n)
          (setf current-digit (car spec-sublist)
                incr-digit (second spec-sublist))

          (setf new-digit (+ current-digit incr-digit)
                new-spec (list new-digit incr-digit)
                new-spec-sublist (replace-list new-spec-sublist digit-n new-spec))
          (cond 
           ((= digit-n 0)
            (setf new-symbol (format nil "~A~A" 
                                     prefix  current-digit) ))
           (t 
            (setf new-symbol (format nil "~A~A~A" 
                                     new-symbol betw-symbol current-digit))))
          ;;end dolist, change-only-first-index-p= NIL, cond
          )))
      (setf new-symbol-string (format nil "~A~A" new-symbol end)
            new-symbol (my-make-symbol  new-symbol-string)
            new-index-spec-list (list prefix new-spec-sublist betw-symbol end))
      
      (values new-symbol new-index-spec-list new-symbol-string)
      ))
   (t nil))
  )
;;TEST
#|(defun testns1 ()
    (make-new-index-symbol '("prefix" ((0 1)(9 4)) "-" "end"))
    )|#
#|returns=PREFIX0-9END
("prefix" ((1 1) (9 4)) "-" "end")
"prefix0-9end"|#
;;  (make-new-index-symbol '("wXX" ((1 1)(1 1)) "" ""))    
#| returns= WXX11
("wXX" ((2 1) (1 1)) "" "")
"wXX11"|#
;;then next cycle (make-new-index-symbol '("wXX" ((2 1) (1 1)) "" ""))
#|returns= WXX21
("wXX" ((3 1) (1 1)) "" "")
"wXX21"|#
 
   



;;************** FUNCTIONS TO DIVIDE STRINGS OR LISTS INTO SUBPARTS ******
;;
;; THESE ARE ESPECIALLY USEFUL FOR PRINTING OUT LINES FOR EQUAL LENGTH
;;	FROM LONG UNDIVIDED STRINGS



;;DIVIDE-SEQUENCE
;;
;;ARG: N/SEQ-LIST MUST BE A LIST SPECIFYING THE NUMBER OF PARTS FOR
;;	EACH LIST--IT IS ROBUST, 
;;		IF TOO SHORT IT WILL DROP LAST ITEMS
;;		IF TOO LONG IT WILL IGNORE AND ADD NILS FOR EXTRA ITEMS
;;
;;works w/dif nums
;;(divide-sequence '(a b c d e f g h i j k) '(3 2 4 4 3))
;;dss
(defun divide-sequence (sequence n/seq-list)
   "GWT-UTIL\TSTRING--seq is string or list--n/seq-list 
      eg.is '(4 2 3). Divides seq into list of 3 lists of 4 2 3 items"
  (let ((seq-length (length sequence))
        (begin-n)
        (end-n)
        (new-subseq)
        (list-of-subseqs)
        (N/SEQ)
	)
    (setf	end-n 0
                list-of-subseqs nil)

    (dolist (n/seq n/seq-list)
      (setf n/seq n/seq
            begin-n  end-n
            end-n (+ begin-n n/seq))

      (if (> end-n  seq-length )
          (setf end-n  seq-length ))

      (cond
       ((> begin-n seq-length)
        nil)
       (t 

        ;;	  (PRINT `( n/seq ,n/seq begin-n ,begin-n end-n ,end-n))
        ;;CHANGE n/seq ONLY IF COMPLETED
    

        (setf new-subseq (subseq sequence begin-n end-n))
        (unless (null new-subseq)
          (setf list-of-subseqs (append list-of-subseqs (list new-subseq))))
        ))
      )
    list-of-subseqs
    ;;end let, defun
    ))




;;MY-DIVIDE-MULTI-LINE-STRING
;;
;;RETURNS LIST OF STRINGS of MAX length length/line
;; WITHOUT REPEATED SPACES & WITHOUT ORIGINAL CARRIAGE RETURNS
;; MISC CAN BE USED TO ENTER 'NO-TAB 'NO-CR OR ANY CHAR SUCH AS " "
;;	WHICH YOU DO NOT WANT REPEATED 
;; makes  **string-list
;;(setf **print-str "this is a long string to be divided" **string-list nil)
;;three substrings must be set to "" before beginning?

;;  (progn(my-divide-string 10 "or engl100 or its equivalent etc to make this longer " )(print `(,**first-pt ,**last-pt)))
;;WORKS FOR VARIETY OF NUMS(progn(setf **string-list nil **first-pt "" **last-pt "")(my-divide-multi-line-string **print-str 16 ))(print `(,**string-list)))
;;;do following to avoid probs with eval below
(setf no-tab 'no-tab no-cr 'no-cr)
;;
(defun my-divide-multi-line-string (string length/line &rest misc)
   "In MyUtilities\\U-tstring.lisp"
;; (PRINT `(CURRENT-PREREQ ,string))
   (setf *str0 string 
	*len/ln0  length/line
	 *misc0 misc)
 ;;(PRINT *len/ln0)
 ;;  (setf *tot-lines (+ *tot-lines 1))
;;(print `(first- ,**first-pt  last- ,**last-pt))
   (cond
	((<= (length *str0) *len/ln0) 
        (setf **string-list (append **string-list (list  *str0)  ))) 
        (t
;;note: following only works bec no-tab & no-cr were set to themselves
;;(append '(a b c) xx) (setf xx '(d e))
        (eval (append `(my-divide-string *len/ln0  *str0)  *misc0))
	(append-list '**string-list  **first-pt) 
       (eval (append `(my-divide-multi-line-string **last-pt *len/ln0) *misc0))
        ))
  **string-list)

;;MY-CONSTANT-LENGTH-STRING
;;
;;RETURNS STRING OF CONSTANT LENGTH 
;;    DROPS OFF ANY PART THAT IS TOO LONG
;;(my-constant-length-string 2 "this")
;;
(defun my-constant-length-string (length string)
  "U-tstring"
  (let
      ((str-length (length string))
       (new-string)
       ) 
;;(print `(str-length ,str-length))  
  (cond
      ((equal str-length length)
       (setf new-string string))
      ((> str-length length)
       (setf new-string (subseq string 0 length)))
      ((< str-length length)
       (setf fill (make-string (- length str-length) :initial-element #\space))
       (setf new-string (format nil "~A~A" string fill)))
      (t nil))
    new-string))



;;(string #\comma) = ","
;;(string #\newline) works
;; (string #\period) = error; 
;; (string #\tab) = "       "

;;DIVIDE-STRING-TO-TOKENS
;;
;;ddd
(defun divide-string-to-all-tokens (string  &key tokens-list (char-delim-list  '(#\space))  
                                          delim-nth (ignore-char-list '(#\comma #\.  #\tab #\newline  #\comma #\; #\? #\[ #\] #\#  #\< #\>  #\|))
                                           nth-char nth-word max-length word-delim-list delete-list)
  "U-TSTRING.lisp divides list or string at delim-nth (default 1) occurance of char-delim-list, RETURNS (values all-tokens-list rest-items). of string. nth-char (default 1) divides string at nth-char or after nth-word if not already divided; max-length limits max-length of entire returned string. Chars may want to put in ignore list include #\newline #\space #\tab #\comma. delete-list actively deletes all its items from the new-word-list (default is NIL, ""; set to NIL if no deletes wanted.). Stops after 2000 words unless change function def.  If tokens-list, appends this list with tokens from string (needed in recusion)."
  (let
      ((first-token)
       (rest-string)
       (all-tokens-list)  
      ;; (rest-items)
      ;; (process-string)
     ;;  (rest-str-length)
       )
    (cond
     ((> (length string) 0)
      (multiple-value-setq (first-token rest-string)
          ;;note: unless no-initial-spaces = T, can cause stack-overflow.
          (divide-string-to-tokens string char-delim-list 
                                    :delim-nth delim-nth
                                   :ignore-char-list ignore-char-list :nth-char nth-char
                                   :nth-word nth-word :max-length max-length 
                                   :word-delim-list word-delim-list :delete-list delete-list))
      ;;make new all-tokens-list
      (if tokens-list (setf all-tokens-list tokens-list))
      (setf all-tokens-list (append all-tokens-list (list first-token)))
    ;;  (afout 'out (format nil "first-token= ~A rest-string= ~A~% all-tokens-list= ~A~%" first-token rest-string all-tokens-list))
  ;;CCC
    (cond
     ((> (length rest-string) 0)
      (multiple-value-setq (all-tokens-list rest-string)
          (divide-string-to-all-tokens rest-string :tokens-list all-tokens-list
                                       :char-delim-list char-delim-list  :delim-nth delim-nth
                                       :ignore-char-list ignore-char-list :nth-char nth-char
                                       :nth-word nth-word :max-length max-length 
                                       :word-delim-list word-delim-list :delete-list delete-list)))
     (t nil))
    ;;end clause string > 0
    )
    (t nil))
    (values all-tokens-list rest-string)
    ))
;;TEST
;;  (divide-string-to-all-tokens "This is a test of xxxxxxx")
;; CL-USER 6 > (divide-string-to-all-tokens "Another,  test of, this function")
;;results ("Another" "test" "of" "this" "function")

;;DIVIDE-STRING-TO-TOKENS
;;
;;ddd
(defun divide-string-to-tokens (string char-delim-list &key (no-initial-spaces T)
                                       delim-nth ignore-char-list nth-char nth-word 
                                       max-length word-delim-list  delete-list)
  "U-TSTRING.lisp divides list or string at delim-nth (default 1) occurance of char-delim-list, RETURNS (values first-pt last-pt). of string. nth-char (default 1) divides string at nth-char or after nth-word if not already divided; max-length limits max-length of entire returned string. Chars may want to put in ignore list include #\newline #\space #\tab #\comma.
delete-list actively deletes all its items from the new-word-list (default is NIL, ""; set to NIL if no deletes wanted.). RETURNS (values first-pt last-pt new-word-list). Stops after 2000 words unless change function def."
  (let
      ((str-length (length string))
       (cur-char 0 )
       (delim-n 0)
       (char-delimiter-found-p)
       (word-delimiter-found-p)       
       (first-pt "")
       (last-pt "")
       (end-word-list "")
       (cur-word "" ) ;;was "")
       (word-n 1)
       (new-word-list )
      ;; (fin? 'no)
      (char-divide-fin-p)
       (cur-place 0)
       (cur-char "")
       (cur-char-string)
       (cur-place-num 0)
       (len-first+word)
       )
    ;;the default value = 1
    (unless delim-nth
      (setf delim-nth 0))
    ;;default value for number of words to stop after is 1000
    (unless nth-word
      (setf nth-word 2000))
    (unless word-delim-list
      (setf word-delim-list  '(" " #\space #\tab #\comma "," ";" "." "?" "-" "!" "'")))
    (unless delete-list
      (setf delete-list '("" NIL)))

    (dotimes (n  str-length)
      (setf cur-char (char string n)
            cur-char-string (string cur-char))

      ;;if cur-char is member of ignore-char-list, ignore it
      (if (or (member cur-char ignore-char-list  :test 'equal)
                  (member cur-char-string ignore-char-list :test 'string-equal))
          (setf cur-char-string ""))

      ;;if cur-char or cur-char-string member of char-delim-list, set flag char-delimiter-found-p
      (if  (or (member cur-char char-delim-list :test 'equal)
               (member cur-char-string char-delim-list :test 'string-equal))
          (setf  char-delimiter-found-p t
                 delim-n (+ delim-n 1)))    
                 
      ;;if cur-char or cur-char-string member of word-delim-list, set flag word-delimiter-found-p
      (if  (or (member cur-char word-delim-list :test 'equal)
               (member cur-char-string word-delim-list :test 'string-equal))
          (setf  word-delimiter-found-p t))
      
      ;;unless fin? = yes, append the first-pt string
      ;;was (unless (equal fin? 'yes)
        (cond
         ((and char-delimiter-found-p (>= delim-n delim-nth) (null char-divide-fin-p))
          (setf char-divide-fin-p t
           last-pt (subseq string n)))
         ((null char-divide-fin-p)
          (setf  first-pt (format nil "~A~A" first-pt cur-char-string)))
         (t nil))

        (cond
         ((= n  (- str-length 1))
          (setf cur-char-string (subseq string n)
                cur-word (format nil "~A~A" cur-word cur-char-string)
           new-word-list (append new-word-list (list cur-word))))
         (word-delimiter-found-p 
          (setf new-word-list (append new-word-list (list cur-word))
              word-n (+ word-n 1)
              word-delimiter-found-p nil
              cur-word "")
          (cond
           ((>= word-n nth-word )
            (setf end-word-list (subseq string n)
                  new-word-list (append  new-word-list (list end-word-list)))
             (return))
           (t nil)))
           (t (setf  cur-word (format nil "~A~A" cur-word cur-char-string)))
         ;;end fist cond
         )
      ;;FILTER OUT CHAR REPEATS AS INDICATED IN MISC (& REST LIST)	
      ;;NO-LATER MAKE OPTIONAL??   (setf cur-char (my-delete-char-repeats cur-char misc)) 

    ;;  (afout 'out (format nil "n= ~A   cur-char= ~A cur-char-string= ~A cur-word= ~A ~%string= ~A~%first-pt= ~A~%last-pt= ~A~%new-word-list=~A~%" n   cur-char cur-char-string cur-word  string first-pt last-pt new-word-list  ))

      ;;end dotimes
      )
    ;;filter spaces if no-initial-spaces
    (if  no-initial-spaces
      (setf first-pt (my-delete-first-spaces first-pt)
            last-pt (my-delete-first-spaces last-pt)))
    ;;delete delete-list items
    (if delete-list
        (setf new-word-list (delete-list-items delete-list new-word-list)))

      ;;end divide-string-into-tokens
    (values first-pt last-pt new-word-list)))

;;test 
;; (divide-string-to-tokens "(\"intSrq6Extra\",1, intSrq6ExtraQ, \"int\",  FrAnswerPanel.LikeUs7,questionInstancesArray, frameTitle, frameDimWidth, frameDimHeight);" '(",") :no-initial-spaces T  :ignore-char-list '(" " "(" ")" ";"  #\space) :word-delim-list '("," ";"))
;;test
;; (divide-string-to-tokens "this is a test" '(" ")  :no-initial-spaces T  :ignore-char-list '(" " "(" ")" ";"  #\space) :word-delim-list '("," ";")) 
;;results=  "thisisatest"  ""  ("thisisatest")
;;test
;; (divide-string-to-tokens "this, is a test" '(",")  :no-initial-spaces T  :ignore-char-list '(" " "(" ")" ";"  #\space) :word-delim-list '("," ";"))
;;results= "this"   ", is a test"   ("this" "isatest")
;;test  (divide-string-to-tokens "this, is a test" '(#\space) :no-initial-spaces T  :ignore-char-list '(" " "(" ")" ";"  #\space) :word-delim-list '("," ";"))
;;result = "this,"  "is a test"  ("this" "isatest")

#|(defun testnd ()
  (setf out nil)
  (let
      ((string "  public static final String intSrq8TellAllQ = \"I have told my partner almost everything about myself.\";")
        ;;"tknowmor	t-Want to know more of self")
       (no-initial-spaces T )
       ( char-nth 1) 
       ( char-delim-list '(" " #\space #\tab))
       (delim-nth nil)
       (ignore-char-list )
       (nth-char 1)
       (nth-word)
       (max-length 100)
       (word-delim-list '(" " #\tab))
       )
    (divide-string-to-tokens string char-delim-list :no-initial-spaces no-initial-spaces
                             :delim-nth delim-nth
                             :ignore-char-list ignore-char-list :nth-char nth-char 
                             :nth-word nth-word  :max-length max-length :word-delim-list word-delim-list)
;;word-delim-li
    ))|#
;;works, returns=
#|"tknowmor"
 "	t-Want to know more of self"
("tknowmor" "t" "Want" "to" "know" "more" "of" "self")
|#



;;DIVIDE-LISTS-OF-STRINGS-TO-TOKENS
;;
;;SSS modify version below to take all these args??
;;(defun divide-lists-of-strings-to-tokens (string-list char-delim-list &key no-initial-spaces
;;                       delim-nth ignore-char-list nth-char nth-word max-length word-delim
;;;;NOTE--TAKES A REALLY LONG TIME TO WORK
;;
;;ddd
(defun divide-lists-of-strings-to-tokens (string-list &key (no-initial-spaces T)    
       (char-nth 1)   (char-delim-list '(" " #\space #\tab)) delim-nth
       ignore-char-list  (nth-char 1)  nth-word  (max-length 100)
       (word-delim-list '(" " #\tab)))
  "In U-tstring"
  (let*
      ((list)
       (string)
       (new-string-list)
  #|     (no-initial-spaces T)
       (char-nth 1) 
       (char-delim-list '(" " #\space #\tab))
       (delim-nth nil)
       (ignore-char-list )
       (nth-char 1)
       (nth-word)
       (max-length 100)|#
       (word-delim-list '(" " #\tab))
       )
    (dolist (list string-list)      
      (setf  string (car list))
     ;; (afout 'out (format nil "string= ~A~%" string))
      (multiple-value-setq (first-pt last-pt)
          (divide-string-to-tokens string char-delim-list :no-initial-spaces no-initial-spaces
                                   :delim-nth delim-nth
                                   :ignore-char-list ignore-char-list :nth-char nth-char 
                                   :nth-word nth-word  :max-length max-length
                                   :word-delim-list word-delim-list))
      (setf new-string-list (append  new-string-list (list (list first-pt last-pt))))
      ;;end dolist
      )
    ;;word-delim-list
    (setf  *shaq-sym-label-list2 new-string-list)
    ))

#|
;;test and actual use to change SHAQ lists--results-worked
;;NOTE--Takes a really long time to work
;;  (divide-lists-of-strings-to-tokens *part3 ) ;;*SHAQ-sym-label-list) ;; '( *test-shaq-list) ;;*SHAQ-sym-label-list) ;; '( ( "tknowmor	t-Want to know more of self") ("tknowmor1	    t-Want to know more of self1")))
|#

#|
(defun divide-lists-of-strings-to-tokens (string-list char-delim-list &key no-initial-spaces
                       delim-nth ignore-char-list nth-char nth-word max-length word-delim-list)
  (let
      ((new-string-list)
       (first-pt)
       (last-pt)
       (new-word-list)
       (string) ;; "tknowmor	t-Want to know more of self")
       (string-list)
       )
    ;;(afout 'out (format nil "string-list= ~A~%" string-list))
    ;;    (loop
    ;;     for list in string-list
    ;;     do
    (dolist (list string-list)
      (setf string (car list))
      (afout 'out (format nil "string= ~A~%" string))
      (multiple-value-setq (first-pt last-pt new-word-list)
          (divide-string-to-tokens string char-delim-list :no-initial-spaces no-initial-spaces
                                   :delim-nth delim-nth
                                   :ignore-char-list ignore-char-list :nth-char nth-char 
                                   :nth-word nth-word  :max-length max-length :word-delim-list word-delim-list))
      (setf  string-list (list fist-pt last-pt)
             new-string-list (append new-string-list (list string-list)))
      (afout 'out (format nil "new-string-list= ~A~%" new-string-list))
      ;;end loop
      )
    new-string-list
    ))

(defun runwss ()
  (setf out nil)
  (write-SHAQ-sym-label-list-divided))

;;ddd
(defun write-SHAQ-sym-label-list-divided ()
  (let*
      ((string-list1 *SHAQ-sym-label-list)
       ;;  (string "tknowmor	t-Want to know more of self")
       (no-initial-spaces T )
       ( char-nth 1) 
       ( char-delim-list '(#\tab));;for this list ;; '(" " #\space #\tab))
       (delim-nth nil)
       (ignore-char-list  '(" "))
       (nth-char 1)
       (nth-word)
       (max-length 100)
       (word-delim-list nil) ;; '(" " #\tab))
       )
   (format t "string-list1= ~A~%" string-list1)
    (setf  *SHAQ-sym-label-list-divided
          (divide-lists-of-strings-to-tokens string-list1 char-delim-list
                                             :no-initial-spaces no-initial-spaces
                                             :delim-nth delim-nth
                                             :ignore-char-list ignore-char-list :nth-char nth-char 
                                             :nth-word nth-word  :max-length max-length
                                             :word-delim-list word-delim-list))
    ))

|#
;;MY-DELETE-FIRST-SPACES
;;
;;ddd
(defun my-delete-first-spaces (string)
  "In U-tstring.lisp,  deletes all spaces including TABS before first non-space char."
  (let
      ((cur-char)
       (cur-char-string)
       (str-length (length string))
       (new-string string)       
       )
    (dotimes (n str-length)
      (setf cur-char (char string n)
            cur-char-string (string cur-char))
      (cond
       ((or (member cur-char '(#\space  #\TAB) :test 'equal)
            (member cur-char-string '(" " #\TAB) :test 'string-equal))
        (setf new-string (subseq string (+ n 1))))
       (t (return)))
      )
    new-string))
;;test
;;   (my-delete-first-spaces "     test string")
;;      (my-delete-first-spaces  "tknowmor")
;; works, returns "test string";; also works removing initial TABs
;; (equal " " #\space)
;;  (member #\b '(#\space " " #\TAB) :test 'char-equal) = ERROR " " NOT A CHAR
;;    BUT IF USE EQUAL, WON'T MATCH DIF CASES
;;  (member #\c  '(" " #\TAB) :test 'string-equal)  works= NIL

;;test member

;;MATCH-CHAR-LIST-TO-STRING
;;
;;ddd
(defun match-char-list-to-string (char-list string &key start end case-sensitive)
  "In U-tstring.lisp, start = 0, end = str-length by default. Seaches betw start and end. RETURNS (values rest-string first-string result-string). Char can be a symbol, string, or char. NUMBERS MUST be in form of a string or char. RETURNS (values result  rest-string first-string )"
  (let
      ((str-length (length string))
       (cur-char)
       (str-char)
       (result "")
       (first-string)
       (rest-string)
       )
    ;;set default part of string to search (all)
    (unless start
      (setf start 0))      
    (unless end
      (setf end  (- str-length 1 )))

  ;;test each char from start to en
    (loop
      for n from start to end
      do
      (setf cur-char (char string n)
            str-char (string cur-char))
      (cond
       ((null case-sensitive)
        (cond
         ((member str-char char-list :test 'string-equal)
          (setf result str-char)
          ;;  (afout 'out (format nil " n= ~A result= ~A~%" n result))
          (setf first-string (subseq string 0 n)
                rest-string (subseq string n))
          (return))
         (t (setf result "")))
        ;;end null case-sensitive
        )
       (t
        (cond
         ((member str-char char-list :test 'equal) 
          (setf result str-char)
          ;;  (afout 'out (format nil " n= ~A result= ~A~%" n result))
          (setf first-string (subseq string 0 n)
                rest-string (subseq string n))
          (return))
         (t (setf result "")))))
        ;;end loop
        )
    (if (equal result "")(setf result nil))
    ;;end match-substring-char-list
    (values result  rest-string first-string )
    ))
;;test
;;  (match-char-list-to-string '(c #\1 x) "  this abcd 123 ux")
;;works, returns "c" "cd 123 ux" "  this ab" 



;;MATCH-SUBSTRINGS
;;
;;ddd
(defun match-substrings (substring-list string &key start end case-sensitive)
  "In U-tstring.lisp, start = 0, end = str-length by default. Seaches betw start and end. RETURNS (values rest-string first-string result-string) for FIRST SUBSTRING MATCHED."
  (let
      (( rest-string)
       (first-string)
       (result-string)
       )
    (dolist (item substring-list)
      (setf item (string item))
      (multiple-value-setq  (rest-string first-string result-string)
          (match-substring item string :start start :end  end :case-sensitive case-sensitive))
      (if rest-string (return))
      ;;end dolist
      )
      (values rest-string first-string result-string)  
      ))
;;test 
;;  (match-substrings '(a x m ) "mthisa bx")
;; works returns "thisa bx" "m" "m"
;;(match-substrings '(a x ) "mthisa bx") =  "" "mthisa bx" "x"
;; (match-substrings '(f  g ) "mthisa bx") = NIL NIL ""
;;  ;; (match-substrings  '( = +)    "public static final String romSrq1SurpriseQ ="  ) ; :start 20)  


;;MATCH-SUBSTRING
;;
;;ddd
(defun match-substring (substring string &key start end case-sensitive)
  "In U-tstring.lisp, start = 0, end = str-length by default. Seaches betw start and end. RETURNS (values rest-string first-string result-string)"
  (let
      ((str-length (length string))
       (sub-length (length substring))
       (ss-char)
       (str-char)
       (result-string "")
       (first-string)
       (rest-string)
       (test-n -1)
       )
    ;;set default part of string to search (all)
    (unless start
      (setf start 0))      
    (unless end
      (setf end  (- str-length 1 )))

  ;;test each char from start to en
    (loop
      for n from start to end
      do
      (incf test-n)
      (setf ss-char (char substring test-n)
            str-char (char string n))
      (cond
       ((null case-sensitive)
      (cond
       ((char-equal ss-char str-char)
        (setf result-string (format nil "~A~A" result-string str-char))
      ;;  (afout 'out (format nil " n= ~A result-string= ~A~%" n result-string))
        (cond
         ((string-equal substring result-string)
          (setf first-string (subseq string  0  (+ n 1))
                rest-string (subseq string  (+ n 1)))
          (return))
         (t nil)))
       (t (setf result-string ""
                test-n -1)))
      ;;end null case-sensitive
      )
       (t
      (cond
       ((string-equal ss-char str-char)
        (setf result-string (format nil "~A~A" result-string str-char))
      ;;  (afout 'out (format nil " n= ~A result-string= ~A~%" n result-string))
        (cond
         ((string-equal substring result-string)
          (setf first-string (subseq string  0  (+ n 1))
                rest-string (subseq string  (+ n 1)))
          (return))
         (t nil)))
       (t (setf result-string ""
                test-n -1)))
        ;;end loop
        )))
    (values rest-string first-string result-string)
    ))
        
;;  (match-substring  "PCategory"   "PCategory intSrq6Extra = new PCategory(\"intSrq6Extra\",1, intSrq6ExtraQ, \"int\",  FrAnswerPanel.LikeUs7,questionInstancesArray, frameTitle, frameDimWidth, frameDimHeight);" :start 10)    
;;works, returns:
#|"(\"intSrq6Extra\",1, intSrq6ExtraQ, \"int\",  FrAnswerPanel.LikeUs7,questionInstancesArray, frameTitle, frameDimWidth, frameDimHeight);"
"intSrq6Extra = new PCategory"
"PCategory"|#

#|  
  (match-substring  "PUBLIC"  " public static final String intSrq6ExtraQ =
   \"INSTRUCTIONS FOR ALL QUESTIONS ON THIS SCALE:\n\"+
   \"  Apply all questions on this scale to your closest relationship. If you are married or in a close romantic relationship, apply them to that relationship. OR, you may apply them to an imagined future relationship.  OR, if you want help in working on a particular relationship, answer according to how the questions apply to that relationship.\n\n\"+
   \"Question 1.If I am under more stress than usual, my partner will usually do extra things for me.\";")
;;RETURNS
" static final String intSrq6ExtraQ =
   \"INSTRUCTIONS FOR ALL QUESTIONS ON THIS SCALE:n\"+
   \"  Apply all questions on this scale to your closest relationship. If you are married or in a close romantic relationship, apply them to that relationship. OR, you may apply them to an imagined future relationship.  OR, if you want help in working on a particular relationship, answer according to how the questions apply to that relationship.nn\"+
   \"Question 1.If I am under more stress than usual, my partner will usually do extra things for me.\";"
" public"
"public"
;; following also works
  (match-substring  "static"  " static final String intSrq6ExtraQ = ETC...

  (match-substring  "static" "  static final String intSrq6ExtraQ =
   \"INSTRUCTIONS FOR ALL QUESTIONS ON THIS SCALE:\n\"+
   \"  Apply all questions on this scale to your closest relationship. If you are married or in a close romantic relationship, apply them to that relationship. OR, you may apply them to an imagined future relationship.  OR, if you want help in working on a particular relationship, answer according to how the questions apply to that relationship.\n\n\"+
   \"Question 1.If I am under more stress than usual, my partner will usually do extra things for me.\";")
;;RETURNS
" final String intSrq6ExtraQ =
   \"INSTRUCTIONS FOR ALL QUESTIONS ON THIS SCALE:n\"+
   \"  Apply all questions on this scale to your closest relationship. If you are married or in a close romantic relationship, apply them to that relationship. OR, you may apply them to an imagined future relationship.  OR, if you want help in working on a particular relationship, answer according to how the questions apply to that relationship.nn\"+
   \"Question 1.If I am under more stress than usual, my partner will usually do extra things for me.\";"
"static"
"static"
|#








;;MATCH-TOKEN-PHRASE
;;  
;;yyy
;;ddd
(defun match-token-phrase (phrase-token-list string &key start)
  "In U-tstring.lisp, searches for a match between a list of string symbols or token-strings and a string.  Eg.   public static final String intSrq6ExtraQ = . NOTE: CAN have intervening tokens, but tokens in list must be found in SAME ORDER as in list. RETURNS (values rest-string first-string result-string-list token-matched-p)"
  (let
      ((rest-string)
       (first-string)
       (result-string)
       (str-length (length string))
       (token-matched-p T)
       (result-string-list)
       )
    (unless start
      (setf start 0))
#|    (unless end
      (setf end (- str-length 1)))|#
 ;;    (afout 'out (format nil "phrase-token-list= ~A~% string= ~A~%" phrase-token-list string))
    (setf rest-string (subseq string start ))  ;;caused error (- end 2)))
    ;;check each token for match.  Must match IN ORDER.
    (loop
     for token in phrase-token-list
     do
;;     (afout 'out (format nil "1  result-string= ~A token= ~A~%rest-string= ~A" result-string token rest-string))
     (unless (null token-matched-p)       
       (setf token (format nil "~A" token)
             rest-string (format nil "~A" rest-string))
       (multiple-value-setq (rest-string first-string result-string)
           (match-substring token rest-string))  ;;:start start ))  ;;caused error (- end 1)))
       (setf start (- str-length (length first-string) 2))
       ;;  end (- (length rest-string) 3))
   ;;     (afout 'out (format nil "2  result-string= ~A token= ~A~%rest-string= ~A" result-string token rest-string))
       ;;check if the current token is matched
       (cond
        ((string-equal result-string token)
         (setf result-string-list (append result-string-list (list result-string))))
        (t (setf token-matched-p nil)
           (return)))
 ;;      (afout 'out (format nil "3  result-string= ~A token= ~A~%rest-string= ~A~%result-string-list= ~A~% token-matched-p= ~A~%" result-string token rest-string result-string-list token-matched-p))
       ;;end unless, loop
       ))    
     (values rest-string first-string result-string-list token-matched-p)
     ))
;;test
;;  (testmtp)
#|(defun testmtp ()
  (setf out nil)
  (let
      ((X) 
              )
 ;; works,   (match-token-phrase  '(public static)     "  public static String frameTitle = \"Romantic Relationship\";") = " String frameTitle = \"Romantic Relationship\";" " static" ("public" "static") T
;;also weren't being found 
 "  public static int frameDimWidth = 805;"
 "  public static int frameDimHeight = 460;"

;;    (match-token-phrase  '(public static final string) "public static final String romSrq5CelebrQ = \"My partner and I celebrate special days together almost once a month.\";")
  (match-token-phrase  '(public static final string) " public static final String intSrq6ExtraQ =
   \"INSTRUCTIONS FOR ALL QUESTIONS ON THIS SCALE:n\"+
   \"  Apply all questions on this scale to your closest relationship. If you are married or in a close romantic relationship, apply them to that relationship. OR, you may apply them to an imagined future relationship.  OR, if you want help in working on a particular relationship, answer according to how the questions apply to that relationship.nn\"+
   \"Question 1.If I am under more stress than usual, my partner will usually do extra things for me.\";")  ;; :start 0)
  ))|#
;;works, returns:
#|" intSrq6ExtraQ =
   \"INSTRUCTIONS FOR ALL QUESTIONS ON THIS SCALE:n\"+
   \"  Apply all questions on this scale to your closest relationship. If you are married or in a close romantic relationship, apply them to that relationship. OR, you may apply them to an imagined future relationship.  OR, if you want help in working on a particular relationship, answer according to how the questions apply to that relationship.nn\"+
   \"Question 1.If I am under more stress than usual, my partner will usually do extra things for me.\";"
" String"
("public" "static" "final" "String")|#
;;  (match-token-phrase  '(public static final string) "public class bsSelfManagement extends iExecSelf")


;;FUZZY-LIST-SEARCHER
;;
;;ddd
(defun fuzzy-list-searcher (search-item  list &key return-symbols-p
                                          auto-cutoff  n-matched-cutoff   min-seq-length )
  "In U-tstring.lisp, matches search-item (symbol or string) to strings or symbols in a list that partially match.  RETURNS list result-string-list (incls search-item, matched-string-list).  If return-symbols, returns list of symbols as second value. If n-matched-cutoff not NIL and > number of matched chars, then NIL not string1 is returned. n-matched-cutoff simply requires that that many chars in seq in string1 match SOME chars in string2.    min-seq-length requires a SUBSTRING match of at least that many chars. The returned matched-seq is the LAST matched seq."
  (let
      ((search-item-string)
       (item-string)
       (result)
       (matched-string)
       (matched-string-list)
       (result-string-list)
       (matched-sym)
       (matched-sym-list)
       (result-sym-list)
       )

    (setf search-item-string (string search-item))

    (dolist (item list)
      (setf item-string (string item))
      (setf result  (fuzzy-matcher search-item-string item-string
                                   :auto-cutoff auto-cutoff  :n-matched-cutoff n-matched-cutoff  
                                   :min-seq-length  min-seq-length))
    ;;(afout 'out (format nil "search-item-string= ~A item-string= ~A~% result= ~A~%" search-item-string item-string  result))
      (cond
       (result
        (if  return-symbols-p
            (setf matched-sym (my-make-symbol result)
                  matched-sym-list (append matched-sym-list (list result))
                  result nil))

        ;;if match, make matched-string-list
        (setf matched-string-list (append matched-string-list (list result))
              result nil))
       (t nil))
      ;;end dolist
      )

     (setf result-string-list (list search-item matched-string-list))
        (if return-symbols-p 
            (setf result-sym-list (list search-item matched-sym-list)))

    ;;end fuzzy-list-searcher
   (values result-string-list result-sym-list)
    ))
;;testz
;; (fuzzy-list-searcher "abcthisxyz" '(defmno lmthis lmxyzghi mywhat dexyzfghijk)  :auto-cutoff 0.5 :return-symbols-p t)
;;works, returns ("abcthisxyz" ("LMXYZGHI" "DEXYZFGHIJK")) ("abcthisxyz" (LMXYZGHI DEXYZFGHIJK))
;; (fuzzy-list-searcher 'abcthisxyz '(defmno lmthis  mywhat dexyzfghijk) :min-seq-length 4 :return-symbols-p t)
;;works, returns (ABCTHISXYZ ("LMTHIS" "DEXYZFGHIJK")) (ABCTHISXYZ (LMTHIS DEXYZFGHIJK))
;;(fuzzy-list-searcher 'abcthisxyz '(defmno lmthisu  mywhat dexyzfghijk)  :min-seq-length  3 :return-symbols t)     
;; (progn (setf out nil)(fuzzy-list-searcher "lvietnam"  '(LANG "bio7lang" "English" "Spanish" "Vietnamese" "Cambodian" "Chinese" "Korean" "Portuguese" "German" "French" "Other Asian" "Other European" "Other")  :min-seq-length 4))



;;FUZZY-MATCHER
;;
;;NOTE:  In U-lists.lisp this function is added to 
;;    COMPARE-NESTED-LIST-ITEMS to get fuzzy mataches of strings in lists
;;
;; 
;;ddd
(defun fuzzy-matcher (item1 item2 &key return-symbol2-p
                                      auto-cutoff  n-matched-cutoff   min-seq-length)
  "In U-tstring.lisp, matches items (strings or symbols) that partially match.  RETURNS 
   (values string2-result string1  matched-seq matched-char-strings unmatched-char-strings  matched-chars unmatched-chars  matched-chars-n symbol2-result) . If n-matched-cutoff not NIL and > number of matched chars, then NIL not string1 is returned. n-matched-cutoff simply requires that that many chars in seq in string1 match SOME chars in string2.    min-seq-length requires a SUBSTRING match of at least that many chars. The returned matched-seq is the LAST matched seq. If return-symbol2-p, then the 9th return is the symbol of item2--or NIL if no match."
  (let*
      ((string1 (string item1))
       (string2 (string item2))
       (length-str1 (length string1))
       (length-str2 (length string2))
       (char1)
       (char2)
       (char1-str)
       (char2-str)
       (matched-chars)
       (unmatched-chars)
       (matched-char-strings)
       (unmatched-char-strings)
       (matched-chars-n 0)
       (result-string)
       (matched-p)
       (matched-seq-list) 
       (end-str)
       (match-str)
       (n-start 0)
       (n-end 0)
       (last-cycle-p)
       (seq-start-end-dif 0)
       (string2-result)
       (symbol2-result)
       )
    ;;(afout   'out (format nil "AA string1= ~A String2=~A~% length-str1= ~A length-str2=~A~%" string1 String2 length-str1 length-str2))
    ;;if  auto-cutoff not nil, then either use default or auto-cufoff value
    (cond
     (auto-cutoff
      (cond
       ;;if numberp, auto-cutoff, leave alone
       ((and (numberp auto-cutoff)) NIL)
       ;;if not numberp, set auto-cutoff to default 
       (t (setf auto-cutoff  0.6)))
      ;;in either case use the auto-cutoff to calc the n-matched-cutoff for string1
      (setf n-matched-cutoff  (round (* length-str1 auto-cutoff))))
     (t nil))
       
    ;;compare the chars of the two strings.
    (dotimes (n1  length-str1) 
      (setf char1 (char string1 n1)
            char1-str (string char1))
      ;;to allow full processing of last char in string1
      (if (= n1 (- length-str1 1))
          (setf last-cycle-p T))

      (dotimes (n2 length-str2)  ;; (- length-str2 1))
        (setf char2 (char string2  n2)
              char2-str (string char2))
        ;;(afout  'out (format nil "BB char1=~A~% char2=~A~% n1=~A   n2=~A~%"   char1 char2 n1 n2 ))
        (cond
         ((or (string-equal char1-str char2-str) (char-equal char1 char2))
          (setf matched-p t )
          (cond
           ((and (= n-start 0) (not (= n2 0))
                 (setf n-start n2
                       n-end n-start)))
           (t (incf n-end)))
          (return)
          )
         (t nil ))
        ;;end inner dotimes
        )  
      ;;(afout 'out (format nil "CC  min-seq-length= ~A~% matched-seq-list= ~A~%  seq-start-end-dif = ~A~%  n-start 1= ~A  n-end 1= ~A~%matched-p= ~A~%" min-seq-length matched-seq-list  seq-start-end-dif  n-start n-end matched-p))
      (cond
       ;;if matched keep track of sequences of matched chars
       (matched-p
        (setf matched-chars-n (+ matched-chars-n 1)
              matched-chars (append matched-chars (list char1))
              matched-char-strings (append matched-char-strings (list char1-str))
              matched-seq-list (append matched-seq-list (list char1))))
        (t nil))
      
      ;;if not matched OR last char in string1, marks end of a matched-seq.
      (cond 
       ((or (not matched-p) (and last-cycle-p matched-p))
        (setf unmatched-chars (append unmatched-chars (list char1))
              unmatched-char-strings (append unmatched-char-strings (list char1-str))
              seq-start-end-dif (+ (- n-end  n-start) 1)) 
        ;; do return here
        (cond
         ;;to continue testing the seq must be longer than min-seq-length
         ((and min-seq-length matched-seq-list 
               (or (>= seq-start-end-dif min-seq-length)
                   (if (and last-cycle-p (>= seq-start-end-dif (- min-seq-length 1)))
                       T)))
          ;;make the chars into a string for test            
          (setf matched-seq  (format nil "~{~A~}" matched-seq-list))
          (multiple-value-setq (end-str match-str result-string)
              (match-substring matched-seq  string2)) 
;;works (match-substring "lvietnam" "Vietnamese") 
          ;;(afout 'out (format nil "DD- MATCH-SUBSTRING  n-start= ~A n-end= ~A~%matched-seq= ~A~% seq-start-end-dif= ~A~% result-string= ~A~%" n-start n-end matched-seq seq-start-end-dif  result-string  ))

          ;;if this the substring matches a substring in string2, 
          ;;       return MATCH = do nothing more (otherwise, string1 is set ot NIL)
          (cond
           ;;must search entire string2, since could have more than one occurance of beginning char
           ((string-equal matched-seq  result-string)
            (setf string2-result string2)
            (return))
           (t (setf string2-result nil)))
          ;;end seq length test clause
          )
         (t nil))
        ;;reset values if seq doesn't meet min standards (and the loop isn't broken by return)
        (setf matched-seq-list nil
              matched-seq ""
              seq-start-end-dif  0
              n-start 0
              n-end 0)
        ;;end of not matched-p
        )
       (t )) ;;never reset this?? (setf matched-chars-n 0)))
      
       ;;reset  matched-p for new inner do cycle
        (setf matched-p nil)

      ;;(afout   'out (format nil "EE  char1= ~A~% char2= ~A~%  char1-str= ~A~% matched-char-strings= ~A~% unmatched-char-strings= ~A~%  matched-seq-list= ~A~% n-start= ~A n-end= ~A~%" char1 char2  char1-str matched-char-strings unmatched-char-strings  matched-seq-list   n-start n-end))
      ;;end outer dotimes
      ) 
    ;;if these matched-cutoff not nil, and num chars matched < n-matched-cutoff, set string2= nil
    (cond
     (n-matched-cutoff
      (cond
       ((>  n-matched-cutoff  (length matched-chars))
        (setf string2-result nil))
       (t (setf string2-result string2))))
     (t nil)) 

    ;;if  n-matched-cutoff not nil, the max sequence must be > n-matched-cutoff or set string2=NIL
    (cond
     ((and n-matched-cutoff (>= matched-chars-n  n-matched-cutoff))
      (setf string2-result string2))
     (t NIL))  ;;no?? was (setf string1-result nil)))
    
    ;;if appropriate, make a symbol2-result, NIL if no match
    (if (and string2-result return-symbol2-p)
        (setf symbol2-result (my-make-symbol string2)))

    ;;(afout  'out (format nil "FF-END matched-chars-n= ~A~%  n-matched-cutoff= ~A~%string1-result= ~A~%" matched-chars-n  n-matched-cutoff string1-result ))
    ;;end my-fuzzy-matcher
    (values string2-result string1 matched-seq matched-char-strings unmatched-char-strings  matched-chars unmatched-chars  matched-chars-n symbol2-result)
    ))

;;(apply 'max '(1 2 3))
;;(apply 'max nil));; '(1 2 3))
;;ttt test 
;;(fuzzy-matcher string1 string2 (&key auto-cutoff  n-matched-cutoff n-matched-cutoff )
;;
;;NOTE: CHANGED ORDER OF VALUES RETURNED AFTER THESE TO
;;  (values result-string1 string2 MATCHED-SEQ MATCHED-CHAR-STRINGS UNMATCHED-CHAR-STRINGS  matched-chars unmatched-chars  max-matched-chars-n )
;; (fuzzy-matcher "abcde" "xdey" :n-matched-cutoff 5)
;; works, returns NIL "xdey" (#\d #\e) (#\a #\b #\c) ("d" "e") ("a" "b" "c") 2
;;(fuzzy-matcher "abcde" "xdey" :auto-cutoff  0.5)
;; returns NIL"xdey" (#\d) (#\a #\b #\c) ("d") ("a" "b" "c") 0
;;(progn (setf out nil)(fuzzy-matcher "abcdefg" "xcdey" :auto-cutoff  t))
;;works, NIL "xcdey" (#\c #\d #\e) (#\a #\b #\f #\g) ("c" "d" "e") ("a" "b" "f" "g") 3
;;(fuzzy-matcher "abcde" "xdey" :auto-cutoff  0.2)
;; works, returns "abcde""xdey" (#\d #\e) (#\a #\b #\c) ("d" "e") ("a" "b" "c") 2
;;(progn (setf out nil)(fuzzy-matcher "abcdefg" "xcdey" :n-matched-cutoff 2)) 
;;works, returns  "abcdefg" "xcdey" (#\c #\d #\e) (#\a #\b #\f #\g) ("c" "d" "e") ("a" "b" "f" "g") 3
;;;(progn (setf out nil)(fuzzy-matcher "CaseNum" "TColFacAd"  :n-matched-cutoff 4)) 
;;works, returns NIL "TColFacAd" (#\C #\C #\a #\a) (#\s #\e #\N #\u #\m) ("C" "C" "a" "a") ("s" "e" "N" "u" "m") 2
;;;(progn (setf out nil)(fuzzy-matcher "CaseNumFacTest" "TColFacAd" :min-seq-length 3  ))
;;works,NIL "TColFacAd" (#\C #\C #\a #\a #\F #\a #\a #\c #\c #\T #\t) (#\s #\e #\N #\u #\m #\e #\s) ("C" "C" "a" "a" "F" "a" "a" "c" "c" "T" "t") ("s" "e" "N" "u" "m" "e" "s") 0 ""
;;(progn (setf out nil)(fuzzy-matcher "lrnwrpap" "lrnskls" :min-seq-length 3 ))
;;works, result "lrnwrpap" "lrnskls" (#\l #\r #\n) (#\w) ("l" "r" "n") ("w") 0 "lrn"
;;(progn (setf out nil)(fuzzy-matcher "lrnwrpap" "lrnskls" :min-seq-length 4 ))
;; works, result NIL "lrnskls"(#\l #\r #\n #\r) (#\w #\p #\a #\p) ("l" "r" "n" "r") ("w" "p" "a" "p") 0 ""
;;(fuzzy-matcher "abcthisxyz" "lmthis"  :min-seq-length 3) (fuzzy-matcher "abcthisxyz" "lmthis"  :min-seq-length 3)
;;works, "abcthisxyz" "lmthis" "this" ("t" "h" "i" "s") ("a" "b" "c" "x") (#\t #\h #\i #\s) (#\a #\b #\c #\x) 0
;;(progn (setf out nil)(fuzzy-matcher "abcthisxyz" "dexyzfghijk"  :min-seq-length 3))
;;  (fuzzy-matcher "abcthisxyz"  "dexyzfghijk" :min-seq-length 4)
;;  (progn (setf out nil)(fuzzy-matcher "lvietnam" "Vietnamese"  :min-seq-length 4))





;;MATCH-FIRST-TOKEN 
;;
;;ddd
(defun match-first-token (token string &key delimiter-list case-sensitive)
  "In U-string.lisp, tests to see if token matches first word (as defined by delimiter #/space or " " OR chars/string in delimiter-list. Returns  (values first-token rest-string)"
  (let
      ((first-token)
       (rest-string)
       )
  (multiple-value-setq (first-token rest-string)
      (find-first-word  string :delimiter-list delimiter-list))
  (cond
   ((null case-sensitive)
    (if (string-equal token first-token)
        (values first-token rest-string)
      ))
   (t
    (if (string-equal token first-token)
        (values first-token rest-string))
      ))
  ))
    
;; (match-first-token  "PCategory"   "PCategory intSrq6Extra = new PCategory(\"intSrq6Extra\",1, intSrq6ExtraQ, \"int\",  FrAnswerPanel.LikeUs7,questionInstancesArray, frameTitle, frameDimWidth, frameDimHeight);")
;;works, returns the 2 parts.    
;;   (match-first-token "PCategory" " PCategory romSrq5Celebr = new PCategory(\"romSrq5Celebr\",3, romSrq5CelebrQ, \"int\",  FrAnswerPanel.LikeUs7,questionInstancesArray, frameTitle, frameDimWidth, frameDimHeight);")


;;FIND-FIRST-WORD
;;
;;ddd
(defun find-first-word (string &key delimiter-list)
  "In U-string.lisp, finds first word followed by a space in a string of various words, returns word and rest of string (minus that space). If delimiter list contains other chars to use as delimiters."
  (let*
      ((cur-char)
       (cur-char-string)
       (new-string "")
       (str-length (length string))
       (rest-string)
       (non-delimit-char-p)
       (found-first-char-p)
       )
    ;;default delimiter-list
    (unless delimiter-list
        (setf delimiter-list  `(#\space #\; #\, #\. #\( #\) #\tab #\+ #\[ #\] #\|    #\\ )))  
              ;;was (" " #\space "," ";" ")" "(" "]" "[" "?" #\tab "+")))
    ;;check each char for delimiter, accumulate rest into first and last string parts
    (dotimes  (n str-length)
      (setf cur-char (char string n)
            cur-char-string (string cur-char))
      ;;test for delimiter for each do item
      (cond
       ((or (member cur-char-string delimiter-list  :test 'string-equal))
            (member cur-char delimiter-list :test 'char-equal)
        (setf non-delimit-char-p nil))
       (t (setf non-delimit-char-p t)))
      ;;this only set once
      (if  non-delimit-char-p
          (setf found-first-char-p t))
      ;;look for space and return word and rest of string (without the space)
      (cond
       ((null found-first-char-p)  
        ;;if not ever found first char, do nothing yet
        nil)
       ;;if found first char,  test for delimiter= end first word
       ((null non-delimit-char-p)
        (setf rest-string (subseq string (+ n 1)))
        (return))
       ;;otherwise keep adding chars to first word
       (t (setf new-string (format nil "~A~A" new-string cur-char-string))))
    ;;  (afout 'out (format nil "cur-char-string= ~A~%new-string= ~A~%non-delimit-char-p= ~A~%found-first-char-p= ~A~%" cur-char-string new-string non-delimit-char-p found-first-char-p))
      ;;end dotimes
      )
    (values new-string rest-string)
    ))
;;test
;;works, returns "this" "is a test"
#|(defun tm ()
  (let*
      ((string "   this is a test")
        )
    (setf out nil)
    (find-first-word string)
    ))|#

#|
         ("Email	")
         ("ZipCode")	
         ("Nation	")
         ("HrsWork")	
         ("UserRate")	
         ("tknowmor	t-Want to know more of self")
         ("texperie	t-Experienced self-help user")
         ("twanttho	t-Want thorough assessment")
         ("twantspe	t-Want specific help")
         ("tworknga	t-worknga")
|#


;;FIND-STRING-IN-STRING
;;
;;ddd
(defun find-strings-in-string (string)
  "In U-tstring.lisp, finds all strings embedded in strings. RETURNS a list of them."
  (let
      ((string-length (length string))
       (char-str)
       (new-string)
       (string-list)
       (new-non-string)
       (non-string-list)
       (string-p)
       (non-string-p)
       (char)
       )
    (loop
     for n from 0 to (- string-length 1)
     do
     (setf char (char string n)
           char-str (string char))
     (cond
      ;;Is the char a double-quote--begin or end of inner string??
      ;;note (string-equal "\"" #\") = T
      ((or (char-equal char  #\") (string-equal char-str "\""))    
       (cond
        ;;quote is beginning of new string
        ((null string-p)
         (setf new-string char
               string-p t))
        ;;quote is end of new-string
        (t         
         (setf new-string (format nil "~A~A" new-string (string char))
          string-list (append string-list (list new-string)))))
       ;;FINISH SSS STARTING HERE
       )
      (t 
       (cond
        (string-p
         (setf new-string (format nil "~A~A" new-string (string char))))
        (t nil))))
     ;;end loop
     )
     string-list
     ))

;;test SSS
;;  (testsis)
#|(defun testsis ()
  (let
      ((string "  public static final String intSrq7CommitQ = \"A long term commitment (would) cause(s) me to feel trapped.\";")
       )
  (find-strings-in-string string)      
  ))|#
;;works, returns: ("\"A long term commitment (would) cause(s) me to feel trapped.\"")
     







;;MY-DIVIDE-STRING (Largely REPLACED BY DIVIDE-STRING-TO-TOKENS, 2014, ABOVE)
;;
;;(list *first-pt **last-pt)
;;WORKS ON ALL NUMBERS (my-divide-string 8  "this string is long")
;;(my-divide-string 8  "this        
;;string          is long" 'no-cr " ")
;;DDD
;;new version-SETS ABSOLUTE LIMIT ON WIDTH
;; ALSO IT CAN IGNORES TABS AND/OR CARRIAGE RETURNS
;;    JUST ADD 'NO-TAB OR 'NO-CR TO &REST ARGS
;;latest version
;;THIS FUNCTION IS INEFFICIENT IF THERE IS A LONG SECOND PART, BECAUSE
;;	IT READS EACH SECOND PART CHARACTER UNNCESSECARILY
;;
(defun my-divide-string (place string &rest misc)
   "In MyUtilities\\U-tstring.lisp sets absolute max width at place, returns values first-pt last-pt"
  (let
        ((str-length (length string))
	(cur-char 0)
	(first-pt "")
	(last-pt "")
	(last-char! "")
	(current-word "")
	(fin? 'no)
        )

  (cond
;;IF TOTAL STRING IS LESS THAN PLACE, JUST USE ENTIRE STRING
     ((<= str-length place) 
     (setf first-pt string))
;;OTHERWISE MUST DIVIDE IT
      (t
      (dotimes (n  str-length)
;;(print `(cur-char ,*cur-char **first-pt ,**first-pt  **current-word ,**current-word **last-pt ,**last-pt))	
         (setf cur-place n ;;(- n 1)
		  old-char cur-char
	          cur-char-num (char string cur-place)
		  cur-char (string cur-char-num))

;;FILTER OUT CHAR REPEATS AS INDICATED IN MISC (& REST LIST)	
         (setf cur-char (my-delete-char-repeats cur-char misc)) 


  ;;if it is a return, ignore it
	 (if (and (member 'no-cr misc :test 'equal)
	         (string-equal cur-char #\newline)) 
	    (setf cur-char ""))

  ;;if it is a TAB, ignore it
	 (if (and (member 'NO-TAB misc :test 'equal)
	         (string-equal cur-char #\tab))
	    (setf cur-char ""))

	(cond
	   ((equal fin? 'no)
;;(print 'no)
              (setf current-word (format nil "~A~A"
					current-word cur-char))
	      
	   (cond
	      ((or (string-equal cur-char " ")(= cur-place (- str-length 1)))
;;(print `(cur-place ,cur-place))
	      (setf len-first+word
  		 (+ (length first-pt)
			(length current-word)))
	      (cond
;;IF THE TOTAL STRING IS TOO LONG, 
		   ((> len-first+word place)
;;(print '>)	
		   (setf fin? 'yes
		      last-pt current-word))		
;;IF THE TOTAL STRING WITH BOTH WORD ISN'T TOO LONG, DONT EXIT
;;				 set first-pt = first plus BOTH WORDS
	           ((< len-first+word place)
	           (setf  first-pt (format nil "~A~A"
				first-pt current-word )
		      current-word ""))
;;IF EQUAL
	           ((= len-first+word place)
;;(print '=)
	           (setf fin? 'yes
		           first-pt (format nil "~A~A"
				first-pt  current-word)
		          current-word ""		
	                  last-pt ""))
	           (t (print `(ERROR? ON cur-char  ,cur-char )))))
	;;if cur-char not = " "
	       (t nil)))
;;IF FIN? 'YES JUST CHANGE LAST-PT
	   ((equal fin? 'yes)
;;(print 'Yes)
	    (setf last-pt (format nil "~A~A"
					last-pt cur-char)))
	   (t (print `(ERROR? cur-char ,cur-char)))))))
  (values first-pt last-pt)
  ;;end let,my-divide-string
  ))
;;TEST
;; ( my-divide-string  8 "THIS IS A TEST STRING TO SEE IF THIS WORKS")
;; works?  results = "THIS IS " "A TEST STRING TO SEE IF THIS WORKS"


;;MY-SIMPLE-CONCAT
;;
;;TAKES A MIXTURE OF SYMBOLS, NUMBERS, STRINGS IN A LIST &
;;    RETURNS A STRING OF THEM ALL
;;works(my-simple-concat '(this "test quote list" 999))
;;ddd
(defun my-simple-concat (string-symbol-list)
  "In U-tstring.lisp, GWT-UTIL\TSTRING"
  (let
      ((ss-list string-symbol-list)
       (print-str "")
       )
    (cond
     ((listp ss-list)
      (dolist (item ss-list)
        (setf print-str (format nil "~A ~A"  print-str item))))
     (t (setf print-str (format nil "~A ~A"  print-str ss-list))))
    print-str))



;;MY-DELETE-CHAR-REPEATS-IN-STRING
;;
;; (my-delete-char-repeats-in-string '("a" "c") "baccaaadccc")
;;(my-delete-char-repeats-in-string '(" " ) " this    is a   that")
;;
(defun my-delete-char-repeats-in-string (char-list sequence)
  "gwt-util\\tstring"
  (let
      ((new-str "")
       (last-char! "")
       (char)
       (new-char)
       )
    (dotimes (n (length sequence))
      (setf char (string (char sequence n))
            new-char (my-delete-char-repeats char char-list ))
      (setf new-str (format nil "~A~A" new-str new-char)))
    new-str
    ))


;;MY-DELETE-CHAR-REPEATS 
;;
;;DELETES ANY REPEATS OF ANY OF THE CHARS IN CHAR-LIST (in string form)
;;--MUST SETF *THIS-CHAR "" BEFORE USING THIS FUNCTION
;;(progn (setf *last-char! "")(my-delete-char-repeats "c" `("a" "c"))  "bacccdaagc"))
;;(member "c" '("a" "c") :test 'equal)
(defun my-delete-char-repeats (char char-list)
  (list "gwt-util\tstring--MUST SETF *last-char! = double-quotes first")
 ;;(print `(,*last-char! ,char ,char-list))
  (cond ((and (member char char-list :test 'string-equal) (string-equal char *last-char!))
	(setf *last-char! char) "")
	(t
	(setf *last-char! char)
	char)))

;;  STRING-EQUAL = BEST TEST??
;; (member #\b '(a b c) :test 'string-equal) = (B C)
;; (member #\b '(a "b" c) :test 'string-equal) = ("b" C)
;; (member #\b '(a  #\b c) :test 'string-equal) = (#\b C)
;; (member #\b '(a  "B" c) :test 'string-equal) = ("B" C)

;;MY-DELETE
;;
;; THIS DELETES ANY CHAR-STRING IN DELETE LIST FROM A STRING
;;
;;(my-delete "-" "- ENGL100")
;;(my-delete (string 10) "THIS IS A END")
;;ddd
(defun my-delete (delete-char-list string)
  "U-tstring, deletes a single char-string  in delete-char-list from string.delete-char-list can actually be a single non-list char-string."
  (let
      ((new-char-str "")
       (char)
       (char-str)
       (new-char-str "")
       )
  (dotimes (n (length string))
     (setf char (char string n)
           char-str (string char))
     (cond 
	((or
          (and (listp delete-char-list)
              (not (member char-str delete-char-list :test 'string-equal)))
          ;;if delete-char-list not a list
          (and (not (listp delete-char-list))
               (not (string-equal char-str delete-char-list))))
	 (setf new-char-str (format nil "~A~A"	
			new-char-str char-str)))
	(t nil))
     ;;end dotimes
     )
  new-char-str
  ))
;; (my-delete "-" "-ENGL100") = "ENGL100"
;; (member "a"  '("b" "A" "d") :test 'string-equal) = ("A" "d")
;;
;; (my-delete  '(";" "+" "=") "This = \"a test of whatever.\" + ;")
;; works, returns   "This  \"a test of whatever.\"  "
;; SSS
;;  (format t "~A~%" (my-delete '("," ";") "{\"12 or more\",\"11\",\"10\",\"9\",\"8\",\"7\",\"6\",\"5\",\"4\",\"3\",\"2\",\"1\",\"0\"};") ) ;;NIL 'EOF-FOUND)))  ))
;;  (my-delete '( #\space  #\newline) "This is a test        of 
;;my-delete        .")  =  "Thisisatestofmy-delete."
;;  (my-delete '("    "  #\newline) "This is a test        of  my-delete        .") ;;NOT WORK

;;MY-SEARCH
;;
;;DESIGN LATER, USE MY-SUBSTITUTE AS A BEGINNNING ALGORITHM??
;;

;;MY-DELETE-SUBSTRING
;;
;;ddd
;;new version using my-equal
(defun my-delete-substring (substring string &key (delete-all-p t) from-end-p)
  "In U-tstring.lisp, Deletes the first substring. RETURNS (values new-string first-string rest-string substring) new-string = nil if no match."
  (if from-end-p
      (setf string (reverse string)
            substring (reverse substring)))      
  (let
      ((length-str (length string))
       (length-ss (length substring))
       (begin-n 0)
       (last-match-n 0)
       (first-string "")
       (rest-string "")
       (new-string "")
       (string-found-p)
       (test-ss)
       (end-n)
       )
    (loop
     for n from 0 to length-str
     do
     (setf end-n (+ n length-ss))
     (cond
      ((and (>= length-str (+ last-match-n length-ss))
            (>= length-str  end-n)
            (>= n last-match-n))
       (setf begin-n n)
       (setf test-ss (subseq string begin-n end-n))
       (cond
        ((my-equal substring test-ss)
         (setf string-found-p T
               first-string (format nil "~A~A" first-string
                                    (subseq string  last-match-n begin-n))
               rest-string (subseq string end-n   length-str)
               new-string (format nil "~A"  first-string) 
               last-match-n end-n)
         ;;return after first match?
         (unless delete-all-p 
           (setf new-string (format nil "~A~A" first-string rest-string))
           (return)))
        (t ;;(/= n end-n)
           (setf new-string (format nil "~A~A" new-string (subseq string n (+ n 1))))))
       )
      ((<= length-str (+ last-match-n length-ss ))
       (setf new-string (format nil "~A~A" new-string 
                                (subseq string  last-match-n  length-str)))
       (return))      
      ((<= length-str  (+ begin-n length-ss ))
       (setf new-string (format nil "~A~A" new-string 
                                (subseq string  (+ begin-n 1) length-str )))
       (return))
      (t nil))
     ;;(afout 'out (format nil "new-string= ~A begin-n=~A end-n=~A~%" new-string begin-n end-n))
     ;;end loop
     )
    ;;in case no match found
    (if (null string-found-p)
        (setf  new-string string))
    (if from-end-p
        (setf new-string (reverse new-string)
              first-string  (reverse rest-string)
              rest-string (reverse first-string)))
 
    ;;end my-delete-substring
    (values new-string first-string rest-string substring)
    ))
;;test
;; (progn (setf out nil) (my-delete-substring "this"  "123this4567890"))
;; works "1234567890" "123"  "4567890" "this"
;; (progn (setf out nil) (my-delete-substring "this"  "123this4567this890"))
;;works "1234567890" "1234567" "890" "this"
;; (progn (setf out nil) (my-delete-substring "this"  "123567this" ))
;; works "123567" "123567" "" "this"
;; (progn (setf out nil) (my-delete-substring "this"  "  xxxthis what "))
;;works, returns "  xxx what " "  xxx" " what ""this"
;; (progn (setf out nil) (my-delete-substring "this"  "123this4567this890" :delete-all-p nil)) "1234567this890" "123" "4567this890" "this"
;; (progn (setf out nil) (my-delete-substring "this"  "123this4567890"))
;; works "1237890" "123" "7890" "this"
;;  (my-delete-substring "  "  "  xxx  this is                 what            .")
;; works= "xxxthis is what."
;; (progn (setf out nil) (my-delete-substring "this"  "this"))
;;works = "" "" "" "this" 


;;MY-SUBSTITUTE
;;
;;ddd
(defun my-substitute (newitem olditem sequence &key (start 0) end 
                                           match-case-p  (count 0)  from-end-p)
  "In U-tstring.lisp. If count, substitutes newitem for only count occurramces of olditem in sequence. Note: number of spaces in newitem and olditem need NOT be the same.  start and end refer to orig sequence even if  from-end-p = t"
  (let
      ((newitem-str (string newitem))
       (new-char)
       (new-char-str "")
       (olditem-str "")
       (old-char)
       (old-char-str "")
       (olditem-length)
       (newitem-length)
       (sequence-str "")
       (seq-char)
       (seq-char-str "")
       (seq-length (length sequence))
       (match-n 0)
       (count-n 0)
       (seq-first-pt "")
       (seq-last-pt "")
       (new-seq "")
       (last-matched-end-n)
       (n1)
       )
    (cond
     ((and (setf newitem-str (string newitem)
                 olditem-str (string olditem)
                 sequence-str (string sequence)))
      (setf  olditem-length (length olditem-str)
             newitem-length (length newitem-str))

      (unless end
        (setf end (- seq-length 1)))

      (loop
       for i from start to end
     ;;  with n 
       do
       ;;from-end-p?
       (cond
        (from-end-p
         (setf n (- end  i)
                 match-n (- olditem-length 1)))
        (t (setf  n  i)))
       ;;set chars and char strings
       (setf seq-char (char sequence-str n)
             seq-char-str (string seq-char)
             new-char (char newitem-str match-n)
             new-char-str (string new-char)
             old-char (char olditem-str match-n)
             old-char-str (string old-char))
          ;;   seq-first-pt (format nil "~A~A" seq-first-pt seq-char-str))

          ;;(afout 'out (format nil "newitem-str= ~A~% olditem-str= ~A~% sequence-str= ~A~% old-char-str= ~A seq-char-str=~A~% newitem-str= ~A~%new-seq= ~A~% seq-length= ~A olditem-length= ~A~% n= ~A~% match-n= ~A~%"newitem-str olditem-str sequence-str  old-char-str seq-char-str newitem-str new-seq seq-length olditem-length n match-n))	
       (cond
        ;;do the current chars-strs match?
        ((or (and (null match-case-p) ;;if don't need to match case
                  (string-equal old-char-str seq-char-str))
             ;;if do need to match case
             (equal old-char-str seq-char-str))

         ;;IF CHAR-STRS MATCH, THEN 
         (incf match-n)
         ;;(afout 'out (format nil "MATCHED match-n= ~A~%" match-n))
         ;;is end of old-item reached? Length of new-item not important.
         (cond
          ((= match-n  olditem-length)
           (incf count-n)
           (cond
            (from-end-p
             (setf  new-seq (format nil "~A~A" newitem-str new-seq ) ;; seq-last-pt)
                    match-n 0))
            (t          
             (setf  new-seq (format nil "~A~A" new-seq newitem-str) ;; seq-last-pt)
                    match-n 0)))
           ;;(afout 'out (format nil "MATCHED ALL match-n= ~A count-n= ~A~% new-seq= ~A~%" match-n count-n new-seq))
           ;;if count, return or increment count-n, otherwise nothing
           (cond
            (count
             (cond
              ((= count-n count)
               ;;if count limit reached, return new-seq w/ rest of seq attached
               (cond
                (from-end-p
                 ;;here new-seq is starting from end, accumulating gradually, 
                 ;;   seq-last-pt is part left over -- which is first part of sequence
                 (setf seq-last-pt (subseq sequence start (+ n  1))
                                       ;;was  (string  (subseq sequence start (+ n  1))
                       new-seq (format nil "~A~A"  seq-last-pt new-seq)))
                 (t 
                  (setf seq-last-pt (string  (subseq sequence (+ n 1)))
                        new-seq (format nil "~A~A" new-seq seq-last-pt))))
               (return))
              (t nil)))
             (t nil))
           ;;end of matched whole seq.
           )
          ;;if matched, but not end of olditem-str incf match-n
          (t 
           ;;end matched clause
           )))
           ;;CHARS DON'T MATCH
           (t (setf  match-n 0)
              (cond
               (from-end-p t
                 (setf  new-seq (format nil "~A~A" seq-char-str  new-seq )))
               (t (setf  new-seq (format nil "~A~A" new-seq  seq-char-str))))
            ))
          ;;end loop and set items to strings clause
          ))
     (t (format t "ERROR--ONE OF THE OBJECTS==> ~A~%, ~A~%   OR ~A  IS NOT A STRING--IN QUOTES" newitem-str olditem-str sequence-str)))
         
    ;;what to return
    (cond
     ((> (length new-seq) 0)
      new-seq)
     (t nil))
    ))

;;TEST
;;    (my-substitute   #\newline   (read-from-string "\\n")  "anbncndnee\n") = subs for all n's
;;   (read-from-string "\\n") = \n
;;  (my-substitute   #\newline (format nil "~C~A" #\\  "n")  "anbncndnee\n") = "anbncndneen"
;;  (my-substitute   #\newline (format nil "~S~A" #\\  "n")  "anbncndnee\n")
;;  NOTE: "anbncndnee\n" EVALS to "anbncndneen" (drops \)
;;  not-work (my-substitute  (my-substitute  (format nil "~%") (format nil "\\n")  "anbncndnee\n") "\\n"  "anbncndnee\n") = "anbncndneen" (only eliminates \)
;; not-work (my-substitute  #\newline "\n"  "anbncndnee\n") = newline sub for every n
;;works, (my-substitute  "xx" "n"  "anbncndneen") =  "axxbxxcxxdxxeexx"
;; works (my-substitute  "xx" "n"  "anbncndneen" :count 2)
;; works (progn (setf out nil) (my-substitute "xx" "n"  "anbncndnee\n" :from-end-p t))
;; works (progn (setf out nil) (my-substitute "xx" "n"  "anbncndnee\n" :from-end-p t :count 2))
;;"anbncndnxxeexx"
;; (my-substitute  #\newline (format t "\\~A" #\n)  (format t "~A" "anbncndnee\n"))
;; (my-substitute  #\newline #\\ "anbncndnee\n")
;;works (progn (setf out nil) (my-substitute  "xyz" "abc" "xyabcde")) = "xyxyzde"
;;not work: (progn (setf out nil) (my-substitute  "xyz" "abc" "xyabcde" :from-end-p t))
;;works (my-substitute  "and" "&" "&xyabcde") = "andxyabcde"
;;works (my-substitute  'and "&" "xya & bcde") = "xya AND bcde"
;;Following only works if num spaces matches in newitem and olditem
;;  works (my-substitute  (format nil "~% ") "\\n" "ab cn de \\n  x yn z") 
;; works (my-substitute (string #\newline) "   " "this is a    this is second" ) 
;;works = "thisisa
;; thisissecond"
;;  (char  (string #\newline) 0) = #\Newline
;; (print "\\n" :escape :ESCAPE *PRINT-ESCAPE*)




;;
;;CONVERTS ANY KIND OF STRING WITH ANY MIXTURE TO INITIAL CAPS-REST LOWER CASE
;;--PRECEDING SPACE SIGNALS NEXT LETTER TO BE A CAPITAL
;;(char " " 0)
;;works (convert-to-upper&lower-case "TEST STRING of letters" t)
;;
(defun convert-to-upper&lower-case (string roman-num?)
  (list "gwt-util\\tstring")
  (setf *char #\newline
	*newstring "")
  (dotimes (n (length string))
    (setf *oldchar *char 
	*char (char string n))
    (cond
	((or (string-equal *newstring "")
	   (string-equal *oldchar #\space )  ;(code-char 73)#\I
         (and (equal roman-num? 'yes)(string-equal *oldchar #\I)
		(string-equal *char #\I)))
	(setf *newchar (string (char-upcase *char))
		*oldchar *newchar))
	(t  (setf *newchar (string (char-downcase *char)))))
    (setf *newstring (string-append *newstring (string *newchar))))
   *newstring)	

;;CONVERTS STRING NUMBERS TO INTEGERS
;;from Steele p. 236
;;
;;(convert-string-to-integer "568")
(defun convert-string-to-integer (str &optional (radix 10))
 "In U-tstring, from Steel, p236, given a digit string & optional radix, return an integer."
 (list "U-tstring.lisp")
   (do ((j 0 (+ j 1))
	(n 0 (+ (* n radix)
		(or (digit-char-p (char str j) radix)
			(error "Bad radix-~D digit: ~C"
				radix (char str j))))))
	((= j (length str)) n)))


;;CONVERT-STRING-TO-FLOAT
;;
;;ddd
(defun convert-string-to-float (float-str &optional (radix 10))
  "In U-tstring, based on Steel, p236, given a float digit string & optional radix, return an integer. If float-str is number, returns it, otherwise returns nil."
  (let
      ((length-str) 
       (digit-list)
       (decimal-digit-list)
       (decimal-digit-p)
       (num-sign '+)
       (number)
       (element)
       (digit)
       )
    (cond
     ((numberp float-str)
      (setf  number float-str))
     ((stringp float-str)
      (setf length-str (length float-str))
      (loop
       for i from 0 to (- length-str 1)
       do
       (setf element (char float-str i))
       (cond
        ((char-equal element #\-)
         (setf num-sign '-))
        ((char-equal element #\.)
         (setf decimal-digit-p T))
        (T
         (setf digit (digit-char-p element radix))
         (cond
          (decimal-digit-p
           (setf decimal-digit-list (append decimal-digit-list (list digit))))
          (t
           (setf digit-list  (append digit-list (list digit)))))
         ))
       ;;end loop
       )
      (setf number (convert-digit-list-to-float digit-list decimal-digit-list))
      ;;end stringp clause
      )   
     (t nil))
    number
    ;;end let, convert-string-to-float
    ))
  

;;TEST
;; (convert-string-to-float "123.45")



;;CONVERT-DIGIT-LIST-TO-FLOAT
;;
;;ddd
(defun convert-digit-list-to-float (digit-list &optional decimal-digit-list)
  "In U-tstring"
  (let*
      ((number (convert-digit-list-to-integer digit-list))
       (decimal-num (convert-digit-list-to-integer decimal-digit-list))
       (num-decimals (list-length decimal-digit-list))
       (decimal-multiplier (/ 1.0 (expt 10 num-decimals)))
       )
    (setf number (+ number (* decimal-num  decimal-multiplier)))
    ;;end let, convert-digit-list-to-float
  ))
;;TEST
;;  (convert-digit-list-to-float '(2 3 4) '(9 8 7 6))
;; works =  234.9876

;;CONVERT-DIGIT-LIST-TO-INTEGER
;;
;;ddd
(defun convert-digit-list-to-integer (digit-list) 
  "In U-tstring"
  (let
      ((multiplier 1)
       (new-num 0)
       (i -1)
       (num)
       )
    (loop
     for digit in (reverse digit-list)
     do
     (incf i)
     (if (> i 0) (setf multiplier (* multiplier 10)))
      (setf num (* multiplier digit)
           new-num (+ new-num num))
     ;;end loop
     )
    new-num
    ;;end let, convert-digit-list-to-number
    ))
;;TEST
;;  (convert-digit-list-to-integer  '(1 2 3))
;; works = 123



; CONVERT-STRING-TO-OBJECTS
;;
;;ddd
(defun convert-string-to-objects (string &key object-list (start 0))
  "In U-tstring.lisp, converts a string to objects. Returns object-list."
  (let 
      ((x)
       (length-string (length string))
       (begin-n start)
       (first-object)
       (rest-string)
       )

    (loop
     for n from 0 to length-string
     do

     (cond
      ;;if line too short
      ((< length-string 2) 
       (return))
      ;;line > 2
      ;; basic processing to objects is here--process it first here
        (t
         ;;yyy
         (multiple-value-setq (first-object rest-string begin-n)
             (convert-object-in-string string :start start))
         (if first-object
             (setf object-list (append object-list (list first-object))))

       ;;work on rest-string here 
       (cond
        ((> (length rest-string) 1)
         (multiple-value-setq (object-list first-object rest-string )
             (convert-string-to-objects rest-string :object-list object-list :start begin-n))
         (return))
        (t (return)))

       ;;(afout 'out (format nil "n= ~A first-object= ~A~%  rest-string= ~A~%  object-list= ~A~%  "n first-object rest-string object-list ))
       ;;end line > 2 clause, cond
       ))
     ;;end loop
     )
    (values object-list first-object rest-string  )
    ))
;;test  
;;  (progn (setf out nil)(convert-string-to-objects2 "this is a 1 2 3 test"))
;; works, returns (THIS IS A 1 2 3 TEST)
#|
(setf *test-out9 (my-delete '("+" "=" ";")
" intSrq6ExtraQ =
   \"INSTRUCTIONS FOR ALL QUESTIONS ON THIS SCALE:n\"+
   \"  Apply all questions on this scale to your closest relationship. If you are married or in a close romantic relationship, apply them to that relationship. OR, you may apply them to an imagined future relationship.  OR, if you want help in working on a particular relationship, answer according to how the questions apply to that relationship.nn\"+
   \"Question 1.If I am under more stress than usual, my partner will usually do extra things for me.\";"))
 (convert-string-to-objects  *test-out9)
|#


;;CONVERT-OBJECT-IN-STRING
;;   
;;ddd
(defun convert-object-in-string (string &key (start 0)  return-list-p)
  "In U-tstring.lisp, converts a string to objects. If return-list-p, returns list of first-object,  rest-string, begin-n."
  (let*
      ((first-object)
       (rest-string )
       (begin-n)
       (end-string (- (length string) start 1))
       ;;had to include this because read-from-string didn't work right--wouldn't search only bounded region of some strings
       (new-string)
       ;;not needed?   (substring (subseq string start))
       )
    (setf new-string (my-substitute "\\(" "\(" string ))
    (unless (> start end-string)      
          (multiple-value-setq (first-object begin-n)
              (read-from-string new-string nil 'eof  :start 0)) ;; :end end-string ))            
      (setf rest-string (subseq new-string begin-n))
      (if (equal first-object 'eof)
          (setf first-object nil))
      ;;end unless
      )
    ;;end convert-object-in-string
    (cond
     ((null return-list-p)
      (values first-object rest-string begin-n))
     (t  (list first-object rest-string begin-n)))    
    ))
;; (convert-object-in-string  "    System.out.println(\"questionFramesArray[0] to setVisible(true) \"") ;;:start 22 :return-list-p  t))
;;;; (convert-object-in-string  "(true) \"" :return-list-p t) = ((TRUE) "\"" 7)
;;(char "    System.out.println(\"questionFramesArray[0] to setVisible(true) \""22)
;; works returns (SYSTEM.OUT.PRINTLN "(\"questionFramesArray[0] to setVisible(true) \"" 22)
;;  (convert-object-in-string "this is a test") 
;; works, returns= THIS "is a test"  5
;; (convert-object-in-string "this is a test" :return-list-p  t)
;;works, returns  (THIS "is a test" 5)
;;(convert-object-in-string "this is a test" :start 5)
;;works, returns IS "a test" 8

;;(length "(\"questionFramesArray[0] to setVisible(true) ")
;;(read-from-string "(\"questionFramesArray[0] to setVisible(true) \"" nil 'eof :start 0 :end 44)
;;(length "(\"questionFramesArray[0] to setVisible(true) \"") = 46
;;(read-from-string "(\"questionFramesArray[0] to setVisible(true) \"") ;; nil 'eof :start 0 :end 44)
;; (unwind-protect (read-from-string "(\"questionFramesArray[0] to setVisible(true) \"" :start 10 )) = same error
;;(read-from-string "\(\"questionFramesArray[0] to setVisible(true) \")") 

;;;(read-from-string (my-substitute "\\(" "\("  "(\"questionFramesArray[0] to setVisible(true) \""))
;;(read-from-string   "    System.out.println(\"questionFramesArray[0] to setVisible(true) \"" nil 'eof  :start 22) =  Error: End of file while reading stream #<SYSTEM::STRING-INPUT-STREAM 23FCE687>.
;;  (read-from-string   "    System.out.println(\"questionFramesArray[0] to setVisible(true) \"")  = SYSTEM.OUT.PRINTLN 22
;;(read-from-string   "    System.out.println(\"questionFramesArray[0] to setVisible(true) \""  :start 33) = SYSTEM.OUT.PRINTLN 22

;; (read-from-string "    System.out.println (\"questionInstancesArray[0] \" questionInstancesArray[0]);" nil 'eof :start 50 :end 79 ) = Error: End of file while reading stream 
;; (length "    System.out.println (\"questionInstancesArray[0] \" questionInstancesArray[0]);")


;;FORMAT-STRING-LIST
;;
;;ddd
(defun format-string-list (string-list &key stream (line-width 100)  
                                       (add-newlines 0) add-top-lines add-bottom-lines 
                                       (justify-type :left ) (left-margin-spaces 0) )
  "In U-tstring.lisp, takes list of text and creates one formated string.ADD-NEWLINES adds add-newlines newlines between strings. RETURNS (values all-string-text  n-strings. justify-type :center, :left, :right. Should use LINE-WIDTH if justify. RETURNS the formated string.  If stream, sends string to stream first. LEFT-MARGIN-SPACES adds that many spaces to left of each line."
  (let
      ((all-strings-text "")
       (new-string)
       (new-strings-list)
       (left-margin-str (format nil "~vA" left-margin-spaces #\space))
       )
    
    (loop
     for string in string-list
     do
     ;;1-JUSTIFY EACH LINE or not
     (cond
      ((equal justify-type :center)
       (setf new-string (format nil "~V<~;~A~A~;~;~>~V%"  
                                line-width left-margin-str string add-newlines)))
      ((equal justify-type :left)
       (setf new-string (format nil "~V<~A~A~;~;~>~V%" 
                                line-width left-margin-str string add-newlines)))
      ((equal justify-type :right)
       (setf new-string (format nil "~V<~A~A~>~V%"
                                line-width left-margin-str string add-newlines)))
      ;; (format nil "~V<~A~>~V%" 50  "this test string" 0)
      (t  (setf new-string (format nil "~A~A" left-margin-str string))))

     ;;add the string to a new-strings-list (justified or not)
     (setf new-strings-list (append new-strings-list (list new-string)))
     ;;end loop
     )

    ;;ADD LINES AT TOP OR BOTTOM
    (if add-top-lines
        (setf new-strings-list
              (append (list (format nil "~V%" add-top-lines  ))
                      new-strings-list)))
    ;;works (format t "~V%~A" 2 "this is a test")
    (if add-bottom-lines
        (setf new-strings-list (append new-strings-list
                                       (list  (format nil "~V%"  add-bottom-lines )))))
    ;;(format nil "~V%"  3 )
    (if stream
        (format stream "~{~A~}" new-strings-list))

    (setf  all-strings-text (format nil "~{~A~}" new-strings-list))
    ;;end format-string-list
    ))
;;test
;;  (format-string-list (list "this one" "1 3 5 7" "final one"))
;;  works = "this one1 3 5 7final one" 3
;;  (format-string-list (list "this one" "1 3 5 7" "final one") :add-newlines 1)
;; works =
#|"this one
1 3 5 7
final one"
3|#
;; (format-string-list (list "this one" "1 3 5 7" "final one") :add-newlines 1 :left-margin-spaces 10)
;;works =
#|"          this one                                                                                  
          1 3 5 7                                                                                   
          final one                                                                                 
"|#
;; also works with 1,2 etc add-newlines
;;  (format-string-list (list "this one" "1 3 5 7" "final one") :add-newlines 1 :add-top-lines 1 :add-bottom-lines 1)
;;  (format-string-list (list "this one" "1 3 5 7" "final one") :add-newlines 2 :justify-type :left)


;;center-text
;;
;;ddd
(defun center-text (string line-width &key stream)
  "Approx centers text in line of stream with width of line-width"
  (format stream "~V<~;~A~;~;~>" line-width string)
  )
;;(center-text "center this text" 72)



;;check-string-number-range
;;
;;ddd
(defun check-string-number-range (num-string lo-num hi-num)
  "In U-Tstring.lisp, returns (values T or NIL if num is >= lo-num and <= hi-num AND num or the original num-string or nil if not a number."
  (let
      ((length-num) ;; )
       (num)
       (result)
       )
    ;;if its not blank or a number, make it a number
    (cond
     ((stringp num-string)
      (setf length-num (length num-string))
     (if (> length-num 0)
         (setf num (my-make-symbol num-string)))
     )
     ((numberp num-string)
      (setf num num-string))
     (t  nil))
    ;;check on its value    
    (cond
     ((and (numberp num) (>= num lo-num) 
           (<= num hi-num))
      (setf result T))
     (t nil))
    (values result num)
    ))
;;TEST
;; (check-string-number-range "44" 30 99) = T 44
;; (check-string-number-range "22" 30 99)  = NIL 44
;; (check-string-number-range "" 30 99)   = NIL ''"
;; (check-string-number-range  45 30 99)
;; (check-string-number-range  11 30 99)


;;
;;ddd
(defun greatest-string-length (list-of-strings)
  "In U-tstring, returns VALUES 1-length of longest string and the 2-longest string and 3-position in list--starting with 0."
  (let
      ((return-string)
       (return-length 0)
       (return-n 0)
       (n -1)
       (str-length)
       )
    (loop
     for string in list-of-strings
     do
     (incf n)
       (setf str-length (length string))
       (cond
        ((> str-length return-length)
         (setf return-string string
               return-length str-length
               return-n n))         
        (t nil))
       ;;end loop
       )
    (values return-length return-string return-n)
    ;;end let, greatest-string-length
    ))
;;TEST, works
;;   (greatest-string-length '("ABC" "cde" "lmnop" "np")) = 5  "lmnop"  2
;;  (greatest-string-length '("lmnop" "ABC" "cde" "lmnop" "np")) = 5 "lmnop" 0
;;  (greatest-string-length '("lmnop" "ABC" "cde" "lmnop" "np" "1234567"))  = 7 "1234567"  5           



;;MY-DO-STRINGS
;; Just a sham function to call make me aware of and/or use the built-in function below
;;ddd
#|(defmacro my-do-strings (string-var value-var table &optional result &body forms)
  "JUST USE LW BUILT-IN FUNCTION DO-STRINGS INSTEAD = Do-Strings (String-Var Value-Var Table [Result]) {declaration}* {form}*
  Iterate over the strings in a String Table.  String-Var and Value-Var
  are bound to the string and value respectively of each successive entry
  in the string-table Table in alphabetical order.  If supplied, Result is
  a form to evaluate to get the return value."

  (editor:do-strings (list string-var value-var table result) forms)
  )|#

;;; DO-STRINGS  --  PUBLIC  (from src/editor  user-macros.lisp)
;;; LW BUILT-IN FUNCTION, DO NO RE-EVALUATE UNLESS CHANGE NAME
;;; Iterate over the entries in a string table using the NEXT link.
;;;
#|(defmacro do-strings ((string-var value-var table &optional result) &body forms)
  "Do-Strings (String-Var Value-Var Table [Result]) {declaration}* {form}*
  Iterate over the strings in a String Table.  String-Var and Value-Var
  are bound to the string and value respectively of each successive entry
  in the string-table Table in alphabetical order.  If supplied, Result is
  a form to evaluate to get the return value."
  (let ((tab (gensym))
	(current (gensym))
	(i (gensym))
	(vec (gensym)))
    `(let* ((,tab ,table)
	    (,vec (string-table-table ,tab)))
       (dotimes (,i (string-table-num-entries ,tab) ,result)
	 (declare (fixnum ,i))
	 (let* ((,current (svref ,vec ,i))
		(,string-var (string-table-entry-proper ,current))
		(,value-var (string-table-entry-value ,current)))
	   (declare (simple-string ,string-var))
	   ,@forms)))))|#










;; hhh ------------------------------------------- HELP -------------------------------------------------

;;  (length "abcedfgh") = 8
;;   gp::*default-font* = #<GRAPHICS-PORTS::GENERIC-FONT {:SYSTEM :SYSTEM * * * * *} 20E42DB7>
;;  (gp::string-width "this is a string" gp::*default-font*) = error
;;  gp::port

;;  STRING-EQUAL = BEST TEST??
;; (member #\b '(a b c) :test 'string-equal) = (B C)
;; (member #\b '(a "b" c) :test 'string-equal) = ("b" C)
;; (member #\b '(a  #\b c) :test 'string-equal) = (#\b C)
;; (member #\b '(a  "B" c) :test 'string-equal) = ("B" C)


;;from Seibel
#|(count 1 #(1 2 1 2 3 1 2 3 4)) = 3
(remove 1 #(1 2 1 2 3 1 2 3 4)) = #(2 2 3 2 3 4)
(remove 1 '(1 2 1 2 3 1 2 3 4)) = (2 2 3 2 3 4)
(remove #\a "foobarbaz") = "foobrbz"
(substitute 10 1 #(1 2 1 2 3 1 2 3 4)) = #(10 2 10 2 3 10 2 3 4)
(substitute 10 1 '(1 2 1 2 3 1 2 3 4)) = (10 2 10 2 3 10 2 3 4)
(substitute #\x #\b "foobarbaz") = "fooxarxaz"
(find 1 #(1 2 1 2 3 1 2 3 4))  = 1
(find 10 #(1 2 1 2 3 1 2 3 4)) = NIL
(position 1 #(1 2 1 2 3 1 2 3 4)) = 0
|#
;; SSS PLAY HERE-- LEARN ABOUT CHARS AND STRINGS WITH BLANKS
;;   TO FIX MY-SUBSTITUTE
;;  (equal "this" "THIS") = NIL
;;  (string-equal  "this" "THIS")  = T
;;  (equal  #\a #\A) = NIL
;;  (char-equal  #\a #\A) = T
;;  (string-equal #\space " ") = T
;;  (string-equal #\c "C")  = T

;; (setf **a "a"  **b "  ")
;; (length (format t "~A~A" **a **b)) ;;" "" " ")
;; (char " " 0)

;;  #\a#\b#\c = error
;;  #\a #\b #\c  = values of same 
;; (length #\a) = error
;; (length (string #\a)) = 1

;;USE OF CHAR AND SCHAR -- PREFER CHAR
#| (setq my-simple-string (make-string 6 :initial-element #\A)) =>  "AAAAAA"
 (schar my-simple-string 4) =>  #\A
 (setf (schar my-simple-string 4) #\B) =>  #\B|#

#|STRING x => string
ARGUMENTS and Values:
x---a string, a symbol, or a character.
string---a string.
DESCRIPTION: Returns a string described by x; specifically:
If x is a string, it is returned.
If x is a symbol, its name is returned.
If x is a character, then a string containing that one character is returned.
|#

;;  FORMAT USING ~S TO PRINT STRINGS AND READ READABLE
;;  (setf tilde-info "tilde S prints in (read) able output--eg strings are surrounded with quotes")
;;  (format t "This is tilde-info= ~S" tilde-info) = 
;; returns:  This is tilde-info= "tilde S prints in (read) able output--eg strings are surrounded with quotes"
;;  (format t "~S"  #\space) = #\Space
;;  (format t "~S"  (string #\space)) = " "
;;  (format nil "~S"  #\space) = "#\\Space"
;;  (format nil "~S"  (string #\space)) = "\" \""
;;
;;STRING ELT or CHAR
;;returns char
;; (elt "already a string" 7) = #\Space
;; (char "already a string" 7) = #\Space
;;eeturns string
;;(string (elt "already a string" 7)) = " "

;;POSITION
;; (position "bcd" "xabcdefg") = nil
;; (position #\b "xabcdefg") = 2
;; CHAR
;; (char "abc def" 3) = #\Space
;; (elt "abc def" 3) = #\Space
;; POSITION
;; (position " " "abc def") = NIL
;; (position #\Space "abc def") = 3
;;xxx SEARCH
;; (search " "   "abc def") = 3
;; (search #\Space "abc def") = error not of type sequence
;; (search (string #\Space) "abc def") = 3
;; (search "A B"  "xyz a bcda b c") = NIL
;; (search "A B"  "xyz a bcda b c" :test 'string-equal)  = 4
;; (search "A B"  "xyz a bcda b c" :test 'string-equal :from-end t) = 9
;;FIND
;; (find #\b "abcbe b f" :test 'equal) = #\b 
;; (find #\b  '(a b  c d)) = NIL
;;
;;Function REPLACE
;;  replace sequence-1 sequence-2 &key start1 end1 start2 end2 => sequence-1
;;Destructively modifies sequence-1 by replacing the elements of subsequence-1 bounded by start1 and end1 with the elements of subsequence-2 bounded by start2 and end2.
;; NO (replace 'string "\\n" (concatenate  (string #\\)(string #\n)) "this is an ntest\\n" :test 'equal)

;;xxx SUBSTITUTE
;;THESE WORK
;; (substitute  #\x  #\d  "abcdef g h" :test 'equal) = "abcxef g h"
;; (substitute  #\space  #\d  "abcdef g h" :test 'equal) = "abc ef g h"
;; (substitute  #\newline  #\n "abcdef g h \n  x y z" :test 'equal) = "abcdef g h 
;;  x y z"

;;THESE DON'T WORK
;; (substitute  #\x#\y  #\d#\e  "abcdef g h" :test 'equal)
;; (substitute "xy"  "bc"  "abcdef g h" :test 'equal) = 
;;  (substitute (string #\newline) "\n"  "   \"INSTRUCTIONS FOR ALL QUESTIONS ON THIS SCALE:\\n\"+" :test 'equal) = doesn't work
;;  (substitute  "this" "all"  "   \"INSTRUCTIONS FOR ALL QUESTIONS ON THIS SCALE:\\n\"+" :test 'equal) = doesn't work
;;  (substitute  "this" "all "  "   \"INSTRUCTIONS FOR ALL QUESTIONS ON THIS SCALE:\\n\"+" :test 'equal) = doesn't work


;;xxx NUMBERS
;; (string-equal 7 "7")  Error: Cannot coerce 7 to type STRING.
;;  (equal 7 "7") = NIL

;;xxx READ-LINE
;;  (test-read-line)
#|(defun test-read-line ()
  (let
      (( lines "line1 \" \"   a b #\space
            line2    ")
       (input1)
       (input2)
       )
  (setf input1 (read-line (setq input-stream (make-string-input-stream lines))))
  (setf input2 (read-line  input-stream (make-string-input-stream lines)))
  (values input1 input2)
        ))|#
;;works, returns:
#|   "line1 \" \"   a b #\space"
"            line2    "|#



;;XXX
;;
;;COERCE object result-type => result
;;Examples:
#| 
;;list to vector,string,
(coerce '(a b c) 'vector) =>  #(A B C)
(coerce '(a b c) 'string) => error
(string #\A) = "A"
;;symbol to char
 (coerce 'a 'character) =>  #\A
;;numbers
 (coerce 4.56 'complex) =>  #C(4.56 0.0)
 (coerce 4.5s0 'complex) =>  #C(4.5s0 0.0s0)
 (coerce 7/2 'complex) =>  7/2
 (coerce 0 'short-float) =>  0.0s0
 (coerce 3.5L0 'float) =>  3.5L0
 (coerce 7/2 'float) =>  3.5
;; cons
 (coerce (cons 1 2) t) =>  (1 . 2)
 (coerce 7 'string) = Error: Cannot coerce 7 to type STRING.
|#



;;000---------------------------------------------------- OLDER ---------------------------------------------


#|
;;(DEFINE-INSTANCE *engl100 (:is engl100))
;;(DEFINE-INSTANCE *math122 (:is math122))


;;ADD-MARGINS moved to r-intro because it wouldn't compile right


;;ADD-MARGINS
;;
;;NEW 10/90
;;
;;-->NOTE: IF RIGHT-MARGIN-PLACE IS NIL, THEN WILL USE "NATURAL" RIGHT MARGINS
;;
;;(add-left-margins (string left-margin-spaces  pstream 
;;			&key right-margin-place new-string print-to-screen)

;; note: ADD-LEFT-MARGINS in planner uses this function wholly
;;(add-margins "(Note: FIN464 is not required for the Real Estate concentration.)" 5 t :right-margin-place 64)
;;
;(defun add-margins (string left-margin-spaces  pstream 
;			&key right-margin-place new-string print-to-screen)
;
;   (list "TSTRING--ADDS RIGHT MARGINS AT NEAR SPACE TOO")
;   (let* 
;	 ((pre-string 
;	    (make-string  left-margin-spaces  :initial-element #\space ))
;	 ;;(string-stream (make-string-input-stream string))
;	 (line-string)
;	 (end-place)
;	 (string-length (length string))
;	 (rest-string)
;	 (space-place)
;	 (current-right-margin right-margin-place)
;	)
;
 ;    (cond
;       (current-right-margin
;	 (setf string (substitute #\space #\newline string :test 'equal))
;	 
;	 ;;MAKE SURE THE RIGHT MARGIN IS NOT PAST THE STRING LENGTH
;	 (cond
;	   ((<= string-length current-right-margin)
;	     (setf current-right-margin string-length)
;	     )
;
;	 ;;OTHERWISE--FIND A BLANK-SPACE NEAR THE RIGHT MARGIN
;	   ((setf space-place (search " "  string 
;				       :start2 (- current-right-margin 6)
;				       :end2 (+ current-right-margin 6)))
;
;	     (setf current-right-margin space-place)
;	     )
;	   (t nil))
;
;;; (print `(rigtht-margin-place ,right-margin-place str-lenth ,string-length))
;
;	 (setf line-string (subseq string 0 current-right-margin))
;
;	 (unless (<= string-length current-right-margin)
;	       (setf  rest-string (subseq string (+ current-right-margin 1) )))
;	 )
 ;     ((setf end-place  (search (string #\newline) string))
;	(setf line-string (subseq string 0 end-place)
;	      rest-string (subseq string (+ end-place 1)))
;	)
;       (t
;	(setf end-place string-length
;	      line-string (subseq string 0 end-place)   
;	      rest-string nil)
;	))
;
;
;	(setf line-string (format nil "~A~A" pre-string line-string))
;
;;;  (print `(end-place ,end-place line-string ,line-string rest-string ,rest-string))
;
;	(cond
;	    (new-string
;	      (if line-string
;	        (setf new-string (format nil "~A~A~A" 
;					  new-string #\newline line-string)))
;	      )
;	    (t (setf new-string (format nil "~A"  line-string ))))
;
;	(cond
;	  (rest-string 
;	  (setf new-string
;	     (add-left-margins rest-string left-margin-spaces t :new-string new-string
;			       :right-margin-place right-margin-place))
;;;;	  (my-substitute (string #\newline) "   " "this is a      this is second" )
;
;	  )
;	  (t nil))
;
;	(unless (equal pstream t)
;	  (setf new-string (format nil "~A~A" new-string  #\newline))
;	  (format pstream "~A" new-string)
;	  ;;done in larger function
;	  (if print-to-screen
;	      (format t "~A" new-string))
;
;	  )
;	new-string
;	))
; 

 |#

#|
(setf *test-shaq-list '(
   ("tknowmor	t-Want to know more of self")
         ("texperie	t-Experienced self-help user")
         ("twanttho	t-Want thorough assessment")
         ("twantspe	t-Want specific help")
         ("tworknga	t-worknga")
         ("tu100stu	t-CSULB U100 student")
         ("tcsulbst	t-CSULB other student")
         ("totherst	t-Other student")
         ("tcolstu	t-Other college student")
         ("tinstruc	t-Instructor")
         ("tcolfaca	t-College faculty-admin")
         ("twanthel	t-Want help with problem")
         ("wantspq	g-Specific questionnaire")
         ("gsuchap	g-Success-happiness")
         ("gacadsuc	g-Academic success")
         ("gemocop	g-Emotional coping")
         ("gslfest	g-Self-esteem")
         ("gprocrst	g-Procrastination")
         ("gtimeman	g-Time Management")
         ("grelat	g-Relationships")
         ))

 (setf *test-shaq-list2
       '(
                  ("Email	")
         ("ZipCode")	
         ("Nation	")
         ("HrsWork")
         ))
|#


#|
;;OLD VERSION
(defun my-substitute2 (newitem olditem sequence)
  "In U-tstring.lisp. Note: number of spaces in newitem and olditem MUST? BE SAME--add spaces, etc if necessary?"
  (let
      ((newitem-str " ")
       (olditem-str " ")
       (olditem-length)
       (sequence-str " ")
       (seq-length (length sequence))
       (n-match 0)
       (new-seq "")
       (new-char "")
       ;; (old-char0 "")
       (old-char "")
       )
    (cond
     ((and (setf newitem-str (string newitem)
                 olditem-str (string olditem)
                 sequence-str (string sequence)))
      (setf  olditem-length (length olditem-str))
      (dotimes (n seq-length)
        ;;was (setf old-char0 (char sequence-str n))
        (setf old-char (string (char sequence-str n)))
     ;;   (afout 'out (format nil "newitem-str= ~A~% olditem-str= ~A~% sequence-str= ~A~% old-char= ~A~% newitem-str= ~A~%new-seq= ~A~% seq-length= ~A olditem-length= ~A~% n= ~A~% "newitem-str olditem-str sequence-str  old-char newitem-str new-seq seq-length olditem-length n))	
        (cond
         ((string-equal old-char (char olditem-str n-match))  ;;was old-char0
          (setf new-char (string (char newitem-str n-match)))
          (incf n-match )	
          (cond
           ((= olditem-length n-match)  ;;was  (length olditem-str) n-match)
            (setf new-seq (format nil "~A~A" new-seq newitem-str)
                  n-match 0))
           (t nil)))
         (t (setf n-match 0
                  new-char old-char
                  new-seq (format nil "~A~A" new-seq new-char))))
        )
      new-seq)
     (t (print `(ERROR--ONE OF THE OBJECTS==> ,newitem-str ,olditem-str OR ,sequence-str  IS NOT A STRING--IN QUOTES))))
    ))

|#;; (setf **a "a"  **b "  ")
;; (length (format t "~A~A" **a **b)) ;;" "" " ")
;; (char " " 0)


;; OLD VERSION -- MOSTLY WORKS
#|(defun my-delete-substring (substring string)
  "In U-tstring.lisp, Deletes the first substring. RETURNS (values new-string first-string rest-string substring) new-string = nil if no match."
  (let
      ((char)
       (char-str)
       (length-str (length string))
       (length-ss (length substring))
       (ss-n 0)
       (ss-char-str "")
       (matched-str "")
       (continuous-match-p)
       (first-string "")
       (rest-string "")
       (new-string)
       )
    (loop
     for n from 0 to length-str
     do
     (unless (and (> ss-n 0)(>= ss-n length-ss))
       (setf ss-char-str (string (char substring ss-n))
             char (char string n)
             char-str (string char)))
#|     (setf last-string (format nil "~A~A" first-string char-str
      rest-string|#
     (cond
      ((and (= ss-n 0) (string-equal char-str ss-char-str) )
       (setf continuous-match-p t
             ss-n 1
        matched-str (format nil "~A~A" matched-str char-str)))
     ((= length-ss (+ ss-n 1))
       (setf rest-string (subseq string (+ n 1))
             first-string (subseq string 0  (- length-str (+ n 2)))  ;;  length-ss  1))
             new-string (format nil "~A~A" first-string rest-string))
       (return))
      ((and continuous-match-p (string-equal char-str ss-char-str))
      (if (< ss-n length-ss) (incf ss-n))
       (setf matched-str (format nil "~A~A" matched-str char-str)))
     (t  (setf ss-n 0
               matched-ss ""
               continuous-match-p nil)))   
  ;;  (afout 'out (format nil "char-str= ~A~% length-str= ~A~% length-ss= ~A~% ss-n= ~A~% ss-char-str= ~A~% matched-str= ~A~%  continuous-match-p= ~A~% first-string= ~A~% rest-string= ~A~% new-string= ~A~%" char-str length-str length-ss ss-n ss-char-str matched-str  continuous-match-p first-string rest-string new-string))
          
     ;;end loop
     )
    ;;end my-delete-substring
    (values new-string first-string rest-string substring)
    ))|#

