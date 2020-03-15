;;************************** U-lists.lisp ****************************
;;= n
;;SSS NOTE:  INSERT VALUE EITHER 1.AFTER INNER KEY, 2. NTH IN LIST, OR 3. APPEND VALUE TO LIST CONTAINING KEY=FIRST
;;(KEY  XXX :KEY VALUE  ...) OR (KEY .. NTH= VALUE..) OR (KEY .. (APPEND VALUE)
;;
;; ** ALSO SEE  U-symbol-trees.lisp FOR LIST TREE CREATING AND MODIFYING FUNCTIONS-- uses ART conventions, but generalizable
;;





;;GET-KEY-LIST
;;
;;ddd
(defun get-key-list (key list)
  "In U-lists, searches a flat list for a sublist that begins with key, RETURNS (values list  key) if found, nil if not. Uses my-equal which will even match symbols and strings."
  (let
      ((result)
       (first-item)
       )
    (loop
     for sublist in list
     do
     (cond
      ((and (listp sublist)(my-equal key (car sublist)))
       (return (setf result sublist)))
      (t nil))
     ;;end loop
     )
    (values result key)
    ;;end let, get-key-list
    ))
;;TEST
;;  (get-key-list 'this '(a (that b c) (4 5 6)("this" l m)(w m 2))) = ("this" L M)


;;GET-NTHS-IN-LISTS
;;
;;ddd
(defun get-nths-in-lists (nth-list list-of-lists)
  "In U-lists, finds corresponding nth-n nth to list-n instead of same nth for all lists. Uses mapcar."
    (mapcar #'nth nth-list list-of-lists)
    ;;end let, get-nths-in-lists
    )
;;TEST
;; (get-nths-in-lists '(0 1 2) '((a b c)(d e f)(g h i))) = (A E I)
;; following have unmatched list lengths
;; (get-nths-in-lists '(0 1 2) '((a b c)(d e f)(g h i) (j k l))) = (A E I)
;; (get-nths-in-lists '(0 1 3) '((a b c)(d e f)))  = (A E)


;;GET-NTH-IN-ALL-LISTS
;;
;;ddd
(defun get-nth-in-all-lists (nth list-of-lists)
  "In U-lists, RETURNS all nth items from lists in list-of-lists"
  (let
      ((list)
       (item)
       (return-list)
       )
    (loop
     for list in list-of-lists
     do
     (when (listp list)
       (setf item (nth nth list))
       (if item (setf return-list (append return-list (list item))))
       ;;end when, loop
       ))
    return-list
    ;;end let, get-nth-in-all-lists
    ))
;;TEST
;;  (get-nth-in-all-lists 0  '((a b c)(1 2 3)(m n o p) x)) = (A 1 M)
;;  (get-nth-in-all-lists 3  '((a b c)(1 2 3)(m n o p) x)) = (P)


;;GET-ALL-NTHS-IN-LISTS--DEPRECIATED, but works
;;
;;ddd
(defun get-all-nths-in-lists (nth list-of-lists)
  "In U-lists, DEPRECITED, REPLACED BY get-nth-in-all-lists RETURNS all nth items from lists in list-of-lists"
  (get-nth-in-all-lists nth list-of-lists)
  )
;;TEST
;;  (get-all-nths-in-lists 0  '((a b c)(1 2 3)(m n o p) x)) = (A 1 M)
;;  (get-all-nths-in-lists 3  '((a b c)(1 2 3)(m n o p) x)) = (P)



;;GET-NTH-IN-KEYLIST
;;
;;ddd
(defun get-nth-in-keylist (key nth list)
  "In U-lists, searches a flat list for a sublist beginning with key, RETURNS (values nth-element keylist) where nth is the nth item in the sublist."
  (let*
      ((keylist (get-key-list key list))
       (nth-element (nth nth keylist))
       )
    (values nth-element keylist)
    ;;end let get-nth-in-keylist
    ))
;;TEST
;;  (get-nth-in-keylist 'key1  2  '(a b (1 2 3) x (key1 9 10 11 12) x y))
;; works = 10    (KEY1 9 10 11 12)    


;;(nth 3 '(1 2 3 4 5))

;;GET-NTH-IN-2NESTED-LIST
;;
;;ddd
(defun get-nth-in-nth-2nested-list (nth nth-item 2nested-list &key match-item)
  "In U-lists. RETURNS (values all-found-nested-lists  all-found-matched-lists) using nth item in a single nested list. nth-item must be a list. Both nth starts with 0. 2nested-list is a list of nested lists.  eg. ((a b c)(d e (1 X 3))(g h (1 Y 3))) can find x, y."
  (let
      ((found-item)
       (matched-sublist)
       (matched-nested-list)
       (found-list)
       (found-nested-list)
       (all-found-nested-lists)
       (all-found-matched-lists)
       )
    (loop
     for nested-list in 2nested-list
     do
     (multiple-value-setq (found-item matched-sublist)
         (get-nth-in-nth-nested-list nth nth-item nested-list :match-item match-item))
     (afout 'out1 (format nil "nested-list= ~A~% found-item= ~A~% matched-sublist= ~A~%" nested-list found-item matched-sublist))
     (cond
      ((and match-item matched-sublist)
       (setf found-nested-list nested-list             
             matched-nested-list nested-list
       all-found-nested-lists (append all-found-nested-lists (list nested-list))
       all-found-matched-lists (append all-found-matched-lists (list nested-list)))
       )
      (found-item
       (setf found-nested-list nested-list             
       all-found-nested-lists (append all-found-nested-lists (list nested-list)))
       )
      (t nil))

     ;;end loop
     )
    (values all-found-nested-lists  all-found-matched-lists)
    ;;end let, get-nth-in-nth-2nested-list
    ))
;;TEST
;; (progn (setf out1 nil) (get-nth-in-nth-2nested-list 4  3 '((1 (40 NIL) "Wup1-3" (1 3 2 TO 1 1 3)) (1 (80 NIL) "Wup1-3" (1 3 2 TO 2 1 3)) (1 (120 NIL) "Wup1-3" (1 3 2 TO 3 1 3)) (1 (160 NIL) "Wup1-3" (1 3 2 TO 4 1 3)) (1 (200 NIL) "Wup1-3" (1 3 2 TO 5 1 3)) (2 (240 NIL) "Wup1-3" (2 3 2 TO 1 1 3)) (2 (280 NIL) "Wup1-3" (2 3 2 TO 2 1 3)))))
;;result= ((1 (40 NIL) "Wup1-3" (1 3 2 TO 1 1 3)) (1 (80 NIL) "Wup1-3" (1 3 2 TO 2 1 3)) (1 (120 NIL) "Wup1-3" (1 3 2 TO 3 1 3)) (1 (160 NIL) "Wup1-3" (1 3 2 TO 4 1 3)) (1 (200 NIL) "Wup1-3" (1 3 2 TO 5 1 3)) (2 (240 NIL) "Wup1-3" (2 3 2 TO 1 1 3)) (2 (280 NIL) "Wup1-3" (2 3 2 TO 2 1 3)))  NIL
;; USE :MATCH-ITEM 2
;; (progn (setf out1 nil) (get-nth-in-nth-2nested-list 4  3 '((1 (40 NIL) "Wup1-3" (1 3 2 TO 1 1 3)) (1 (80 NIL) "Wup1-3" (1 3 2 TO 2 1 3)) (1 (120 NIL) "Wup1-3" (1 3 2 TO 3 1 3)) (1 (160 NIL) "Wup1-3" (1 3 2 TO 4 1 3)) (1 (200 NIL) "Wup1-3" (1 3 2 TO 5 1 3)) (2 (240 NIL) "Wup1-3" (2 3 2 TO 1 1 3)) (2 (280 NIL) "Wup1-3" (2 3 2 TO 2 1 3))) :match-item 2))
;;RESULTS= ((1 (40 NIL) "Wup1-3" (1 3 2 TO 1 1 3)) (1 (80 NIL) "Wup1-3" (1 3 2 TO 2 1 3)) (1 (120 NIL) "Wup1-3" (1 3 2 TO 3 1 3)) (1 (160 NIL) "Wup1-3" (1 3 2 TO 4 1 3)) (1 (200 NIL) "Wup1-3" (1 3 2 TO 5 1 3)) (2 (240 NIL) "Wup1-3" (2 3 2 TO 1 1 3)) (2 (280 NIL) "Wup1-3" (2 3 2 TO 2 1 3)))
;;((1 (80 NIL) "Wup1-3" (1 3 2 TO 2 1 3)) (2 (280 NIL) "Wup1-3" (2 3 2 TO 2 1 3)))



;;GET-NTH-IN-NTH-NESTED-LIST
;;
;;ddd
(defun get-nth-in-nth-nested-list (nth nth-item nested-list &key match-item)
  "In U-lists. Returns the nth item in a single nested list. nth-item must be a list. Both nth starts with 0. RETURNS (values found-item matched-list). If match-item, returns matched-list if found-item my-equal match-item."
  (let
      ((found-item)
       (found-list (nth nth-item nested-list))
       (matched-sublist)
       )
    (when (listp found-list)
      (setf found-item (nth nth found-list)))
    (when (and match-item (my-equal match-item found-item))
      (setf matched-sublist found-list))
    ;;(break)
    (values found-item matched-sublist)
    ;;end let, get-nth-in-nth-nested-list
    ))
;;TEST
;; (get-nth-in-nth-nested-list  3  2  '(1 2 (a b c d) 4 5)) = D
;; (get-nth-in-nth-nested-list  3  2  '(1 2 (a b c d) 4 5) :match-item  "d")    
    




;;GET-KEY-VALUE-IN-SINGLE-NESTED-LIST
;;
;;ddd
(defun get-key-value-in-single-nested-list (key single-nested-list &key return-list-p)
  "In U-lists, uses get-key-value-in-nested-lists. RETURNS (values return-value return-key extra-return-nth-items extra-items-list) OR entire list with key if return-list-p. KEY must be first item in target list."
   (get-key-value-in-nested-lists (list (list key 0))  single-nested-list :return-list-p return-list-p)
   )
;;TEST
;; (get-key-value-in-single-nested-list 'key1 '((x x x)b (key0 a b)(key1 d e f)(key2 1 2)))
;;works= D  KEY1  NIL  (B)
;;  (get-key-value-in-single-nested-list 'key1 '((x x x)b (key0 a b)(key1 d e f)(key2 1 2)) :return-list-p t) 
;;works= (KEY1 D E F)  KEY1  NIL  (B)

;;GET-KEY-VALUE-IN-NESTED-LISTS
;; gets values in all sorts of lists and nested lists
;;
;;ddd
(defun get-key-value-in-nested-lists (key-spec-lists nested-lists 
                                                      &key return-list-p no-return-extra-p
                                                      final-key-inside-keylist-p) ;; return-nth (now in spec)
  "In U-lists.lisp, DEPRECIATED--(Replaced by get-keyvalue-in-nested-list ) RETURNS first value that matches key (values return-value key extra-return-nth-items extra-items). The spec-lists is a list of 2 or 3 item spec lists. (key find-nth return-nth)  If  key = T, searches entire list of lists for key if find-nth = T, searches entire sublist for the key.  If return-list-p, RETURNS entire key-list as return-value. Extra items are extra non-list items on all level lists that might contain important info. Can process infinite? levels of key lists. Can match with equal or string-equal automatically. The spec-lists are arranged from outer-most list to inner-most list. final-key-inside-keylist-p means that the last key is inside a list--so items in ourer list won't cause a false match."
  (let*
      ((return-value)
       (return-key)
       (extra-items-list)
       (extra-return-nth-items)
       (spec-list (car key-spec-lists))
       (key (first spec-list))
       (find-nth (second spec-list))
       (return-nth (third spec-list))
       (new-spec-lists (cdr key-spec-lists))
        (extra-items1 )
        (extra-return-nth-items1 )
        (current-level-return-extra-value1 )
        ( match-item2 )
        (extra-items1 )
        (length-item2 )
       ( extra-return-nth-items1 )
       ( extra-items1)
       ( list-length3)
       ( match-item3)
       )
    (unless  find-nth (setf find-nth 0))
    (unless return-nth (setf return-nth (+ find-nth 1)))
 
    (cond
     ;;SPEC IS TO SEARCH EVERY LIST AT THIS LEVEL
     ;;yyy
     ((or (equal key t) (equal find-nth t))
      ;;(afout 'out (format nil "NEW CALL TO T  key= ~S~% (car of list= ~S~%" key (car nested-lists)))   
      (loop
       for item  in nested-lists
       do
       ;;(afout 'out (format nil "T OUTER-LOOP new-spec-lists= ~S~%  item= ~S~%" new-spec-lists item))
       ;;Should it return a  value from this level (eg name))
       (if (and return-nth (listp item))
           (setf current-level-return-extra-value1 (nth return-nth item)))

       ;;test to see what the spec-list indicates
       (cond
        ((and item (listp item))
         ;;note: this may call other recursive calls
         (multiple-value-setq (return-value return-key extra-return-nth-items1 extra-items1)
             (get-key-value-in-nested-lists   new-spec-lists  item
                                              :return-list-p return-list-p
                                              #|:return-nth return-nth|# 
                                              :no-return-extra-p no-return-extra-p
                                              :final-key-inside-keylist-p final-key-inside-keylist-p))

         ;;these are the extra items want to return (bec inside of target containing list)         
         (cond
          (return-key  
           ;;add extra items from list of item contaning the matched key
           (unless no-return-extra-p
           (if extra-items1 (setf extra-items-list (append extra-items-list (list extra-items1))
                                  extra-return-nth-items (append extra-return-nth-items (list extra-return-nth-items1)))))
           (return))
          (t nil))
         ;;end item clause
         )
        ;;may be non-list items such as other keys providing info re: found outer list.
        (t nil)) ;; (if item (setf extra-items-list (append extra-items-list (list item))))))
       ;;end loop, find-nth = t
       ))
     ;;SSS
     ;;AT LOWEST LIST, SEARCH FOR KEY-VALUE
     ((and (null new-spec-lists) key)
      (loop
       for item in nested-lists
       do
       ;;(afout 'out (format  nil "1 LOWEST LEVEL TEST key= ~S  match-item= ~S~% find-nth= ~S~% " key match-item find-nth ))
       ;;(afout 'out (format  nil "1 LOWEST LEVEL TEST key= ~S  match-item= ~S~% find-nth= ~S~%nested-lists= ~S~% IN find-key-value-in-lists " key match-item find-nth nested-lists))

       (cond
        ;;SEARCH INSIDE A NESTED LIST -- NOT IN OUTER LIST FOR KEY
        ((and (listp item) (null final-key-inside-keylist-p))
         (setf  length-item2 (list-length item))       
         (unless (>= find-nth  length-item2))
         (setf match-item2 (nth find-nth item))
         (cond
          ((my-equal key match-item2)
           (cond
            (return-list-p
             (setf return-value item
                   return-key match-item2))
            (t
             (setf return-value (nth return-nth item)
                   return-key key)))
           (return))
          (t nil))
         ;;end listp item
         )
        ;;if not list, search current list for item, just check to see if item = key
        ;;problem if an item matches key but real key is inside a later list
        ;;SEARCHES OUTER LIST FOR KEY MATCH--NOT AN INNER LIST
        (final-key-inside-keylist-p
         (cond
          ((my-equal key item)
           ;;was (or (equal key item) (if (stringp item) (string-equal key item)))
           (cond
            (return-list-p 
             (setf return-value nested-lists
                   return-key item))
            (t
             (setf return-value (nth return-nth nested-lists)
                   return-key key)))
           (return))
          (t (if  item (setf extra-items-list (append extra-items-list (list item)))))))
         ;;problem
        (t (if item (setf extra-items-list (append extra-items-list (list item)))))
        ;;end cond, loop, equal null new-spec-lists
        )))
     ;;SPEC IS TO SEARCH THIS LEVEL (not last level) BY KEY at FIND-NTH
     ((and  new-spec-lists key)
      ;;(afout 'out (format nil "OUTER-LOOP SEARCH new-spec-lists= ~S~%" new-spec-lists))         
      ;;check each list at this level lll
      (loop
       for item  in nested-lists
       ;;  with new-spec-list1
#|       with extra-return-nth-items1 )
       with extra-items1 
       with list-length3  
       with match-item3|#
       do
       ;;for each sublist, check find-nth
       ;;(afout 'out (format nil "KEY OUTER-LOOP key= ~S~% item= ~S~%new-spec-lists= ~S~%" key item new-spec-lists))
       (cond
        ((listp item)
         (setf  list-length3 (list-length item))     
         (unless (>= find-nth  list-length3))
         (setf match-item3 (nth find-nth item))
         ;;(afout 'out (format  nil "OUTER LOOP TESTING key= ~S  match-item3= ~S~%  IN find-key-value-in-lists" key match-item3))
         (cond
          ((my-equal key match-item3)
           ;;(or (equal key match-item) (if (stringp match-item) (string-equal key match-item)))
           (multiple-value-setq (return-value return-key extra-return-nth-items1 extra-items1)
               (get-key-value-in-nested-lists  new-spec-lists item
                                              #| :return-nth return-nth|#
                                               :return-list-p return-list-p
                                               :no-return-extra-p no-return-extra-p))
           ;;(afout 'out (format nil "ON RECURSE RETURN return-value= ~S~% return-key=~S~%" return-value return-key))
           (unless no-return-extra-p
             (if extra-items1 
                 (setf extra-items-list (append extra-items-list (list extra-items1))))
             (if extra-return-nth-items1
                  (setf extra-return-nth-items (append extra-return-nth-items 
                                                      (list extra-return-nth-items1)))))          
           (return))
          (t nil))
         ;;end listp item
         )
        (t (if item (setf extra-items-list (append extra-items-list (list item))))))
       ;;end loop, equal new-spec-lists
       ))
     ;;IF TOP LEVEL NONE-OF-ABOVE
     (t nil)) ;;no items at top level 
    ;;end find-key-value-in-lists
    (if  no-return-extra-p
        (setf extra-items-list nil))
    (values return-value return-key extra-return-nth-items extra-items-list)
    ;;end let, get-key-value-in-nested-lists
    ))
;;TEST
;; (get-key-value-in-nested-lists '((T 0) (THVSELFAQ 0)) *all-shaq-questions  :return-list-p t)
;;works =  (THVSELFAQ ("There are one or more aspects (or parts) of myself that I have a hard time accepting or do not like.") TBV-INSTR THVSELFACCEPTQ)     THVSELFAQ      (NIL)     ((TBV))
;;TEST
;;(get-key-value-in-nested-lists '((T 0) ("iecicont" 0)) *shaq-question-variable-lists :return-list-p t)
;;WORKS =  ("iecicont" "ie-I control life-happiness" "spss-match" "iecILOFCiVSe" ("iecILOFCiVSe" "2" "iecILOFCiVSeQ" "int" "Agree7" "questionInstancesArray" "frameTitle" "frameDimWidth" "frameDimHeight" "iIEcontrol.java") (:HELP NA NA))     "iecicont"   (NIL)    ((IE))
;;(get-key-value-in-nested-lists '((T 0) ("userid"  0)) *shaq-question-variable-lists :return-list-p t)  =  ("UserID" "UserID" "spss-match?" NO-PC-INST-MATCH (:HELP NA NA))    "UserID"    (NIL)   ((ID))
;; (get-key-value-in-nested-lists  (list (list 1 1))  '((:DIMSIM 1 ((1 (40 110) WUP1-1 (1 1)))) (:DIMSIM 2 ((2 (80 200) WUP1-2 (2 1)))))   :return-list-p T)
;;works = (:DIMSIM 1 ((1 (40 110) WUP1-1 (1 1))))

;;yyy
;; (progn (setf out nil) (get-key-value-in-nested-lists '((t 0)("smtsdevelopment" 0)) *all-shaq-pc-instances :return-list-p t))
;;  (progn (setf out nil)  (get-key-value-in-nested-lists  '(("iAcademicMotivation.java" 1)( "acmESOCSTudy" 0)) '((PC-INSTANCES  "iAcademicMotivation.java"      ("[]questionInstancesArray1)")      ("acmNDROPcourses" "30" "acmNDROPcoursesQ" "int" "FrAnswerPanel.LikeMe7" "questionInstancesArray" "frameTitle" "frameDimWidth" "frameDimHeight")      ("acmESOCSTudy" "3" "acmESOCSTudyQ" "int" "FrAnswerPanel.LikeMe7" "questionInstancesArray" "frameTitle" "frameDimWidth" "frameDimHeight"))))) ;;(fout out)) ;; :return-list-p  t))
;;works, returns"acmESOCSTudyQ"  "acmESOCSTudy"  ((PC-INSTANCES "iAcademicMotivation.java"))
;;
;; (progn (setf out nil) (get-key-value-in-nested-lists (list '(T 0)'(smtExercizeQ 0)) *testq-vars3)));; (fout out) *all-shaq-questions)))
;; (progn (setf out nil) (get-key-value-in-nested-lists '((T 0) (thvUncondCareQ 0)) *all-shaq-questions))
;;(get-key-value-in-nested-lists '((T 0) (ugoals 0)) *shaq-question-variable-lists :return-list-p t)

;;  (get-key-value-in-nested-lists '((5 0))  '((1 0 (1 2 A)) (5 1 (1 6 (QUOTE (A B))))))
;;  works= 1 5 NIL NIL
;;  (get-key-value-in-nested-lists '((this 0))  '((a b)(c (1 2))(this (3 4 5))(x y)))
;; works, returns= (3 4 5)  THIS NIL
;;  (get-key-value-in-nested-lists '((x 0))  '((a b)(c (1 2))(this (3 4 5))(x y)) )
;; works, returns Y X NIL
;;   (get-key-value-in-nested-lists '((5 0))  '((1 0 (1 2 A)) (5 1 (1 6 (QUOTE (A B)))))) = 1 5 NIL
;; (get-key-value-in-nested-lists  '((5 0)) '((1 0 (1 2 A)) (5 1 (1 6 (QUOTE (A B))))) :return-list-p t)
;; works, returns (5 1 (1 6 (QUOTE (A B))))  5 NIL
;; SSS
;; (progn (setf out nil) (get-key-value-in-nested-lists  '((5 0)(1 0)) '((1 0 (1 2 A)) (5 1 (1 6 (QUOTE (A B))))) :return-list-p t))(fout out))
;;works, returns  (1 6 (QUOTE (A B)))  1 NIL
;; (get-key-value-in-nested-lists  '((5 0)(1 0))  '((1 0 (1 2 A)) (5 1 (1 6 (QUOTE (A B))))) :return-nth 2) 
;;works, returns (1 6 (QUOTE (A B)))  1 NIL
;; (nth 0 '((1 0 (1 2 A)) (5 1 (1 6 (QUOTE (A B)))))) (1 0 (1 2 A))
;; (progn (setf out nil)  ( get-key-value-in-nested-lists '(( "iWorldviewFears.java" 1)("wovNoLove" 0)) *all-shaq-pc-instances  :return-list-p t))  
;;works, returns= ("wovNoLove" "16" "wovNoLoveQ" "int" "FrAnswerPanel.Fear7" "questionInstancesArray" "frameTitle" "frameDimWidth" "frameDimHeight")"wovNoLove"  NIL     (PC-INSTANCES "iWorldviewFears.java")
;; (progn (setf out nil)  (get-key-value-in-nested-lists "wovNoLove" *all-shaq-pc-instances :find-outer-key "iWorldviewFears.java" :find-nth-first 1)) ;;  :return-list-p t))
;;works, = "16"  "wovNoLove"  NIL  (PC-INSTANCES "iWorldviewFears.java")
;;
;; (progn (setf out nil) (multiple-value-setq (*testfn1 *testfn2 *testfn3 *testfn4) (get-key-value-in-nested-lists "wovNoLove" *all-shaq-pc-instances  :find-outer-key  t   :return-list-p t) )(fout out)))
 
;;  (progn (setf out nil)  (get-key-value-in-nested-lists    "acmESOCSTudy" '(PC-INSTANCES  "iAcademicMotivation.java"      ("[]questionInstancesArray1)")      ("acmNDROPcourses" "30" "acmNDROPcoursesQ" "int" "FrAnswerPanel.LikeMe7" "questionInstancesArray" "frameTitle" "frameDimWidth" "frameDimHeight")      ("acmESOCSTudy" "3" "acmESOCSTudyQ" "int" "FrAnswerPanel.LikeMe7" "questionInstancesArray" "frameTitle" "frameDimWidth" "frameDimHeight")) :return-list-p  t))
;;works, returns ("acmESOCSTudy" "3" "acmESOCSTudyQ" "int" "FrAnswerPanel.LikeMe7" "questionInstancesArray" "frameTitle" "frameDimWidth" "frameDimHeight")  "acmESOCSTudy"  NIL  (PC-INSTANCES "iAcademicMotivation.java")

;;  (get-key-value-in-nested-lists 'this  '((a b)(c (1 2))(this (3 4 5))(x y)))
;; works, returns= (3 4 5)  THIS NIL NIL
;;  (get-key-value-in-nested-lists 'x '((a b)(c (1 2))(this (3 4 5))(x y)))
;; works, returns Y X NIL NIL
;;   (get-key-value-in-nested-lists 5  '((1 0 (1 2 A)) (5 1 (1 6 (QUOTE (A B)))))) = 1 5 NIL NIL
;; (get-key-value-in-nested-lists 5  '((1 0 (1 2 A)) (5 1 (1 6 (QUOTE (A B))))) :return-list-p t)
;; works, returns (5 1 (1 6 (QUOTE (A B))))  5 NIL NIL
;; (get-key-value-in-nested-lists 5  '((1 0 (1 2 A)) (5 1 (1 6 (QUOTE (A B))))) :return-nth 2) 
;;works, returns (1 6 (QUOTE (A B)))  5 NIL NIL
;; (nth 0 '((1 0 (1 2 A)) (5 1 (1 6 (QUOTE (A B)))))) (1 0 (1 2 A))
;; (get-key-value-in-nested-lists  '((2 1)) '((dim 1 xx)(dim 2 bb)(dim 3 yy)) :return-list-p T) 
;;works = (DIM 2 BB) 2 nil nil



;;REPLACE-KEYLIST
;;
;;ddd
(defun replace-keylist (key nested-lists replacement &key (key-n 0))
  "In U-lists, Replaces the first matched keylist in nested-lists (matches key to key-n item) with replacement (list or nonlist). RETURNS (values new-nested-lists matched-list. Nested list may contain non-list items. Uses my-equal to test."
  (let
      ((matched-keylist)
       (new-nested-lists)
       )
    (loop
     for item in nested-lists
     do
     (cond
      ((and (listp item)
                (my-equal key (nth key-n item)))
       (setf matched-keylist item
             new-nested-lists (append new-nested-lists (list replacement))))
      (t (setf new-nested-lists (append new-nested-lists (list item)))))
     ;;end loop
     )
    ;;end let, replace-keylist
    (values new-nested-lists  matched-keylist)
    ))
;;TEST
;;  (replace-keylist 'key1 '(x (key0 a b c)((nested))(key1 d e f) 77 (key2 l l)) '(replacement-list))
;; (my-equal 'key1 (nth 0 '(key1 d e f)))
;;works = (X (KEY0 A B C) ((NESTED)) (REPLACEMENT-LIST) 77 (KEY2 L L))    (KEY1 D E F)


  

;;APPEND-GROUP-WITH-SAME-BEGIN
;;
;;ddd
(defun append-groups-with-same-begin (begin-keys  keylist  groups 
                                                 &key append-nth-match-only (append-nested-p T)
                                                 (append-last-group-list-p T) (append-if-not-found T))
  "In U-lists. Appends the group within groups with same begin keys.  If no group exists and append-if-not-found-p, then makes a new group with keylist. RETURNS (values new-groups matched groups). Only appends first group found. Unless append-nth-match-only, appends ALL matching occurances; if append-nth-only appends only the nth item. append-nested-p removes extra parens in lists to append keylist. Processes LARGE VARIETY OF APPEND TYPES."
  (let*
      ((new-group)
       (new-groups)
       (matched-group)
       (matched-groups)
       (begin)
       (rest-group)
       (nth-match 0)
       (begin-n (list-length begin-keys))
       (rest-keylist (nthcdr begin-n keylist))
       (n-groups (list-length groups))
       (non-modified-groups)
       (group-found-p)
       ;;(old-group)( new-group)
       )
    (loop
     for group in groups
     for n from 1 to n-groups
     do
     (setf begin (butlast group (- (list-length group) begin-n))
           rest-group (nthcdr begin-n group))
     ;;(afout 'out (format nil "AT 1 begin= ~A rest-group= ~A%group= ~A~%  begin-keys= ~A rest-keylist= ~A~% begin-n= ~A"  begin  rest-group group begin-keys rest-keylist begin-n))
     (cond
      ((my-equal begin-keys begin)
       (setf group-found-p T)
       (cond
        ((or (null append-nth-match-only)
                 (and (= append-nth-match-only nth-match)))
         (cond
          ((and append-nested-p append-last-group-list-p)
           (setf matched-groups (append matched-groups (list group))
                 new-group (list (append  begin-keys (butlast rest-group)
                                      (list  (append-1nested-lists nil (car (last rest-group)) rest-keylist))))
                 new-groups (append new-groups (list new-group))))
;;(append-lists nil '(B C (WHAT)) '((NEW LIST))) = (B C (WHAT) (NEW LIST))
;;(append '(bc) (list (append-1nested-lists nil  ' (WHAT) '((NEW LIST))) = (B C (WHAT) (NEW LIST)))) = (BC (WHAT NEW LIST))
          (append-nested-p
           (setf matched-groups (append matched-groups (list group))
          ;;       new-group (list (append  begin-keys (butlast rest-group)
          ;;                              (append-lists NIL  rest-group rest-keylist)))
                new-group (append-1nested-lists group rest-keylist)
                 new-groups (append new-groups (list new-group)))
           )
          (t 
           (setf matched-groups (append matched-groups (list group))
                 new-group 
                 (append-lists begin-keys  rest-group rest-keylist)
                 new-groups (append new-groups (list new-group)))))
              ;;(afout 'out (format nil "new-groups= ~A~%" new-groups))
         ;;end append
         )
        (t (setf non-modified-groups (append non-modified-groups (list group))
                 new-groups (append new-groups (list group)))))
       ;;end my-equal
       )

       (t (setf non-modified-groups (append non-modified-groups (list group))
                 new-groups (append new-groups (list group)))))
      ;;(afout 'out (format nil "AT XX group= ~A~% matched-groups~A~% begin-keys~A~% rest-keylist~A~%new-groups= ~A~%"  group matched-groups begin-keys rest-keylist new-groups))
     ;;end loop
     )
    (when  (and append-if-not-found (null group-found-p))
       (setf new-groups (append new-groups (list keylist))))
         
    (values new-groups matched-groups non-modified-groups)
    ;;end let, append-group-with-same-begin
    ))
;;TEST
;;:append-nested-p nil :append-last-group-list-p nil
;; Appends the new-list to end of old group
;; NOTE:  to get (B C (WHAT) (NEW LIST))
;; (progn (setf out nil) (append-groups-with-same-begin  '(b c) '(b c (NEW LIST))  '((D YES)(A (THIS))(B C (WHAT))(X Y (THAT))(A (ANOTHER))(B C X (TOO)))  :append-nested-p nil :append-last-group-list-p nil))
;; works= ((D YES) (A (THIS)) (B C (WHAT) (NEW LIST)) (X Y (THAT)) (A (ANOTHER)) (B C X (TOO) (NEW LIST)))   ((B C (WHAT)) (B C X (TOO)))   ((D YES) (A (THIS)) (X Y (THAT)) (A (ANOTHER)))
;;
;;For  :append-nested-p T :append-last-group-list-p T;
;; note how LAST ITEM IN OLD IS APPENDED WITH NEW
;; appends the new list items inside and removes parens around old last item
;; to get ((B C (WHAT NEW LIST)))
;;(progn (setf out nil) (append-groups-with-same-begin  '(b c) '(b c (NEW LIST))  '((D YES)(A (THIS))(B C (WHAT))(X Y (THAT))(A (ANOTHER))(B C X (TOO)))  :append-nested-p T :append-last-group-list-p T))
;;works= ((D YES) (A (THIS))    ((B C (WHAT NEW LIST)))    (X Y (THAT)) (A (ANOTHER))  ((B C X (TOO NEW LIST))))        ((B C (WHAT)) (B C X (TOO)))   ((D YES) (A (THIS)) (X Y (THAT)) (A (ANOTHER)))
;;
;;  :append-nested-p T :append-last-group-list-p nil
;; all parens removed on ONLY NEW ITEMS--not appended to last item in the old list.
;;to get (B C (WHAT) NEW LIST)
;; To remove ALL parens, use ??
;;(progn (setf out nil) (append-groups-with-same-begin  '(b c) '(b c (NEW LIST))  '((D YES)(A (THIS))(B C (WHAT))(X Y (THAT))(A (ANOTHER))(B C X (TOO)))  :append-nested-p T :append-last-group-list-p nil))
;; works= ((D YES) (A (THIS)) (B C (WHAT) NEW LIST) (X Y (THAT)) (A (ANOTHER)) (B C X (TOO) NEW LIST))   ((B C (WHAT)) (B C X (TOO)))   ((D YES) (A (THIS)) (X Y (THAT)) (A (ANOTHER)))
;;
;; append-if-not-found T (if not found puts it at end>
;; IF matching group not found, appends keylist to END of group.
;; (progn (setf out nil) (append-groups-with-same-begin  '(m n) '(b c (NEW LIST))  '((D YES)(A (THIS))(B C (WHAT))(X Y (THAT))(A (ANOTHER))(B C X (TOO)))  :append-nested-p T :append-last-group-list-p nil :append-if-not-found T))
;; works= ((D YES) (A (THIS)) (B C (WHAT)) (X Y (THAT)) (A (ANOTHER)) (B C X (TOO)) (B C (NEW LIST)))      NIL    ((D YES) (A (THIS)) (B C (WHAT)) (X Y (THAT)) (A (ANOTHER)) (B C X (TOO)))
;;extra
;; (progn (setf out nil) (append-lists  '(B C (WHAT)) '(X Y (THAT))))
;; (progn (setf out nil testx nil new-matched-group1 nil)(append-lists 'new-matched-group1 (append-lists 'testx '((NEW LIST)) '((WHAT)) )  '(b c)))



;;REPLACE-LIST-IF-SAME-BEGIN
;;
;;ddd
(defun replace-list-if-same-begin (begin-keys replacement groups 
                                              &key append-if-not-found-p replace-nth-only)
  "In U-lists, Replaces all matched keylists in groups (matches begin-keys to first part of each list) with replacement (list or nonlist) UNLESS replace-nth-only (begins w 1). RETURNS (values new-groups matched-keylists).  Uses my-equal to test. begin-items can be almost any type. If append-if-not-found-p, appends replacement to the list NOTE: replacement must have same begin-keys."
  (let*
      ((matched-keylists)
       (new-groups)
       (begin-n (list-length begin-keys))
       (item-n)
       (dif-n)
       (replacement-rest (nthcdr begin-n replacement))
       (nth-match 0)
       (begin-item)
       )
    (loop
     for item in groups
     ;;for nth-item from 1 to (list-length groups)
     do
     (cond
      ((listp item)
       (setf item-n (list-length item))             
       (cond
        ((>= item-n begin-n)
         (setf dif-n (- item-n begin-n)
               begin-item (butlast item dif-n))
         (cond
          ((my-equal begin-keys begin-item)
           (incf nth-match)
           (cond
            ((or (null replace-nth-only)
                 (and replace-nth-only (= replace-nth-only nth-match)))
             (setf matched-keylists (append matched-keylists (list item))
                   new-groups (append new-groups (list replacement)))
             )
            (t (setf new-groups (append new-groups (list item)))))
           ;;end my-equal
           )
          (t (setf new-groups (append new-groups (list item)))))
         ;;end >= item-n begin-n
         )
        (t (setf new-groups (append new-groups (list item)))))
       ;;end listp
       )
      (t (setf new-groups (append new-groups (list item)))))
     ;;end loop
     )
    (when (and append-if-not-found-p (null matched-keylists))
      (setf new-groups (append new-groups (list (append begin-keys replacement-rest)))))
    ;;end let, replace-list-if-same-begin
    (values new-groups  matched-keylists)
    ))
;;TEST
;; Replace ALL occurances
;;  (replace-list-if-same-begin '(b) '(B NEW LIST)  '((D YES)(A (THIS))(B (WHAT))(X Y (THAT))(A (ANOTHER))(B X (TOO))))
;; works=((D YES) (A (THIS)) (B C (NEW LIST)) (X Y (THAT)) (A (ANOTHER)) (B C (NEW LIST)))      ((B C (WHAT)) (B C X (TOO)))

;; Replace 1st occurance
;; (replace-list-if-same-begin '(b) '(B NEW LIST)  '((D YES)(A (THIS))(B (WHAT))(X Y (THAT))(A (ANOTHER))(B X (TOO))) :replace-nth-only 1)
;;works = ((D YES) (A (THIS)) (B NEW LIST) (X Y (THAT)) (A (ANOTHER)) (B X (TOO)))     ((B (WHAT)))
;;for n = 2
;;(replace-list-if-same-begin '(b) '(B NEW LIST)  '((D YES)(A (THIS))(B (WHAT))(X Y (THAT))(A (ANOTHER))(B X (TOO))) :replace-nth-only 2)
;;works= ((D YES) (A (THIS)) (B (WHAT)) (X Y (THAT)) (A (ANOTHER)) (B NEW LIST))     ((B X (TOO)))
;;
;;(replace-list-if-same-begin '(y) '(Y (NEW LIST))  '((D YES)(A (THIS))(B (WHAT))(X Y (THAT))(A (ANOTHER))(B X (TOO))) :append-if-not-found-p t)
;; works= ((D YES) (A (THIS)) (B (WHAT)) (X Y (THAT)) (A (ANOTHER)) (B X (TOO)) (Y (NEW LIST)))   NIL
;;
;;(replace-list-if-same-begin '(b c) '(b c (NEW LIST))  '((D YES)(A (THIS))(B C (WHAT))(X Y (THAT))(A (ANOTHER))(B C X (TOO))))




        
       

;;FIND-LIST-ELEMENT-N
;;
;;ddd
(defun find-list-element-n (item list 
                                 &key from-end ) ;;problems (test 'my-equal))
  "In U-lists.lisp, . RETURNS (values n result), NIL NIL if not found. Uses my-equal for test."
  (let
      ((testlist)
       (result)
       (n -1)
       )
    (if from-end
        (setf testlist (reverse list)))
    (loop
     for element in list
     do
     (incf n)
     (cond
      ((and element (my-equal item element)) ;;problems (eval `(,test ,item element))) ;; ) ;; (eval `(,test ,item ,element)))
       (setf result element)
       (return) )
      (t nil))
     ;;end loop
     )
    (if from-end
        (setf n (- (list-length list) n)))

    (when (null result)
      (setf n nil))

    (values n result)
    ;;end let, find-list-element-n
    ))
;;TEST
;;  (find-list-element-n "this" '(a b x 33 this 4 that))
;; works =  4  THIS
;;  (find-list-element-n "thm33goa"  (get-all-shaq-variables)) works= 343  "thm33goa" 
   
;;FIND-NTH-IN-LIST
;;
;;ddd
(defun find-nth-in-list (nth list &key from-end)
  "In U-lists.lisp, finds the nth element of a list"
  (let
      ((element)
       (search-list list)
       )
    (if from-end
        (setf search-list (reverse list)))
    (loop
     for n from 0 to (- (list-length list) 1)
     for item in search-list
     do
     (when (= nth n)
       (setf element item)
       (return))
     ;;end loop
     )
    element
    ;;end let, find-nth-in-list
    ))
;;TEST
;;  (find-nth-in-list 148 *test-var-list)   = "stucolle" works



;;FIND-LIST-ITEM
;;
;;ddd
(defun find-list-item (item list &optional return-first-p)
  "In U-lists.lisp, finds a symbol or string item in a list (returns list of all matched items or NIL. If return-first-p, then only returns first item (not a list)."
  (let 
      ((test-item)
       (return-item)
       )
    (dolist (test-item list)
      (cond
       ((equal item test-item)
        (cond
         (return-first-p
            (setf return-item test-item)
            (return))
         (t
          (setf return-item (append return-item (list test-item))))))
       (t nil))
      ;;end dolist
      )
    return-item
    ))
;;test
;;  (find-list-item 'this  '(a b c this x y this z)) = THIS
;;  (find-list-item 'this  '(a b c this x y z) t)


;;PRINT-LIST
;;
;;ddd
(defun print-list (list &key stream no-newline-p incl-quotes-p incl-parens-p incl-label sort-string-test (num-spaces 1) separator-str )
  "in U-lists.lisp, Prints each list item on newline (or not) with quotes (or not) for strings (only). If sort-string-test, use it (eg. #'sting<) to sort the list. ALSO see FORMAT-STRING-LIST--generally a better choice. separator-str puts that string in place of spaces between elements."
  (let
      ((string "")
       (item-string "")
       (space " " )
       (space1)
       )
    (cond
     (separator-str
      (setf space separator-str))
     ((> num-spaces 0)
              (dotimes (n  num-spaces)
                (setf space (format nil "~A " space))))
     (t (setf space "")))

  (if sort-string-test
      (setf list (sort list sort-string-test)))
  (if incl-parens-p
      (setf string (format nil "~A" #\( )))
  (if incl-label
      (setf string (format nil "~A~A "string incl-label)))
  ;;for each item
  (loop
   for item in list
   for n from 0 to (list-length list)
   do
  ;; (format t "item= ~A item-string= ~A~%" item item-string)
    ;;incl-quotes-p -- Include quotes or not?
    (if (> n 0) (setf space1 space)
        (setf space1 ""))
    
    (cond
     (incl-quotes-p
      (setf item-string (format nil "~A~A~S" item-string space1 item)))
     (t
      (setf item-string (format nil "~A~A~A" item-string space1 item))))
    ;;newline-p--newline at end of each item?
    (if (null no-newline-p)
       (setf item-string (format nil "~A~%" item-string)))
    ;;end loop
    )
  (cond
   (incl-parens-p
      (setf string (format nil "~A~A~A"string item-string  #\))))
   (t (setf string (format nil "~A~A"string item-string ))))

  (format stream "~A" string)
  ))
;;test
;;  (print-list '(I L F M) :no-newline-p t :separator-str "-") = "I-L-F-M"
;;
;;  (print-list '("a" "b" C "d") :no-newline-p t :num-spaces 0)
;;  works = "abCd"
;;  (print-list '("a" "b" C "d") :no-newline-p t :separator-str "-")
;;  works= "a-b-C-d"
;;(print-list '("a" "b" C "d") :no-newline-p t :num-spaces 2) = "a   b   C   d"
;; (print-list '("a" "b" c "d"))
;; (print-list '(z a l m) :sort-string-test #'string< :stream T :no-newline-p t ) 
;; works = ALMZ
;; works =  A  L  M  Z NIL
;; (print-list '("a" "b" c "d") :stream t :incl-quotes-p t)
;;works
#|"a"
"b"
C
"d"|#
;; (print-list '("a" "b" c "d") :stream t :incl-quotes-p t :incl-parens-p t)
;;  (print-list  '(test (a (b 1 2 3)(c 4 5 6) d (e 7 ("x" 1 2 3) f  "g")))  :stream t :incl-quotes-p t :incl-parens-p t :incl-label 'MY-TEST-LABEL)
;; (setf xxyy '(a b c d))
;; (print-list xxyy)
;; (print-list 
;; (make-one-dim-indecies '(3 1 1 1))
;;

#|
;;OLD DEFINITION
(defun print-list (list &key stream no-newline-p incl-quotes-p incl-parens-p incl-label sort-string-test)
  "in U-lists.lisp, Prints each list item on newline (or not) with quotes (or not) for strings (only). If sort-string-test, use it (eg. #'sting<) to sort the list."
  (if sort-string-test
      (setf list (sort list sort-string-test)))
  (if incl-parens-p
      (format stream " ~A" #\( ))
  (if incl-label
      (format stream "~A~%" incl-label))
  (unless stream
    (setf stream nil))
  ( dolist (item list)
    ;;incl-quotes-p -- Include quotes or not?
    (cond
     (incl-quotes-p
      (format stream " ~S" item))
     (t
      (format stream " ~A" item)))
    ;;newline-p--newline at end of each item?
    (if (null no-newline-p)
        (format stream "~%"))
    ;;end dolist
    )
  (if incl-parens-p
      (format stream " ~A" #\) ))
  )|#
(defun print-list2 (list)
  (let
      ((string "")
       )
    (loop
     for item in list
     do
     (setf string (format nil "~A~A" string item))
     )
    (format nil "~A" string)
    ))
;; (print-list2 '("a" "b" C "d")) 
;; works = "abCd"


;;PRINT-NESTED-LIST
;;
;;ddd
(defun print-nested-list (list-of-lists &key stream no-newline-p
                                        incl-quotes-p no-outer-parens-p  incl-label)
  "in U-lists.lisp, Prints each list item on newline (or not) with quotes (or not) for strings (only). If incl-label = a label, it is printed as first item in list."
    (unless no-outer-parens-p  (format stream " ~A" #\( ))
    (if incl-label
        (format stream "~A~%" incl-label))
  ( dolist (item list-of-lists)
    (cond
     ((listp item)
      (format stream " ~A" #\( )
      (print-list item :stream stream :no-newline-p no-newline-p
                  :incl-quotes-p incl-quotes-p)
      (format stream "  ~A~%" #\)))
     (t 
      ;;incl-quotes-p -- Include quotes or not?
      (cond
       (incl-quotes-p
        (format stream " ~S" item))
       (t
        (format stream " ~A" item)))
      ;;newline-p--newline at end of each item?
      (if (null no-newline-p)
          (format stream "~%"))))
    ;;  (format stream " ~A" #\)
    ;;end dolist
    )
   (unless no-outer-parens-p   (format stream " ~A" #\) ))
 )
;;test
;;   (print-nested-list '("a" ("b" c) "d" (e (x "y" z) f)) :stream t :incl-quotes-p t)
;;  (print-nested-list  '(test (a (b 1 2 3)(c 4 5 6) d (e 7 ("x" 1 2 3) f  "g" ) (x y z)(l m n)))  :stream t :incl-quotes-p t   :incl-label 'MY-TEST-LABEL)
;;  


;;PRINT-DOUBLE-NESTED-LIST
;;
;;ddd
(defun print-double-nested-list (list-of-nested-lists &key stream 
                              incl-label no-newline-p incl-quotes-p (no-outer-parens-p t))
                 ;; LATER ADD? pre-spaces)
  "in U-lists.lisp, Prints each list item on newline (or not) with quotes (or not) for strings (only). Incl-label includes the label given with the keyword as first list item."
  (format stream " ~A" #\( )
  ( dolist (nested-list  list-of-nested-lists)
    (cond
     ((listp nested-list)
     ;; (format stream " ~A" #\( )
       (print-nested-list nested-list  :stream stream  :no-newline-p no-newline-p
         :incl-label incl-label :incl-quotes-p incl-quotes-p :no-outer-parens-p no-outer-parens-p)
#|      (print-list item :stream stream :no-newline-p no-newline-p
                  :incl-quotes-p incl-quotes-p)|#
     ;; (format stream "  ~A~%" #\))
              )
     (t 
      ;;incl-quotes-p -- Include quotes or not?
      (cond
       (incl-quotes-p
        (format stream " ~S" nested-list))
       (t
        (format stream " ~A" nested-list)))
      ;;newline-p--newline at end of each item?
      (if (null no-newline-p)
          (format stream "~%"))))
    ;;  (format stream " ~A" #\)
    ;;end dolist
    )
      (format stream " ~A" #\) )
 )
;;test
;;   (print-double-nested-list '("a" ("b" c) "d" (e (x "y" z) f)) :stream t :incl-quotes-p t)


#|
;;PRINT-STRING-LIST
;;
;;ddd
(defun print-string-list (string-list &optional stream)
  "in U-lists.lisp,"
  (unless stream
    (setf stream nil))
  ( dolist (string string-list)
    (format stream "~A~%" string)
    ))
;;test
;; (print-string-list *test-output T)
|#

;;MY-LESSP
;;
;;ddd
(defun my-lessp (item1 item2)
  "In U-lists, items can be numbers, strings, or symbols. If both numbers, compares using >, if symbols, converts to strings, then compares by string-greaterp."
  (let
      ((result)
       (item1-str)
       (item2-str)
       )
    (cond
     ((and (numberp item1)(numberp item2))
      (setf result (< item1 item2)))
     ((and (stringp item1)(stringp item2))
      (setf result (string-lessp item1 item2)))
     (t (setf item1-str (format nil "~A" item1)
              item2-str (format nil "~A" item2)
              result (string-lessp item1-str item2-str))))
    result
    ;;end let, my-lessp
    ))
;;TEST
;;  (my-lessp 7  3) = NIL
;;  (my-lessp 3 7) = T
;;  (my-lessp "THIS" "apple") = NIL
;;  (my-lessp "APPLE" "this")  = 0
;;  (my-lessp 'this 'apple) = NIL
;;  (my-lessp 'apple 'this) = 0


;;MY-GREATERP
;;
;;ddd
(defun my-greaterp (item1 item2)
  "In U-lists, items can be numbers, strings, or symbols. If both numbers, compares using >, if symbols, converts to strings, then compares by string-greaterp. RETURNS item1 if greater, otherwise NIL."
  (let
      ((result)
       (item1-str)
       (item2-str)
       )
    (cond
     ((and (null item1)(null item2)) nil)
     ((null item2)(setf result item1))
     ((null item1) nil) ;;bec item2 NOT greater than item1
     ((and (numberp item1)(numberp item2))
      (setf result (> item1 item2)))
     ((and (stringp item1)(stringp item2))
      (setf result (string-greaterp item1 item2)))
     (t (setf item1-str (format nil "~A" item1)
              item2-str (format nil "~A" item2)
              result (string-greaterp item1-str item2-str))))
    result
    ;;end let, my-greaterp
    ))
;;TEST
;; (my-greaterp 'a nil) = A
;; ;; (my-greaterp nil 'a ) = nil
;; (my-greaterp nil nil) = nil
           

;;MY-SORT-LISTS- OLD
;;
;;ddd
#|(defun my-sort-lists (nth list &key (remove-duplicates-p T))
  "In U-lists sorts a list of lists by nth item (in each) and removes duplicate items, keeps the duplicate item with the highest value of the priority-nth item in the list (or first if priority-nth = nil."
  (let
      ((sorted-list)
       
       )
    ;;first sort the lists
    (loop
     for sublist in list
     with item
     with greaterp
     with rest-sorted-list 
     with temp-list
     do
     ;;(afout 'out (format nil "OUTER-LOOP, sublist=~A~%sorted-list= ~A~%" sublist sorted-list))
     (setf item (nth nth sublist)
           rest-sorted-list nil)
     (cond
      ((null sorted-list) (setf sorted-list (list sublist)))
      (t
       (loop
        for new-sublist in sorted-list
        for n from 0 to (list-length sorted-list)
        with new-item
        do
        (setf  new-item (nth nth new-sublist))
        (setf rest-sorted-list (nthcdr n sorted-list))
        ;;(afout 'out (format nil "INNER-LOOP, sublist=~A~%new-sublist= ~A~%sorted-list= ~A~% temp-list= ~A" sublist new-sublist sorted-list temp-list))
        (setf greaterp (my-greaterp item new-item))
        (cond
         (greaterp (setf temp-list (append temp-list (list sublist)(list new-sublist))))
         (t (cond
             (rest-sorted-list 
              (setf sorted-list (append temp-list 
                                        (list new-sublist) (list sublist) rest-sorted-list)))
             (t (setf sorted-list (append temp-list 
                                        (list new-sublist) (list sublist)))))
            (return)))
        ;;end inner loop, t, cond, outer loop
        ))))
    sorted-list
    ;;end let, my-sort-lists
    ))|#
;;TEST
;;  (my-sort-lists 1 '((a 3  x)(z 9 l)(m 1 mm)(a  5  nn)))



;;MY-SORT-LISTS
;;
;;ddd
(defun my-sort-lists (nth lists  &key ascending-p from-end) 
  "In U-lists.  Sorts by largest item first only.  RETURNS (values descending-lists ascending-lists non-list-items)  ascending-lists = NIL unless ASCENDING-P. If from-end, counts from end and returns both lists."
  (let
      ((descending-lists)
       (ascending-lists)
       (ordered-list)
       (temp-list)
       (non-list-items)
       (greaterp)
       (first-lists)
       (item-deleted-p)
       (length-list)
       (last-item)
       )
    ;;no (if from-end   (setf lists (reverse lists)))

    (loop
     for list in lists
     do
     ;;ignore  if not a list
     (cond
      ((listp list)
       (setf  length-list (list-length list))
    
       (cond
        (descending-lists
         (setf first-lists (butlast descending-lists)
               last-item (car (last descending-lists)))
         (multiple-value-setq (ordered-list greaterp)
             (my-list-nth-greaterp nth list last-item :from-end from-end)) 
       
         ;;in any case
         (setf descending-lists (append first-lists ordered-list))
         (cond
          ((null greaterp) NIL)
          (t (setf descending-lists (my-sort-lists nth descending-lists :from-end from-end))))
         ;;end descending-lists clause
         )
        (t (setf descending-lists (list list))))
       ;;end listp clause
       )
      (t (setf non-list-items (append non-list-items (list list)))))
    ;; (afout 'out (format nil "1 Nth= ~A~%  list= ~A~% descending-lists= ~A~%first-lists= ~A~%last-item= ~A~%" nth list descending-lists first-lists last-item))
     ;;end loop
     )
    (cond
     (ascending-p
      (setf ascending-lists (reverse descending-lists)))
     (t nil))

    (values descending-lists ascending-lists non-list-items)
    ;;end let, my-sort-lists
    ))
;;TEST
;;  (my-sort-lists 1 '((a 3  x)(z 9 l)(m 1 mm)(a  5  nn)))  
;; works = ((Z 9 L) (A 5 NN) (A 3 X) (M 1 MM)) NIL
;;  (my-sort-lists 0 '((a 3  x)(z 9 l)(m 1 mm)(a  5  nn)))  
;; works ((Z 9 L) (M 1 MM) (A 3 X) (A 5 NN)) NIL
;;  (my-sort-lists 1 '((a 3  x)(z 9 l)(m 1 mm)(a  5  nn)) :ascending-p t) 
;; works = ((Z 9 L) (A 5 NN) (A 3 X) (M 1 MM))  ((M 1 MM) (A 3 X) (A 5 NN) (Z 9 L))
;;  (my-sort-lists 0 '((a 3  x)(z 9 l)(m 1 mm)(a  5  nn)) :from-end t)  
;;works = ((A 3 X) (A 5 NN) (M 1 MM) (Z 9 L)) NIL
;;  (my-sort-lists 0 '((a 3  x b)(z 9 l)(m 1 mm)(a  5  nn)) :from-end t)  
;; works = ((A 5 NN) (M 1 MM) (Z 9 L) (A 3 X B)) NIL
;; (my-sort-lists 0 '((a 3  x b)(z 9 l)(m 1 mm)(a  5  nn)) :from-end t :ascending-p t)
;; works= ((A 5 NN) (M 1 MM) (Z 9 L) (A 3 X B))  ((A 3 X B) (Z 9 L) (M 1 MM) (A 5 NN))
;;what if a nil in the list??
;;  ;;  (my-sort-lists 1 '((a 3  x)(z 9 l)(m nil  mm)(a  5  nn)))  
;;works = ((Z 9 L) (A 5 NN) (A 3 X) (M NIL MM)) NIL

;;  (setf *textbl (my-sort-lists 0 '((("life_goals_and_meaning.htm" "Life Goals-Values" "Life Goals and Meaning ") ("test_anxiety.htm"  "Perform Anxiety" "Reducing Test or Performance Anxiety") .5)(("assert req.html" "Assertive Request" "How to Make an Assertive Request for a Behavior Change" )(("life_goals_and_meaning.htm" "Life Goals-Values" "Life Goals and Meaning ") ("test_anxiety.htm"  "Perform Anxiety" "Reducing Test or Performance Anxiety") .5) ("c14-lisn.htm" "Intimacy" "Assertive Communication Skills to Create Understanding and Intimacy" ) .2)) :from-end t))
;; works
;; (delete-all-duplicate-nth-lists 0  *textbl);; NO, ONLY WORKS ON UNNESTED LISTS
;;
;;DO IN SEPARATE FUNCTION
;;TEST delete-duplicates-nth    delete-largest-p
;;  (my-sort-lists 0 '((a 3  x)(m 1 mm)(z 9 l)(m 4 llm)(a  5  nn))) 
;;works = ((Z 9 L) (M 1 MM) (M 4 LLM) (A 3 X) (A 5 NN))
;;  (my-sort-lists 0 '((a 3  x)(m 1 mm)(z 9 l)(m 4 llm)(a  5  nn)) :ascending-p t) 
;; works = ((Z 9 L) (M 1 MM) (M 4 LLM) (A 3 X) (A 5 NN)) ((A 5 NN) (A 3 X) (M 4 LLM) (M 1 MM) (Z 9 L))



;;SORT-KEYLISTS-BY-KEY
;;
;;ddd
(defun sort-keylists-by-key (key-nth keylists &key (sort-dim-n 1)
                                     (sort-groups 'ascending))
  "In U-lists groups keylists by same dim-n number or letter. RETURNS (values ascending-lists descending-lists labels-list). (key-nth begins w/ 0).  sort-groups-p causes groups to be sorted either 'ascending or 'descending by sort-dim-n. "
  (let
      ((label)
       (labels-list)
       (new-group)
       (grouped-keylists)
       (new-grouped-keylists)
       (labeled-groups)
       (group-found-p)
       (ascending-p)
       (descending-group)(ascending-group)
       )

    (loop
     for dimlist in keylists
     do
     (setf  label (nth key-nth dimlist)
            labels-list (append labels-list (list label)))
     ;;(afout 'out (format nil "label= ~A" label))
 
     (cond
      (grouped-keylists
       (loop
        for group in grouped-keylists
        do
        ;;(afout 'out (format nil "group= ~A" group))

        (cond
         ((get-key-value-in-nested-lists `((,label ,key-nth)) group)
          ;;(break)
          (setf new-group (append group (list dimlist))   ;;eg ((1 3 2)(2 3 2))
                grouped-keylists (replace-list-item  new-group group grouped-keylists)
                group-found-p T)
          )
         (t nil))
         ;;  (replace-list-item  '((1 3 2)(2 3 2)) '((1 3 2)) '(((1 3 2))))
#|         (t (setf new-group   (list dimlist)
                  grouped-keylists (append grouped-keylists (list new-group)))))|#

         ;;(afout 'out (format nil "group= ~A new-group= ~A grouped-keylists= ~A" group new-group grouped-keylists))               
        ;;(break)
        (setf new-group nil)
        ;;end inner loop
        )
       ;;end grouped-keylists
       )
      (t nil))
         ;(setf grouped-keylists (list (list dimlist)))))
    (cond
     ((null group-found-p)
      (setf grouped-keylists (append grouped-keylists (list (list dimlist)))))
     (t (setf group-found-p nil)))
            
       
       ;;(break)
     ;;end outer loop
     )   
    ;;TO SORT WITHIN EACH GROUP (by sort-dim-n) ascending or descending
    (when sort-groups
      (when (equal sort-groups 'ascending)
        (setf ascending-p T))
      (loop
       for group in grouped-keylists
       do
       (multiple-value-setq (descending-group ascending-group)
            (my-sort-lists  (- sort-dim-n 1) group :ascending-p ascending-p))
       (cond
        ((equal sort-groups 'ascending)
             (setf new-grouped-keylists
                   (append new-grouped-keylists (list ascending-group))))
        (t (setf new-grouped-keylists
                   (append new-grouped-keylists (list descending-group)))))
       )
      (setf grouped-keylists new-grouped-keylists))

      (values grouped-keylists  labels-list)
    ;;end let, sort-keylists-by-key
    ))
;;TEST
;;  (sort-keylists-by-key 0  '((1 3 2) (2 3 2) (3 3 2) (4 3 2) (1 4 2) (2 4 2) (3 4 2)))
;; works = (((1 4 2) (1 3 2)) ((2 4 2) (2 3 2)) ((3 4 2) (3 3 2)) ((4 3 2)))   (1 2 3 4 1 2 3)
;;  (sort-keylists-by-key 0 '((L (1 2 3))(F (3 4 5))(L (6 78))))
;; works= (((L (6 78)) (L (1 2 3))) ((F (3 4 5))))   (L F L)





;;SORT-LIST-INTO-NESTED-LISTS
;;
;;ddd
(defun sort-list-into-nested-lists  (flat-list items-per-list &key preitems postitems
                                               pre-add-dim-n-p double-quote-nested-item-p reset-x-pixs)
  "In U-lists, if items-per-list= integer, sorts list into lists with that many items (remainder is in last list).  If items-per-list = list of integers, sort according to integers, if not enough integers puts rest of items in last list. Appends preitems to beginning of each dimlist, postitems to end of each dimlist. pre-add-dim-n-p adds dim-n at end of preitems and reset-x-pixs  (both ONLY work on ART formated flat lists).  reset-x-pixs is a list of begin-x-pix incr-x-pix"
  (let*
      ((nested-lists)
       (n-items (list-length flat-list))
       (n-lists)
       (sublist)
       (nth 0)
       (nth-sublist 0)
       (sublist-nth  0)
       (length-sublist)
       (x-pix)
       (x-pix0)
       (x-pix-incr)
       (dim-n)
       (length-nested-lists)
       ) 
    ;;convert  items-per-list = integer into a list of integers
    (when
        (integerp items-per-list)
      (setf n-lists (ceiling (/ n-items  items-per-list))
            ;;note: last list may have too many elements
            items-per-list (make-list n-lists :initial-element items-per-list)))

    (setf  length-sublist (nth nth-sublist items-per-list))

    (when reset-x-pixs
      (setf  x-pix-incr (second reset-x-pixs)
             x-pix0 (car reset-x-pixs)
             x-pix x-pix0))
          
    ;;SORT THE FLAT-LIST
    (loop
     for item in flat-list
     do
     (incf nth)
     (incf sublist-nth)

     ;;reset item pix values; eg.  item= (1 (40 110) WUP1-1 (1 2 2 to 4 1 3))
     (when reset-x-pixs  
       (multiple-value-bind (itemn xy  var vdims rest)
           (values-list item)
           (setf x-pix (+ x-pix x-pix-incr))
           (if rest (setf item (list itemn (list x-pix (second xy)) var vdims rest))
             (setf item (list itemn (list x-pix (second xy)) var vdims)))))

     ;;PREITEMS? (also set below)
     (when (= nth 1)
       (cond
        ((and preitems pre-add-dim-n-p)
         (setf dim-n (car item))
         (setf sublist (append preitems (list dim-n))))
        (preitems
         (setf sublist (list preitems)))
        (t (setf sublist nil))))
     ;;(afout 'out (format nil "1 sublist=~A~%nested-lists= ~A~%item= ~A~% " sublist nested-lists item))

     (cond
      ;;at last item in flat-list
      ((= nth  n-items)
       (if double-quote-nested-item-p
           (setf sublist (append-double-quoted-sublist sublist  item))
         ;;otherwise
       (setf sublist (append sublist (list item))))
       (setf nested-lists (append nested-lists (list sublist)))
       (return))
      ;;at last item in sublist
      ((= sublist-nth  length-sublist)
       (if double-quote-nested-item-p
           (setf sublist (append-double-quoted-sublist sublist  item))
         ;;otherwise
         (setf sublist (append sublist (list item))))
       (when postitems
         (setf sublist (append sublist postitems)))
       (setf nested-lists (append nested-lists (list sublist)))
   ;;(afout 'out (format nil "SET NESTED HERE  sublist=~A~%nested-lists= ~A~%item= ~A~%" sublist nested-lists item))    
  ;; (append-double-quoted-sublist   '(DIMLIST 1 ((1 (40 110) WUP1-1 (1 1))))  '(1 (40 110) WUP1-1 (1 1)))
       ;;reset sublist values
       (incf  nth-sublist)
       (setf  length-sublist (nth (- nth-sublist 1) items-per-list)
              sublist-nth 0)
       (setf x-pix x-pix0)
       ;;Appends preitems to beginning of each dimlist, postitems to end of each dimlist. pre-add-dim-n-p adds dim-n at end of preitems (ONLY works on ART formated flat lists).
       (cond
        ((and preitems pre-add-dim-n-p)
         (setf dim-n (car item))
         (setf sublist (append preitems (list (+ dim-n 1 ))))
         ;;(break)
               ) 
        (preitems
         (setf sublist (append preitems)))
        (t (setf sublist nil)))
       ;;end last item clause
       )
      ;;otherwise append sublist
      (t 
       (if double-quote-nested-item-p
           (setf sublist (append-double-quoted-sublist sublist  item))
         ;;otherwise
       (setf sublist (append sublist (list item))))
       ;;end last t, cond
       ))
     ;;(afout 'out (format nil "2-end LOOP  sublist=~A~%nested-lists= ~A~%" sublist nested-lists))
     ;;end loop
     )
    nested-lists
    ;;end let, sort-list-into-nested-lists
    ))
;;TEST
;;  (sort-list-into-nested-lists '(1 2 3 4 5 6 7 8 9 10 11)  3) = ((1 2 3) (4 5 6) (7 8 9) (10 11))
;;   (sort-list-into-nested-lists '(1 2 3 4 5 6 7 8 9 10 11 12)  3) = ((1 2 3) (4 5 6) (7 8 9) (10 11 12))    
;;   (sort-list-into-nested-lists '(1 2 3 4 5 6 7 8 9 10 11)  '(2 4 1 3)) = ((1 2) (3 4 5 6) (7) (8 9 10) (11))
;;FOR ART FORMAT TO make-graph-line
;;newest   old (1 (40 110) WUP1-1 (1 1))
;;  
;; ;;   (sort-list-into-nested-lists  '((1 (40 110) WUP1-1 (1 1)) (2 (80 200) WUP1-2 (2 1))(1 (40 50) WUP1-2 (1 2)) (2 (80  20) WUP2-2 (2 2))) 2 :pre-add-dim-n-p T :preitems '(:DIMSIM)  :double-quote-nested-item-p T )
;;works= ((:DIMSIM 1 ((1 (40 110) WUP1-1 (1 1)) (2 (80 200) WUP1-2 (2 1)))) (:DIMSIM 2 ((1 (40 50) WUP1-2 (1 2)) (2 (80 20) WUP2-2 (2 2)))))
;; (sort-list-into-nested-lists  '((1 (40 110) WUP1-1 (1 1)) (2 (80 200) WUP1-2 (2 1))(1 (40 50) WUP1-2 (1 2)) (2 (80  20) WUP2-2 (2 2))) 2 :pre-add-dim-n-p T :preitems '(:DIMSIM)  :double-quote-nested-item-p T :reset-x-pixs  '(0 40))
;;((:DIMSIM 1 ((1 (40 110) WUP1-1 (1 1)) (2 (80 200) WUP1-2 (2 1)))) (:DIMSIM 2 ((1 (40 50) WUP1-2 (1 2)) (2 (80 20) WUP2-2 (2 2)))))

;; ;;   (sort-list-into-nested-lists  '((1 (40 110) WUP1-1 (1 1)) (2 (80 200) WUP1-2 (2 1))(1 (40 50) WUP1-2 (1 2)) (2 (80  20) WUP2-2 (2 2))) 2)
;; works= (((1 (40 110) WUP1-1 (1 1)) (2 (80 200) WUP1-2 (2 1))) ((1 (40 50) WUP1-2 (1 2)) (2 (80 20) WUP2-2 (2 2))))


#|
CL-USER 65 > (truncate (/ 9  4 )) = 2   1/4
CL-USER 66 > (truncate (/ 9 (* 4 1.0))) = 2  0.25
CL-USER 68 > (ceiling (/ 9  4)) = 3  -3/4
|#


;;RESORT-NESTED-LISTS-BY-N
;;
;;ddd
(defun resort-nested-lists-by-n (nested-list &key n-lists n-items-per-list)
  "In U-lists, RETURNS (values resorted-list n-nested-lists). Any degree of nesting ok. Sorts EITHER BY n-lists OR n-items-per-list. If both specified, sorts by n-lists with n-items-per-list and puts rest items in rest-list. If left over items in any sorting, puts items in rest-list. RETURNS  sorted-list.  "
  (let*
      ((resorted-list)
       (flat-list (flatten-count-nested-lists nested-list))
       (flat-list-length (list-length flat-list))
       (n-nested-lists)
       (n-items)
       )
   (when (listp flat-list)
   (cond
    (n-lists
     (cond
      (n-items-per-list
       (setf n-items n-items-per-list))
      (t (setf n-items  (ceiling (/ flat-list-length  n-lists)))))
     ;;(afout 'out (format nil "n-items= ~A" n-items))
     (setf resorted-list (sort-list-into-nested-lists flat-list n-items)
           n-nested-lists (list-length resorted-lists))
     ;;end n-lists
     )
    (n-items-per-list
     (setf resorted-list (sort-list-into-nested-lists flat-list n-items-per-list)
           n-nested-lists (list-length resorted-list)))
    (t nil))
   ;;end when
   )
   (values resorted-list n-nested-lists)
   ;;end let, resort-nested-lists-by-n
   ))
;;TEST
;;  (resort-nested-lists-by-n '((1 2 3)((4 5) 6 7) 8 9 (10 11)) :n-lists 3)
;; works= ((1 2 3 4) (5 6 7 8) (9 10 11))  3
;;  (resort-nested-lists-by-n '((1 2 3)((4 5) 6 7) 8 9 (10 11)) :n-items-per-list 5)
;; works= ((1 2 3 4 5) (6 7 8 9 10) (11))  3



;;RESORT-NESTED-LISTS
;;
;;ddd
(defun resort-nested-lists (nested-lists &key (max-n-items 500))
  "In U-lists, takes a list of 2-level nested lists and resorts them so that all first items are in one list, all second items in another, etc. max-n-items means max length of longest sublist."
  (let*
      ((item)
       (newlist)
       (sorted-list)
       (length-nested-lists (list-length nested-lists))
       )
    (loop
     for n from 0 to max-n-items
     do     
    (loop
     for list in nested-lists
     ;;for listn from 1 to length-nested-lists
     do
     (setf item (nth n list))
     (when item
       (setf newlist (append newlist (list item))))
     ;;end inner loop
     )    
    ;;reset newlist or append to sorted-list
    (cond
     ((null newlist)
      (return))
     (t (setf sorted-list (append sorted-list (list newlist))
              newlist nil)))
    ;;end outer loop
    )
    sorted-list
    ))
;;TEST
;;   (resort-nested-lists '(("testC1FC1F" "testC1FC2F" "testC1FC3F") ("testC2FC1F" "testC2FC2F" "testC2FC3F") ("testC3FC1F" "testC3FC2F" "testC3FC3F") ("testC4FC1F" "testC4FC2F" "testC4FC3F")))
;; WORKS= (("testC1FC1F" "testC2FC1F" "testC3FC1F" "testC4FC1F") ("testC1FC2F" "testC2FC2F" "testC3FC2F" "testC4FC2F") ("testC1FC3F" "testC2FC3F" "testC3FC3F" "testC4FC3F"))

;;NOTE: MUST RESORT POST-DIMLIST REMOVED LISTS
;;;;   (resort-nested-lists  '(((1 (40 0.070434004) "Wup1-1" (1 1)) (1 (80 0.06954) "Wup1-2" (1 2)) (1 (120 0.026330002) "Wup1-3" (1 3)) (1 (160 0.038548) "Wup1-4" (1 4)) (1 (200 0.112005) "Wup1-5" (1 5))) ((2 (40 0.08712201) "Wup2-1" (2 1)) (2 (80 0.0074070008) "Wup2-2" (2 2)) (2 (120 0.082205005) "Wup2-3" (2 3)) (2 (160 0.017092) "Wup2-4" (2 4)) (2 (200 0.063878) "Wup2-5" (2 5))) ((3 (40 0.100085005) "Wup3-1" (3 1)) (3 (80 0.009195) "Wup3-2" (3 2)) (3 (120 0.13808) "Wup3-3" (3 3)) (3 (160 0.096807) "Wup3-4" (3 4)) (3 (200 0.08280101) "Wup3-5" (3 5))) ((4 (40 0.032587998) "Wup4-1" (4 1)) (4 (80 0.076096006) "Wup4-2" (4 2)) (4 (120 0.022009) "Wup4-3" (4 3)) (4 (160 0.097701006) "Wup4-4" (4 4)) (4 (200 0.05166) "Wup4-5" (4 5))) ((5 (40 0.107535005) "Wup5-1" (5 1)) (5 (80 0.009791001) "Wup5-2" (5 2)) (5 (120 0.057769) "Wup5-3" (5 3)) (5 (160 0.046295997) "Wup5-4" (5 4)) (5 (200 0.056875) "Wup5-5" (5 5))) ((6 (40 0.046445) "Wup6-1" (6 1)) (6 (80 0.07848) "Wup6-2" (6 2)) (6 (120 0.024542002) "Wup6-3" (6 3)) (6 (160 0.048978) "Wup6-4" (6 4)) (6 (200 0.104704) "Wup6-5" (6 5))) ((7 (40 0.023946) "Wup7-1" (7 1)) (7 (80 0.11111101) "Wup7-2" (7 2)) (7 (120 0.07505301) "Wup7-3" (7 3)) (7 (160 0.011728) "Wup7-4" (7 4)) (7 (200 0.022009) "Wup7-5" (7 5))) ((8 (40 0.0033840002) "Wup8-1" (8 1)) (8 (80 0.019178002) "Wup8-2" (8 2)) (8 (120 0.12347801) "Wup8-3" (8 3)) (8 (160 0.012324) "Wup8-4" (8 4)) (8 (200 0.047934998) "Wup8-5" (8 5))) ((9 (40 0.026181002) "Wup9-1" (9 1)) (9 (80 0.046147) "Wup9-2" (9 2)) (9 (120 0.126458) "Wup9-3" (9 3)) (9 (160 0.080417) "Wup9-4" (9 4)) (9 (200 0.13361001) "Wup9-5" (9 5)))))
;;WORKS= (((1 (40 0.070434004) "Wup1-1" (1 1)) (2 (40 0.08712201) "Wup2-1" (2 1)) (3 (40 0.100085005) "Wup3-1" (3 1)) (4 (40 0.032587998) "Wup4-1" (4 1)) (5 (40 0.107535005) "Wup5-1" (5 1)) (6 (40 0.046445) "Wup6-1" (6 1)) (7 (40 0.023946) "Wup7-1" (7 1)) (8 (40 0.0033840002) "Wup8-1" (8 1)) (9 (40 0.026181002) "Wup9-1" (9 1))) ((1 (80 0.06954) "Wup1-2" (1 2)) (2 (80 0.0074070008) "Wup2-2" (2 2)) (3 (80 0.009195) "Wup3-2" (3 2)) (4 (80 0.076096006) "Wup4-2" (4 2)) (5 (80 0.009791001) "Wup5-2" (5 2)) (6 (80 0.07848) "Wup6-2" (6 2)) (7 (80 0.11111101) "Wup7-2" (7 2)) (8 (80 0.019178002) "Wup8-2" (8 2)) (9 (80 0.046147) "Wup9-2" (9 2))) ((1 (120 0.026330002) "Wup1-3" (1 3)) (2 (120 0.082205005) "Wup2-3" (2 3)) (3 (120 0.13808) "Wup3-3" (3 3)) (4 (120 0.022009) "Wup4-3" (4 3)) (5 (120 0.057769) "Wup5-3" (5 3)) (6 (120 0.024542002) "Wup6-3" (6 3)) (7 (120 0.07505301) "Wup7-3" (7 3)) (8 (120 0.12347801) "Wup8-3" (8 3)) (9 (120 0.126458) "Wup9-3" (9 3))) ((1 (160 0.038548) "Wup1-4" (1 4)) (2 (160 0.017092) "Wup2-4" (2 4)) (3 (160 0.096807) "Wup3-4" (3 4)) (4 (160 0.097701006) "Wup4-4" (4 4)) (5 (160 0.046295997) "Wup5-4" (5 4)) (6 (160 0.048978) "Wup6-4" (6 4)) (7 (160 0.011728) "Wup7-4" (7 4)) (8 (160 0.012324) "Wup8-4" (8 4)) (9 (160 0.080417) "Wup9-4" (9 4))) ((1 (200 0.112005) "Wup1-5" (1 5)) (2 (200 0.063878) "Wup2-5" (2 5)) (3 (200 0.08280101) "Wup3-5" (3 5)) (4 (200 0.05166) "Wup4-5" (4 5)) (5 (200 0.056875) "Wup5-5" (5 5)) (6 (200 0.104704) "Wup6-5" (6 5)) (7 (200 0.022009) "Wup7-5" (7 5)) (8 (200 0.047934998) "Wup8-5" (8 5)) (9 (200 0.13361001) "Wup9-5" (9 5))))






;;APPEND-DOUBLE-QUOTED-SUBLIST
;;
;;ddd
(defun append-double-quoted-sublist  ( list item)
  "In U-lists. Appends a list of form (item1 item2 SUBLIST) in which sublist is a double-quoted list.  Appends only the sublist. Eg list= (:DIMSIM 1 ((1 (40 110) WUP1-1 (1 1)))). If last item NOT a list, the appends list with double-quoted item. "
  (let
      ((new-sublist)
       (new-list)
       (begin)
       (sublist)
       )
    (setf begin (butlast list)
          sublist (car (last list)))
    (cond
     ((listp sublist)
       (setf  new-sublist (append sublist (list item))
          new-list (append begin (list new-sublist))))
     (t (setf new-list (append list (list (list item))))))
    new-list
    ;;end let, append-double-quoted-sublist
    ))
;;TEST
;; (append-double-quoted-sublist '(:DIMSIM 1 ((1 (40 110) WUP1-1 (1 1)))) '(this list))
;; works= (:DIMSIM 1 ((1 (40 110) WUP1-1 (1 1)) (THIS LIST)))
;; (append-double-quoted-sublist '(:DIMSIM 1 ((1 (40 110) WUP1-1 (1 1)))) '(2 (80 210) WUP2-1 (2 1)))
;; works= (:DIMSIM 1 ((1 (40 110) WUP1-1 (1 1)) (2 (80 210) WUP2-1 (2 1))))
;; (append-double-quoted-sublist '(a b) '(this list)) = (A B ((THIS LIST)))

;;(append-double-quoted-sublist   '(DIMLIST 1 ((1 (40 110) WUP1-1 (1 1))))  '(1 (40 110) WUP1-1 (1 1)))

    




;;REMOVE-NTH-DUPLICATES
;;
;;ddd
(defun remove-nth-duplicates (nth lists &key  delete-largest-p)
  "In U-lists.  DELETE-DUPLICATES-NTH (if duplicate by delete-duplicates-nth item, deletes the item with smallest nth item (unless delete-largest-p). RETURNS (values new-list duplicates-list."
  (let
      ((new-list)
       (sorted-list.)
       (greaterp)
       (first-lists)
       ( item-deleted-p)
       (sorted-lists)
       (last-item)
       (ordered-list)
       (last-item)
       (delete-duplicates-nth)
       (new-list)
       )
    (loop
     for list in lists
     do
     (cond
      (sorted-list.
       (setf first-lists (butlast sorted-lists)
             last-item (car (last sorted-lists)))
       (multiple-value-setq (ordered-list greaterp)
           (my-list-nth-greaterp nth list last-item))

       ;;delete-duplicates-nth (if duplicate by delete-duplicates-nth item,
       ;; deletes the item with smallest nth item (unless delete-largest-p)
       (when delete-duplicates-nth
         (cond
          ((my-equal (nth delete-duplicates-nth list)
                     (nth delete-duplicates-nth last-item))
           (setf item-deleted-p t)
           (cond
            (delete-largest-p
             (setf ordered-list (second ordered-list)))
            (t  (setf ordered-list (first ordered-list))))
           ;;end my-equal clause
           )
          (t  nil))
         ;;end when delete-duplicates-nth
         )           

       ;;(afout 'out (format nil "1 list= ~A~% sorted-lists= ~A~%first-lists= ~A~%" list sorted-lists first-lists))
       ;;in any case
       (setf sorted-lists (append first-lists ordered-list))
       (cond
        (item-deleted-p NIL)
        (t
         (cond
          ((null greaterp) NIL)
          (t (setf sorted-lists (my-sort-lists nth sorted-lists)))))
        ;;end outer cond, sorted-lists clause
        ))
      (t (setf sorted-lists (list list))))
     ;;end loop
     )
    sorted-lists
    ;;end let, my-sort-lists
    ))


;;MY-LIST-NTH-LESSP
;;
;;ddd
(defun my-list-nth-lessp (nth list1 list2)
  "In U-lists RETURNS (values sorted-list result) where result is T if the nth item in list1 is my-lessp than the one in list2."
  (let
      ((sorted-list)
       (result)
       )
    (cond
     ((my-lessp (nth nth list1) (nth nth list2))
      (setf sorted-list (list list1 list2)
            result T))
     (t (setf sorted-list (list list2 list1)
              result NIL)))
    (values sorted-list result)
  ;;end let, my-list-nth-lessp
  ))
;;; TEST
;; (my-list-nth-lessp 1 '(A 2 XX) '(B 1 MM N)) = ((B 1 MM N) (A 2 XX)) NIL
;; (my-list-nth-lessp 0 '(A 2 XX) '(B 1 MM N)) = ((A 2 XX) (B 1 MM N))  T

;;MY-LIST-NTH-GREATERP
;;
;;ddd
(defun my-list-nth-greaterp (nth list1 list2 &key from-end)
  "In U-lists RETURNS (values sorted-list result) where result is T if the nth item in list1 is my-greater than the one in list2. ROBUST, works on even lists = nil or lists too short or not lists."
  (let
      ((sorted-list)
       (result)
       (nth1 nth)
       (nth2 nth)
       )
    (when (and from-end (and (listp list1)(listp list2)))
      (setf nth1 (- (list-length list1) nth 1)
            nth2 (- (list-length list2) nth 1)))
    ;;in case of nil lists
    (cond
     ((or (and (null list1)(null list2))
          (and (< nth1 0)(< nth2 0))
          (and (null (listp list1))(null (listp list2))))
      (setf result nil sorted-list (list nil nil)))
     ((or (null list1)(< nth1 0)(null (listp list1)))
      (setf result nil sorted-list (list list2 nil)))
     ((or (null list2)(< nth1 0) (null (listp list2)))
      (setf result list1 sorted-list (list list1)))
     (t
      ;;otherwise process normal lists    
      (cond
       ((my-greaterp (nth nth1 list1) (nth nth2 list2))
        (setf sorted-list (list list1 list2)
              result list1))
       (t (setf sorted-list (list list2 list1)
                result NIL)))
      ;;end T, cond
      ))
    (values sorted-list result)
  ;;end T, cond, let, my-list-nth-greaterp
  ))
;;; TEST
;; (my-list-nth-greaterp 1 '(A 2 XX) '(B 1 MM N)) = ((A 2 XX) (B 1 MM N)) T
;; (my-list-nth-greaterp 0 '(A 2 XX) '(B 1 MM N)) = ((B 1 MM N) (A 2 XX)) NIL
;; (my-list-nth-greaterp 2 '(A 2 XX) '(B 1 MM y)) = ((A 2 XX) (B 1 MM Y))  T
;; (my-list-nth-greaterp 0 '(A 2 XX) '(B 1 MM Y) :from-end t) = ((B 1 MM Y) (A 2 XX)) NIL
;;;; if one is nil
;;(my-list-nth-greaterp 2 '(A 2 XX) '(B 1 nil N)) = ((A 2 XX) (B 1 NIL N)) 
;;  (my-list-nth-greaterp 1 '(A 2 XX) nil) =((A 2 XX)) (A 2 XX)
;; (my-list-nth-greaterp 1 '(A 2 XX) nil :from-end t) = ((A 2 XX)) (A 2 XX)
;;  (my-list-nth-greaterp 0 nil  '(B 1 MM Y) :from-end t) = ((B 1 MM Y) NIL) NIL
;; (my-list-nth-greaterp 0 nil  '(B 1 MM Y)) = ((B 1 MM Y) NIL)  NIL
;; (my-list-nth-greaterp 3 '(A 2 XX) '(B 1 MM Y) :from-end t) = ((B 1 MM Y) NIL)  NIL
;;(my-list-nth-greaterp 3 '(A 2 XX) '(B 1 MM Y)) = ((B 1 MM Y) (A 2 XX)) NIL
;; (my-list-nth-greaterp 3 '(A 2 XX LL) '(B 1 MM)) = ((A 2 XX LL) (B 1 MM))   (A 2 XX LL)
;; (my-list-nth-greaterp 0 '(A 2 XX LL) 7) = ((A 2 XX LL)) (A 2 XX LL)
;; (my-list-nth-greaterp 2 '(A 2 XX LL) 7 :from-end t) = ((A 2 XX LL)) (A 2 XX LL)
;; (my-list-nth-greaterp 2 "ab" '(A 2 XX LL)  :from-end t) = ((A 2 XX LL) NIL)  NIL

;;MY-COMPARE-LIST-NTH
;;
;;ddd
(defun my-compare-list-nth (nth list1 list2 &key (test 'my-greaterp))
  "In U-lists RETURNS (values sorted-list result) where result is T if the nth item in list1 is TEST (usually 'my-greaterp or 'my-lessp than the one in list2. list1 and list2 MUST be 1-lists and 2-at least nth long. This is a SIMPLE,NON-ROBUST function. Use my-list-nth-greaterp or my-list-nth-lessp for unknown lists that may have nils, non-list items, or lists < nth long."
  (let
      ((sorted-list)
       (result)
       )
    (cond
     ((funcall test (nth nth list1) (nth nth list2))
      (setf sorted-list (list list1 list2)
            result T))
     (t (setf sorted-list (list list2 list1)
              result NIL)))
    (values sorted-list result)
  ;;end let, my-list-nth-greaterp
  ))
;;TEST
;; (my-compare-list-nth 1 '(a b c) '(x y z)) 
;; works = ((X Y Z) (A B C)) NIL
;; (my-compare-list-nth 1 '(a b c) '(x y z) :test 'my-lessp)
;; works = ((A B C) (X Y Z)) T
;; (my-compare-list-nth 1 '(1 2 3) '(x 0 z) :test 'my-lessp)
;; works = ((X 0 Z) (1 2 3)) NIL
;;
;; (funcall  #'my-greaterp "this" "apple") = 0
;;  (setf xx 'car) (funcall xx '(a b c)) = a

;;DELETE-ALL-DUPLICATE-NTH-LISTS
;;
;;ddd
(defun delete-all-duplicate-nth-lists (nth lists  &key compare-nth return-lessp)
  "In U-lists"
  (let
      ((return-list)
       (duplicates)
       (first-list)
       (rest-lists)
       (new-lists)       
       (newlist)
       ( result-list)
       (list1)
       (duplicate)
       (matchp)
     ;;;SSS START HERE FINISH THIS (NEED RECURSIVE ??)
       )
    (loop
     for n from 1 to (list-length lists)
     do
     (setf list1 (car lists)
           rest-lists (cdr lists))
     ;;(afout 'out (format nil "LOOP1 n= ~A list1= ~A~%, rest-lists=~A~%" n list1 rest-lists))
     (loop
      for list2  in rest-lists
      do 
      (multiple-value-setq (result-list duplicate matchp)
          (delete-duplicate-nth nth list1 list2  :compare-nth compare-nth
                                :return-lessp return-lessp)) 
     ;;(afout 'out (format nil "After mvs, result-list= ~A~% duplicate= ~A~%" result-list duplicate))
      (setf list1 (car result-list))
      (if duplicate (setf duplicates (append duplicates  duplicate)))
            
      ;;eliminate duplicates from original lists above
      (unless matchp (setf new-lists (append new-lists  (list (second result-list)))))
     ;;(afout 'out (format nil "LOOP2 list1= ~A~%matchp= ~A  new-lists= ~A~%" list1 matchp new-lists))                      
      ;;end loop
      )
     (if list1
         (setf newlist (append newlist (list list1))
               lists new-lists
               new-lists nil))
          ;;(afout 'out (format nil "AFTER LOOP2, newlist= ~A~%lists= ~A~%" newlist lists))
     ;;end loop
     )
    (values newlist duplicates)
    ;;end let, delete-all-duplicate-nth-lists
    ))
;;TEST
;;SSS START HERE DEBUGGING USE TO DELETE EXTRA HELP-LINKS
;;  (delete-all-duplicate-nth-lists 1 '((x a  3)(a a 4)(b u 3)(m d 6)(x m 0)))
;;  (delete-all-duplicate-nth-lists 1 '((x a  3)(a a 4)(b u 3)(m d 6)(x m 0)):compare-nth 2)
;;works= ((A A 4) (B U 3) (M D 6) (X M 0))    ((X A 3))

;;DELETE-ALL-DUPLICATE-NTH-LISTS
;;
;;ddd
#|(defun delete-all-duplicate-nth-listsOLD (nth lists  &key compare-nth return-lessp)
  "In U-lists"
  (let
      ((return-list)
       (duplicates-list)
       (first-list)
       (last-list)
       (newlist)
       )
    (loop
     for list in lists
     do
     (cond
      (return-list
       (loop
        for r-list in return-list
        do
        (multiple-value-setq (newlist duplicate matchp)
            (delete-duplicate-nth nth list r-list  :compare-nth compare-nth
                                  :return-lessp return-lessp))
          (setf return-list (append return-list newlist)
                duplicates-list (append duplicates-list duplicate))
          ;;end inner loop
          ))
       (t (setf return-list (list list))))
     ;;end outer loop
     )
    (values return-list duplicates-list)
    ;;end let, delete-all-duplicate-nth-lists
    ))|#

        
      




;;DELETE-DUPLICATE-NTH
;;
;;ddd
(defun delete-duplicate-nth (nth list1 list2 &key compare-nth return-lessp )
  "In U-lists. Compares same nth item in list of lists and deletes lists with duplicates. RETURNS (values new-list duplicate-list duplicatep) new-list is a list of both lists unless nth item is my-equal.  If compare-nth, then the one with the largest value of compare-nth will be returned (unless return-less-p)."
  (let
      ((new-list)
       (result)
       (duplicate-list)
       (duplicatep)
       )
    (cond
     ((my-equal (nth nth list1)(nth nth list2))
      (setf duplicatep t)
      (cond
       (compare-nth
        (setf ordered-list  (my-list-nth-lessp compare-nth list1 list2))
        (cond
         (return-lessp
          (setf new-list (list (car ordered-list))
                duplicate-list (list (second ordered-list))))
         (t (setf new-list (list (second ordered-list))
                  duplicate-list (list (first ordered-list)))))
        ;;end compare-nth
        )
       (t (setf new-list (list list1)
                duplicate-list (list list2))))
      ;;ene my-equal
      )
     (t (setf new-list (list list1 list2))))
    (values new-list duplicate-list duplicatep)
    ;;end let, delete-duplicate-nth
    ))
;;TEST
;;  (delete-duplicate-nth 1 '(a b c d) '(one b 2 x))  = ((A B C D)) ((ONE B 2 X))  T
;;  (delete-duplicate-nth 0  '(a b c d) '(one b 2 x)) = ((A B C D) (ONE B 2 X))  NIL NIL
;;  (delete-duplicate-nth 1 '(a b c d) '(one b 2 x) :compare-nth 3) = (ONE B 2 X)  (A B C D) T
;;  (delete-duplicate-nth 1 '(a b c d) '(one b 2 x) :compare-nth 3 :return-lessp t) = (A B C D)  (ONE B 2 X) T





;;COMPARE-LISTS
;;
;;ddd
(defun compare-lists (list1 list2)
  "In U-lists, compares lists.  Checks all items from list1 to see if they are members (test 'equal) to items in list2. RETURNS (values matched-items unmatched-list1-items"
  (let
      ((matched-items)
       (unmatched-list1-items)
       )
    (dolist (item list1)
      (cond
       ((member item list2 :test 'equal)
        (setf matched-items (append matched-items (list item))))
       (t
        (setf unmatched-list1-items (append unmatched-list1-items (list item)))))
      ;;end dolist
      )
    (values matched-items  unmatched-list1-items)
    ))
;;test
;;  
#|(defun testcl ()
  (let
      ((list1 '(a b c d e))
       (list2 '(a x b y e g h))
       (v1)
       (v2)
       )
    (multiple-value-setq (v1 v2)
    (compare-lists list1 list2))
    (values v1 v2)
    ))
;;works, returns (A B E)  (C D)
|#
(defun compare-nested-list-items (list-of-lists1 nth-item1 list-of-lists2 nth-item2 
                          &key fuzzy-match-p auto-cutoff n-matched-cutoff n-match-length)
  "In U-lists.lisp, compares the nth-item1 in each sublist in the  list-of-lists1 (OR if nth-item1 = nil to plain items) to each of nth-item2 in sublists (or simple items) of list-of-lists2. RETURNS (values match-items matched-sublists unmatched-items unmatched-sublists). Compares using 'equal  If items 1 and 2 are strings, can use fuzzy-match-p. n-matched-cutoff  = num of chars must have in common to specifiy a fuzzy match.  If NIL, test = 'equal is used. auto-cutoff default is 0.6 means 0.6 of the length of the item1 is the cutoff. Must be betw 0 < 1.0.  If want auto-cutoff default must set it to T or 0.6. n-match-length specifies that a substring match of at least that many chars MUST happen or no match."
  (let
      ((item1)
       (item2)
       (matched-items)
       (matched-sublists)
       (item-matchlist)
       (sublist-matchlist)
       (unmatched-items)
       (unmatched-sublists)
       (matched-p)
       (item1-return)
       (fuzzy-item)
       (fuzzy-chars)
       (fuzzy-list)
       )
    (if (or auto-cutoff n-matched-cutoff n-match-length)
        (setf fuzzy-match-p T))
    ;;find item1 -- either it is the next item in simple list or nth item in nested list
    (dolist (list1 list-of-lists1)
      (cond
       ((null nth-item1)
        (setf  item1 list1))
       (t (setf item1 (nth nth-item1 list1))))
      ;;now find item2 as nth item in list-of-lists2 -- which can also be a simple list
      (dolist (list2 list-of-lists2)
        (cond
         ((null nth-item2)
          (setf  item2 list2))
         (t (setf item2 (nth nth-item2 list2))))
        ;;see if item1 is equal to item2
        (cond
         ;;if use a fuzzy match insert a fuzzy-list with matched item plus matched chars
         (fuzzy-match-p
          (multiple-value-setq (item1-return fuzzy-item fuzzy-chars)
              (fuzzy-matcher  item1 item2 :auto-cutoff auto-cutoff
                       :n-matched-cutoff  n-matched-cutoff :n-match-length n-match-length ))
          (if  item1-return
          (setf item-matchlist (append item-matchlist (list fuzzy-item))))
                ;;not needed  sublist-matchlist (append sublist-matchlist (list (list fuzzy-item))))))
          #|   (setf  fuzzy-list (list fuzzy-item fuzzy-chars)
                 matched-items (append matched-items (list item1 fuzzy-list))
                matched-sublists (append matched-sublists (list (append list1 (list fuzzy-list)))))|#
          (unless (null item1-return)
            (setf matched-p T)))
         ;;if not using fuzzy match, use test = equal and don't add any list
         ((equal item1 item2)
          (setf matched-items (append matched-items (list item1))
                matched-sublists (append matched-sublists (list list1))
                matched-p T))
         (t  NIL))
        ;;end nested dolist
        )
      ;;if fuzzy match, must add item and sublist matchlists to overall lists
      (if (and matched-p fuzzy-match-p)
          (setf   matched-items (append matched-items (list item1 item-matchlist))
                  matched-sublists  (append matched-sublists (list (append  list1 (list item-matchlist))))
                  ;;reset item-matchlist
                  item-matchlist nil))

      ;;if no match, include the unmatched item and sublist on unmatched lists
      (unless matched-p
        (setf unmatched-items (append unmatched-items (list item1))
              unmatched-sublists  (append unmatched-sublists (list list1))))
      ;;reset the matched-p to nil
      (setf matched-p nil)
      ;;end outer dolist
      )
    ;;return from compare-nested-list-items
    (values matched-items matched-sublists unmatched-items unmatched-sublists)
    ))
;;tests
#|(defun testcl2 ()
  (let
      ((list1 '((1 c 11)(2 b 6 7 8)(3 x)))   ;; '(a b c d e))
       (list2 '((1 b 2 3)(2 c 4 5)(3 d y e g h)))
       (v1)
       (v2)
       (v3)
       (v4)
       )
    (multiple-value-setq (v1 v2 v3 v4)
    (compare-nested-list-items list1 1 list2 1))
    (values v1 v2 v3 v4)
    )) |#
;;works for (list1 '(a b c d e)), returns (B C D) (B C D) (A E)
;;works for (list1 '((1 c 11)(2 b 6 7 8)(3 x))), returns   (C B)  ((1 C 11) (2 B 6 7 8))  (X) ((3 X))

;;test for fuzzy match
#|
(defun testcl3 ()
  (setf out nil)
  (let
      ((list1  '(("CaseNum" "CaseNumOrigFile") ("CaseType" "")))
#| ("SourceFile" "Source files PARTnum") ("FileDate" "") ("Instr" "Instructor") ("Resr" "Researcher") ("Name" "") ("IDnum" "") ("Sex" "Sex 1=M 2=F") ("Age" "") ("Email" "") ("ZipCode" "") ("Nation" "") ("HrsWork" "") ("UserRate" "") ("tknowmor" "t-Want to know more of self") ("texperie" "t-Experienced self-help user") ("twanttho" "t-Want thorough assessment") ("twantspe" "t-Want specific help") ("tworknga" "t-worknga") ("tu100stu" "t-CSULB U100 student") ("tcsulbst" "t-CSULB other student") ("totherst" "t-Other student") ("tcolstu" "t-Other college student") ("tinstruc" "t-Instructor") ("tcolfaca" "t-College faculty-admin") ("twanthel" "t-Want help with problem") ("wantspq" "g-Specific questionnaire") ("gsuchap" "g-Success-happiness") ("gacadsuc" "g-Academic success") ("gemocop" "g-Emotional coping") ("gslfest" "g-Self-esteem") ("gprocrst" "g-Procrastination") ("gtimeman" "g-Time Management") ("grelat" "g-Relationships") ("gmeetpeo" "g-Meeting people")))|#
       (list2    '( "CaseNum" "CaseType" "Group" "Var2" "FileDate" "Instr" "Resr" "Name" "SSN"   
       "Sex" "Age" "Email" "ZipCode" "Nation" "HrsWork" "UserFBra" "TKnowMor" "TExperie" "TWantTho" "TWantSpe" "TWorkngA"   
       "TU100Stu" "TCSULBSt" "TOtherSt" "TColStu" "TInstruc" "TColFacAd" "TWantHel" "WantSpQ" "GSucHap" "GAcadSuc" "GEmoCop"   
       "GSlfEst" "GProcrstn" "GTimeMan" "GRelat" "GMeetPeo" "GLonelyF" "GExValus" "GDepres" "GAnxFear" "GAggrAng" "GCompltA"   
       "GCarPlan" "GNotTake" "GCarOnly"   
          ;;  43 scales
        "SPersBio" "SAAchApt" "SAMotSat" "SAMotiv"   
        "SLrnSkls" "SLrnArea" "SLrnDisb" "SSelfMan" "SEmotCop" "SLiThAch" "SLiThSoc" "SLiThNeg" "SLiThInt" "StbSlfWo" "SIEcontr"   
        "SWorldVi" "SGrFears" "SSlfConf" "SAssert" "SIntimat" "SIndepRe" "SRomantc" "SLibRole" "SeHappy" "SRelHlth" "SRPeople"   
        "SrDepres" "SrAnxiet" "SrAngAgg" "SrEmotPr" "Sb2Ethic" "SinCar" "SinBus" "SinEngr" "SinFineA" "SinHelp" "SinLang" "SinMed"   
        "SinMiltC" "SinNatSc" "SinSocSc" "SinWoEth" "SinWrite"   
         ;;   questions
         "bio3educ" "bioHSGPA" "bioColle" "bio4job" "Student" "Manager" "ProPeop" "ProTech"   
         "Consulta" "Educator" "Sales" "Technici" "Clerical" "Service" "OwnBus10" "OthrSfEm" "Other"   
        "bio5inco" "bio7lang" "LEnglish" "LSpanish" "LVietname" "LCambodn" "LChinese" "LKorean" "LPortugue" "LGerman" "LFrench"   
        "LOthrAsn" "LOthrEur" "LOther" "bio1ethn" "ENorthAm" "EAfrica" "ENorEur" "ESouEur" "ECambodn" "EChina" "EKorea" "EJapan"   
        "EVietnam" "EOthrAsn" "EMexico" "ECentrAm" "ESouthAm" "EPacific" "EOther" "bioRelAf" "Catholic" "Jewish" "Islam"   
        "LatterD" "Buddhist" "Baptist" "Methodst" "Episcop" "Lutheran" "Presbyte" "PrOLiber" "PrOFunda" "NoAffil" "Agnostic"   
         "OthrNoAn" "stParEd" "stuColle" "CoCSULB" "CcCSU" "CoUCal" "CoOPublc" "CoPrivCA" "CoPrivOt" "CoCAComC"   
         "CoOthCC" "CoOthNAT" "CoPrGrad" "CoTech" "HighSch" "CoOther" "stuClass" "stuDegre" "stuMajor" "MLibArt"   
         "MSocSci" "MBiolSci" "MArt" "MNatSci" "MBus" "MEnginr" "MEducat" "MMedical" "MOtCompu" "MOthTech" "MRecrPE"   
         "MDoesNA" "MUndecid" "stuSpeci" "STranCC" "STran4yr" "SAdultRe" "SEOP" "SUSImmig" "SVisa" "SHonor"   ))
  
       (v1)
       (v2)
       (v3)
       (v4)
       )
    (multiple-value-setq (v1 v2 v3 v4)
        (compare-nested-list-items list1 0 list2 nil :n-match-length 4))  ;;:auto-cutoff 0.5 )) ;; n-matched-cutoff 6
    (values v1 v2 v3 v4)
    ))
|#
;;(compare-nested-list-items list1 list2 nil :auto-cutoff 0.7 )
;;works, returns NIL NIL plus 2 unmatched lists
;;



;;REPLACE-LIST-ITEM
;;
;;ddd
(defun replace-list-item (new-item old-item list &key delete-instead-p)
  "In U-lists.lisp, replaces old-item with new-item unless delete-instead-p is T, then old item is
    simply deleted (put nil or anything for new-item). RETURNS (values  new-list item-found-p)"
  (let
      ((new-list)
       (post-list)
       (item-found-p)
       )
    (dolist (item list)
      (cond
       ((my-equal item old-item)
        (cond
         ((null delete-instead-p)
          (setf  item-found-p t
                 new-list (append new-list (list new-item))))
         (t nil)))
       (t (setf new-list (append new-list (list item)))))
      ;;(afout 'out (format nil "item= ~A old-item= ~A new-item= ~A~% new-list= ~A~%" item old-item new-item new-list))
      )
      
   (values  new-list item-found-p)
    ))
;;TEST
;;(replace-list-item '((1 3 2)(2 3 2))  '((1 3 2)) '(((2 5 1))((1 3 2))((1 4  3))))
;; works= (((2 5 1)) ((1 3 2) (2 3 2)) ((1 4 3)))   T
#|(defun testrli ()
 ;; (replace-list-item 'x NIL '(a b c d NIL e) :delete-instead-p t) ;;works, returns (A B C D E)
  (replace-list-item 'x "" '(a b c d "" e) :delete-instead-p t) ;;works, returns (A B C D E)
  )|#
  ;;(replace-list-item '(new group)   '((L (((I 5 2))))) '(((M (((I L F))))) ((F (((I L 2))))) ((L (((I 3 2))))) ((I (((1 3 2) (2 3 2) (3 3 2) (4 3 2))))) ((L (((I 4 2))))) ((I (((1 4 2) (2 4 2) (3 4 2) (4 4 2))))) ((L (((I 3 2) (I 4 2) (I 5 2))))) ((L (((I 5 2))))) ((I (((1 5 2) (2 5 2) (3 5 2) (4 5 2))))) ((F (((I L 2) (I L 3))))) ((F (((I L 3))))) ((L (((I 3 3))))) ((I (((1 3 3) (2 3 3) (3 3 3) (4 3 3))))) ((L (((I 4 3))))) ((I (((1 4 3) (2 4 3) (3 4 3) (4 4 3))))) ((L (((I 3 3) (I 4 3) (I 5 3))))) ((L (((I 5 3))))) ((I (((1 5 3) (2 5 3) (3 5 3) (4 5 3))))))) = works
;;result= (((M (((I L F))))) ((F (((I L 2))))) ((L (((I 3 2))))) ((I (((1 3 2) (2 3 2) (3 3 2) (4 3 2))))) ((L (((I 4 2))))) ((I (((1 4 2) (2 4 2) (3 4 2) (4 4 2))))) ((L (((I 3 2) (I 4 2) (I 5 2))))) (NEW GROUP) ((I (((1 5 2) (2 5 2) (3 5 2) (4 5 2))))) ((F (((I L 2) (I L 3))))) ((F (((I L 3))))) ((L (((I 3 3))))) ((I (((1 3 3) (2 3 3) (3 3 3) (4 3 3))))) ((L (((I 4 3))))) ((I (((1 4 3) (2 4 3) (3 4 3) (4 4 3))))) ((L (((I 3 3) (I 4 3) (I 5 3))))) ((L (((I 5 3))))) ((I (((1 5 3) (2 5 3) (3 5 3) (4 5 3))))))   T

(defun delete-list-items (delete-list  list)
  "In U-lists.lisp, deletes items on delete-list from list if they are on the list"
  (let
      ((new-list)
       )
    (dolist (item list)
      (cond
       ((member item delete-list :test 'equal)
        NIL)
       (t (setf new-list (append new-list (list item)))))
      )
    new-list
    ))
;;test 
;;  (delete-list-items '(c  NIL ""  " "  ) '(a b c NIL d "" x yz)) ;;works, returns (A B D X YZ)

;;
;; FOR CHANGING LIST TO STRING, OR MATCHING SUBSEQ, GO TO U-SEQUENCES.LISP  AND/OR U-TSTRING.LISP
;;
;;REPLACE-LIST 
;;replaces nth list item
;;ddd
(defun replace-list (list n new-item)
  "In U-lists.lisp, replaces nth item in a list with new-item--item can be sublist or not. begin n= 0."
  (let 
      ((nth -1)
        (new-list)
        )
    (cond 
     ((listp list)
      (dolist (item list)
        (incf nth)
        (cond
         ((= nth n)
          (setf  new-list (append new-list (list new-item))))
         (t (setf new-list (append new-list (list item)))))
        ;;end dolist
        ))
      (t  (setf new-list "ERROR-NOT A LIST")))
    new-list
    ))
;;TEST
;;  (replace-list '
;;works
#|(defun testnl ()
  (values
  (replace-list '((a b)(c d)(e f)(g (1 2) h)(i j))  3 '(x y))
  (replace-list '((a b)(c d)(e f)(g (1 2) h)(i j))  3 "this"))
  )|#




;;REPLACE-LIST-INTEGERS
;;
;;ddd
(defun replace-list-items (list new-items &key if-integerp)
  "Lists must be same length to replace each item with an item in same place in new-items.  If if-integer-p, then only replaces integers."
  (let
      ((new-list)
       )
    (loop
     for item in list
     for new-item in new-items
     do
     (cond
      ((and if-integerp (integerp item))
       (setf new-list (append new-list (list new-item))))
      (if-integerp ;;if not an integer, don't replace item
       (setf new-list (append new-list (list item))))
      (t 
       (setf new-list (append new-list (list new-item)))))
     ;;end loop
     )
    new-list
     ;;end let, replace-list-items
     ))
;;TEST
;; (replace-list-items '(I  2 3 to 4 5 F) '(I L F TO I L F) :if-integerp T) = (I L F TO I L F)
;; (replace-list-items '(X  2 3 to 4 5 F) '(I L F TO I L F) :if-integerp T) = (X L F TO I L F)

     





;;REPLACE-NTH
;;
;;ddd
(defun replace-nth (list nth value 
                         &key (append-if-no-nth-p T) delete-nth-p)
  "U-lists, replace nth item in a list with value. RETURNS (values new-list old-item value). If n greater than list length, appends it to end if append-if-no-nth-p.  if nth = :last, replaces last. If DELETE-NTH-P, deletes nth instead of replacing it. nth begin= 0"
  (let 
      ((new-list)
       (n -1)
       (old-item)
       )
     (when (equal nth :last)
      (setf nth (- (list-length list) 1)))
    (cond
     ((>=  nth (list-length list))
      (cond
       (append-if-no-nth-p 
        (setf new-list (append list (list value))))
       (t  (setf value nil
                 new-list list)))
      )
     (t
      (loop
       for item in list
       do
       (incf n)
       (cond
        ((= n nth)
         (setf old-item item)
         (cond
          ((null delete-nth-p)
              (setf new-list (append new-list (list value))))
          (t nil)))
        (t (setf new-list (append new-list (list item)))))
       ;;end loop, t, cond
       )))
    (values new-list old-item value)
    ;;end let, replace-nth
    ))
;;TEST
;;  (replace-nth '(A B C D E) 2  'THIS) = (A B THIS D E) C THIS
;;  (replace-nth '(A B C D E) 9  'THIS) = (A B C D E THIS)  NIL  THIS
;;  (replace-nth '(A B C D E) 9  'THIS :append-if-no-nth-p nil) = (A B C D E) NIL NIL
;;  (setf  listxx1 '(1 2 3 4 5))
;;  (setf listxx1 (replace-nth listxx1 9 'this)) = (1 2 3 4 5 THIS)
;;  (replace-nth '(a (b c) d) 3 'this) = (A (B C) D THIS)  NIL THIS
;;  (replace-nth '(A B C D E) :last  'THIS) = (A B C D THIS)  E  THIS
;;
;;  (progn (setf out nil) (replace-nth '((D YES)(A (THIS))(B C (WHAT))(X Y (THAT))(A (ANOTHER))(B C X (TOO)))   2   '(b c (NEW LIST))))  
;; works= ((D YES) (A (THIS)) (B C (NEW LIST)) (X Y (THAT)) (A (ANOTHER)) (B C X (TOO)))      (B C (WHAT))   (B C (NEW LIST))      
;; (replace-nth '(1 2 3 4 5 6) 2 nil)  = (1 2 NIL 4 5 6)   3   NIL
;; (replace-nth '(1 2 3 4 5 6)  2 nil  :delete-nth-p T)  = (1 2 4 5 6)  3  NIL




;;DELETE-NTH
;;
;;ddd
(defun delete-nth (list nth)
  "In U-lists, deletes nth, RETURNS (new-list old-item). Uses replace-nth. nth begin= 0"
  (multiple-value-bind (new-list old-item)
      (replace-nth list nth nil  :delete-nth-p T)

    (values new-list old-item)
    ;;end mvb, delete-nth
    ))
;;TEST
;;  (delete-nth '(1 2 3 4 5) 3) = (1 2 3 5)  4






;;FIND-KEY-VALUES-IN-LIST 
;;
;;ddd
(defun find-keys-and-values-in-list  (key-list list)
  "In U-list.lisp, in a list of misc placed key-value pairs, will return  the keys and values in key-list in the form of a list. Then use values-list if want to convert to values list"
  (let*
      (;;(key)
       (value)
       (n 0)
       (matching-key-values-list)
       )
    (dolist (item list)
      (incf n)
      (cond
       ((member item key-list :test 'equal)
       (setf value (nth n list)
             matching-key-values-list 
             (append matching-key-values-list (list item value))))             
       (t nil))
      ;;end dolist
      )
    matching-key-values-list))

;;test
;;  (find-keys-and-values-in-list  '(:k2 :k3 :k9) '(:k4 99 :k2 'this :k5 'that :k3 67))
;; works, returns (:K2 (QUOTE THIS) :K3 67)
;;TO RETURN VALUES INSTEAD
;;   (values-list '(:K2 (QUOTE THIS) :K3 67)) returns :K2 (QUOTE THIS) :K3 67
;; doesn't work (find-keys-and-values-in-list '(2)  '((1 (a b))(2 (c d)))) = NIL 

;;NOTE: RANDOMIZE WORKS WELL, BUT THE 'LESSP AND 'GREATERP ARE
;; UNRELIABLE--SOMETHING TO DO WITH HOW THE STRINGS ARE COMPARED??


;;FIND-KEYS-IN-LISTS
;;
;;ddd
(defun find-keys-in-lists (key-lists)
  "In U-lists. RETURNS list of keys/cars of all first-order lists in key-lists."
  (let
      ((keys-list)
       ( key)
       )
    (loop
     for list in key-lists
     do
     (if (listp list)
         (setf key (car list)
               keys-list (append keys-list (list key))))
     ;;end loop
     )
    keys-list
    ;;end let, find-keys-in-lists
    ))
;;TEST
;;  (find-keys-in-lists *select-shaq-scales-keylist)


;;FIND-KEYS-IN-NESTED-LISTS
;;
;;ddd
(defun find-keys-in-nested-lists (nested-list &key sublist-newline-p)
  "In U-lists, finds 1st and 2nd-order keys (initial objects in sublists) and returns a list of only the keys and nested keys. RETURNS A STRING of all keys. When sublist-newline-p = t, adds newlines."
  (let
      ((return-list-string (format nil "~%LIST OF KEYS~%"))
       ( new-sublist-string)
       (key2)
       (key2-next)
       )
    ;;eliminate extra quote--added for use with read
    (if (and (listp nested-list) (equal (car nested-list) 'quote))
        (setf nested-list (second nested-list)))

    (loop
     for sublist in nested-list
     do
     (setf  new-sublist-string  "(")
     (cond
      ((listp sublist)
       (loop
        for item in sublist
        do
        (cond
         ((listp item)
          (setf  key2 (car item))
          (if (> (list-length item) 1) 
              (setf key2-next (second item))
            (setf key2-next ""))
          (cond 
           (sublist-newline-p
            (setf new-sublist-string  
                  (format nil "~A~%(~A  ~A )" new-sublist-string key2 key2-next)))
           (t (setf new-sublist-string  
                    (format nil "~A  (~A   ~A)" new-sublist-string key2 key2-next))))
          ;;end inner listp
          )
         (t (setf new-sublist-string
                  (format nil "~A~A" new-sublist-string item))))
        ;;end inner loop
        )
       (setf return-list-string
             (format nil "~A~%~A~%)"return-list-string new-sublist-string))
       ;;end outer listp clause
       )
      ;;sublist is not a list
      (t (setf return-list-string
               (format nil "~A~A)" return-list-string sublist))))
     ;;end outer loop
     )
    return-list-string
    ;;end let, 
    ))
;;TEST
;;  (find-keys-in-nested-lists *SHAQ-question-variable-lists)
;; works SAMPLE = (IE(iecselfs)(iecicont)(iecgenet)(iecpeopl)(iecdepen)(ieccofee)(ieccoprb)
;;  (find-keys-in-nested-lists *SHAQ-question-variable-lists :sublist-newline-p t)
#| works SAMPLE =
(IE
(iecselfs)
(iecicont)
(iecgenet)
(iecpeopl)
(iecdepen)
(ieccofee)
(ieccoprb)
)|#
;;  (find-keys-in-nested-lists *select-shaq-scales-keylist)


;; (setf  plist '(a b c :key1 99  :key2 nil))
;;
;;APPEND-KEY-VALUE
;;
;; added 2016-04
;;ddd
(defun append-key-value (key value listsym
                             &key   append-as-flatlist-p
                             append-first-as-flatlist-p
                             list-first-value-p   set-listsym-to-newlist-p
                             recursive-call-p)
  "In U-lists, In a list set to listsym, finds key and replaces next element with value. Works for flat lists and list of  key-lists. If key not found, appends key and value to list. Uses my-equal. RETURNS (values new-list key-found old-value new-keylist) NTH-ITEM = 1 modifies first item after key, etcAPPEND-FIRST-AS-FLATLIST-P  causes append list with flat new key and value. Afterwards, APPEND-AS-FLATLIST-P causes any ANY keylist to be form of key old-value new-value.  If  LIST-FIRST-VALUE-P, then if key not found, then puts value in a list, on future appends, just appends that list.  NOTE: If no key found and append, appends as key value (nth-item has no effect). New 2016, sets listsym to new list."
(let
      ((target-list (eval listsym))
       )

  (multiple-value-bind (new-list return-key-found return-old-value
                                 return-new-keylist)
      (append-key-value-in-list  key value target-list 
                               :append-as-flatlist-p append-as-flatlist-p)


    (when set-listsym-to-newlist-p
      (set listsym new-list))

    (values new-list return-key-found return-old-value return-new-keylist)
    ;;end mvb, let, set-key-value
    )))
;;TEST
;; (setf listsymX '(a b (:key1 x y) c (:key2 1 2 3) d e :key3 11 12  :key4 (l m)))
;;   (append-key-value :key2 "NEW VALUE" 'listsymX  :append-as-keylist-p nil )
;; works= (A B (:KEY1 X Y) C (:KEY2 1 "NEW VALUE" 2 3) D E :KEY3 11 12 :KEY4 (L M))  :KEY2  1  (:KEY2 1 "NEW VALUE")
;;
;; key in flatlist, also :set-listsym-to-newlist-p = t
;; (append-key-value :key3 "NEWEST VALUE" 'listsymX  :set-listsym-to-newlist-p T )
;; works= (A B (:KEY1 X Y) C (:KEY2 1 2 3) D E :KEY3 11 "NEWEST VALUE" 12 :KEY4 (L M))  :KEY3  11  (:KEY3 11 "NEWEST VALUE")
;;also
;; CL-USER 44 > listsymX  = (A B (:KEY1 X Y) C (:KEY2 1 2 3) D E :KEY3 11 "NEWEST VALUE" 12 :KEY4 (L M))
;;
;;
;;FOR MORE EXAMPLES, SEE  APPEND-KEY-VALUE-IN-LIST TEST SECTION






;;APPEND-KEY-VALUE-IN-LIST
;;  COMPARE TO set-key-value-in-list and later revise one?
;;
;;ddd
(defun append-key-value-in-list (key value list
                             &key   append-as-flatlist-p
                             append-first-as-flatlist-p
                             list-first-value-p 
                             recursive-call-p)
  "In U-lists, COMPARE TO SET-KEY-VALUE-IN-LIST.  In non-nested list, finds key and replaces next element with value. Works for flat lists and list of  key-lists. If key not found, appends key and value to list. Uses my-equal. RETURNS (values new-list key-found old-value new-keylist) NTH-ITEM = 1 modifies first item after key, etc. . NOTE: If no key found and append, appends as key value (nth-item has no effect). :APPEND-FIRST-AS-FLATLIST-P  causes append list with flat new key and value. Afterwards, APPEND-AS-FLATLIST-P causes any ANY keylist to be form of key old-value new-value.  If  LIST-FIRST-VALUE-P, then if key not found, then puts value in a list, on future appends, just appends that list. NE 2016,Note: The existing key value can only be one value (list, symbol, etc), not a series. If key is at end of list and has no value, then if a list is appended, it is appended as a double-parens list (to which later appends will be put within outer parens."
  (let
      ((key-found-p)
       (key-found)
       (new-element)
       (old-value)
       (new-keylist)
       (new-list) 
       (new-sublist)
       (return-new-keylist)
       (return-old-value)
       (return-key-found)
       (rest-list)
       (len-list (list-length list))
       )
    (when (listp list)
      (loop
       for element in list
       for nth from 1 to len-list 
       do
       ;;FOR LIST ELEMENT, recurse
       (cond
        ((and (listp element)(null key-found-p))
         ;;Use to prevent adding new keylist to every level
#|   not needed?      (when recursive-call-p
           (setf append-as-keylist-p nil
                 append-as-flatlist-p nil))|#

         (multiple-value-setq (new-sublist key-found old-value new-keylist)
             (append-key-value-in-list key value element 
                                       :list-first-value-p list-first-value-p
                                       :append-as-flatlist-p append-as-flatlist-p
                            :recursive-call-p T))
         
         (when key-found 
           (setf return-new-keylist new-keylist
                 return-key-found key-found
                 return-old-value old-value
                 append-as-keylist-p nil
                 append-as-flatlist-p nil))
        ;;(when (equal (car element)  :key2  )(break "key-found?"))
        
         ;;If new-sublist found append it, otherwise append element to new-list
         (cond
          (new-sublist 
           (setf new-list (append new-list (list new-sublist))))
           (t (setf new-list (append new-list (list element)))))
         )
        (T
         ;;FOR NON-LIST ELEMENT
         (cond
          (key-found-p
           (setf return-key-found  key
                 return-old-value element
                 rest-list (nthcdr nth list) ;;only rest of sublist, if a sublist 
                 ;;note (nthcdr 0 '(1 2 3 4))=(1 2 3 4)
                 old-value element
                 new-element value
                 key-found key)
           ;;new-keylist (append new-keylist (list new-element))
           ;;key-found-p nil

           ;;type of append set here 
           (cond
            (append-as-flatlist-p
             (setf new-keylist (list key old-value value)
                   new-list (append new-list (list old-value) (list value) rest-list)))
            ((listp old-value)
             (setf new-keylist (list key (append old-value (list value)))
                   new-list (append new-list (append (list (append old-value (list value))) rest-list))))      
            (t (setf new-keylist  (list key (list old-value value))
                     new-list (append new-list (list (list old-value value)) rest-list))))                            ;; (setf new-list (append new-list (list element value) (nthcdr nth list)))
           (return)
           )
          ((equal key element)
           (setf key-found-p T
                 new-keylist (list key)
                 new-element key))
          (t (setf new-element element)))

         ;;APPEND THE NEW-LIST for this cycle
         (cond
          ;;in case LAST ITEM in list is a key with NO VALUE FOLLOWING IT
          ;;  makes double-parens for first value if value is a list
          ((and (= nth len-list) (equal key element))          
           (setf return-key-found key)
           (cond
            ((listp value)
             (setf new-keylist (list key (list value))))
            (t (setf new-keylist (list key value))))
           ;;then append new-list
           (setf new-list (append new-list new-keylist)))
          (t
           ;;otherwise append new-list with new-element
           (setf new-list (append new-list (list new-element)))))
           ;;end T, cond
           ))
       ;;end loop
       )
   ;;IF KEY NOT FOUND APPEND KEY TO LIST
    ;;(when (null recursive-call-p) (break "at append key"))

    (when (and (null return-key-found)
               (null key-found)(null key-found-p) (null recursive-call-p))
      (cond
       (append-first-as-flatlist-p
        (cond
         (list-first-value-p
          (setf  return-new-keylist (list key (list value))
                 new-list (append new-list return-new-keylist)))
         (t 
          (setf  return-new-keylist (list key value)
                 new-list (append new-list return-new-keylist))))
        )
       (t
        (cond
         (list-first-value-p
          (setf  return-new-keylist (list key (list value))
                 new-list (append new-list (list return-new-keylist))))
         (t
          (setf  return-new-keylist (list key value)
                 new-list (append new-list (list return-new-keylist)))))   ))       

      ;;end inner when and when listp 
      ))

    ;;avoids problem of  new-keylist being reset after recurse 
    (when new-keylist 
      (setf return-new-keylist  new-keylist))
    (when  key-found
      (setf  return-key-found  key-found))
    (when old-value
      (setf  return-old-value old-value))

    (values new-list return-key-found return-old-value return-new-keylist)
    ;;end let, set-key-value-in-list
    ))
;;TEST
;; append keylist in a list
;;  (append-key-value-in-list :key2 "NEW VALUE" '(a b (:key1 x y) c (:key2 1 2 3) d e :key3 11 12  :key4 (l m)) ) 
;; works= (A B (:KEY1 X Y) C (:KEY2 (1 "NEW VALUE") 2 3) D E :KEY3 11 12 :KEY4 (L M))   :KEY2  1  (:KEY2 (1 "NEW VALUE"))
;;
;;append-as-flatlist-p
;; (append-key-value-in-list :key2 "NEW VALUE" '(a b (:key1 x y) c (:key2 1 2 3) d e :key3 11 12  :key4 (l m)) :append-as-flatlist-p T ) 
;;works= (A B (:KEY1 X Y) C (:KEY2 1 "NEW VALUE" 2 3) D E :KEY3 11 12 :KEY4 (L M))   :KEY2   1   (:KEY2 1 "NEW VALUE")

;;If value of :key2 is a list that want to append
;; (append-key-value-in-list :key2 "NEW VALUE" '(a b (:key1 x y) c (:key2 (1 2 3)) d e :key3 11 12  :key4 (l m))) ;  :add-keyvalue-p nil)
;; works= (A B (:KEY1 X Y) C (:KEY2 (1 2 3 "NEW VALUE")) D E :KEY3 11 12 :KEY4 (L M))   :KEY2   (1 2 3)   (:KEY2 (1 2 3 "NEW VALUE"))
;;
;;append as keylist if not found
;;  (append-key-value-in-list :key5  "NEW VALUE" '(a b (:key1 x y) c (:key2 1 2 3) d e :key3 11 12  :key4 (l m)) ) 
;; works= (A B (:KEY1 X Y) C (:KEY2 1 2 3) D E :KEY3 11 12 :KEY4 (L M) (:KEY5 "NEW VALUE"))    NIL   NIL   (:KEY5 "NEW VALUE")
;;
;;list-first-value-p
;; (append-key-value-in-list :key5  "NEW VALUE" '(a b (:key1 x y) c (:key2 1 2 3) d e :key3 11 12  :key4 (l m)) :list-first-value-p T)
;; works= (A B (:KEY1 X Y) C (:KEY2 1 2 3) D E :KEY3 11 12 :KEY4 (L M) (:KEY5 ("NEW VALUE")))    NIL  NIL  (:KEY5 ("NEW VALUE"))
;;
;;append as flat key items if not found
;; (append-key-value-in-list :key5  "NEW VALUE" '(a b (:key1 x y) c (:key2 1 2 3) d e :key3 11 12  :key4 (l m))    :append-as-flatlist-p T)
;; works= (A B (:KEY1 X Y) C (:KEY2 1 2 3) D E :KEY3 11 12 :KEY4 (L M) :KEY5 "NEW VALUE")  NIL  NIL  (:KEY5 "NEW VALUE")
;;
;; (append-key-value-in-list :key3  "NEW VALUE" '(a b (:key1 x y) c (:key2 1 2 3) d e :key3 11 12  :key4 (l m))   )
;; works= (A B (:KEY1 X Y) C (:KEY2 1 2 3) D E :KEY3 11 "NEW VALUE" 12 :KEY4 (L M))  :KEY3  11  (:KEY3 11 "NEW VALUE")

;; ;; (append-key-value-in-list :bipaths '(("father" NIL  (TEST-PCSYM (POLE1) NIL))) '("FATHER" "father" ELM2-1-1-99 NIL NIL))
;; ("father" NIL (TEST-PCSYM (POLE1) NIL))
;; ("FATHER" "father" ELM2-1-1-99 NIL NIL)
;; results= ("FATHER" "father" ELM2-1-1-99 NIL NIL (:BIPATHS (("father" NIL (TEST-PCSYM (POLE1) NIL)))))
;; append again
;; (append-key-value-in-list :bipaths '(("father" NIL  (NEW-PCSYM (POLE1) NIL))) '("FATHER" "father" ELM2-1-1-99 NIL NIL (:BIPATHS (("father" NIL (TEST-PCSYM (POLE1) NIL)))))  )
;; (append-key-value-in-list :key2 '(this) '(a b :key1 (what) d e :key2 (old 1 2) f g 99))
;;  works= (A B :KEY1 (WHAT) D E :KEY2 (OLD 1 2 (THIS)) F G 99)    :KEY2   (OLD 1 2)    (:KEY2 (OLD 1 2 (THIS)))
;; If no value after key (at end of list) ;;puts extra parens on new value if its a list
;; (append-key-value-in-list :key2 '(this) '(a b :key1 (what) d e :key2))
;; works= (A B :KEY1 (WHAT) D E :KEY2 ((THIS)))  :KEY2  NIL  (:KEY2 ((THIS)))
;; If no value and value not a list, no double-parens
;; (append-key-value-in-list :key2 'this '(a b :key1 (what) d e :key2))
;; works= (A B :KEY1 (WHAT) D E :KEY2 THIS)   :KEY2  NIL  (:KEY2 THIS)
;;
;; (append-key-value-in-list :pc-values '(CF    "cf vs ncf"    :SINGLE    "The most important (positive) thing in my life"    "1.000"     12   1   12   12   SCORED-NORMAL   PRIORITY12)   '(:CSQ-DATA-LIST  :PCSYM-ELM-LISTS  ((CF (MOTHER FATHER BEST-M-FRIEND))   (CG (MOTHER FATHER TEACHER))  (CH (MOTHER BEST-M-FRIEND TEACHER))   (CI (FATHER BEST-M-FRIEND TEACHER)))) :append-first-as-flatlist-p T)

;; (append-key-value-in-list :PCSYM-ELM-LISTS '("BEST-M-FRIEND" etc etc)  '(:PCSYM-ELM-LISTS (("BEST-F-FRIEND" "best-f-friend" ELM5-1-1-99 NIL "jan"))))
;; works = (:PCSYM-ELM-LISTS (("BEST-F-FRIEND" "best-f-friend" ELM5-1-1-99 NIL "jan") ("BEST-M-FRIEND" ETC ETC)))        :PCSYM-ELM-LISTS    (("BEST-F-FRIEND" "best-f-friend" ELM5-1-1-99 NIL "jan"))         (:PCSYM-ELM-LISTS (("BEST-F-FRIEND" "best-f-friend" ELM5-1-1-99 NIL "jan") ("BEST-M-FRIEND" ETC ETC)))




;;SET-NTH-IN-LIST
;;2016
;;ddd
(defun set-nth-in-list (value nth list)
  "In U-list, sets nth item in list to value. RETURNS new list."
  (let
      ((new-list)
       )
    (loop
     for item in list
     for n from 0 to (list-length list)
     do
     (cond
      ((= n nth)
       (setf new-list (append new-list (list value))))
      (t (setf new-list (append new-list (list item)))))
     ;;end loop
     )
    new-list
    ;;end let, set-nth-in-list
    ))
;;TEST
;; (set-nth-in-list 'new-value 2  '(1 2 (3 a) 4 5)) = (1 2 NEW-VALUE 4 5)

     




;;SET-KEY-VALUE
;;
;;  loops thru entire list even if key found. May want to use rest-list later, tho that must keep calc cdrs for every element
;;ddd
(defun set-key-value (key value listsym
                          &key (append-as-keylist-p T) append-as-flatlist-p 
                          (set-listsym-to-newlist-p NIL))
  "In U-lists, In a list which LISTSYM evals to, finds key and replaces next element with value. If key not found, appends key and value) to list. Uses equal. RETURNS (values new-list key-found old-value) . APPEND-AS-KEYLIST-P causes a keylist to be appended if no key found, APPEND-AS-FLATLIST-P (has priority) causes key value to be appended. If both are NIL, then nothing appended if key not found.  NOTE: RECURSIVE function, may replace set-key-value-in-nested-lists (this newer, doesn't need key-specs, better for simplier cases?). Use set-key-value-in-list if start with list instead of listsym."
  
  (let
      ((target-list (eval listsym))
       )

  (multiple-value-bind (new-list return-key-found return-old-value
                                 return-new-keylist)
      (set-key-value-in-list  key value target-list 
                      :append-as-keylist-p append-as-keylist-p 
                      :append-as-flatlist-p append-as-flatlist-p)

    (when set-listsym-to-newlist-p
      (set listsym new-list))

    (values new-list return-key-found return-old-value return-new-keylist)
    ;;end mvb, let, set-key-value
    )))
;;TEST
;; for replacing a value in flatlist
;; (setf keylist1 '(a b :key1 99 c d :key2 66 e :key3 '(this)))  
;; (set-key-value :key2 '(new-value)  'keylist1)
;; works= (A B :KEY1 99 C D :KEY2 (NEW-VALUE) E :KEY3 (QUOTE (THIS)))  :KEY2  66   (:KEY2 (NEW-VALUE))
;; for a new key
;; (set-key-value :key4 '(new-value) 'keylist1)
;; works= (A B :KEY1 99 C D :KEY2 66 E :KEY3 (QUOTE (THIS)) (:KEY4 (NEW-VALUE)))  NIL  NIL  (:KEY4 (NEW-VALUE))
;;
;;replace value for key in nested list
;;  (setf keylist2 '(a b :key1 99 c d ( :key2 66 ) e :key3 '(this)))
;;  (set-key-value :key2 '(new-value) 'keylist2)
;; ;;works= (A B :KEY1 99 C D (:KEY2 (NEW-VALUE)) E :KEY3 (QUOTE (THIS)))   :KEY2   66    (:KEY2 (NEW-VALUE))
;;
;; FOR MORE EXAMPLES, SEE TEST AREA BELOW set-key-value-in-list


;;SET-KEY-VALUE-IN-LIST 
;;
;;ddd
(defun set-key-value-in-list (key value list 
                          &key (append-as-keylist-p T) append-as-flatlist-p recursive-call-p)
  "In U-lists, In a  LIST (not LIST-SYM), finds key and replaces next element with value. If key not found, appends key and value) to list. Uses equal. RETURNS (values new-list key-found old-value) . APPEND-AS-KEYLIST-P causes a keylist to be appended if no key found, APPEND-AS-FLATLIST-P (has priority) causes key value to be appended. If both are NIL, then nothing appended if key not found.  NOTE: RECURSIVE function, may replace set-key-value-in-nested-lists (this newer, doesn't need key-specs, better for simplier cases?)"
  (let
      ((key-found-p)
       (key-found)
       (new-element)
       (old-value)
       (new-keylist)
       (new-list) 
       (new-sublist)
       (return-new-keylist)
       (return-old-value)
       (return-key-found)
       (rest-list)
       )
    (when (listp list)
      (loop
       for element in list
       do
       ;;FOR LIST ELEMENT, recurse
       (cond
        ((and (listp element)(null key-found-p))
         ;;Use to prevent adding new keylist to every level
#|   not needed?      (when recursive-call-p
           (setf append-as-keylist-p nil
                 append-as-flatlist-p nil))|#

         (multiple-value-setq (new-sublist key-found old-value new-keylist)
             (set-key-value-in-list key value element  :append-as-keylist-p append-as-keylist-p
                            :append-as-flatlist-p append-as-flatlist-p 
                            :recursive-call-p T))
         
         (when key-found 
           (setf return-new-keylist new-keylist
                 return-key-found key-found
                 return-old-value old-value
                 append-as-keylist-p nil
                 append-as-flatlist-p nil))
        ;;(when (equal (car element)  :key2  )(break "key-found?"))

         ;;If new-sublist found append it, otherwise append element to new-list
         (cond
          (new-sublist 
           (setf new-list (append new-list (list new-sublist))))
           (t (setf new-list (append new-list (list element)))))
         )
        (T
         ;;FOR NON-LIST ELEMENT
         (cond
          (key-found-p
           (setf return-key-found  key
                 return-old-value element
                 return-new-keylist (list key value)
                 old-value element
                 new-element value
                 key-found key
                 new-keylist (append new-keylist (list new-element))
                 key-found-p nil))
          ((equal key element)
           (setf key-found-p T
                 new-keylist (list key)
                 new-element key))
          (t (setf new-element element)))
         (setf new-list (append new-list (list new-element)))
         ;;end T, cond
         ))  
       ;;end loop
       )
   ;;IF KEY NOT FOUND APPEND KEY TO LIST
    ;;(when (null recursive-call-p) (break "at append key"))

    (when (and (null return-key-found)
               (null key-found)(null key-found-p) (null recursive-call-p))
      (cond
       (append-as-flatlist-p
        (setf  return-new-keylist (list key value)
               new-list (append new-list return-new-keylist)))
       (append-as-keylist-p
        (setf  return-new-keylist (list key value)
         new-list (append new-list (list return-new-keylist)))   )        
       (t nil))) 
    ;;(break "At end of append")
      ;;end when listp 
       )

    ;;avoids problem of  new-keylist being reset after recurse 
    (when new-keylist 
      (setf return-new-keylist  new-keylist))
    (when  key-found
      (setf  return-key-found  key-found))
    (when old-value
      (setf  return-old-value old-value))

    (values new-list return-key-found return-old-value return-new-keylist)
    ;;end let, set-key-value-in-list
    ))
;;TEST
;;for flat key list
;;(setf keylist1 '(a b :key1 99 c d :key2 66 e :key3 '(this)))  
;; (set-key-value-in-list :key2 '(new-value)  '(a b :key1 99 c d :key2 66 e :key3 '(this)))
;; works = (A B :KEY1 99 C D :KEY2 (NEW-VALUE) E :KEY3 (QUOTE (THIS)))   :KEY2  66  (:KEY2 (NEW-VALUE))
;;for new key
;;if want whole keylist appended
;; (set-key-value-in-list :key4 '(new-value) '(a b :key1 99 c d :key2 66 e :key3 '(this)))
;; works= (A B :KEY1 99 C D :KEY2 66 E :KEY3 (QUOTE (THIS)) (:KEY4 (NEW-VALUE)))   NIL  NIL  (:KEY4 (NEW-VALUE))
;;for adding a new flatlist key & value
;;  (set-key-value-in-list :key4 '(new-value) '(a b :key1 99 c d :key2 66 e :key3 '(this)) :append-as-flatlist-p T)
;;works= (A B :KEY1 99 C D :KEY2 66 E :KEY3 (QUOTE (THIS)) :KEY4 (NEW-VALUE))   NIL  NIL  (:KEY4 (NEW-VALUE))
;;
;;for keylist in list
;; (set-key-value-in-list :key2 '(new-value) '(a b (:key1 99) c d (:key2 66) e (:key3 '(this))))
;;works= (A B (:KEY1 99) C D (:KEY2 (NEW-VALUE)) E (:KEY3 (QUOTE (THIS))))   :KEY2   66   (:KEY2 (NEW-VALUE))
;;
;;for keylist in middle of nested list
;; (set-key-value-in-list :key1 '(new-value) '(a b (x y :key1 99  w z) c d (:key2 66) e (:key3 '(this))))
;; works= (A B (X Y :KEY1 (NEW-VALUE) W Z) C D (:KEY2 66) E (:KEY3 (QUOTE (THIS))))   :KEY1   99   (:KEY1 (NEW-VALUE))
;;
;;for keylist inside a double-nested-list
;; (set-key-value-in-list :key1 '(new-value) '(a (b) (list1 2 3 (x y :key1 99  w z) 4 5) c d (:key2 66) e (:key3 '(this))))
;; works= (A (B) (LIST1 2 3 (X Y :KEY1 (NEW-VALUE) W Z) 4 5) C D (:KEY2 66) E (:KEY3 (QUOTE (THIS))))  :KEY1  99  (:KEY1 (NEW-VALUE))

;;OTHER
;; (set-key-value-in-list :key1 'new-keyvalue '(6 "newdata nth=1" :KEY1 (C D E) "newdata nth=4"))
;; works= (6 "newdata nth=1" :KEY1 NEW-KEYVALUE "newdata nth=4")   :KEY1   (C D E)   (:KEY1 NEW-KEYVALUE)

;;flatlist
;;  (set-key-value-in-list :info "NEW-FLAT-VALUE" '("MOTHER" "mother" CS1-1-1-1 (DATALIST THIS :KEY1 "that") NIL :INFO "this is info" :KEY3 (A B C)))
;; works= ("MOTHER" "mother" CS1-1-1-1 (DATALIST THIS :KEY1 "that") NIL :INFO "NEW-FLAT-VALUE" :KEY3 (A B C))  :INFO "this is info"  (:INFO "NEW-FLAT-VALUE")

;; simpliest case?
;; (set-key-value-in-list :key2 '(new-value) '(:key2 66))
;; works= (:KEY2 (NEW-VALUE))  :KEY2  66  (:KEY2 (NEW-VALUE))





;;GET-KEY-VALUE
;;
;;modified 2016-04
;;ddd
(defun get-key-value (key list &key (nth-item 1) )
  "In U-lists. gets key value in a flat list of keys or list including keylists. nth-item =1 is value following key.In U-lists, searches a flat list for a key, RETURNS (values value key keylist) the value following key if found, nil if not. Uses my-equal which will even match symbols and strings. NOTE: Differs from GETF which requires a keyword. Any string or symbol can be used for key in get-key-value."
  (let
      ((value)
       (key-foundp)
       (nth 0)
       (keylist)
       )
    (when (listp list)
      (loop
       for element in list
       do
       ;;FOR LISTS (check for key, return if found)
       (when (listp element)
         (when  (my-equal (car element) key)
           (setf keylist element
                 key-foundp key
            value (nth nth-item element))
           (return)))
          
       ;;OTHERWISE continue processing whether list or not
       (cond
        ((my-equal element key)
         (setf  key-foundp key)
         (incf nth))
        ((= nth nth-item)
         (setf value element)
         (return))
        ((> nth 0) (incf  nth))
        (t nil))
       ;;end loop, when
       ))

    (values value key-foundp keylist)
    ;;end let, get-key-value
    ))
;;TEST
;; for non keylists
;; (get-key-value  'key2 '(a b 77 key1 99 key2 a b c key3 99))
;; works=  A  KEY2  NIL
;; (get-key-value  'key2 '(a b 77 key1 99 key2 a b c key3 99) :nth-item 2)
;; works= B  KEY2 NIL
;;FOR KEYLISTS
;;(get-key-value  'key2 '(a b 77 (key1 99) (key2 a b c) (key3 99)))
;;works= A  KEY2  (KEY2 A B C)
;;(get-key-value  'key2 '(a b 77 (key1 99) (key2 a b c) (key3 99)) :nth-item 2))
;; works= B  KEY2  (KEY2 A B C)
;;from older version
;;  (get-key-value "this" '(a b c this (1 2 3) d e f))  =  (1 2 3)   "this" NIL
;;note:
;;  (getf  '(a b c this (1 2 3) d e f) "this") = NIL
;;  (getf  '(a b c this (1 2 3) d e f) 'this) = nil
;;  (getf  '(a b c :this (1 2 3) d e f) :this) = nil
;;  (getf  '(a b c :this (1 2 3) d e f)  1) = NIL





;;GET-KEY-VALUES
;;
;;ddd
(defun get-key-values (key list  &key end-key)
  "In U-lists. gets key values in a flat list of keys searches a flat list for a key, RETURNS (values values key keylist) Returns ALL flat list values following key if found UP TO END-KY (or any key if null end-key), nil if not found. Uses my-equal which will even match symbols and strings. NOTE:  Any string or symbol can be used for key in get-key-value. ALSO works on NESTED LIST."
  (let
      ((key-values)
       (key-foundp)
       (keylist)
       )
    (when (listp list)
      (loop
       for element in list
       do
       ;;(break "b1")
       (cond
        ((my-equal element key)
         (setf  key-foundp key))
        ((and key-foundp (keywordp element))       
         (return))
        (key-foundp
         (setf key-values (append key-values (list element))))
        ((listp element)
#|         (when  (my-equal (car element) key)
           (setf key-foundp key)|#
           (multiple-value-setq (key-values key-foundp keylist)
                 (get-key-values key element :end-key end-key))
           (when key-foundp (return))
           )
        (t nil))
       ;;(BREAK "end loop")
       ;;end loop, when
       ))
    (when key-values
      (setf keylist (append (list key) key-values)))

    (values key-values key-foundp keylist)
    ;;end let, get-key-values
    ))
;;TEST
;; (get-key-values :key2 '(list :key1 a b c (1 2 3) :key2 x (y z) u v :key3 11 22 (this list)))
;; works= (X (Y Z) U V)   :KEY2  (:KEY2 X (Y Z) U V)
;;
;;(get-key-values :key2 '(list :key1 a b c (1 2 3) :key2 x (y z) u v))
;; works= (X (Y Z) U V)  :KEY2  (:KEY2 X (Y Z) U V)
;;
;; (get-key-values :key2 '(list :key1 a b c (1 2 3) ( :key2 x (y z) u v :key3 11 22 (this list)) ex1 ex2))
;; works= (X (Y Z) U V)  :KEY2  (:KEY2 X (Y Z) U V)
;;
;;IN A NESTED LIST
;; (get-key-values :key2 '(list1 this ( :key1 a b c (1 2 3)  :key2 x (y z) u v :key3 11 22 (this list)) ex1 ex2))
;; works= (X (Y Z) U V)  :KEY2   (:KEY2 X (Y Z) U V)
;;
;;DOUBLE-NESTED-LIST
;; (get-key-values :key2 '(list1 that (list2 this ( :key1 a b c (1 2 3)  :key2 x (y z) u v :key3 11 22 (this list)) ex1 ex2)))
;; works = (X (Y Z) U V)  :KEY2  (:KEY2 X (Y Z) U V)






;;SET-KEY-TO-VALUE-IN-PLISTS
;;
;;ddd
(defun set-key-to-value-in-plists (plist-list &key eval-value)
  "In U.lists.lisp, sets key to value eg. '((a 7)(b 'this)....), If eval-value is T, sets the key = (eval value), otherwise sets key = value. RETURNS (values plist-list plist)."
  (let ((key)
        (value)
        (plist)
        )
    (dolist (plist plist-list)
      (cond
       ((= (length plist) 2)
        (cond
         (eval-value 
          (set (car plist) (eval (second plist))))
         (t (set (car plist) (second plist))))))
   ;; (afout 'out (format nil "In    (set (car plist)= ~A (second plist))= ~A (eval (car plist)= ~A~%"  (car plist) (second plist) (eval (car plist))))
    ;;end dolist
    )
    (values plist-list plist)
    ))
;;TEST
;; ;;works
#| (defun testsk ()
  (setf  xxyy 'this)
  (set-key-to-value-in-plists '((a 7)(b 'this)(c xxyy)))  ;; NOTE: b = 'this c= xxyy
  (set-key-to-value-in-plists '((d 8)(e 'that)(f xxyy)) :eval-value t)) ;; e= that, f= this|# 

;;SET-SYMS-TO-VALUES
;;
;;ddd
(defun set-syms-to-values (symlist value-list)
  "In U-lists, sets each symbol in symlist to the corresponding value in value-list"
  (let
      ((sym-value-list)
       )
    (loop
     for sym in symlist
     for value in value-list
     do
     (set sym value)
     (setf sym-value-list (append sym-value-list (list (list sym value))))
     )
    sym-value-list
    ))
;;TEST
;; (set-syms-to-values '(X1 X2 X3) '(99 "THIS" '(WHAT)))
;; works= ((X1 99) (X2 "THIS") (X3 (QUOTE (WHAT))))  
;; also X1 = 99,  x2="THIS",  x3= (QUOTE (WHAT))






;;SSS START HERE MAKING THIS NON-DISTRUCTIVE, also see Seibel p145 and before on destructive modifs.  It seems to work, but ORGANIZE-SUBLISTS STILL DOESN'T EVEN THO I BASE IT ON THIS FUNCTION
;;MY-SORT
;;  A function to NON-DESTRUCTIVELY sort lists (REPLACES SORT for lists)
;;ddd
(defun my-sort (sequence predicate &key key)
  "In U-lists.lisp, function to NON-DESTRUCTIVELY sorts sequences. REPLACES SORT.Eg.  (my-sort list-of-lists #'test-greaterp  :key 'car) also (my-sort \"xylmno\" #'char>) RETURNS  (values new-sequence sequence). Inner lists less than symbols."
  (let
      ((new-sequence (copy-seq sequence))
       )
    (setf new-sequence (sort  new-sequence predicate :key key))
    (values new-sequence sequence)
    ))
;;TEST
;;   (my-sort "xylmno" #'char>) =  "yxonml" "xylmno"
;;
;;  (my-sort '(this a 7 0 and) #'test-lessp)
;; works= (0 7 A AND THIS)  (THIS A 7 0 AND)
;;  (my-sort '(this (mand) 7 (1 2 3)(and)) #'test-lessp)
;; results=  ((1 2 3) (AND) (MAND) 7 THIS)   (THIS (MAND) 7 (1 2 3) (AND))


  
#|(defun testsl2 ()
  (setf out nil)
  (let
      ;;must use flat lists
      ((sublists '((x 1 2)(f 3)(2 6 5)(b 3 2) (99 6 2)))
       (sublists1 '((4 3)(1 6)(8 7 3)(5 a b)))

       (y1)(y2)(y3)(y4)(y5)(o1)(o2)
       )
    ;;   (sort sublists #'test-lessp :key 'car)))
    (multiple-value-setq (y1 o1)   (my-sort sublists1 #'test-greaterp  :key 'car))
;;   (setf y1 (organize-sublists sublists1 :descend-sort t))
   ;;(afout  'out (format nil "sublist1= ~A~% y1= ~A~%o1= ~A~% " sublists1 y1 o1))
   ;;works-- afout=> sublist1= ((4 3) (1 6) (8 7 3) (5 A B))
    ;; y1= ((8 7 3) (5 A B) (4 3) (1 6))
    ;; o1= ((4 3) (1 6) (8 7 3) (5 A B))
;;    (setf y1  (my-sort sublists #'test-lessp  :key 'car))
;;   (setf y2 (organize-sublists sublists1 :ascend-sort t))
;;  (afout 'out (format nil "sublists1= ~A~% o2= ~A~%" sublists1 o2))
;;    (setf  y3 (organize-sublists sublists1   :randomize t))
;;   (afout 'out (format nil "sublists1= ~A~%" sublists1))
;;   (afout 'out (format nil "sublists1= ~A~%" sublists1))
;;    (setf y4 (sort sublists1 #'test-lessp :key 'car))
;;   (afout 'out (format nil "sublists1= ~A~%" sublists1))
;;    (setf y5 (sort sublists1 #'test-greaterp :key 'car))
;;   (afout 'out (format nil "sublists1= ~A~%" sublists1))
    (fout out)
    (values y1 y2 y3 y4 y5)
    ))|#




;;ORGANIZE-SUBLISTS
;;
;;ddd
(defun organize-sublists (sublists &key randomize descend-sort ascend-sort)
  "In U-lists.lisp, used by screensaver, sorts subdirs. MUST FLATTEN TREE DIRS FIRST--returns orgnaized list"
  (let*
      ((list-length (length sublists))
       (n)
       (sublist)
       (organized-sublists)
       )
    (cond
     (randomize 
      (setf organized-sublists (my-randomize-list sublists)))
     (ascend-sort
      (setf organized-sublists  (my-sort sublists #'test-lessp  :key 'car)))
     ;;2013-12 was (sort sublists #'test-lessp :key 'car))) destructively modifies old list
     (descend-sort
      (setf organized-sublists  (my-sort sublists #'test-greaterp :key 'car)))
     ;;2013-12 was  (sort sublists #'test-greaterp :key 'car)))  
     )
    ;;  (afout 'out (format nil "randomize= ~A descend-sort=~A ascend-sort=~A~%   organized-sublists= ~A~%"  randomize descend-sort ascend-sort organized-sublists))
    organized-sublists))

;;test organize-sublists
#|(defun testsl ()
  (setf out nil)
  (let
      ;;must use flat lists
      ((sublists '((x 1 2)(f 3)(2 6 5)(b 3 2) (99 6 2)))
       (sublists1 '((4 3)(1 6)(8 7 3)(5 a b)))

       (y1)(y2)(y3)(y4)(y5)
       )
    ;;   (sort sublists #'test-lessp :key 'car)))
  (setf y1 (organize-sublists sublists1 :descend-sort t))
;;   (afout 'out (format nil "sublists1= ~A~%" sublists1))
   (setf y2 (organize-sublists sublists1 :ascend-sort t))
;;  (afout 'out (format nil "sublists1= ~A~%" sublists1))
    (setf  y3 (organize-sublists sublists1   :randomize t))
;;   (afout 'out (format nil "sublists1= ~A~%" sublists1))
;;   (afout 'out (format nil "sublists1= ~A~%" sublists1))
    (setf y4 (my-sort sublists1 #'test-lessp :key 'car))
;;   (afout 'out (format nil "sublists1= ~A~%" sublists1))
    (setf y5 (my-sort sublists1 #'test-greaterp :key 'car))
   ;;(afout 'out (format nil "sublists1= ~A~%" sublists1))
    (fout out)
    (values y1 y2 y3 y4 y5)
    ))|#
;;works returns
#| ((8 7 3) (5 A B) (4 3) (1 6))  ;;>
((1 6) (4 3) (5 A B) (8 7 3))    ;;<
((8 7 3) (4 3) (1 6) (5 A B))    ;;random
((1 6) (4 3) (5 A B) (8 7 3))    ;;<
((8 7 3) (5 A B) (4 3) (1 6))    ;;>
|#



;;MY-RANDOMIZE-LIST
;;
;;ddd
(defun my-randomize-list (list)  
  "In U-lists.lisp, takes a list or non-nested list-of-lists and non-destructively randomizes it's elements.  Serves as default element in case somehow replacement with real item not made."
  (let*
      ((list-length (list-length list))
       (new-list)  
       (rest-list list)
       (rest-n  list-length)
       ( item)
       (n)
       )
    ;;  (afout 'out (format nil "rest-n= ~A~%" rest-n))
    (loop
     for i from 1 to list-length
     do
     (cond
      ((<  i list-length)
       (setf n  (random (- rest-n 1)))
       (setf item (nth n rest-list))
       (setf new-list (append new-list (list item)))
       ;;may remove wrong item if multiple identical items, tho for randomization, that is ok
       (setf rest-list (remove-list-nth n rest-list))    ;;was (remove item rest-list))
       (decf  rest-n)
       )
      (t 
       (setf item (car rest-list)
             new-list (append new-list (list item)))))
     ;;(afout 'out (format nil "n= ~A new-list= ~A~%rest-list= ~A~%" n new-list rest-list))
     )
    new-list
    ;;end let, my-randomize-list
    ))
;;TEST
;; (my-randomize-list '(a b c d e f g)) 
;;  works= (F B D A E C G)
#|(defun testmr ()
  (setf out nil)
  (let
      ((list '(x a (1 2) (l m n) 99  3))
       )
  ;;  (fout out)
    (my-randomize-lists list)))|#
;;works returns (A (L M N) X 99 (1 2) 3),  ((L M N) 99 X (1 2) A 3), then ((1 2) A 99 (L M N) X 3)




;;MY-RANDOM-SELECTION
;;2016
;;ddd
(defun my-random-selection (list percent &key total-n selections )
  "In U-lists, RETURNS (values selections rest-list)"
  (let
      ((list-n (list-length list))
       (selection)
       (selected-n)
       (percent-used)
       (new-list)
       )
    (cond
     ((> list-n 5000)
      (extend-current-stack 400))
     ((> list-n 3000)
      (extend-current-stack 300))
     ((> list-n 1000)
      (extend-current-stack 100))
     (t nil))     
    
    (cond
     ((null total-n) 
      (setf total-n list-n
            percent-used 0))
      (t (setf percent-used (* 100 (/ (- total-n list-n) total-n)))))

    (when (and (< percent-used  percent)(> list-n 0))
      (setf selected-n (random list-n))
      (multiple-value-setq (new-list selection)
            (delete-nth list selected-n ))
      (setf selections (append selections (list selection)))
      ;;recurse
      (multiple-value-setq (selections list)
          (my-random-selection new-list percent :total-n total-n
                               :selections selections))
      ;;end when
      )
    (when (= list-n 0)(break))
    (values selections list)
    ;;end let, my-random-selection
    ))
;;TEST
;;  (my-random-selection '(1 2 3 4 5 6 7)  100) = (2 3 6 4 1 7 5)  NIL
;;  (my-random-selection '(1 2 3 4 5 6 7)  0) = NIL  (1 2 3 4 5 6 7)
;;  (my-random-selection '(1 2 3 4 5 6 7)  30) = (7 3 5)   (1 2 4 6)













;;DELETE-ITEMS-FROM-LIST
;;
;;ddd
(defun delete-items-from-list (item-list list 
                             &key from-end (test  'equal)  (start 0) end remove-only-first-p )
  "Uses CL delete.  Inefficient to use because searches entire list for each item in the list. Here count is the TOTAL of all items removed--not separate count for each item removed."
  (let*
      (;;(new-list list)
       (test-list list)
       (newlist-length) 
       (butlast-n 0)
       (result-list)
       (removed-items)
       (removed-items1)
       )
    (if from-end
        (setf test-list (reverse test-list)))
    (setf test-list (nthcdr start test-list))
    (when end
        (setf  newlist-length (list-length test-list)
               butlast-n (- newlist-length end)
               test-list (butlast  test-list butlast-n)))

#|    (loop
     for item in item-list
     do|#
    ;;(afout 'out (format nil "test-list= ~A~%" test-list))
     (loop
      for testitem in test-list
      for n from 0 to (list-length test-list)
      do
     (cond
      ((member testitem item-list :test test)
       (cond
        (remove-only-first-p
         (setf removed-items (append removed-items (list testitem))
               item-list (delete testitem item-list))
         )
        (t  ;;do nothing to result-list
         (setf removed-items (append removed-items (list testitem)))))
       )
      ;;testitem not a member of the item-list
      (t (setf result-list (append result-list (list testitem)))))
         ;;(afout 'out (format nil "result-list= ~A~%" result-list))
     ;;end loop
     )
    (if from-end
        (setf result-list (reverse result-list)))

    (values result-list removed-items)
    ;;end let,delete-items-from-list
    ))
;;TEST
;; (setf out nil)
;;(delete-items-from-list '("SUBSCALE" "COMPOSITE-SCALE" "BIO-TEXT") '("SUBSCALE" "COMPOSITE-SCALE" "BIO-TEXT" "ST1HIGHERSELF" "ST2SOCINTIMNOFAM" "ST3FAMCARE" "ST4SUCCESSSTATUSMATER" "ST5-ORDERP") :test  'equal)
;;works= ("ST1HIGHERSELF" "ST2SOCINTIMNOFAM" "ST3FAMCARE" "ST4SUCCESSSTATUSMATER" "ST5-ORDERP")
;; (delete-items-from-list '(c d x)  '(a b c d e f c c l m x bb  x y z))
;; works = (A B E F L M BB Y Z)  (C D C C X X)
;; (delete-items-from-list '(c d x)  '(a b c d e f c c l m x bb  x y z) :remove-only-first-p T)
;; works = (A B E F C C L M BB X Y Z)  (C D X)
;; (delete-items-from-list '(c d x)  '(a b c d e f c c l m x bb  x y z) :remove-only-first-p T :from-end T)
;;works = (A B C E F C L M X BB Y Z)  (X C D)
;;  ;; (delete-items-from-list '(3  6  8)  '(0 1 2 3 4 5 6 7 8 9  10 11) :start 3 :end 7)
;; works = (4 5 7 9)  (3 6 8)




;;DELETE-ITEMS-FROM-LIST-COUNT
;;
;;ddd
(defun delete-items-from-list-COUNT (item-list list 
                             &key from-end (test  'equal) test-not (start 0) end count )
  "Uses CL delete.  Inefficient to use because searches entire list for each item in the list. However, this version has SOME KEYS NOT IN BETTER VERSION."
  (let
      ((new-list list)
       )
    (loop
     for item in item-list
     do
     (setf new-list (delete item new-list  :from-end from-end  :test test  :test-not test-not :start start :end end :count count ))
     ;;end loop
     )
    new-list
    ;;end let,delete-items-from-list
    ))
;;TEST
;;(delete-items-from-list '("SUBSCALE" "COMPOSITE-SCALE" "BIO-TEXT") '("SUBSCALE" "COMPOSITE-SCALE" "BIO-TEXT" "ST1HIGHERSELF" "ST2SOCINTIMNOFAM" "ST3FAMCARE" "ST4SUCCESSSTATUSMATER" "ST5-ORDERP") :test  'equal)
;;works= ("ST1HIGHERSELF" "ST2SOCINTIMNOFAM" "ST3FAMCARE" "ST4SUCCESSSTATUSMATER" "ST5-ORDERP")
;;



;;REMOVE-LIST-NTH
;;
;;ddd
(defun remove-list-nth (nth list &key from-end)
  "Like Remove for lists only, but removes (nondestructively) the nth item. Returns nil if nth > list-length."
  (let
      ((list-length (list-length list))
       (new-list)
       )
    (if  from-end
        (setf nth (- list-length (+ nth 1))))
    (loop
     for element in list
     for i from 0 to (- list-length 1)
     do
     (if (> nth list-length) (return nil))
     (unless (= i nth)
       (setf new-list (append new-list (list element))))
     )
    new-list))

;;test

(defun testmrl ()
  (let ((list '(a b c (1 2 3) d (4 5) e))
        )
    (remove-list-nth 2 list :from-end t)
    ))
;;  (testmrl) nth= 3 works returns (A B C D (4 5) E)
;; nth= 9 returns nil as it should
;; nth= 2 from-end t   returns (A B C (1 2 3) (4 5) E)
    
  ;;  (length '(a b c d))

#|
 (remove 2 '(1 2 3 4 5))
(let ((list '(a b c d))
      (new-list))
      (setf new-list (setf (nth 2 list)  'this))
  list)  ;;replaces 2nd => (A B THIS D)
|#
;;  (testsl)
       ;;NOTE--these functions DESTRUCTIVELY CHANGE THE LISTS, 
       ;; therefore, y1-y5 point to underlying lists THAT HAVE CHANGED??
       ;; SO THEY ALL REPORT THE SAME VALUES??
       ;;--EVIDENCE-- FOUT SHOWS CHANGES, THE VALUES Y1-Y5 DON'T
       ;;  ALSO, these functions WORK IF DONE ALONE, but when repeated with same
      ;;    sublist arg they DO NOT WORK.


;;------- UNDERSTANDING THE SORT FUNCTION ----
;; It DESCRUCTIVELY MODIFIES LISTS, ETC.
;; EXAMPLES
#|
(defun testsort ()
  (let
      ((list '(a x y  b  cat))
       (result)
       )
    (setf result (sort list #'string-lessp))
    (setf rx2 list)
    (values list result)))
;;returns (A B CAT X Y)  (A B CAT X Y);; so changes value of list  (also sets rx2 to changed list)
|#

;;works (sort '(c  x 99 d 3 f 1) #'test-lessp)

;;(sort '((x 1 2)(f 3)(2 6 5)(b 3 2) (99 6 2))  #'test-lessp :key 'car )
#|
(char "xyz" 0) = #\x
(char  897 0) = error
(char  (format nil "~A" 897) 0) = #\8 
;; works (test-car 'this '(this that))
|#

;;test works
#|
(defun tcl ()
  (test-lessp 3 (car '(5 a b))))
;;
(defun tcl2 ()
  (test-lessp 5  (car '(9 21))))
(car '(11 1))

(defun tcl3 ()
  (test-lessp "this" (car '("what" "apple"))))
|#

;;ddd
(defun test-lessp (item1 item2)
  "In U-lists.lisp, tests item vs (car list) for any type items for 'lessp"
  (test-any-type item1 item2 'lessp))

;;ddd
(defun test-greaterp (item1 item2)
  "In U-lists.lisp, tests item vs (car list) for any type items for 'greaterp"
   (test-any-type item1 item2 'greaterp))
;;didn't help  (string-test-any-type item1 item2 'greaterp))

;;ddd
(defun test-equal (item1 item2)
  "In U-lists.lisp, tests item vs (car list) for any type items for  'equalp"
  (test-any-type item1 item2 'equal))  
#|
(test-any-type 9 (car '(11 21)) 'lessp)
(test-any-type 9 11 'lessp)  ;;me, may only evaluate the FIRST char so 11 <  9 
(test-any-type 'y 'x 'lessp)
(string-lessp "31" "21")
(
|#
;;
(defun test-any-type (item1 item2 test)
  "In U-lists.lisp, tests item vs (car list) for any type items for 'lessp 'greaterp or 'equalp, case-insensitive."
  (let
      ((item1-char)   ;;(format nil "~A" item1))
       (item2-char)   ;; (format nil "~A" item2))
       (result)
       )
    ;;test to see if both are numbers--use number tests
    (cond 
     ((and (numberp item1) (numberp item2))
      (setf item1-char item1
            item2-char item2)
      (format t "1= ~A   2=  ~A" item1-char item2-char)
      (cond
       ((equal test 'lessp)
        (setf result (<  item1-char item2-char)))
       ((equal test 'greaterp)
        (setf result  (> item1-char item2-char)))
       ((equal test 'equal)
        (setf result (= item1-char item2-char)))
       (t nil))
      result
      )
     ;;if not convert both to strings--use string tests
     (t 
      (setf item1-char (format nil "~A" item1)
            item2-char (format nil "~A" item2))

      (format t "1= ~A   2=  ~A" item1-char item2-char)
      (cond
       ((equal test 'lessp)
        (setf result (string-lessp item1-char item2-char)))
       ((equal test 'greaterp)
        (setf result  (string-greaterp item1-char item2-char)))
       ((equal test 'equal)
        (setf result (if stringp item1 (string-equal item1-char item2-char))))
       (t nil))
      ))
    result
    ))

(defun string-test-any-type (item1 item2 test)
  "In U-lists.lisp, tests item vs (car list) for any type items for 'lessp 'greaterp or 'equalp, by CONVERTING ALL TO STRINGS FIRST"
  (let
      ((item1-char)   ;;(format nil "~A" item1))
       (item2-char)   ;; (format nil "~A" item2))
       (result)
       )
      (setf item1-char (format nil "~A" item1)
            item2-char (format nil "~A" item2))
      (format t "1= ~A   2=  ~A" item1-char item2-char)
      (cond
       ((equal test 'lessp)
        (setf result (string-lessp item1-char item2-char)))
       ((equal test 'greaterp)
        (setf result  (string-greaterp item1-char item2-char)))
       ((equal test 'equal)
        (setf result (string-equal item1-char item2-char))) ;;was string-equal
       (t nil))
    result
    ))

#|
(string-lessp "abc" "xyz") = 0
(string-lessp  "xyz" "abc") = nil
(string-lessp  "9" "2") = nil
(string-lessp   "2" "9") = 0
|#
;;works



;;INTEGERS-LIST-P
;;
;;ddd
(defun integers-list-p (list &key exceptions)
 "U-LISTS,  RETURNS N-ITEMS if it is a list with all integers. If exceptions, excepts any item in exceptions list."
 (let
     ((n-items 0)
      )
   (when (listp list)
     (dolist (item list)
       (cond
        ((my-member item exceptions) NIL)
        ((integerp item)
         (incf n-items))
        (t (setf n-items nil)
           (return)))))
   n-items
   ))
;;TEST
;; (integers-list-p '(1 2 3 4 5)) = 5
;; (integers-list-p '(1 2 3  a 4 5))  = NIL
;; (integers-list-p '(1 2 3 TO 4 5 6) :exceptions '(THIS TO)) = 6
;; (integers-list-p '(1 2 3 TO 4 5 6) :exceptions '(THIS "TO")) = 6


















(defun testflt ()
  (flatten-list-tree '(1 2 3 4 (a b c) (d (e f (g)) h) (i (j) k))))  
#|=> FINAL, flat-list= ((A B C) (D) (E F) (G H) (I) (J K))
 flat-sublist= (J K)
 simple-list= (A B C D E F G H I J K)|#

;;works  (flatten-list-tree '(a b c (d) (e f (g h)))))
;;
;;FLATTEN-LIST-TREE
;;ddd
(defun flatten-list-tree (list-tree)
  "In U-lists.lisp, takes a list tree and removes extra parens of sublists to return values
    a flat list ol sublists and single list of all elements"
  (let 
      ((flat-list)
       (flat-sublist)
       (simple-list)
       (sublist)
       (element)
       )
  (multiple-value-setq (flat-list flat-sublist simple-list)
      (flatten-list-tree1 list-tree flat-list simple-list))
  (if flat-sublist (setf flat-list (append flat-list (list flat-sublist))))
 ;;(show-text (format nil "FINAL, flat-list= ~A~% simple-list= ~A~%"flat-list  simple-list) 200 t)
  (values flat-list simple-list)))

;;ddd
(defun flatten-list-tree1 (list-tree &optional flat-sublist simple-list)
  (let 
      ((last-sublist)
       (flat-list)
       (return-flat-sublist)
       (simple-sublist)
       )
    (cond
     ((> (length list-tree) 0)
      (dolist (element list-tree)
        (cond
         ((null element) nil)
         ((not (listp element))
          ;;   (show-text
          ;;           (format nil "element= ~A~% flat-list= ~A~%" element flat-list) 100 t)
          (setf flat-sublist (append flat-sublist (list element))
                simple-list (append  simple-list (list element))))
         (t
          (if flat-sublist  (setf flat-list (append flat-list (list flat-sublist))))
          (multiple-value-setq (return-flat-sublist flat-sublist simple-sublist)
              (flatten-list-tree1 element)) ;; flat-sublist))  ;; flat-list simple-list)
          ;;either of above returns a list             
          (if  return-flat-sublist  
              (setf flat-list  (append  flat-list  return-flat-sublist)))  ;;was (list flat-sublist))
          (setf  simple-list (append simple-list simple-sublist))))))
     ;;if no more items in dir list, append the flat-sublist to flat-list
     (t nil ))
    #| (show-text (format nil "1 FLATTEN, flat-list= ~a~% flat-sublist= ~a~% simple-list= ~a~% "    flat-list flat-sublist simple-list) 200 T)|#
    (values flat-list flat-sublist simple-list)))


;;FLAT-LIST-EQUAL
;;
;;(flat-list-equal '(a b c) '(a b c)) = (A B C) A
;;(flat-list-equal '(a b c) '(a d  c)) = nil B
;;(flat-list-equal '(a b c) '(a b  c d)) = nil nil
;;(flat-list-equal '(a b c d e) '(a b  c d)) = nil E
(defun flat-list-equal (list1 list2)
  "In U-lists.lisp, tests to see if two flat lists are identical; returns values list1 & first item OR nil and first item not equal, "
  (let
      ((item (car list1))
       (return-item)
       (return-list)
       )
    ;;(afout  'eout (format nil "item= ~A  list1= ~A~%list2= ~A~%" item list1 list2))
      (cond
       ((equal item (car list2))
        (setf return-item item
              return-list (cdr list1))
        (unless (null item)
          (multiple-value-setq (return-list return-item) (flat-list-equal return-list (cdr list2)))))
       (t (setq return-list nil 
                return-item item)))
      (values return-list return-item)))



;;MY-MAKE-LIST
;;
;;ddd
(defun my-make-list (length &key (initial-element 0)  delta-element-function fun-rest-args)
  "In U-list.lisp, makes a list length long.  If delta-element-function = NIL, just uses CL make-list.  If delta-element-function (eg. 'incf), then applies delta-element-function to the previous element= first arg with fun-rest-args as the rest of the args to make the new computed value for the next element."
  (let
      ((element initial-element)
       ;;(new-element)
       (new-list)
       (times-n)
       )
    (cond
     (delta-element-function
      (cond
       (initial-element
        (setf new-list (append new-list (list initial-element))
              times-n (- length 1)))
        (t (setf times-n length)))
      (dotimes (n times-n)
        (cond
         (fun-rest-args
          (setf element (eval (append `(,delta-element-function ,element) fun-rest-args))))
         (t (setf element (eval `(,delta-element-function ,element)))))
              ;;(apply delta-element-function  (list 'element)))
        (setf new-list (append new-list (list element)))
        ))
     (element
      (setf new-list (make-list length :initial-element element)))
     (t (setf new-list (make-list length))))
    new-list
    ;;end let, my-make-list
    ))
;;TEST
;;  (my-make-list 7) = (NIL NIL NIL NIL NIL NIL NIL)
;;  (my-make-list 7 :initial-element 'a) = (A A A A A A A)
;;  (my-make-list 7 :initial-element 0 :delta-element-function  '+ :fun-rest-args '(1) ) = (1 2 3 4 5 6 7)
;;  (progn (defun myadd3 (x) (+ x 3)) (my-make-list 4 :initial-element 0 :delta-element-function  'myadd3)) = (3 6 9 12)
;;  (my-make-list 7  :delta-element-function  '+ :fun-rest-args '(1) ) = (0 1 2 3 4 5 6)
;; (my-make-list 7 :initial-element 1  :delta-element-function  '+ :fun-rest-args '(1) ) = (1 2 3 4 5 6 7)



;;MAKE-LIST-N-LONG
; 
;;ddd
(defun make-list-n-long ( length list &key default-element second-list)
  "In U-list.lisp, makes a list length long by starting with list and either adding a second-list or default-element (s) or both to fill in extra length.  NIL used otherwise.  If  length < list length, then it cuts off items. Also can use MAKE-LIST or MY-MAKE-LIST"
  (let
      ((new-list (make-list length :initial-element default-element))
       (list-length (list-length list))
       (second-list-length 0)
       (n2 0)
       (item)
       )
    (if second-list
               (setf second-list-length (list-length second-list)))
    (loop
     for n from 0 to length
     do
     (cond
      ((< n list-length)
       (setf item (nth n list)
        new-list (replace-list new-list n item)))
      ((and (>= n list-length) second-list (< n (+ list-length 1)))  ;; second-list-length -1)))
         (loop
          for item2 in second-list
          for n2 from n to (+ n second-list-length)
          do
           (setf new-list (replace-list new-list n2 item2))
           ;;end 2nd loop and (>= n list-length)
           ))
      (t  (return new-list)))
     ;;(afout  'out (format nil "In make-list-n-long, new-list= ~A~%  n= ~A  n2= ~A list-length= ~A second-list-length= ~A~%" new-list n n2 list-length second-list-length))
     ;;end loop
     )
    new-list
    ))

 ;;test
#|
(defun testmnl ()
  (setf out nil)
  (let
      ((new-list)
       (list '(a b c d))
       (length 9)
       )
 ;; (setf new-list (make-list-n-long length list)) 
  (setf new-list (make-list-n-long length list :default-element 0 :second-list '(1 2 3)))
   ;;works returns (A B C D 1 2 3 0 0)
  (fout out)
  new-list
   ))
|#
;;  (testmnl)
;; with no keys returns (A B C D NIL NIL NIL NIL NIL)
;;with 



;;PRINT-LIST-WITH-NEWLINES
;;
;;ddd
(defun print-list-with-newlines (list out-stream &key without-quotes)
  "In U-files.lisp. Prints list with newlines to out-stream -- can be NIL or T. without-quotes only works for lists of strings. NOTE: Prints symbols to CAPS."
  (let
      ((new-list)
       )
    (dolist (item list)
      (cond
       (without-quotes
        (format out-stream "~A" item)(format out-stream "~%"))
       (t 
        (cond
         ((stringp item)
          (format out-stream "~S" item)(format out-stream "~%"))
         (t
          (format out-stream "~C~A~C" #\" item #\")(format out-stream "~%")))))
      )
    new-list
    ))
;; (print-list-with-newlines *spss-var-names8 t :without-quotes nil)
;;(print-list-with-newlines   '(CaseNum  CaseType  Group  var2  FileDate  Instr  Resr  Name  SSN  Sex  Age  Email  ZipCode  Nation  HrsWork  UserRate  tknowmor  texperie  twanttho  twantspe  tworknga  tu100stu) t :without-quotes nil)
;;works, returns; NOTE: ALL ARE IN CAPS
#|"CASENUM"
"CASETYPE"
"GROUP"
"VAR2"
"FILEDATE"
... etc NIL|#
;; (print-list-with-newlines   '(CaseNum  CaseType  Group  var2  FileDate  Instr  Resr  Name  SSN  Sex  Age  Email  ZipCode  Nation  HrsWork  UserRate  tknowmor  texperie  twanttho  twantspe  tworknga  tu100stu) t :without-quotes t)
;;works, returns NOTE: ALL ARE IN CAPS
#|CASENUM
CASETYPE
GROUP
VAR2
FILEDATE
INSTR|#
;;(print-list-with-newlines   '("CaseNum"  "CaseType"  "Group"  "var2")  t :without-quotes nil) = works, returns:
#|"CaseNum"
"CaseType"
"Group"
"var2"
NIL|#
;;  (print-list-with-newlines   '("CaseNum"  "CaseType"  "Group"  "var2")  t :without-quotes t) = works, returns
#|CaseNum
CaseType
Group
var2
NIL|#



;;FIND-GREATEST-MAX-SUBTRACT-LIST
;;
;;ddd
;; NOT USED?--- FIX LATER-DOESN'T RETURN WHAT IT'S SUPPOSED TO
(defun find-greatest-max-subtract-list (list-of-lists)
  "In U-lists.lisp, subtracts each first-item from second-item and returns the entire list from the list-of-lists. RETURNS (values results-list max-dif). Used in fuzzy-matcher."
  (let
      ((item1 0)
       (item2 0)
       (dif 0)
       (new-results-lists)       
       (n -1)
       (max-dif)
       (list-n)
       (result-list)
       )
    (dolist (list list-of-lists)
      (incf n)
      (setf item1 (first list)
            item2 (second list)
            dif  (- item2 item1)
            new-results-lists (append new-results-lists (list (list dif  n list))))
      ;;(afout  'out (format nil "item1= ~A item2= ~A~%  dif= ~A new-results-lists= ~A~%"  item1 item2  dif  new-results-lists))
      ;;end dolist
      )
    (setf max-dif (apply 'max (mapcar #'car new-results-lists)))
    (setf  list-n (get-key-value-in-nested-lists `((,max-dif  0 )) 
                                                 new-results-lists :return-list-p t)
           result-list (third (nth n list-of-lists)))
 ;;end find-greatest-max-subtract-list  
 (values result-list max-dif)
 ))
;;test
;; NOT USED?--- FIX LATER-DOESN'T RETURN WHAT IT'S SUPPOSED TO
;; (progn (setf out nil)  (find-greatest-max-subtract-list '((1 2 a)(1 6 '(a b))(2 5 x))))
;; works, returns
;;  (find-greatest-max-subtract-list '((1 2 a)(1 6 '(a b))(2 5 x)))

  ;;   (nth 1 '(a b c))
  ;;works (apply 'max (mapcar #'car '((1 a)(2 b)(3 c)))) = 3
;;  (apply 'max (mapcar #'car   '((1 0 (1 2 A)) (5 1 (1 6 (QUOTE (A B)))) (3 2 (2 5 X)))))

#|(defun find-key-value-in-nested-lists (key list-of-lists &key return-list-p
                                           (find-nth 0) (return-nth 0) ) 
  "In U-lists.lisp, RETURNS first value that matches key (values value key). find-nth searches for nth item in the list for the first key. if return-nth, returns nth instead of second item in list. :find-key-in-nested looks for the key in the nth-key of the second order nested lists--not in first list set.  If  return-list-p, returns entire sublist. "
  (let
      ((item)
       (value)
       (list-length (list-length list-of-lists))
       )
    (dolist (list list-of-lists)
      (unless (>= find-nth)
      (setf item (nth find-nth list))
      
      (cond
       ((equal key item)
        (cond
         (return-list-p
          (setf value list))
         ((numberp return-nth)
          (setf value (nth return-nth list)))
         (t (setf value (second list))))
        (return))
       (t nil))
      ;;end do, unless
       ))
    (values value key)
    ))
|#

#| REPLACED BY GET-KEY-VALUE-IN-NESTED-LISTS
(defun find-key-value-in-lists2  (key list-of-lists &key return-list-p
                                           (find-nth 0) (return-nth 1) ) 
  "In U-lists.lisp, RETURNS first value that matches key (values value key). find-nth searches for nth item in the list for the first key. if return-nth, returns nth instead of second item in list. :find-outer-key looks for the key in the nth-key of the second order nested lists--not in first list set.  If  return-list-p, returns entire sublist. "
  (let
      ((result-value)
       (result-key)
       (extra-items)
       )
      (loop
       for item in list-of-lists
       with match-item
       with item-length
       do
       ;;(afout 'out (format nil "LOOP item ~A~%" item))   
       (cond
        ((listp item)
         (setf item-length (list-length item))       
         (unless (>= find-nth  item-length))
         (setf match-item (nth find-nth item))
       ;; (afout 'out (format  nil "TESTING key= ~A  match-item= ~A~%  IN find-key-value-in-lists" key match-item))
         (cond
          ((or (equal key match-item) (if (stringp match-item) (string-equal key match-item)))
           (cond
            (return-list-p
             (setf result-value item
                   result-key match-item))
            (t
             (setf result-value (nth return-nth item)
                   result-key key)))
           (return))
          (t nil))
         ;;end listp item
         )
        ;;if item is not a list, then item may be outer keys or info want to keep
        (t (if  item (setf extra-items (append extra-items (list item))))))
       ;;end loop
       )
  (values result-value result-key extra-items)
  ))|#
;;test
;;  (progn (setf out nil)  (find-key-value-in-lists    "acmESOCSTudy" '(PC-INSTANCES  "iAcademicMotivation.java"      ("[]questionInstancesArray1)")      ("acmNDROPcourses" "30" "acmNDROPcoursesQ" "int" "FrAnswerPanel.LikeMe7" "questionInstancesArray" "frameTitle" "frameDimWidth" "frameDimHeight")      ("acmESOCSTudy" "3" "acmESOCSTudyQ" "int" "FrAnswerPanel.LikeMe7" "questionInstancesArray" "frameTitle" "frameDimWidth" "frameDimHeight")) :return-list-p  t))
;;works, returns ("acmESOCSTudy" "3" "acmESOCSTudyQ" "int" "FrAnswerPanel.LikeMe7" "questionInstancesArray" "frameTitle" "frameDimWidth" "frameDimHeight")  "acmESOCSTudy"  NIL  (PC-INSTANCES "iAcademicMotivation.java")

;;  (find-key-value-in-lists 'this  '((a b)(c (1 2))(this (3 4 5))(x y)))
;; works, returns= (3 4 5)  THIS NIL NIL
;;  (find-key-value-in-lists 'x '((a b)(c (1 2))(this (3 4 5))(x y)))
;; works, returns Y X NIL NIL
;;   (find-key-value-in-lists 5  '((1 0 (1 2 A)) (5 1 (1 6 (QUOTE (A B)))))) = 1 5 NIL NIL
;; (find-key-value-in-lists 5  '((1 0 (1 2 A)) (5 1 (1 6 (QUOTE (A B))))) :return-list-p t)
;; works, returns (5 1 (1 6 (QUOTE (A B))))  5 NIL NIL
;; (find-key-value-in-lists 5  '((1 0 (1 2 A)) (5 1 (1 6 (QUOTE (A B))))) :return-nth 2) 
;;works, returns (1 6 (QUOTE (A B)))  5 NIL NIL
;; (nth 0 '((1 0 (1 2 A)) (5 1 (1 6 (QUOTE (A B)))))) (1 0 (1 2 A))



#| REPLACED BY GET-KEY-VALUE-IN-NESTED-LISTS
(defun find-key-value-in-nested-lists-old (key list-of-lists &key return-list-p
                                 (find-nth 0) (return-nth 1) find-key-in-nested (find-nth-first 0))
  "In U-lists.lisp, RETURNS first value that matches key (values value key outer-items). FIND-NTH searches for nth item in the list for the first key. if RETURN-NTH, returns nth instead of second item in list. :find-outer-key looks for the first key in the nth-key of the second order nested lists--not in first list set. It IGNORES keys in the first list order if = T,  otherwise it searchs only the list with the key set to :find-outer-key.  If  RETURN-LIST-P, returns entire sublist.  :FIND-NTH-FIRST is used only if :find-outer-key is set to a value."
  (let
      ((search-list list-of-lists)
       (list-length (list-length list-of-lists))
       (result-value)
       (result-key)
       (outer-extra-items)
       (inner-extra-items)
       )
    ;;  (afout 'out (format nil "NEW CALL TO FUNCTION key= ~A~% list~A~%" key list-of-lists))
    ;;finds a list to search in outer/first order set of lists (by key)
    ;;SSS debug here
    (cond
     ;;if  T all first-order lists must be searched
     ((equal find-key-in-nested t)
    ;;  (afout 'out (format nil "NEW CALL TO T  key= ~A~% (car of list= ~A~%" key (car list-of-lists)))   
      (loop
       for item  in list-of-lists
       with inner-extra-items1
       with outer-extra-items1
       do
      (afout 'out (format nil "T OUTER-LOOP key= ~A~% first-order-item= ~A~%" key first-order-item))
       (cond
        ((and item (listp item))
         (multiple-value-setq (result-value result-key inner-extra-items1 outer-extra-items1)
             (find-key-value-in-lists  key  item
                                         :find-nth find-nth  :return-list-p t))
         ;;these are the extra items want to return (bec inside of target containing list)
         (if inner-extra-items1
             (setf inner-extra-items (append inner-extra-items (list inner-extra-items1))))
         (if outer-extra-items
             outer-extra-items (append outer-extra-items (list outer-extra-items1)))
         (if result-key
             (return))
         )
        ;;may be non-list items such as other keys providing info re: found outer list.
        (t (setf outer-extra-items (append outer-extra-items (list first-order-item)))))
       ;;end loop equal
       ))

     ;;if first-order key is specified by find-key-in-nested, find that sublist first
     (find-key-in-nested
      (loop
       for first-order-item in list-of-lists
       with match-key1
       with result-key1
       with outer-extra-items1
       with inner-extra-items1
       with search-list
       do
       ;;(afout 'out (format nil "OUTER-LOOP #2 first-order-item= ~A~%" first-order-item))
       (multiple-value-setq (search-list result-key1 outer-extra-items1 inner-extra-items1)
           (find-key-value-in-lists  find-key-in-nested  list-of-lists
                                            :find-nth find-nth-first :return-list-p t))
      


         (if inner-extra-items1
             (setf inner-extra-items (append inner-extra-items (list inner-extra-items1))))
         (if outer-extra-items
             outer-extra-items (append outer-extra-items (list outer-extra-items1)))
         ;;if successful finding match with outer-key in item
         (cond
          (result-key1
           (multiple-value-setq (result-list key search-list inner-extra-items1)
               (find-key-value-in-lists  key  search-list
                                                :find-nth find-nth :return-list-p return-list)))
           (t nil))
         ;;end loop, find-key-in-nested
         ))
     ;;otherwise, normal call to find-key-value-in-lists
     (t 
      (multiple-value-setq (result-list key search-list inner-extra-items1)
               (find-key-value-in-lists  key  search-list 
                                                :find-nth find-nth :return-nth return-nth
                                                :return-list-p return-list))
      ))
 
    ;;end find-key-value-in-nested-lists
    (values result-value  result-key outer-extra-items inner-extra-items)
    ))|#

;;SSS START DEBUG HERE
;;test
;; (progn (setf out nil)  ( find-key-value-in-nested-lists "wovNoLove" *all-shaq-pc-instances :find-outer-key "iWorldviewFears.java" :find-nth-first 1  :return-list-p t))  
;;works, returns= ("wovNoLove" "16" "wovNoLoveQ" "int" "FrAnswerPanel.Fear7" "questionInstancesArray" "frameTitle" "frameDimWidth" "frameDimHeight")"wovNoLove"  NIL     (PC-INSTANCES "iWorldviewFears.java")
;; (progn (setf out nil)  (find-key-value-in-nested-lists "wovNoLove" *all-shaq-pc-instances :find-outer-key "iWorldviewFears.java" :find-nth-first 1)) ;;  :return-list-p t))
;;works, = "16"  "wovNoLove"  NIL  (PC-INSTANCES "iWorldviewFears.java")
;;
;; (progn (setf out nil) (multiple-value-setq (*testfn1 *testfn2 *testfn3 *testfn4) (find-key-value-in-nested-lists "wovNoLove" *all-shaq-pc-instances  :find-outer-key  t   :return-list-p t) )(fout out)))
 
;;  (progn (setf out nil)  (find-key-value-in-nested-lists    "acmESOCSTudy" '(PC-INSTANCES  "iAcademicMotivation.java"      ("[]questionInstancesArray1)")      ("acmNDROPcourses" "30" "acmNDROPcoursesQ" "int" "FrAnswerPanel.LikeMe7" "questionInstancesArray" "frameTitle" "frameDimWidth" "frameDimHeight")      ("acmESOCSTudy" "3" "acmESOCSTudyQ" "int" "FrAnswerPanel.LikeMe7" "questionInstancesArray" "frameTitle" "frameDimWidth" "frameDimHeight")) :return-list-p  t))
;;works, returns ("acmESOCSTudy" "3" "acmESOCSTudyQ" "int" "FrAnswerPanel.LikeMe7" "questionInstancesArray" "frameTitle" "frameDimWidth" "frameDimHeight")  "acmESOCSTudy"  NIL  (PC-INSTANCES "iAcademicMotivation.java")

;;  (find-key-value-in-nested-lists 'this  '((a b)(c (1 2))(this (3 4 5))(x y)))
;; works, returns= (3 4 5)  THIS NIL NIL
;;  (find-key-value-in-nested-lists 'x '((a b)(c (1 2))(this (3 4 5))(x y)))
;; works, returns Y X NIL NIL
;;   (find-key-value-in-nested-lists 5  '((1 0 (1 2 A)) (5 1 (1 6 (QUOTE (A B)))))) = 1 5 NIL NIL
;; (find-key-value-in-nested-lists 5  '((1 0 (1 2 A)) (5 1 (1 6 (QUOTE (A B))))) :return-list-p t)
;; works, returns (5 1 (1 6 (QUOTE (A B))))  5 NIL NIL
;; (find-key-value-in-nested-lists 5  '((1 0 (1 2 A)) (5 1 (1 6 (QUOTE (A B))))) :return-nth 2) 
;;works, returns (1 6 (QUOTE (A B)))  5 NIL NIL
;; (nth 0 '((1 0 (1 2 A)) (5 1 (1 6 (QUOTE (A B)))))) (1 0 (1 2 A))



;;JOIN-PARALLEL-LIST-ITEMS
;;
;;ddd
(defun join-parallel-list-items (list1 list2 &key list1-start  list2-start)
  "In U-lists.lisp, takes items in order from 2 lists that match starting with list2-start (default = 0).  Then makes a new list by creating sublists of the matched parallel items. Cuts off extra list1 items; Puts nil in last list2 item if extra list2 items."
  (let
      ((item1)
       (item2)
       (newlist)
       (length-list2 (list-length list2))
       (length-list1 (list-length list1))
       )
    (unless list1-start
      (setf list1-start 0))
    (unless list2-start
      (setf list2-start 0))

    (loop
     for n-item1 from list1-start to length-list1
     for n-item2 from list2-start to length-list2
     do
     (setf item1 (nth n-item1 list1)
           item2 (nth n-item2 list2))
     (unless (null item1)
           (setf newlist (append newlist (list (list item1 item2)))))
     ;;end loop
     )
    newlist
    ))
;;test
;;  (join-parallel-list-items '(a b c d e f) '(x y z))
;;works, returns ((A X) (B Y) (C Z) (D NIL)) ;;NOTE EXTRA ITEM = NIL
;;  (join-parallel-list-items '(a b c d) '(x y z l m n o))    
;;works, returns ((A X) (B Y) (C Z) (D L))


;; JOIN-NESTED-PARALLEL-LIST-ITEMS
;;
;;ddd
(defun join-nested-parallel-list-items (nlist1 nlist2 &key nlist1-start  nlist2-start)
  (let
      ((item1)
       (item2)
       (sublist)
       (new-nlist)
       (length-nlist2 (list-length nlist2))
       (length-nlist1 (list-length nlist1))
       )
    (unless nlist1-start
      (setf nlist1-start 0))
    (unless nlist2-start
      (setf nlist2-start 0))

    (loop
     for n-item1 from nlist1-start to length-nlist1
     for n-item2 from nlist2-start to length-nlist2
     do
     (setf item1 (nth n-item1 nlist1)
           item2 (nth n-item2 nlist2))
     ;;
     (setf sublist (join-parallel-list-items item1 item2))
     (setf new-nlist (append new-nlist (list sublist)))
     ;;end loop
     )
     new-nlist
     ))
;;test
;; (join-nested-parallel-list-items '((a b  c)(d e f)) '((x y z)(l m n)))
;;works, returns  (((A X) (B Y) (C Z)) ((D L) (E M) (F N)) NIL)



;;ADD-SYMS-FROM-PARALLEL-LIST
;;
;;ddd
(defun add-syms-from-parallel-list (list-receive list-send)
  "In U-lists.lisp, Adds an item from one nested list to another nested-list-of-lists. Appends the item to the end of each sublist.  LISTS MUST BE PERFECTLY PARALLEL--except one has items the other sublists parallel to the items."
  (let
      ((send-sym)
       (n1 -1)
       (n2 -1)
       (rec-sublist)
       (rec-sublist-list)
       (new-rec-sublist)
       (new-rec-sublist-list)
       (new-rec-list)
       )
    ;;get group lists
    (dolist (send-sublist list-send)
      (incf n1)
      (setf rec-sublist (nth n1 list-receive))
      ;;get sublists and specific items
      (dolist (send-sublist-item send-sublist)
        (incf n2)
          (setf rec-sublist-list (nth n2 rec-sublist))
        (cond
         ((> n2 0) 
          (setf new-rec-sublist-list (append  rec-sublist-list  (list send-sublist-item))
                new-rec-sublist (append new-rec-sublist (list new-rec-sublist-list))))
         (t (setf new-rec-sublist (append new-rec-sublist (list send-sublist-item)))))
        ;;(afout 'out (format nil "new-rec-sublist= ~A~%" new-rec-sublist))
        ;;end inner dolist
        )
           
      (setf new-rec-list  (append new-rec-list (list new-rec-sublist)))
            (setf new-rec-sublist-list nil
                  new-rec-sublist nil
                  n2 -1)
      ;;end outer dolist
      )
    new-rec-list
    ))
;; test
;;  (progn (setf out nil) (setf *outputx2 (add-syms-from-parallel-list *add-to-this *from-here  )))
;;  (setf *output2p (print-nested-list *outputx2  :stream t :incl-quotes-p t :no-outer-parens-p t))


;;MAKE-ASCENDING-LIST
;;
;;ddd
(defun make-ascending-list (lowest-num list-length)
  "In U-lists.lisp"
    (loop
     for n from lowest-num to (+ lowest-num list-length -1)
     collect n)
    )
;;test, works
;;  (make-ascending-list 5 10) = (5 6 7 8 9 10 11 12 13 14)

;;MAKE-DESCENDING-LIST
;;
;;ddd
(defun make-descending-list (highest-num list-length)
  "In U-lists.lisp"
     (loop
      for n downfrom highest-num  downto (+ (- highest-num list-length) 1)
      collect n)
    ) 
;;test, works
;;  (make-descending-list 14 10) = (14 13 12 11 10 9 8 7 6 5)

;;APPEND-NTH-ITEM
;;ddd
(defun append-nth-item (nth new-item list &key (append-to-short-list-p t))
  "In U-lists, first item= 0. If append-to-short-list-p = T, appends new-item to the end of a list that is too short. nth can be keyword :last."
  (let
      ((length-list (list-length list))
       (new-list)
       )
    (loop
     for n from 0 to (- length-list 1)
     for item in list
     do
     (cond
      ((or (equal nth :last)
        (= nth length-list)
           (and (> nth length-list) append-to-short-list-p))
       (setf new-list (append list (list new-item)))
       (return))
      ((= nth n)
       (setf new-list (append new-list (list new-item) (nthcdr n list)))
       (return))     
      (t (setf new-list (append new-list (list item)))))
       ;;end loop
     )
    new-list
    ;;end let,append-nth-item
    ))
;;TEST
;;  (append-nth-item 3 "this" '(a b c d e f g h))
;; works = (A B C "this" D E F G H)
;;  (append-nth-item 3 '(this) '(a b c d e f g h))
;; works = (A B C (THIS) D E F G H)
;; (append-nth-item 4 "this" '(a b c d )) = (A B C D "this")
;;  (append-nth-item 7 "this" '(a b c d )  :append-to-short-list-p T) 
;;   works = (A B C D "this")



;;APPEND-NTH-ITEM-IN-NESTED-LISTS
;;ddd
(defun append-nth-item-in-nested-lists (nth new-item nested-list)
  "In U-lists, first item= 0. Appends ALL lists with new-item at nth position."
  (let
      ((new-nested-list)
       )
    (loop
     for list in nested-list
     do
     (cond
      ((listp list)
     (setf new-nested-list
           (append new-nested-list (list (append-nth-item nth new-item list)))))
      (t (setf new-nested-list
           (append new-nested-list (list list)))))
     ;;end loop
     )
    new-nested-list
    ;;end let, append-nth-item-in-nested-lists
    ))
;;TEST
;;  (append-nth-item-in-nested-lists  2  "this" '((1 2 3 4)nonlist(a b c d e f g h)(m n o p)))
;; works = ((1 2 "this" 3 4) NONLIST (A B "this" C D E F G H) (M N "this" O P))
;;


;;APPEND-NTH-ITEM-IN-2ND-NESTED-LISTS
;;
;;ddd
(defun append-nth-item-in-2nd-nested-lists (nth new-item double-nested-list) 
  "In U-lists, first item= 0. Appends ALL 2nd-nested lists with the item at position nth."
  (let
      ((new-2nested-list)
       )
    (loop
     for nested-list in double-nested-list
     do
     (cond
      ((listp nested-list)
       (setf new-2nested-list 
             (append new-2nested-list 
                     (list (append-nth-item-in-nested-lists nth new-item nested-list)))))
      (t (setf new-2nested-list 
               (append new-2nested-list (list nested-list)))))
     ;;end loop
     )
    new-2nested-list
    ;;end let, append-nth-item-in-2nd-nested-lists
    ))
;;TEST
;;  (append-nth-item-in-2nd-nested-lists  2 "TEST" '((A (1 2 3 4)) (B (5 6 7) ) ((9 10 11) NO)(C (a b c d e))))
;;works = ((A (1 2 "TEST" 3 4)) (B (5 6 "TEST" 7)) ((9 10 "TEST" 11) NO) (C (A B "TEST" C D E)))
;; (append-nth-item-in-2nd-nested-lists 2 '(this is a test) '((A (1 2 3 4)) (B (5 6 7) ) ((9 10 11) NO)(C (a b c d e))))
;;works = ((A (1 2 (THIS IS A TEST) 3 4)) (B (5 6 (THIS IS A TEST) 7)) ((9 10 (THIS IS A TEST) 11) NO) (C (A B (THIS IS A TEST) C D E)))
;;
     
;;APPEND-NTH-NESTED-LIST
;;
;;ddd
(defun append-nth-nested-list (new-item nth  nested-list &key (nth-item :last) nth-list)
  "In U-lists, In a nested-list, puts an item in the nth-list in position nth-item. Both nth-list and nth-item can use keyword :last. Some items can be non-lists, but still count in the nth count. nth begins with 1 not 0. If nth-list = integer, counts lists instead of items."
  (let
      ((new-nested-list)
       (n 0)
       (listn 0)
       (nth-item1)
       )
    (multiple-value-bind ( list-length n-lists n-symbols n-strings n-numbers) 
        (my-list-length nested-list)
    (loop
     for item in nested-list
     do
     (incf n)
     (cond
      ((listp item)
       (incf  listn)
       (cond
        ((or (and nth-list (= nth-list listn))
             (and (equal nth-list :last) (= nth-list listn))
             (and (equal nth :last)(= listn n-lists))
             (and (numberp nth)(= nth n)))

         (if (equal nth-item :last)
             (setf nth-item1 :last)
           (setf nth-item1 (- nth-item 1)))
         ;;changes item
         (setf  item (append-nth-item nth-item1 new-item   item))
         ;;SSS START HERE
         )
        (t nil)))
      (t nil))
     ;;append new-nested-list with modified or unmodified item
      (setf new-nested-list (append new-nested-list (list item)))
      ;;end loop
      )
    new-nested-list
    ;;end let,mvb,append-nth-nested-list
    )))
;;TEST
;; (append-nth-nested-list '(xxxx) 2  '((list) a (a b c) d (1 2) 7 b))
;; result= ((LIST) A (A B C) D (1 2) 7 B), not append bec nth 2 not a list
;; (append-nth-nested-list '(xxxx) 3  '((list) a (a b c) d (1 2) 7 b))
;; works= ((LIST) A (A B C (XXXX)) D (1 2) 7 B)
;;  (append-nth-nested-list '(xxxx) 3  '((list) a (a b c) d (1 2) 7 b) :nth-item 2)
;; works= ((LIST) A (A (XXXX) B C) D (1 2) 7 B)
;; (append-nth-nested-list '(xxxx) nil  '((list) a (a b c) d (1 2) 7 b) :nth-item 2 :nth-list 3)
;; works= ((LIST) A (A B C) D (1 (XXXX) 2) 7 B)     
;;
;;From ART
;;  (append-nth-nested-list   '(3 (280 0.08280101)  "Wup3-5" (3 5)) :last  '(:DIMSIM 5 ((1 (200 0.112005) "Wup1-5" (1 5)) (2 (240 0.063878) "Wup2-5" (2 5)))))
;; works= (:DIMSIM 5 ((1 (200 0.112005) "Wup1-5" (1 5)) (2 (240 0.063878) "Wup2-5" (2 5)) (3 (280 0.08280101) "Wup3-5" (3 5))))

;;  (append-nth-nested-list  '(4 (320 0.05166) "Wup4-5" (4 5)) :last  '(:DIMSIM 5 ((1 (200 0.112005) "Wup1-5" (1 5)) (2 (240 0.063878) "Wup2-5" (2 5)) (3 (280 0.08280101) "Wup3-5" (3 5)))))
;;works= (:DIMSIM 5 ((1 (200 0.112005) "Wup1-5" (1 5)) (2 (240 0.063878) "Wup2-5" (2 5)) (3 (280 0.08280101) "Wup3-5" (3 5)) (4 (320 0.05166) "Wup4-5" (4 5))))




;;APPEND-NO-DUPLICATES
;;
;;ddd
(defun append-no-duplicates (list &rest items)
  "In U-lists.lisp. USE ADJOIN INSTEAD? or USE INSTEAD OF APPEND sometimes. Appends items to list that neither the item or ANY subitem match an item in list. Note: subitems of LIST are NOT searched. Uses string-equal for strings. Otherwise works like append."
  (let
      ((search-item)
       (return-list list)
       (duplicate-items-list)
       )
       (loop
        for subitem in items
        do
        (cond
         ((find-item-or-subitem-in-list subitem list)
          (setf duplicate-items-list (append duplicate-items-list (list subitem))))
         (t (setf  return-list (append return-list (list subitem)))))                 
        ;;end loop
        )
       (values return-list duplicate-items-list)
       ;;end append-no-duplicates
       ))
;;TEST works
;;  (append-no-duplicates '(a b c) "this" '(x b) '(4 "that") 'c) = (A B C "this" (4 "that"))   ((X B) C) 
;;  (append-no-duplicates '((a b c) (x y z)(1 2 3)) '(x y z)) = ((A B C) (X Y Z) (1 2 3))      ((X Y Z))
;;  (append-no-duplicates '((a b c) (x y z)(1 2 3)) '(x b z)) =  ((A B C) (X Y Z) (1 2 3) (X B Z))   NIL


;;APPEND-LISTS
;;
;;ddd
(defun append-lists (list &rest new-items) 
  "In U-lists, If new-istems is a list, then it APPENDS EACH ITEM separately to the list (not the list of items). eg. result is  (orig-list A B C D) not  (orig-list (A) (B)(C)(D)) for new-lists '((A)(B)(C)(D)). NOTE: If  an item in new-items is not a list still appends it--removes parens if they exist, not attempt if they don't. "
  (let
      ((newlist list)
       )
    ;;what kind of objects are in new-items?  append appropriately
    (cond
     ((listp new-items)
      (loop
       for item in new-items
       do
       (cond
        ((listp item)
         (setf newlist (append newlist item)))
        (t (setf newlist (append newlist (list item)))))
       ;;end loop, listp
       ))
     (t (setf newlist (append newlist (list new-items)))))

    newlist
    ;;end let, append-lists
    ))
;;TEST
;;  (append-lists '(orig-list (a b c))  '(list1) 'a  '(list2 1 2 3) 'b '(list3 4 5))
;; works = (ORIG-LIST (A B C) LIST1 A LIST2 1 2 3 B LIST3 4 5)
;;
;; ;;  (append-lists (list (car '(orig-list (a b c)))) (list (second '(orig-list (a b c)))) '(list1) 'a '(list2 1 2 3) 'b '(list3 4 5) '(also this))




;;APPEND-1NESTED-LISTS
;;
;;ddd
(defun append-1nested-lists (list &rest new-items) 
  "In U-lists, If new-istems is a list, then it APPENDS EACH ITEM separately to the list (not the list of items). Also, removes parens from items 1 nesting level inside. eg. result is  (orig-list (1 2 3) A B C D E) not (orig-list (1 2 3) (A)(B)(C)(D) E)  for new-lists  '((A))((B C)) '(D) 'E . NOTE: If  an item in new-items is not a list still appends it--removes parens if they exist, not attempt if they don't. NOTE: to append nested items in original list use NIL"
  (let
      ((newlist list)
       )
    ;;what kind of objects are in new-items?  append appropriately
    (cond
     ((listp new-items)
      (loop
       for item in new-items
       do
       (cond
        ((listp item)
         (loop
          for initem in item
          do
          (cond
           ((listp initem)
            (setf newlist (append newlist initem)))
           (t (setf newlist (append newlist (list initem)))))
        ;;end inner loop, listp
        ))
        (t (setf newlist (append newlist (list item)))))
       ;;end loop, listp
       ))
     (t (setf newlist (append newlist (list new-items)))))

    newlist
    ;;end let, append-1nested-lists
    ))
;;TEST
;; (append-1nested-lists '(orig list (1 2 3))   '((A)) '((B C)) '(D) 'E)
;; works= (ORIG LIST (1 2 3) A B C D E)
;; TO APPEND NESTED ITEMS IN ORIGINAL LIST USE NIL
;; (append-1nested-lists NIL  '(orig list (1 2 3))   '((A)) '((B C)) '(D) 'E)
;; works= (ORIG LIST 1 2 3 A B C D E)
;; (append-1nested-lists '(b c) '(b c (NEW LIST))  '((D YES)(A (THIS))(B C (WHAT))(X Y (THAT))(A (ANOTHER))(B C X (TOO))))

;;APPEND-LISTS2 -- WORKS, BUT PROBLEM SEE BELOW
;;
;;ddd
#|(defun append-lists2 (listname new-items &optional default-list)
  "In U-lists, appends list= (evaled listname--should be quoted) with lists. If listname is BOUND and its evaled list is not nil, then it first appends default-list to the evaled list. If  listname is UNBOUNDP, then it FIRST sets listname to default-list, then appends new-lists to default-list. NOTE: If  new-lists is not a list, appends the new item to the original list. If new-istems is a list, then it APPENDS EACH ITEM separately to the list (not the list of items). eg. result is  (orig-list A B C D) not  (orig-list (A) (B)(C)(D)) for new-lists '((A)(B)(C)(D)). NOTE: If  an item in new-items is not a list still appends it--removes parens if they exist, not attempt if they don't. PROBLEM: It makes listname a GLOBAL variable even if local in context."
  (let
      ((listname-vals)
       )
    ;;if listname unboundp, set to default-list, otherwise if default-list append it
    (cond
     ((not (and (symbolp listname)(boundp listname)))
      (set listname default-list))
     (default-list
      (set listname (append (eval listname) default-list)))
     (t nil))
    
    ;;set 
    (setf  listname-vals (eval listname))
    ;;(afout 'out (format nil "listname= ~A listname-vals= ~A~%"listname listname-vals))
    ;;what kind of objects are in new-items?  append appropriately
    (cond
     ((listp new-items)
      (loop
       for item in new-items
       do
       (cond
        ((listp item)
         (setf listname-vals (append listname-vals item)))
        (t (setf listname-vals (append listname-vals (list item)))))
       ;;end loop, listp
       ))
     (t (setf listname-vals (append listname-vals (list new-items)))))

    (set listname listname-vals)
    ;;end let, append-lists
    ))|#
;;TEST
;; when originally unboundp
;;  (append-lists 'testlistx2 'this '(default-list))  = (DEFAULT-LIST THIS)
;;  append a symbol
;;  (append-lists 'testlistx2 'that) = (DEFAULT-LIST THIS THAT)
;;  also  testlistx2 = (DEFAULT-LIST THIS THAT)
;;  append a single list
;;  (append-lists 'testlistx2 '(list1)) = (DEFAULT-LIST THIS THAT LIST1)
;;  also teslistx2 = (DEFAULT-LIST THIS THAT LIST1)
;; append mixed multiple-lists and non-lists
;;  (append-lists 'testlistx2 '((list2)(list3) non-list1 (list4)))
;;  works= (DEFAULT-LIST THIS THAT LIST1 LIST2 LIST3 NON-LIST1 LIST4)
;;  also: (DEFAULT-LIST THIS THAT LIST1 LIST2 LIST3 NON-LIST1 LIST4)
;;
;;FOR USE IN 
;;  (setf new-matched-group1 nil)
;;  (append-lists 'new-matched-group1 (list '(b c)  (append '((WHAT)) '((NEW LIST)))))
;;  results= (B C (WHAT) (NEW LIST))
;;  (append-lists 'new-matched-group1  (list '(b c) (append  '((WHAT)) '((NEW LIST)))))
;;  (progn (setf out nil testx nil new-matched-group1 nil)(append-lists 'new-matched-group1 (append-lists 'testx '((NEW LIST)) '((WHAT)) )  '(b c))) 
;;  works= (B C WHAT NEW LIST)
;;  also new-matched-group1 = (B C WHAT NEW LIST)





;;APPEND-NTH
;;2016
;;ddd
(defun append-nth (newitem nth  list &key from-end-p splice-list-p)
  "In U-lists, appends newitem to the nth place in a list, moves later items down one. If SPLICE-LIST-P, splices newitem, if list, into list at nth."   ;;0 1 2 3 4 ;; nth= 0   from-end nth (- 5 0 1) = 4
  (let
      ((newlist)
       (list-n (list-length list))
       )
    (when from-end-p
      (setf nth (- list-n nth )))
   
    (loop
     for item in list
     for n from 0 to list-n
     do
     (cond
      ((= n nth)
       (cond
        ((and splice-list-p (listp newitem))
         (setf newlist (append newlist (list  item)  newitem (nthcdr (+ n 1) list))))
        (t
         (setf newlist (append newlist (list newitem item)(nthcdr (+ n 1) list)))))
       (return))
      (t (setf newlist (append newlist (list item)))))
     ;;end loop
     )
    newlist
    ;;end let, append-nth
    ))
;;TEST
;;  (append-nth 'this 2 '(0 1 2 3 4 5 6 7 8 9)) = (0 1 THIS 2 3 4 5 6 7 8 9)
;;  (append-nth 'this 2 '(0 1 2 3 4 5 6 7 8 9) :from-end-p T)
;;   works = (0 1 2 3 4 5 6 7 THIS 8 9)
;;
;; (append-nth '(a b c) 4 '(0 1 2 3 4 5 6 7 8 9) :splice-list-p T)
;; works=  (0 1 2 3 4 A B C 5 6 7 8 9)
  






;;APPEND-LIST -- original
;;
;;ddd
(defun append-list (listname item  list)
  "In U-lists, sets  symbol listname. Eg (append-list 'xx-list 'this)  =  (ITEM THIS) and (append-list 'xx-list 'that '(test)) = (TEST THAT) "
  (cond
   ((null list)
      (set listname (append (eval listname) (list item))))
   (t (set listname (append list (list item)))))
  ;;end append-list
      )
;;TEST
;;  (setf  xx-list nil) (setf xx-list (append xx-list (list 'item)))
;;  (append-list 'xx-list 'this xx-list)  =  (ITEM THIS)
;;  also: xx-list = (ITEM THIS)
;;   (append-list 'xx-list 'that '(test)) = (TEST THAT)
;;  xx-list = (TEST THAT)


  
;;FIND-ITEM-OR-SUBITEM-IN-LIST
;;
;;ddd
(defun find-item-or-subitem-in-list (item list &key (match-subitems-p T))
  "In U-lists.lisp, searches list. Finds item (or first-order subitem in item (if listp item) and RETURNS item and rest of list (like member does) if found. NOTE: Doesn't test inner parts of lists in LIST."
  (let
      ((result)
       )
    (cond
     ((setf result (my-member item list)))
     ((and (listp item) match-subitems-p)
      (loop
       for subitem in item
       do
       (setf out1 (format nil "subitem= ~A" subitem))
       (cond       
        ((setf result (my-member subitem list))
         (return))              
        (t nil))
       ;;end loop, clause
       ))
     (t nil))
    result
    ;;end find-item-or-subitem-in-list
    ))
;;TEST
;;  (find-item-or-subitem-in-list '(b) '(a b c))
 ;;doesn't test inner parts of lists in list
;; (find-item-or-subitem-in-list '(m y) '(a b c 3 "this" 7 (x y)))  = NIL
;; (find-item-or-subitem-in-list '(m "this") '(a b c 3 "this" 7 (x y))) = ("this" 7 (X Y))



;;MY-EQUAL-LISTS
;;
;;ddd
(defun my-equal-lists (list1 list2 &key exclusion-items)
  "In U-lists.  Tests 2 items in 2 lists using my-equal in order. Omits exclusion-items on either list. List items MUST BE IN SAME ORDER--including exclusion items."
  (let
      ((matched-items)
       (unmatched-items)
       (excluded-items)
       (unmatched-ns)
       (result T)
       )
    (loop
     for item1 in list1
     for item2 in list2
     for n from 1 to (length list1)
     do
     (cond
      ((my-member item1 exclusion-items)
       (setf excluded-items (append excluded-items (list item1 item2))))
      ((my-equal item1 item2)
       (setf matched-items (append matched-items (list (list item1 item2)))))
      (t (setf unmatched-items (append unmatched-items (list (list item1 item2)))
               unmatched-ns (append unmatched-ns (list n))
               result nil)))
     )
    (values result matched-items unmatched-items unmatched-ns excluded-items)
    ;;end let, my-equal-lists
    ))
;;TEST
;;  (my-equal-lists '(L 2 3 TO 2 3 3) '(l 2 3 "to"  2 3 3) :exclusion-items '(TO))
;; works= T  ((L L) (2 2) (3 3) (2 2) (3 3) (3 3))   NIL   NIL  (TO "to")
;;  (my-equal-lists '(L 2 3 TO 1 3 3) '(l 2 3 "to"  2 3 3) :exclusion-items '(TO))
;; works= NIL  ((L L) (2 2) (3 3) (3 3) (3 3))  ((1 2))  (5)  (TO "to")



;;MY-MEMBER
;;
;;ddd
(defun my-member (item list &key (ignore-case-p T))
   "In U-lists.lisp, REPLACE MEMBER,  tests item whether it is a string, number, symbols, or list to see if it is a member of list. Good for UNKNOWN items. Matches strings with symbols if same (eg \"TO\" = TO)."
   (let
       ((result)
        (str1)
        (str2)
        (test1)
        )
     (cond
      ((and (listp item)(setf test1 (member item list :test 'equal)))
         (setf result test1)
         )
      ((listp item)
       (loop
        for item1 in item
        do
        (setf result 
              (my-member item1 list))
        (when result
          (return)))
       ;;end listp
       )
      ;;item not a list, use most liberal matching via format
      (t
       (loop
        for element in list
        for n from 0 to (- (list-length list) 1)
        do
       (setf str1 (format nil "~A" item)
             str2 (format nil "~A" element))
       (when (string-equal str1 str2)
         (setf result  (nthcdr n list))
         (return))
       ;; end 2nd loop, t, cond
       )))
      result
      ;;end my-member
      ))
;;TEST works
;; (my-member 7  '(a b c 3 "this" 7 (a b c))) = (7 (A B C))
;; (my-member '(x y) '(a b c 3 "this" 7 (x y))) = ((X Y))
;;  (my-member "this"  '(a b c 3 "this" 7 (x y)))
;; (my-member "to"  '(a b c 3 "this" TO 7 (x y))) = (TO 7 (X Y))




;;MY-MEMBER-LIST
;;
;;ddd
(defun my-member-list (list1 list2)
  "In U-lists.lisp, For each item of list1tests item whether it is a string, number, symbols, or list to see if it is a member of list2. Good for UNKNOWN items. Matches strings with symbols if same (eg \"TO\" = TO). RETURNS (members non-members) of list2. If list1 not a list, checks to see if the item is a member of list2."
  (let
      ((members)
       (non-members)
       )
    (cond
     ((listp list1)
      
      (loop
       for item in list1
       do
       (cond
        ((my-member item list2)
         (setf members (append members (list item))))
        (t (setf non-members (append non-members (list item)))))
       ;;end loop, listp
       ))
     (t 
      (cond
        ((my-member list1  list2)
         (setf members (append members (list list1 ))))
        (t (setf non-members (append non-members (list list1)))))
      ))
    
    (values members non-members)
    ;;end let, my-member-list
    ))
;;TEST
;;  (my-member-list '(a 7 (this) "the" 77 x (that)) '((this) 7 the "a"))
;;  works= (A 7 (THIS) "the")  (77 X (THAT))
;; (my-member-list  'the  '((this) 7 the "a")) = (THE)  NIL
;; (my-member-list  99  '((this) 7 the "a")) = NIL  (99)












;;MAKE-LIST-TREE-FROM-EVALED-TREE
;;  THIS IS A GOOD RECURSE DEMO 
;;
;;ddd
(defun make-list-tree-from-evaled-tree (topx)
 "In U-lists, INPUT= top node of a tree in which one symbol evals to a list of other symbols, each of which evals to other lists of symbols.  Process stops at and returns final symbol that does not eval to a list. RETURNS a tree of lists, each list is the node with sub-symbols in the second list. EG.  (setf  xx '(x1 x2) x1 '(xx1 xx2) xx1 '(xxx1 xxx2) xx2 '(xxx3 xxx4)) RETURNS ((XX ((X1 ((XX1 (XXX1)) (XX1 (XXX2)))) (X1 ((XX2 (XXX3)) (XX2 (XXX4)))))) (XX (X2)))"
  (let 
      ((sublist)
       (result-list)
       (all-result-lists)
       )
   ;; (incf *cycle-n)
    ;;(afout 'out1 (format nil "*cycle-n= ~A~%topx= ~A~%" *cycle-n topx))
    (cond
     ((boundp topx) (setf  sublist (eval topx))
      (loop
       for subvar in sublist
       do
      ;; (afout 'out1 (format nil "2 subvar= ~A~%" subvar))
       (cond
        ((and (listp sublist)(> (list-length sublist) 0))
         (setf result-list (simple-recurse subvar)
               all-result-lists (append all-result-lists (list (list topx result-list))))      
         )
        (t (setf all-result-lists (append all-result-lists (list subvar)))))
      ;; (afout 'out1 (format nil "*cycle-n= ~A~%result-list= ~A~%all-result-list= ~A~%" *cycle-n result-list all-result-lists))
       ;;end loop
       ))
     (t (setf all-result-lists (append all-result-lists (list topx)))))

    all-result-lists
    ))
;;TEST
;;  (setf  xx '(x1 x2) x1 '(xx1 xx2) xx1 '(xxx1 xxx2) xx2 '(xxx3 xxx4))
;; (progn (setf  *cycle-n 0) (simple-recurse 'xx))
;; works=  ((XX ((X1 ((XX1 (XXX1)) (XX1 (XXX2)))) (X1 ((XX2 (XXX3)) (XX2 (XXX4)))))) (XX (X2)))
           





;;FLATTEN-COUNT-NESTED-LISTS
;;
;;ddd
(defun flatten-count-nested-lists (nested-list)
  "In U-lists, RETURNS (values flat-list length). Any degree of nesting ok."
  (let
      ((element)
       (rest)
       (flat-list)
       (length)
       (new-flat-list)
       )
    (cond
     ((not (listp nested-list))
      (setf flat-list (append flat-list (list nested-list))))
     ((null nested-list) NIL)
     ((listp nested-list)
      (setf element (car nested-list)
            rest (cdr nested-list))
      (multiple-value-setq (new-flat-list)
          (flatten-count-nested-lists element))
      (setf flat-list (append flat-list new-flat-list))
      )
     (t nil))

    (cond
     ((and (listp rest)(> (list-length rest) 0))
      (multiple-value-setq (new-flat-list)
          (flatten-count-nested-lists rest))
      (setf flat-list (append flat-list new-flat-list)))
     (t nil))
    (setf  length (list-length flat-list))

    (values flat-list  length)
    ;;end let, flatten-nested-lists
    ))
;;TEST
;;  (flatten-count-nested-lists '(1 (2 3)((4 5)((6 7)) 8) 9 (10 11))) 
;; works = (1 2 3 4 5 6 7 8 9 10 11)  11
;;  (flatten-count-nested-lists '((0 1) (2 3)((4 5)((6 7)) 8) 9 (10 11)12))  12
;;  works = (0 1 2 3 4 5 6 7 8 9 10 11 12)



;;FLATTEN-1-LEVEL
;;2016
;;ddd
(defun flatten-1-level (lists)
  "In U-lists. "
  (let 
      ((flattened-list)
       )
    (loop
     for item in lists
     do     
     (cond
      ((listp item)
       (setf flattened-list (append flattened-list item)))
      (t (setf flattened-list (append flattened-list (list item)))))
     ;;end loop
     )
    flattened-list
    ;;let,flatten-1-level
    ))
;;TEST
;; (flatten-1-level '((A B) C (((D E F))) (G H)((1 2 3))))
;; works= (A B C ((D E F)) G H (1 2 3))


;;MY-LIST-LENGTH
;;
;;ddd
(defun my-list-length (list)
  "In U-lists, RETURNS (values length n-lists n-symbols n-strings n-numbers)"
  (let
      ((length 0)
       (n-lists 0)
       (n-symbols 0)
       (n-strings 0)
       (n-numbers 0)
       )
    (loop
     for item in list
     do
     (incf length)
     (if (listp item)(incf n-lists))
     (if (symbolp item)(incf n-symbols))
     (if (stringp item)(incf  n-strings))
     (if (numberp item)(incf n-numbers))
     )
    (values length n-lists n-symbols n-strings n-numbers)
    ))
;;TEST
;;  (my-list-length '(a 7 "this"(a b) 9 (d) "that"))
;; works= 7  2  1  2  2




;;FIND-LONGEST-LISTS
;;2016
;;ddd
(defun find-longest-lists (lists)
  "U-list. Puts lists in order by length--longest first. RETURNS (values ordered-lists  list-ns ordered-info-lists)"
  (let
      ((ordered-lists)
       (ordered-info-lists)
       (new-ordered-info-lists)
       (ordered-list-ns)
       (list-ns)
       (listn)
       (lists-info)
       (list-len)
       (num-lists (list-length lists))
       )
    ;;FIND LENGTHS AND ID BY NTH
    (loop
     for list in lists
     for nth-list from 1 to num-lists
     do
     (when (and list (listp list))
       (setf list-len (list-length list)
             lists-info (append lists-info (list (list :list nth-list :length list-len )))
             list-ns (append list-ns (list  list-len)))
     ;;end when, loop
     ))
    (setf ordered-info-lists (my-sort-lists 3 lists-info)
          ordered-list-ns (my-sort list-ns  #'test-greaterp))
    
    (loop
     for info-list in ordered-info-lists
     for i from 1 to num-lists
     do
     (setf info-list (append  (list :order i) info-list)
           new-ordered-info-lists (append new-ordered-info-lists (list info-list)  )
           listn (fourth info-list)
           ordered-lists (append ordered-lists (list (nth (- listn 1) lists))))
     )    
    (values ordered-lists ordered-list-ns new-ordered-info-lists)
    ;;end let, find-longest-lists
    ))
;;TEST
;; (find-longest-lists '((1 2 3 4)(A B C D E F)(l m n o p q r s t)))
;; works= ((L M N O P Q R S T) (A B C D E F) (1 2 3 4))  (9 6 4)  ((:ORDER 1 :LIST 3 :LENGTH 9) (:ORDER 2 :LIST 2 :LENGTH 6) (:ORDER 3 :LIST 1 :LENGTH 4))






;;MAKE-COMBOS-FROM-LISTS
;;2016
;;ddd
(defun make-combos-from-lists (lists &key (return-remainders-p T))
  "In U-lists. Takes item from each list and makes a new combo-list, and appends combo-lists. If not equal lengths, either makes lists from remainder or ends. RETURNS (values combo-lists remainders). Takes lists in order to create combos."
  (let*
      ((combo)
       (combo-lists)
       (num-lists (list-length lists))
       ;;(listn 1)
       (list1 (car lists))
       (list1-n (list-length list1))
       (sublist-item)
       (remainders)
       )
    (loop
     for item in list1
     for nth from 0 to list1-n
     do
     (loop
      for sublist in (cdr lists)
      for listn from 1 to (- num-lists 1)
      do

      (setf sublist-item  (nth nth sublist))

      (cond
       (sublist-item
        (cond
         ((= listn 1)
          (setf combo  (list item sublist-item)))
         ((= listn (- num-lists 1))  ;;note: list1 doesn't count 
          (setf combo (append combo (list sublist-item)))
          (when combo
            (setf combo-lists (append combo-lists (list combo))))
          (setf combo nil)
          ;;(BREAK "INSIDE (= listn num-lists)")
          )
         (t (setf combo (append combo (list sublist-item)))))
        )
       (t (setf remainders (append remainders (list item)))))
 
      ;;end inner loop
      )
     ;;end outer loop
     )

      ;;CHECK SUBLISTS FOR REMAINDERS
      (when return-remainders-p
        (loop
         for list in lists
         do
         (setf remainders (append remainders (list (nthcdr list1-n list))))
         ;;end loop,when
         ))
     (values combo-lists remainders)
    ;;let, make-combos-from-lists
    ))
;;TEST
;; (make-combos-from-lists '((1 2 3 4 5)(a b c d e)(1 2 3 4 5)))
;; works= ((1 A 1) (2 B 2) (3 C 3) (4 D 4) (5 E 5))  NIL
;; (make-combos-from-lists '((1 2 3 4 5)(a b c d e)(1 2 3 4 5 6 7)))  (NIL NIL (6 7))
;; (make-combos-from-lists '((1 2 3 4 )(a b c d e)(1 2 3 4 5 6 7))) 
;;works = ((1 A 1) (2 B 2) (3 C 3) (4 D 4))   (NIL (E) (5 6 7))




;;FILL-LISTS
;;2016
;;ddd
(defun fill-lists (lists fill-list)
  "In U-lists. Takes items from fill-list to fill in lists that are shorter than the longest list in lists. RETURNS filled-lists of equal length."
  (let
      ((filled-lists)
       (list-ns)
       (ordered-lists)
       (ordered-info-lists)
       (longest-listn)
       (num-lists (list-length lists))
       )
    (multiple-value-setq (ordered-lists list-ns ordered-info-lists)
        (find-longest-lists lists))

    (setf longest-listn (car list-ns))
    (loop
     for list in ordered-lists
     for listn in list-ns
     do
     (loop
      for item in fill-list
      do
      (cond
       ((= listn longest-listn)
        (setf filled-lists (append filled-lists (list list)))
        (return))
       (t (incf listn)
          (setf list (append list (list item)))))
      ;;end inner loop
      )
     (when (= (list-length filled-lists) num-lists)
       (return))
     (break "after when")
     ;;end outer loop
     )
    filled-lists

    ;;end let, fill-lists
    ))
;;TEST
;; (fill-lists '((A B C D)(1 2 3 4 5 6)(X Y Z)) '(F1 F2 F3 F4 F5 F6 F7 F8 F9))
;; works= ((1 2 3 4 5 6) (A B C D F1 F2) (X Y Z F1 F2 F3))







;;MY-2COMBOS
;;
;;ddd
(defun my-2combos (list)
  "In U-lists RETURNS all-lists of possible combos of any 2 items in list (even if items are lists."
  (let
      ((item1 (car list))
       (restlist (cdr list))
       (newlist)
       (all-lists)
       )
  (loop
   for item2 in restlist
   do
   (setf newlist (list item1 item2)
         all-lists (append all-lists (list newlist)))
   )

   (when restlist
     (setf all-lists (append all-lists (my-combos  restlist))))
   
   all-lists
   ;;end let, all-possible-combos
   ))
;;TEST
;; (my-2combos '(a b c)) = ((A B) (A C) (B C))
;; (my-2combos  '((A B) (A C) (B C))) = (((A B) (A C)) ((A B) (B C)) ((A C) (B C)))
;; (my-2combos '(I L F))



                            


;;MAKE-POSSIBLE-2-LIST-COMBOS
;;
;;ddd
(defun make-possible-2-list-combos (list1 list2)
  "In U-lists.INPUT 2 lists. Makes new lists of all combos of items across list1 and list2. RETURNS (values all-combos all-n-combos). Used by ART to create PATH symbols."
  (let
      ((list1-n (list-length list1))
       (list2-n (list-length list2))
       (all-combos)
       (all-n-combos)
       )
    (loop
     for item1 in list1
     for n1 from 1 to list1-n
     do

     (loop
      for item2 in list2
      for n2 from 1 to list2-n
      do
      (setf all-combos (append all-combos (list (list item1 item2)))
            all-n-combos (append all-n-combos (list (list n1 n2))))
    ;;(afout 'out (format nil "item2= ~A all-combos= ~A~%" item2  all-combos))
      ;;end inner loop
      )

     ;;end outer loop
     )
    (values all-combos all-n-combos)
    ;;end let, make-possible-2-list-combos
    ))
;;TEST
;;  (make-possible-2-list-combos '((I  3 2)) '((I 1 3))) = (((I 3 2) (I 1 3)))
;;  (make-possible-2-list-combos '((I  3 2)) '((1 1 3)(2 1 3)(3 1 3)))
;; results= (((I 3 2) (1 1 3)) ((I 3 2) (2 1 3)) ((I 3 2) (3 1 3)))   ((1 1) (1 2) (1 3))
;;  (I ((1 4 3) (2 4 3) (3 4 3) (4 4 3)))
;;


;;  (make-possible-2-list-combos '((I L 1)(I L 2)) '((I L 1)(I L 2)(I L 3)))
;;  result= (((I L 1) (I L 1)) ((I L 1) (I L 2)) ((I L 1) (I L 3)) ((I L 2) (I L 1)) ((I L 2) (I L 2)) ((I L 2) (I L 3)))    ((1 1) (1 2) (1 3) (2 1) (2 2) (2 3))
;;  (make-possible-2-list-combos '(A B C) '(D E F))
;;results= ((A D) (A E) (A F) (B D) (B E) (B F) (C D) (C E) (C F))    ((1 1) (1 2) (1 3) (2 1) (2 2) (2 3) (3 1) (3 2) (3 3))
;;FOR F
;;  (make-possible-2-list-combos '(F 2)'(F 3))
;; results= ((F F) (F 3) (2 F) (2 3))
;;  ((1 1) (1 2) (2 1) (2 2))
;;FOR L
;;results= ((L L) (L 1) (3 L) (3 1))
;;  ((1 1) (1 2) (2 1) (2 2))
;;FOR I
;;  (make-possible-2-list-combos '(I 1 2 3 4 5 ) '(I 1 2 3))
;; RESULTS= ((I I) (I 1) (I 2) (I 3) (1 I) (1 1) (1 2) (1 3) (2 I) (2 1) (2 2) (2 3) (3 I) (3 1) (3 2) (3 3) (4 I) (4 1) (4 2) (4 3) (5 I) (5 1) (5 2) (5 3))
;;   ((1 1) (1 2) (1 3) (1 4) (2 1) (2 2) (2 3) (2 4) (3 1) (3 2) (3 3) (3 4) (4 1) (4 2) (4 3) (4 4) (5 1) (5 2) (5 3) (5 4) (6 1) (6 2) (6 3) (6 4))


;;MY-3COMBOS
;;
;;ddd
#|(defun my-3combos (list)
  (let
      ((item1 (car list))
       (item2 (second list))
       (restlist (nthcdr 2 list))
       (newlist)
       (all-lists)
       )
  (loop
   for item3 in restlist
   do
  
   (setf newlist (list item1 item2 item3)
         all-lists (append all-lists (list newlist)))
   )

   (when restlist
     (setf all-lists (append all-lists (my-combos  restlist))))
   
   all-lists
   ;;end let, all-possible-combos
   ))|#
;;TEST
;; (my-3combos '(1 2 3 4 5 6 7 8 9 10))
;; RESULT= ((1 2 3) (1 2 4) (1 2 5) (1 2 6) (1 2 7) (1 2 8) (1 2 9) (1 2 10) (3 4) (3 5) (3 6) (3 7) (3 8) (3 9) (3 10) (4 5) (4 6) (4 7) (4 8) (4 9) (4 10) (5 6) (5 7) (5 8) (5 9) (5 10) (6 7) (6 8) (6 9) (6 10) (7 8) (7 9) (7 10) (8 9) (8 10) (9 10))
 ;;SSS START HERE FINISH THIS TO TEST WHAT ALL COMBOS LOOK LIKE


#|  DELETE -- MAKE SYMBOL TREE INSTEAD IN U-SYMBOL-TREES
;;MAKE-HIERARCHY-2-LIST-COMBOS
;;
;;ddd
(defun make-hierarchy-2-list-combos (topdims dimspecs1 dimspecs2 
                                             &key (separator-sym *art-node-separator))
  "In U-lists. Each item on topdims is a symbol (eg (I L F M) representing a different level (last level is highest).  dimspecs = ((n-items begin-index incr) for each level).  RETURNS (values all-combos-by-level flat-all-combos ). Used to make ART path names (eg WUPI-L-FTOI-L-F). Topdims divided by separator-sym"
  (let
      ((x)
       (level)
       (flat-all-combos)
       (all-combos-by-level)
       (target-dim-n1)
       (target-dim-n1)
       (target-dim-nth1) 
       (target-dim-nth2) 
       )

    ;;find needed Ns
    (multiple-value-setq (n-dims  n-items sublist-ns n-ints)
        (dimlist-length topdims))
    (setf target-dim-n1 (first sublist-ns)
          target-dim-n2 (second sublist-ns)
          target-dim-nth1 (- target-dim-n1 1)
          (target-dim-nth2 (- target-dim-n2 1)))

    (multiple-value-setq (topdims topsubdims1 topsubdims2 
                                  n-topdims n-sub1-dims n-sub2-dims rootstr)
        (find-symdim-info topsym))
      (setf topsymvals (list rootstr topdims nil nil nil ))
      (set topsym topsymvals)))


          (values all-combos-by-level flat-all-combos )
          ;;end let, make-hierarchy-2-list-combos
          ))  
  |#            




;;SORT-LISTS-BY-BEGIN 
;;
;;ddd
(defun sort-keylists-by-begin (keylists &key (n-grouping-syms 1) 
                                        (sort-within-groups 'ascending) (test 'my-equal))
  "In U-lists, groups together all lists with the same beginning n-grouping-syms in a new list beginning  with those syms followed by a list of all the sublists.  If sort-within-groups, then sorted by ascending or descending.  RETURNS grouped-lists."
  (let
      ((labels)
       (labels-list)
       (labels-lists)
       (new-group)
       (matched-group)
       (matched-keylist)
       (grouped-keylists)
       (new-grouped-keylists)
       (labeled-groups)
       (ascending-p)
       (length-keylist)
       (other-items)
       (descending-group)
       (ascending-group)
       (sublist)
       (group)
       (keylist)
       )
  
    (cond
     ((listp keylists)
      (loop
       for keylist in keylists
       do
       (setf  length-keylist (list-length keylist)
              labels (butlast keylist (- length-keylist n-grouping-syms))
              labels-lists (append labels-lists (list labels))
              sublist (nthcdr n-grouping-syms keylist))
       ;;(afout 'out (format nil "AT 1 labels= ~A sublist= ~A~%" labels sublist))
       
       ;;should just be one match within the grouped-keylists
       (setf matched-group (car (get-keylists-by-begin labels grouped-keylists)))

       (cond
        (matched-group
         (setf new-group (append matched-group sublist))
         (multiple-value-setq (grouped-keylists group)
             (replace-list-if-same-begin labels new-group grouped-keylists)))             
        (t (setf new-group keylist
                 grouped-keylists (append grouped-keylists (list keylist)))))

       ;;(afout 'out (format nil "AT 2 matched-group= ~A new-group= ~A grouped-keylists= ~A" matched-group new-group grouped-keylists))

       ;;end loop keylists
       )
       ;;end listp keylists
       )
     ;;if not a list, add to the other-items list
     (t (setf other-items (append other-items (list keylist)))))

      ;;TO SORT WITHIN EACH GROUP (by sort-dim-n) ascending or descending
      (when sort-within-groups
        (when (equal sort-within-groups 'ascending)
          (setf ascending-p T))
         (multiple-value-setq (descending-group ascending-group)
             (my-sort-lists  n-grouping-syms  grouped-keylists :ascending-p ascending-p))
         (cond
          ((equal sort-within-groups 'ascending)
           (setf new-grouped-keylists
                 (append new-grouped-keylists (list ascending-group))))
          (t (setf new-grouped-keylists
                   (append new-grouped-keylists (list descending-group)))))
       
         (setf grouped-keylists (car new-grouped-keylists))
         ;;end loop,when sort groups, listp
         )

      (values grouped-keylists other-items  labels-lists )
      ;;end let, sort-keylists-by-begin
      ))
;;TEST
;; ;;(progn (setf out nil) (sort-keylists-by-begin '
;;(progn (setf out nil) (sort-keylists-by-begin  '((M ((I L F))) (F ((I L 2))) (L ((I 3 2))) (I ((1 3 2) (2 3 2) (3 3 2) (4 3 2))) (L ((I 4 2))) (I ((1 4 2) (2 4 2) (3 4 2) (4 4 2)))  (L ((I 5 2))) (I ((1 5 2) (2 5 2) (3 5 2) (4 5 2)))  (F ((I L 3))) (L ((I 3 3))) (I ((1 3 3) (2 3 3) (3 3 3) (4 3 3))) (L ((I 4 3))) (I ((1 4 3) (2 4 3) (3 4 3) (4 4 3)))  (L ((I 5 3))) (I ((1 5 3) (2 5 3) (3 5 3) (4 5 3)))) :sort-within-groups 'descending))
;;WORKS=
;;((M ((I L F))) (F ((I L 2)) ((I L 3))) (L ((I 3 2)) ((I 4 2)) ((I 5 2)) ((I 3 3)) ((I 4 3)) ((I 5 3))) (I ((1 3 2) (2 3 2) (3 3 2) (4 3 2)) ((1 4 2) (2 4 2) (3 4 2) (4 4 2)) ((1 5 2) (2 5 2) (3 5 2) (4 5 2)) ((1 3 3) (2 3 3) (3 3 3) (4 3 3)) ((1 4 3) (2 4 3) (3 4 3) (4 4 3)) ((1 5 3) (2 5 3) (3 5 3) (4 5 3))))    NIL    ((M) (F) (L) (I) (L) (I) (L) (I) (F) (L) (I) (L) (I) (L) (I))
#|PPRINTED
((M ((I L F)))
 (F ((I L 2)) ((I L 3)))
 (L ((I 3 2)) ((I 4 2)) ((I 5 2)) ((I 3 3)) ((I 4 3)) ((I 5 3)))
 (I
  ((1 3 2) (2 3 2) (3 3 2) (4 3 2))
  ((1 4 2) (2 4 2) (3 4 2) (4 4 2))
  ((1 5 2) (2 5 2) (3 5 2) (4 5 2))
  ((1 3 3) (2 3 3) (3 3 3) (4 3 3))
  ((1 4 3) (2 4 3) (3 4 3) (4 4 3))
  ((1 5 3) (2 5 3) (3 5 3) (4 5 3))))
|#
;; (progn (setf out nil) (sort-keylists-by-begin '((D YES)(A (THIS))(B (WHAT))(X Y (THAT))(A (ANOTHER))(B X (TOO)))))
;; RESULT= (((D (YES))) ((A ((THIS)))) ((B ((WHAT)))) ((X (Y (THAT)))) ((A ((ANOTHER)))) ((B (X (TOO)))))  ((B))
;;  ;; (progn (setf out nil) (sort-keylists-by-begin '((x (9 7 6))(a (f c x))(x 3 2)(b 8 9 9)(a (7 8 9)))))









;;GET-KEYLISTS-BY-BEGIN
;;
;;ddd
(defun get-keylists-by-begin (begin-syms keylists &key (test 'my-equal))
  "In U-lists.  Looks for syms in begin-syms in beginning of each keylist in keylists. RETURNS (values all-matched-keylists non-matched-items)."
  (let
      ((return-keylist)
       (all-matched-keylists)
       (non-matched-items)
       (begin-n (list-length begin-syms))
       (keylist-n)
       (begin-syms2)
       (result)
       )
    (loop
     for keylist in keylists
     do
     (cond
      ((listp keylist)
       (setf  keylist-n (list-length keylist))
       (when (> keylist-n begin-n)
       (setf  begin-syms2 (butlast keylist (- keylist-n begin-n)))
       (cond
        ((equal test 'my-equal)
         (setf result (my-equal begin-syms begin-syms2)))
        (t (setf result (equal begin-syms begin-syms2))))

       ;;if found append and reset or add to non-matched-items
       (cond
        (result
         (setf all-matched-keylists (append all-matched-keylists (list keylist))
               result nil))
         (t (setf non-matched-items (append non-matched-items (list keylist)))))

       ;;end when,listp
       ))
     (t (setf non-matched-items (append non-matched-items (list keylist)))))
     ;;end loop
     )
    (values all-matched-keylists non-matched-items)
    ;;end let,get-keylists-by-begin
    ))
;;TEST
;; (get-keylists-by-begin '(a "b") '((1 2 3)(a b 3 4 5 (6)) (A B (this)) x  33))
;; works= ((A B 3 4 5 (6)) (A B (THIS)))   ((1 2 3) X 33)




;;INSERT-AT-N
;;
;;added 2016-04
;;ddd
(defun insert-at-n (item nth list)
  "In U-lists, in list, inserts item at nth place. nth begins with 0."
  (let
      ((newlist)
       )
    (loop
     for n from 0 to (list-length list)
     for elem in list
     do
     (cond
      ((= n nth)
       (setf newlist (append newlist (list item elem))))
      (t (setf newlist (append newlist (list  elem)))))
     )
    newlist
    ;;end let, insert-at-n
    ))
;;TEST
;; (insert-at-n 'b 1 '(a c d e)) = (A B C D E)



;;new 2016 --both work

;;((1 2) (1 3) (1 4) (1 5) 
;; (2 3) (2 4) (2 5)
;; (3 4) (3 5) (4 5))

;;(U(1 2) U(1 3) U(1 4) X(1 5) 
;; U(2 3) U(2 4)  X(2 5)
;; U(3 4) X(3 5) X(4 5))

;used  1 2,     1 2      1  2      1 3      1  3       1 4
;;     ((1 2 3) (1 2 4) (1 2 5) (1 3 4) (1 3 5) (1 4 5)
;used 2  3      2 3      2 4
;;      (2 3 4) (2 3 5) (2 4 5) 
;used 3 4
;;      (3 4 5))

;;



;;MAKE-COMBOS
;;2016
;;ddd
(defun make-combos (list1 items-per-combo
                          &key (random-sample-percent 10)
                          (randomize-p T) return-all-combos-p
                          (prefix "")(postfix "")
                          global-combos-sym
                          (randomize-min-combo-n 20))
  "In U-LISTS, creates combo-lists of items-per-combo elements and can return a randomized sample of random-sample-percent of the total. RETURNS  If return-all-combos-p, (values global-combos-sym return-combos all-combos) otherwise, (values  return-combos  n-combos new-global-combos-sym all-combos). "
  (let
      ((randomized-combos)
       (n-combos)
       (qnames)
       (nested-combos)
       (all-combos)
       (n-all-combos)
       (new-cat-combo-global-sym)
       (new-global-combos-sym)
       )

    ;;MAKE THE COMBOS
    (setf nested-combos (make-nested-combos list1 items-per-combo)
          all-combos (flatten-1-level nested-combos)
          n-all-combos (list-length all-combos))   
    ;;(break "1")   
      
    ;;RANDOM SELECT COMBOS FROM EACH CATEGORY?
    (cond
     ((and  (or randomize-p random-sample-percent)
            (> n-all-combos randomize-min-combo-n))
      (setf return-combos 
            (my-random-selection all-combos random-sample-percent))
      )
     (t (setf return-combos all-combos)))

    (setf n-combos (list-length return-combos))

    ;;SET A cat-combo-global-sym TO THE RETURN-COMBOS?
    (when global-combos-sym
      (cond
       ((or (not (equal prefix ""))(not (equal postfix ""))(stringp global-combos-sym))
        (setf new-global-combos-sym (my-make-symbol 
                                     (format nil "~A~A~A" prefix  cat-combo-global-sym postfix))))
       ((symbolp global-combos-sym)
        (setf new-global-combos-sym global-combos-sym)
        (set new-global-combos-sym return-combos))
       (t nil)))

    (values  return-combos  n-combos new-global-combos-sym all-combos)
    ;;end let, make-combos
    ))
;;TEST
;; (make-combos   '(A B C D E F G H I J K L M N O P) 3 :global-combos-sym  'test-glsym4)
;; works makes 56 of 560, sym evals to list -- like below

;; (make-combos   '(A B C D E F G H I J K L M) 3 :global-combos-sym  'test-glsym4)
;; works= ((A B M) (H I K) (C F M) (A G K) (B G J) (C G M) (J K M) (A B D) (A C J) (D F J) (A J L) (D G H) (C D H) (D L M) (G H K) (B D K) (C E I) (B E J) (H K L) (C G J) (C H I) (B F H) (A H M) (A F M) (F H M) (F I J) (J L M) (B F M) (B H M))   29   TEST-GLSYM4         ((A B C) (A B D) (A B E) (A B F) (A B G) (A B H) (A B I) (A B J) (A B K) (A B L) (A B M) (A C D) (A C E) (A C F) (A C G) (A C H) (A C I) (A C J) (A C K) (A C L) (A C M) (A D E) (A D F) (A D G) (A D H) (A D I) (A D J) (A D K) (A D L) (A D M) (A E F) (A E G) (A E H) (A E I) (A E J) (A E K) (A E L) (A E M) (A F G) (A F H) (A F I) (A F J) (A F K).... entire all-combos.
;;also TEST-GLSYM4  = ((A B M) (H I K) (C F M) (A G K) (B G J) (C G M) (J K M) (A B D) (A C J) (D F J) (A J L) (D G H) (C D H) (D L M) (G H K) (B D K) (C E I) (B E J) (H K L) (C G J) (C H I) (B F H) (A H M) (A F M) (F H M) (F I J) (J L M) (B F M) (B H M))


;; (make-combos '(testsym1 testsym2 testsym3 testsym4)  3 :global-combos-sym  'test-glsym2)
;;works= ((TESTSYM1 TESTSYM2 TESTSYM3) (TESTSYM1 TESTSYM2 TESTSYM4) (TESTSYM1 TESTSYM3 TESTSYM4) (TESTSYM2 TESTSYM3 TESTSYM4))   4   TEST-GLSYM2
;;also CL-USER 33 > TEST-GLSYM2 =  ((TESTSYM1 TESTSYM2 TESTSYM3) (TESTSYM1 TESTSYM2 TESTSYM4) (TESTSYM1 TESTSYM3 TESTSYM4) (TESTSYM2 TESTSYM3 TESTSYM4))
;;for csq
;; (make-combos *all-elmsyms 3 :global-combos-sym 'test3-glsym)
;; works=  ((MOTHER FATHER BEST-M-FRIEND) (MOTHER FATHER BEST-F-FRIEND) (MOTHER BEST-M-FRIEND BEST-F-FRIEND) (FATHER BEST-M-FRIEND BEST-F-FRIEND))    4   TEST3-GLSYM
;;also CL-USER 39 > TEST3-GLSYM  = ((MOTHER FATHER BEST-M-FRIEND) (MOTHER FATHER BEST-F-FRIEND) (MOTHER BEST-M-FRIEND BEST-F-FRIEND) (FATHER BEST-M-FRIEND BEST-F-FRIEND))






;;MAKE-FLAT-COMBOS
;;2016
;;ddd
(defun make-flat-combos (list1 items-per-combo &optional list2)
"In U-lists. Creates a list of all combinations of items taken items-per-combo at a time.  RETURNS (values all-combos  n-combos). Uses make-combos, then returns FLAT list of all new combos."
   (let
       ((all-flat-combos)
        (allcombos-n)
        (nested-combos)
        )
     (setf nested-combos 
           (make-nested-combos list1 items-per-combo list2))

     (setf all-flat-combos (flatten-list-tree nested-combos)
           allcombos-n (list-length all-flat-combos))
     (values all-flat-combos allcombos-n)
     ;;end let, make-flat-combos
     ))
;;TEST
;;  (make-flat-combos '(1 2 3 4 5) 3)
;; works= ((1 2 3) (1 2 4) (1 2 5) (1 3 4) (1 3 5) (1 4 5) (2 3 4) (2 3 5) (2 4 5) (3 4 5))   10


;;MAKE-NESTED-COMBOS
;;
;;ddd
(defun  make-nested-combos (list1 items-per-combo &optional list2)  
  "In U-lists. Creates a list of all combinations of items taken items-per-combo at a time.  RETURNS (values all-combos  n-combos). Note: Must return NESTED lists. Flatten after final return if want flat list. (old make-combos)."
  (let
      ((list2n) 
       (newcombos)
       (allnewcombos)
       (allcombos-n)
       (newlist1)
       (newlist2)
       (subcombos)
       (combos-n)
       (subcombo-n)
       (allcombos)
       )
    (when (null list2)
      (setf list2  list1))
    (setf  list2n (list-length list2))

    (loop
     for item1 in (butlast list1 ) 
     for j from 1 to list2n
     do
     (setf subcombos
            (append-item-to-lists  (nthcdr j list2)  item1   :to-front-p T)) 
     (setf subcombo-n (list-length (car subcombos)))

     ;;(BREAK "before recurse")
    
     ;;RECURSE TO LENGTHEN LIST1-N?
     (cond
      ((and subcombo-n (> items-per-combo subcombo-n))

       (setf newlist2 (nthcdr  j  list2)
             newlist1 subcombos) ;; (butlast subcombos)) ;;  j))

       (multiple-value-setq (newcombos combos-n) 
           (make-nested-combos newlist1  items-per-combo  newlist2))
       (when newcombos
         (setf allnewcombos (append allnewcombos (list newcombos))))
       ;;clause
       )
      (t (when subcombos (setf allnewcombos (append allnewcombos subcombos)))))
     ;;end loop
     )
    
    (when allnewcombos
      (setf  allcombos (append allcombos allnewcombos)
             allcombos-n (list-length allcombos)))
        
    (values allcombos allcombos-n) 
    ;;end let, make-nested-combos
    ))
;;TEST
;; (make-nested-combos '(1 2 3 4 5) 2)
;; = (((1 2) (1 3) (1 4) (1 5)) ((2 3) (2 4) (2 5)) ((3 4) (3 5)) ((4 5)))
;; (make-nested-combos '(1 2 3 4 5) 3)
;;  = (((1 2 3) (1 2 4) (1 2 5) (1 3 4) (1 3 5) (1 4 5)) ((2 3 4) (2 3 5) (2 4 5)) ((3 4 5)))    3
;; (make-nested-combos '(1 2 3 4 5) 4)
;; = ((((1 2 3 4) (1 2 3 5) (1 2 4 5)) ((1 3 4 5))) (((2 3 4 5))))   2
;; (make-nested-combos '(1 2 3 4 5) 5)
;; = (((((1 2 3 4 5)))))   1
;; (make-nested-combos '(1 2 3 4 5 6 7 8) 4)
;; works= ((((1 2 3 4) (1 2 3 5) (1 2 3 6) (1 2 3 7) (1 2 3 8) (1 2 4 5) (1 2 4 6) (1 2 4 7) (1 2 4 8) (1 2 5 6) (1 2 5 7) (1 2 5 8) (1 2 6 7) (1 2 6 8) (1 2 7 8)) ((1 3 4 5) (1 3 4 6) (1 3 4 7) (1 3 4 8) (1 3 5 6) (1 3 5 7) (1 3 5 8) (1 3 6 7) (1 3 6 8) (1 3 7 8)) ((1 4 5 6) (1 4 5 7) (1 4 5 8) (1 4 6 7) (1 4 6 8) (1 4 7 8)) ((1 5 6 7) (1 5 6 8) (1 5 7 8)) ((1 6 7 8))) (((2 3 4 5) (2 3 4 6) (2 3 4 7) (2 3 4 8) (2 3 5 6) (2 3 5 7) (2 3 5 8) (2 3 6 7) (2 3 6 8) (2 3 7 8)) ((2 4 5 6) (2 4 5 7) (2 4 5 8) (2 4 6 7) (2 4 6 8) (2 4 7 8)) ((2 5 6 7) (2 5 6 8) (2 5 7 8)) ((2 6 7 8))) (((3 4 5 6) (3 4 5 7) (3 4 5 8) (3 4 6 7) (3 4 6 8) (3 4 7 8)) ((3 5 6 7) (3 5 6 8) (3 5 7 8)) ((3 6 7 8))) (((4 5 6 7) (4 5 6 8) (4 5 7 8)) ((4 6 7 8))) (((5 6 7 8))))    5
;; (setf *all-combos (make-nested-combos *all-elmsyms 3))
;; (setf *all-flat-combos (flatten-1-level *all-combos))
;; (list-length *flat-all-combos) = 35990








;;DIVIDE-LIST
;;2016
;;ddd
(defun divide-list (list num-lists &key (equal-parts-p t)  percent-part-list group-n-list)
  "In U-lists, divides list into num-lists equal parts (if equal-parts-p) or if PART-LIST, then divides according to percents in percent-part-list"
  (let*
      ((list-n (list-length list))
       (percent-per-group)
       (n-per-group)
       (group-n)
       (group-nth 0)
       (new-lists)
       (sublist-n 0)
       (sublist-nth 0)
       (sublist)
       (restlist)
       )

    ;;CONVERT PERCENTS and NUMBERS TO A LIST OF SUBLIST Ns
    (cond
     ((and equal-parts-p (null  group-n-list)(null percent-part-list))
      (setf percent-per-group (ceiling (*(/ (/  list-n  num-lists ) list-n) 100.00))
            n-per-group (ceiling (/ list-n num-lists)))

      ;;(BREAK)
      (loop
       for n from 1 to num-lists
       do
       (setf percent-part-list (append percent-part-list (list percent-per-group))
             group-n-list (append group-n-list (list n-per-group)))
             
       ;;end loop, equal parts
       ))
     (percent-part-list
      (setf num-lists (list-length percent-part-list))
      (loop
       for percent in percent-part-list
       do
       (setf group-n (ceiling (/ (* percent list-n) 100))
             group-n-list (append group-n-list (list group-n)))
       ;;end loop,percent-part-list
       ))
     (group-n-list
      (setf num-lists (list-length group-n-list))
      )
     (t nil))
      ;;(break "after percent")
    ;;initialize sublist-n
    (setf sublist-n (car group-n-list))

    ;;MAIN  DIVIDER LOOP USING GROUP-N-LIST
    (loop
     for item in list
     for n from 0 to list-n
     do     
     (cond
      ;;in case left over items
      ((and (> group-nth num-lists)(>  sublist-nth   sublist-n))
       (setf sublist (append sublist (list item))
             new-lists (append new-lists (list sublist))
             restlist  (nthcdr (+ n 1) list))
       ;;(BREAK "IN (> group-nth num-lists)")
       )
      ((or (=   sublist-nth  (- sublist-n 1) )
           (= n list-n))
       (incf group-nth)
       (setf sublist (append sublist (list item))
             new-lists (append new-lists (list sublist))
             sublist-n (nth group-nth  group-n-list)
             sublist nil
             sublist-nth 0)
       ;;(BREAK "IN (=   sublist-nth  (- sublist-n 1) )")
       (unless (> group-nth num-lists)
         sublist-n (nth group-nth group-n-list))
       )
      (t (setf sublist (append sublist (list item)))         
         (incf sublist-nth)))
     ;;(BREAK "END OF LOOP")
     ;;end  loop
     ) 
    (when sublist
      (setf new-lists (append new-lists (list sublist))
            restlist nil))
    
    (values new-lists restlist)
    ;;end let, divide-list
    ))
;;TEST
;; (divide-list '(1 2 3 4 5 6 7 8 9 10) 3) =  ((1 2 3 4) (5 6 7 8) (9 10))  NIL
;; (divide-list '(1 2 3 4 5 6 7 8 9 10) 5) = ((1 2) (3 4) (5 6) (7 8) (9 10))  NIL
;; (equal-parts-p t)  percent-part-list group-n-list)
;; (divide-list '(1 2 3 4 5 6 7 8 9 10) nil :group-n-list '(5 2 3))
;; works= ((1 2 3 4 5) (6 7) (8 9 10)) NIL
;; (divide-list '(1 2 3 4 5 6 7 8 9 10) nil :percent-part-list '(50 20 30))
;; works= ((1 2 3 4 5) (6 7) (8 9 10)) NIL
;; (divide-list '(1 2 3 4 5 6 7 8 9 10) nil :percent-part-list '(33 25 50))
;; works= ((1 2 3 4) (5 6 7) (8 9 10))  NIL




;;DIVIDE-LIST-IN-ORDER
;;2016
;;ddd
(defun divide-list-in-order (list num-lists)
  "In U-lists, divides a list into num-lists, taking items in order and putting item1 in list1, item2 in list2, etc."
  (let*
      ((newlists)
       (list-n (list-length list))
       (listn  0)
       )
    ;;
    (loop
     for item in list
     for nth from 0 to list-n
     do
     (incf listn)

     (cond
      ((< nth num-lists)
       (setf newlists (append newlists (list (list item)))))
      (t
       (setf newlists (append-nth-nested-list item listn  newlists))
       ))
       (when (= listn num-lists)
         (setf listn 0))
     ;;end loop
     )
    ;;vvvv
    newlists
    ;;end let, divide-list-in-order
    ))
;;TEST
;; (divide-list-in-order '(1 2 3 4 5 6 7 8 9 10 11 12) 3) 
;; works= ((1 4 7 10) (2 5 8 11) (3 6 9 12))
;;  (divide-list-in-order '(1 2 3 4 5 6 7 8 9 10 11)  3)
;; works= ((1 4 7 10) (2 5 8 11) (3 6 9))






;;GET-PERCENT-LIST
;;2016
;;ddd
(defun get-percent-list (percent list &key (begin-n 1) return-restlist-p)
  "In U-lists, RETURNS list of first percent of items from begin-n in list. It returns the number of items higher than odd percent. When RETURN-RESTLIST-P, RETURNS (values newlist restlist)."
  (let*
      ((newlist)
       (list-n (list-length list))
       (begin-nth (- begin-n 1))
       (target-nth (- (+ (ceiling (* list-n (/ percent 100.00)))  begin-nth) 1))
       (restlist)
       )
    (when (>= target-nth list-n)
      (setf target-nth (- list-n 1)))

    (loop
     for nth from begin-nth to target-nth
     do
     (setf newlist (append newlist (list (nth nth list))))
     ;;end loop
     )
    
    (when  return-restlist-p 
      (setf restlist (nthcdr (+ target-nth 1) list)))

    (values newlist restlist)
    ;;end let, get-percent-list
    ))
;;TEST
;;  (get-percent-list 30 '(1 2 3 4 5 6 7 8 9 10))  = (1 2 3 4) NIL
;;  (get-percent-list 30 '(1 2 3 4 5 6 7 8 9 10) :return-restlist-p T) 
;;   works= (1 2 3 4)   (5 6 7 8 9 10)
;; (get-percent-list 34 '(1 2 3 4 5 6 7 8 9 10) :return-restlist-p T)





;;APPEND-ITEM-TO-LISTS
;;2016
;;ddd
(defun append-item-to-lists  (lists  item &key to-front-p)
  "Appends an item (symbol, string, number, list) to EACH LIST in lists. If TO-FRONT-P, puts item in front of each returned list. RETURNS newcombos. Does NOT test to see if item is in list."
  (let
      ((newcombo)
       (newcombos)
       (len-lists (list-length lists))
       )
    (loop
     for list in lists
     do
     (cond
      ((listp item)
       (cond
        (to-front-p
         (setf newcombo (append  item (list  list))))
         (t 
          (setf newcombo (append (list list) item))))
       )
      (t (cond
          (to-front-p
           (setf newcombo (list item list)))
          (t 
           (setf newcombo (list list item))))))

     (setf newcombos (append newcombos (list newcombo)))
     ;;end loop
     )
    newcombos
    ))
;;TEST
;; (append-item-to-lists '(1 2 3 4)  5) = ((1 5) (2 5) (3 5) (4 5))
;; (append-item-to-lists '((1 5) (2 5) (3 5) (4 5))  4) 
;;  = ((1 5 4) (2 5 4) (3 5 4) (4 5 4))
;; ;; (append-item-to-lists '(3 4 5) '(1 2) :to-front-p t);;  
;;  = ((1 2 3) (1 2 4) (1 2 5))






;;APPEND-NEW-COMBOS
;;2016
;;ddd
(defun append-new-combos (newitem  lists)
  "In U-lists.  Appends newitem to BEGIN of all lists if NOT IN THE LIST. RETURNS newlists."
  (let
      ((newlist)
       (newlists)
       )
    (loop
     for list in lists
     do
     (when (not (my-member newitem list))
       (setf newlist (append (list newitem) list)))
     (when newlist
       (setf newlists (append newlists (list newlist))))
     ;;reset 
     (setf newlist nil)           
     ;;(afout 'out (format nil "list= ~A newlist= ~A newlists= ~A~%" list newlist newlists))
     )
    newlists
    ;;end let, append-combos
    ))
;;TEST
;; (append-new-combos 1  '((1 2) (1 3) (1 4) (1 5) (2 3) (2 4) (2 5) (3 4) (3 5) (4 5)))
;; WORKS = ((1 2 3) (1 2 4) (1 2 5) (1 3 4) (1 3 5) (1 4 5)) 
 ;;NOTE:can make less 3ple lists that don't already include 1
;;
;;(append-new-combos 2  '((1 2) (1 3) (1 4) (1 5) (2 3) (2 4) (2 5) (3 4) (3 5) (4 5)))
;; works= ((2 1 3) (2 1 4) (2 1 5) (2 1 5) (2 1 5) (2 1 5) (2 3 4) (2 3 5) (2 4 5))







;;APPEND-ITEMS-TO-LIST
;;
;;2016
;;ddd     
(defun append-items-to-list (list items &key (items-per-append 1))
  "In U-lists, appends each item in items ITEMS-PER-APPEND at a time to items in list. Good foundation for making combinations."
  (let
      ((newlist list)
       (newlists)
       (n-items (list-length items))
       (num-added 0)
       (rest-items)
       )
    (loop
     for item in items
     for n from 1 to n-items
     do
     (incf num-added)
     (cond
      ((= n n-items)
       (cond
        ((= num-added items-per-append)
         (setf newlist (append newlist (list item))
          newlists (append newlists (list newlist))))
        (t
         (setf rest-items (append rest-items (list item))))))
      ((= num-added  items-per-append)           
       (setf newlist (append newlist (list item))
               newlists (append newlists (list newlist))
               newlist list
              rest-items  nil             
             num-added 0))
      ((< n n-items)
       (setf newlist (append newlist (list item))
              rest-items (append rest-items (list item))))
      (t nil))
    
     ;;end loop
     )
    (values newlists rest-items)
    ;;end let, append-items-to-list
    ))
;;TEST
;; (append-items-to-list '(A B C) '(1 2 3 4 5 6  7))
;; works= ((A B C 1) (A B C 2) (A B C 3) (A B C 4) (A B C 5) (A B C 6) (A B C 7))      NIL
;; (append-items-to-list '(A B C) '(1 2 3 4 5 6  7) :items-per-append 2)
;; works = ((A B C 1 2) (A B C 3 4) (A B C 5 6))    (7)
;; (append-items-to-list '(A B C) '(1 2 3 4 5 6  7 8) :items-per-append 3)
;;  works= ((A B C 1 2 3) (A B C 4 5 6))   (7 8)
;; (append-items-to-list '(A B C) '(1 2 3 4 5 6  7 8) :items-per-append 5)
;; works= ((A B C 1 2 3 4 5))  (6 7 8)






;;MY-LISTP
;;2016
;;ddd
(defmacro my-listp (item &key (min-length 1))
  "In U-lists, test item to see if it is a list.  Unlike listp, item can be unbound and must be a list of at least MIN-LENGTH (default = 1). NOTE: If min-length = 0, then NIL RETURNS 0 NOT NIL. RETURNS LIST-LENGTH OR NIL."
  `(let 
      ((len-list)
       (result)
       )
     (cond
      ((and (symbolp (quote ,item))
            (boundp (quote ,item))
            (listp ,item))
       (setf len-list (list-length ,item))
      (cond
       ((and (= len-list 0)(= ,min-length 0))
        (setf result len-list))
       ((>= len-list ,min-length)
        (setf result len-list))
       (t nil))
      ;;end and clause
      )
      ;;a symbol, string, number, unboundp etc.
      ((or (and (symbolp (quote ,item))
                (null (boundp (quote ,item))))
            (numberp (quote ,item))
            (stringp (quote ,item)))
       NIL)
      ((listp  ,item)
       (setf result (list-length ,item)))
      (t nil))

     (values result (quote ,item))
    ;;end let, my-listp
    ))
;;TEST
;;  (my-listp 'axx) = NIL (QUOTE AXX)
;;  (my-listp 22) = NIL  22
;;  (my-listp "axx") = NIL  "axx"
;;  (my-listp NIL) = NIL NIL
;; (my-listp NIL :min-length 0)  = 0
;;  (my-listp '(a b c)) = 3   (QUOTE (A B C))
;;  mother = ("MOTHER" "mother" ELM2-1-1-99 NIL NIL :BIPATHS (((MOTHER NIL (TALL3 (POLE2) NIL))) ((MOTHER NIL (GREEN (POLE2) NIL)))))
;; (my-listp 'mother) = nil (QUOTE MOTHER)
;; (my-listp mother) = 7  MOTHER
;; (setf  short-list '(1))
;; (my-listp short-list) = 1 short-list
;; (my-listp short-list   :min-length 2) = NIL SHORT-LIST
;; (my-listp  'short-list) = NIL  (QUOTE SHORT-LIST)
;; (setf long-list '(a b c d e))
;; (my-listp long-list   :min-length 2) = 5 LONG-LIST
;; (my-listp 'long-list   :min-length 2) = NIL  (QUOTE LONG-LIST)


;;SYMBOL-LISTP
;;2016
;;ddd
(defmacro symbol-listp (sym &key (min-length 1) (return-listp t))
  "In U-lists.  Tests to see if symbol is bound and evals to a list.  RETURNS (values result list len-list) RESULT = nil or list length.  LIST = evaled symbol list or nil. SYM must be  bound or error."
  `(let
       ((result)
        (len-list)
        (list)
        )
     (when (symbolp ,sym)
       (cond
        ((and  (boundp  ,sym))      
         (setf len-list (list-length (eval  ,sym)))
         (cond
          ((and (= len-list 0)(= ,min-length 0))
           (setf result len-list)
           (when ,return-listp
             (setf list (eval ,sym))))
          ((>= len-list ,min-length)
           (setf result len-list)
           (when ,return-listp
             (setf list (eval ,sym))))
          (t nil))
         ;;end and clause
         )
        (t nil))
       ;;end when
       )
     (values result list)
     ;;end let, symbol-listp
     ))
;;TEST
;; (symbol-listp 'mother) = 7   ("MOTHER" "mother" ELM2-1-1-99 NIL NIL :BIPATHS (((MOTHER NIL (TALL3 (POLE2) NIL))) ((MOTHER NIL (GREEN (POLE2) NIL)))))
;; (symbol-listp mother) = nil nil



;;UNQUOTED-LISTP
;;2016
;;ddd
(defmacro unquoted-listp (x)
  "In U-lists. Tests whether the the UNQUOTED symbol evals to a list. Also, see my-listp, which does much more."
  `(cond
   ((and (symbolp (quote ,x))
         (boundp (quote ,x)))
    (listp ,x))
   (t nil))
  )
;;TEST
;; (unquoted-listp 'this) = nil  ;;this is unbound
;; (unquoted-listp 'mother) = nil
;; (unquoted-listp mother) = T



        


 
;;FIND-SYMBOLS-FOR-NAMES
;;2016
;;ddd
(defun find-symbols-for-names (namelist)
  "In U-lists, uses my-make-symbol to return a list of symbols from a list of strings/names."
        (loop
         for name in namelist
         do
         collect (my-make-symbol name)
         )
    ;;end find-symbols-for-names
    )    
;;TEST
;; (find-symbols-for-names '("mom" "dad" "best-m-friend")) 
;;works = (MOM DAD BEST-M-FRIEND)
     


;;SET-CATSYM-TO-NESTED-NAMES
;;2016
;;ddd
(defun set-catsym-to-nested-names (all-catlists 
                                   &key (return-all-nested-names-p T) (append-prefix "*")
                                   (append-postfix ""))
  "In U-lists, sets global vars by name of  * + pce-cat to the qvar-names within the cat--for all pce-cats, where the lists are the CDR of the catlist, and the nested-names are the CAR of each sublist. RETURNS (values catvars globalcatvars all-nested-names) if return-all-nested-names-p."
  (let
      ((catlists)
       (catlist)
       (catvar) 
       (globalcatvar)
       (globalcatvars)
       (catvars)
       (catsublists)
       (nested-names)
       (all-nested-names)
       )
    (loop
     for catlist in all-catlists
     do
     (when (listp catlist)
     (setf catvar (car catlist)
           globalcatvar (my-make-symbol 
                         (format nil "~A~A~A" append-prefix catvar append-postfix))
           globalcatvars (append globalcatvars (list globalcatvar))
           catvars (append catvars (list catvar))
          catsublists (cdr catlist))

     (when (listp catsublists)
       (setf nested-names (get-all-nths-in-lists 0 catsublists)
             all-nested-names (append all-nested-names (list nested-names))))

     ;;set the catvar to nested-names
     (set globalcatvar nested-names)
     ;;end when,loop
     ))

    (cond
     (return-all-nested-names-p
      (values catvars globalcatvars all-nested-names))
     (t (values catvars globalcatvars)))
     ;;end let, set-catsym-to-nested-names
     ))
;;TEST
;; (set-catsym-to-nested-names '((CA1 ("name1" b c)("name2" d e))(CA2 ("name3" f g)("name4" x y z)) sym1 "this"))
;;works= 
;;(CA1 CA2)   (*CA1 *CA2)   (("name1" "name2") ("name3" "name4"))
;;CL-USER 18 > *ca1 =  ("name1" "name2")
;;
;;  (set-catsym-to-nested-names '((CA1 ("name1" b c)("name2" d e))(CA2 ("name3" f g)("name4" x y z)) sym1 "this") :append-postfix "-post")
;; works= (CA1 CA2)   (*CA1-POST *CA2-POST)   (("name1" "name2") ("name3" "name4"))
;; CL-USER 26 > *CA2-POST  =  ("name3" "name4")







;;GET-KEYVALUE-IN-NESTED-LIST
;;2016-08
(defun get-keyvalue-in-nested-list (key-spec-lists  nested-lists 
                                                    &key  return-list-p  (max-list-length 1000))
  "In U-lists, RETURNS (values return-value return-keylist new-keylist return-nested-lists  last-key-found-p ) unless RETURN-LIST-P then (values return-keylist return-value new-keylist return-nested-lists  last-key-found-p) . See doc for main function, get-set-append-keyvalue-in-nested-list"

  (multiple-value-bind (return-keylist return-nested-lists new-keylist
                                       return-value old-keylist last-key-found-p )
      (get-set-append-keyvalue-in-nested-list :get  key-spec-lists  nested-lists 
                                              :return-list-p return-list-p
                                              :append-keyvalue-p nil
                                              :max-list-length max-list-length)
  
    (cond
     ;;when return-list-p, switch return-value and return-keylist
     (return-list-p (values return-keylist return-value new-keylist return-nested-lists  last-key-found-p))
     (t
      (values return-value return-keylist new-keylist return-nested-lists  last-key-found-p )))
    ;;end mvb, get-keyvalue-in-nested-list
    ))
;;TEST
;; (get-keyvalue-in-nested-list  '((:key3 T 2)(:key4 0)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95) :key5 (xxx))  )  
;; works= 0.95    (:KEY4 0.95)   (:KEY4 0.95)   ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95) :KEY5 (XXX))    T
;;
;;return-list-p
;;  (get-keyvalue-in-nested-list  '((:key3 T 2)(:key4 0)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95 rest of list) :key5 (xxx))  :return-list-p T )
;; works= 0.95    (:KEY4 0.95 REST OF LIST)    (:KEY4 0.95)   ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95 REST OF LIST) :KEY5 (XXX))   T
;;USE OF T and lists inside list with no key
;;  (progn (setf out nil) (get-keyvalue-in-nested-list  (list (list T  0) (list 'motherq  0)) '(PCE-PEOPLE (PCE-PEOPLE-INSTR (People Important To You *INSTR-NAME-ELEMENT)) (MOTHERQ (Your Mother [or person most like a mother to you]) PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS) (FATHERQ (Your Father [or person most like a father to you]) PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS) (BEST-M-FRIENDQ (A best male friend) PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS) (BEST-F-FRIENDQ (A best female friend) PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS) ) :return-list-p T))
;;works= 
#|(MOTHERQ (YOUR MOTHER [OR PERSON MOST LIKE A MOTHER TO YOU]) PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS)
(YOUR MOTHER [OR PERSON MOST LIKE A MOTHER TO YOU])
(MOTHERQ (YOUR MOTHER [OR PERSON MOST LIKE A MOTHER TO YOU]))
((MOTHERQ (YOUR MOTHER [OR PERSON MOST LIKE A MOTHER TO YOU]) PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS) (PCE-PEOPLE (PCE-PEOPLE-INSTR (PEOPLE IMPORTANT TO YOU *INSTR-NAME-ELEMENT)) (MOTHERQ (YOUR MOTHER [OR PERSON MOST LIKE A MOTHER TO YOU]) PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS) (FATHERQ (YOUR FATHER [OR PERSON MOST LIKE A FATHER TO YOU]) PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS) (BEST-M-FRIENDQ (A BEST MALE FRIEND) PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS) (BEST-F-FRIENDQ (A BEST FEMALE FRIEND) PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS)))
T|#







;;GET-SET-APPEND-KEYVALUE-IN-NESTED-LIST--MAIN FUNCTION
;;2016 version
;;  
#|;;LOGIC FOR get-set-append-keyvalue-in-nested-list
;; NESTED-LISTS MUST BE A LIST
;;1. IF LAST-KEY-FOUND-P
   USE GET-ADD AND PASS FINAL LISTS UP THRU RECURSE
;;2. IF  NULL  NESTED-LISTS, NIL 
;;3. IF NULL KEY-SPEC-LISTS  & KEY-NOT-FOUND-P
            RECURSE PASS UP NESTED-LISTS TO ADD
;;4. LOOP THRU  NESTED-LISTS
;;      FOR ITEM IN NESTED-LISTS, FOR EACH ITEM
     4,1 IF LISTP ITEM: 
;;         4.1.1 IF NUMBERP KEYLOC-N, use NTH on nested-lists 
;;                  4.1.1.1 IF KEY FOUND, (setf new-key-spec-lists (cdr key-spec-lists))
                          4.1.1.1.1 If last-key-found-p, GO TO build final lists (get-set..)
                          4.1.1.1.2 If not last key, Use new-key-spec-lists
                          RECURSE on that list and put head and tail aside for appending later.
                    4.1.1.2 IF KEY NOT FOUND
                                     Add list to return-nested lists, do not recurse.
          4,1,2 IF NOT NUMBERP, loop thru every item on nested-lists
                   LOOP,  FOR EACH ITEM
                  4.2.1 IF ITEM NOT LIST
                        4.2.1.1 ITEM = KEY (then find value and in betw items)
                                (setf new-key-spec-lists (cdr key-spec-lists))
                               4.2.1.1.1 If last-key-found-p, GO TO build final lists (get-set..)
                               4.2.1.1.2 key-spec-lists, RECURSE ON THIS NESTED-LISTS
                                      USING new-key-spec-lists.
                        4.2.1.2 ITEM NOT= KEY,
                                      Add to a return sublist?
                  4.2.2 ITEM IS A LIST, 
                         RECURSE on that ITEM (a list)
                                    and put head and tail aside for appending later.
          5.  AT BOTTOM If  NULLlast-key-found-p ever found , 
                    and ITEM NOT LIST, ADD TO FINAL LISTS.
;;end get-set-append-keyvalue-in-nested-list LOGIC|# 
;;
;;GET-SET-APPEND-KEYVALUE-IN-NESTED-LIST--MAIN FUNCTION
;;2016 version
;;ddd
(defun get-set-append-keyvalue-in-nested-list (new-value  key-spec-lists  nested-lists                                                           &key append-keyvalue-p  add-value-p
                                                          return-list-p 
                                                          return-orig-nested-list-p
                                                          (max-list-length 1000)  
                                                          (if-not-found-append-key-value-p T)
                                                          (if-not-found-append-keyvalue-list-p T)
                                                          (bottom-level-p T)
                                                          orig-list-items-leftn
                                                          put-key-after-items
                                                          put-value-after-items
                                                          begin-items  end-items
                                                          splice-key-value-in-list 
                                                          put-splice-keylist-p
                                                          (recurse-for-key-p T)
                                                          ;;cur-level-list
                                                          last-key-found-p )
  ;;cause errors?             return-keylist new-keylist old-keylist  return-value) 
  "In U-lists. THE NESTED LISTS REQUIREMENTS: (1) Can have exact loc of all keys keyloc-n as a number OR can be NIL or T [In which case will check entire list at that level for key]. Also, if the KEY = T, searches entire list.  DOES NOT check for keys in any other location than keyloc-n (unless T, NIL);   (2) Each new level does NOT need a key in the key-spec-lists (it can be a list with lists containing keys; and (3) VALUE MUST either be A. item next to key (val-nth = 1) OR val-nth [the optional 3rd item in the level spec-list].  If :NR member spec-list, sets RECURSE-FOR-KEY-P to NIL If :R member, sets it to T, then key MUST be IN LIST on FIRST LEVEL of current recursion (doesn't recurse on inner lists for that spec-list). 
   If NEXT-KEY-IN-VALUE MEMBER spec-list,  searches value list for list with next key.
    RETURNS (return-keylist new-return-nested-lists new-keylist return-value  old-keylist last-key-found-p ) If return-list-p, return-keylist is the ENTIRE list containing key, otherwise same as new-keylist. KEY-SPEC-LISTS are lists of (key keyloc-n val-n) [val-nth optional, default= 0) from outermost to innermost (last) key. Eg of proper list (:k1 (a) :key1 (k2 (b) :key2 (k3 (c) k4 (d) :key3 (:key5 OLD-VALUE)...)); key-spec-list= ((:key1 2)(:key2  2)(:key3 4)(:key5 0)) .IF-NOT-FOUND-APPEND-KEY-VALUE-P adds a key and keyvalue to innermost list if not found (but preceding key must be found). If last keyloc-n = 0, puts old previous keyvalue at end of new keylist. If last keyloc-n > 0, puts it first, then fills in nils before new key in last list which is new value of previous key. IF KEY = :NTH, then gets, sets, or appends number in KEYLOC-N PLACE. Note: If second item in spec-list isn't number, uses default keyloc-n.  PUT-KEY-AFTER-ITEMS is a list of items which is used if keyloc-n > 0 to fill in items between key and value [done automatically if old items are there]. splice-key-value-in-list does similar except includes items after the value. If PUT-SPLICE-KEYLIST-P, puts or splices (list key value) into put or splice list. Otherwise puts or splices key value [not a list]. COMPATIBIILITY PROBLEMS: get-key-value-in-nested-lists, val-nth starts with 0 there, with 1 here. APPEND-KEYVALUE-P puts old-value & new-value in a list.  ADD-VALUE-P just adds new-value after old-value."
  (let*
      ((return-nested-lists)
       (new-return-nested-lists)
       (match-item)
       (spec-list (car key-spec-lists))
       (KEY (first spec-list))
       (keyloc-n (second spec-list))
       (val-nth 1)
       (new-spec-lists)
       (new-nested-list1)
       (new-nested-lists)
       (new-key-spec-lists)
       (key-spec-lists1)
       (loop-return-list)
       (add-new-value-p)
       (cur-level-list)
       (list-head)
       (list-tail)
       (length-nnl) 
       (old-value)
       (key-found-p)
       (added-key-value)
       (return-keylist)(new-keylist)(old-keylist)( return-value)
       (testitem)
       (next-key-in-value-p)
       (item)
       )
    
    ;;LISTP NESTED-LISTS--Function only processes list inputs.
    (cond           
     ((listp nested-lists)
      (setf length-nnl (list-length nested-lists))

      ;;SPEC-LIST VARIABLES ----------------------------
      (cond
       ;;KEY = T  (for compatibity with older and is useful)
       ((or  (equal key 'T)(equal key T))
        (setf  key-spec-lists  (cdr key-spec-lists)
               spec-list (car key-spec-lists)
               key (car spec-list)
               keyloc-n (second spec-list)
               recurse-for-key-p T)
        ;;(afout 'out (format nil ">>KEY= T, THEN KEY= ~A,~% spec-list= ~A keyloc-n= ~A  KEY-SPEC-LISTS= ~A~%next-key-in-value-p= ~A recurse-for-key-p= ~A"key  spec-list keyloc-n key-spec-lists   next-key-in-value-p  recurse-for-key-p))
        )
       ;;FOR KEY = :NTH (to find nth in list without key)
       ((equal key :NTH)
        (setf val-nth 0))
       ) 
      ;;KEYLOC-N (position of the key in nested-lists)
      (cond
       ((numberp keyloc-n)
        (setf  recurse-for-key-p NIL))
       ((not (numberp keyloc-n))
        (setf  recurse-for-key-p T)))
      ;;VAL-NTH, place for value after key
      (when (numberp (third spec-list))
        (setf val-nth (third spec-list)))
      ;;SET RECURSE-FOR-KEY-P (Causes search all lists in top list)
      (cond
       ((member :R spec-list)
        (setf  recurse-for-key-p T))
       ((member :NR spec-list)
        (setf  recurse-for-key-p NIL)))
      ;;SET NEXT-KEY-IN-VALUE-P  (Causes search val-nth list for next key)
      (when (member :next-key-in-value spec-list :test 'equal)
        (setf next-key-in-value-p T))
      

      ;;(afout 'out (format nil ">>>>>>>> 1-NESTED-LISTS= ~A~%SPEC-LIST= ~A~%KEYLOC-N= ~a  VAL-NTH= ~a~%next-key-in-value-p= ~A recurse-for-key-p= ~A"  nested-lists spec-list keyloc-n  val-nth  next-key-in-value-p  recurse-for-key-p))

      ;;TOPLEVEL LOOP
      (loop
       for topitem in nested-lists
       for top-n from 0 to max-list-length
       do
       ;;(afout 'out  (format nil "***** TOPLEVEL LOOP, TOPITEM= ~A TOP-N=~a~%SPEC-LIST= ~A" TOPITEM TOP-N spec-list))

       ;;TOP LEVEL COND: Is item a list or not?
       (cond
        ((listp topitem)
         (cond
          ;;IF (NUMBERP KEYLOC-N), check (nth keyloc-n nested-lists) for key
          ((and (numberp keyloc-n) (not (equal key :any-key)))

           ;;First test key at this level
           (setf item (nth keyloc-n topitem))
           ;;(afout 'out (format nil "NUMBERP KEYLOC-N= ~A, found ITEM= ~A key-spec-lists= ~A~%TOPITEM list= ~A" keyloc-n item  key-spec-lists topitem ))
           (cond
            ;;TOPITEM = KEY
            ((or (my-equal key item) (equal key :nth))
             (setf key-found-p T
                   list-head (butlast nested-lists (- length-nnl top-n ))
                   list-tail (nthcdr (+ top-n 1) nested-lists)
                   #|                  n-topitems (list-length topitem)                  
                  topitem-head (butlast topitem (- n-topitems keyloc-n))
                  topitem-tail (nthcdr topitem (+ keyloc-n val-nth))
|#                  new-key-spec-lists (cdr key-spec-lists))
             #|            (when (not (equal keyloc-n (+  1)))
              (setf  ???? here))|#
            
             ;;new-nested-lists topitem)  ;;next key list must be within this list
             ;;(afout 'out (format nil ">>>In numberp, HEAD-TAIL:KEY= ~A FOUND-P= ~A keyloc-n ~A top-n= ~A  length-nnl= ~A~% new-nested-lists= ~A~%list-head= ~A ~%list-tail= ~A"key key-found-p keyloc-n top-n  length-nnl new-nested-lists list-head    list-tail))
             (cond
              ((null new-key-spec-lists)
               ;;copied from old
               (setf old-value (nth (+ keyloc-n  val-nth) topitem)
                     old-keylist topitem
                     last-key-found-p T)

               ;;IS KEY= :NTH?
               (cond 
                ((not (equal key :NTH)) 
                 (multiple-value-setq (return-keylist new-keylist return-value)
                     (get-set-append-keyvalue key old-value new-value
                                              :keyloc-n keyloc-n :val-nth val-nth
                                               :old-keylist TOPITEM
                                               :append-value-p append-keyvalue-p
                                              :add-value-p add-value-p
                                              :put-key-after-items put-key-after-items
                                              :put-value-after-items put-value-after-items
                                              :begin-items begin-items :end-items end-items
                                              :splice-key-value-in-list splice-key-value-in-list
                                              :put-splice-keylist-p put-splice-keylist-p))

#| WAS HERE                )
                ;;was (setf old-keylist (list key old-value)))
                (t (setf old-keylist (list old-value))))|#

               ;;copied from old
               ;;Inserts items betw keyloc-n and val-nth into final returned list
               (cond
                ((> keyloc-n 0)
                 (setf  put-key-after-items (subseq topitem 0  keyloc-n) ;; (+ keyloc-n val-nth))
                        ;;was (+ keyloc-n 1) (+ keyloc-n val-nth))
                       ;;was put-splice-keylist-p NIL ;;SSS or should be NIL?
                        splice-key-value-in-list nil))
                ;;added
                (t (setf put-key-after-items NIL)))
               ;;copied from old end

               ;;set old-keylist
               (setf old-keylist nested-lists)

               ;;(afout 'out (format nil "LAST KEY FOUND= key= ~A  old-keylist= ~a~%put-key-after-items= ~a old-value= ~A" key old-keylist put-key-after-items old-value))
               (setf return-nested-lists  (append list-head
                                                  (list return-keylist)
                                                  list-tail))
               ;;(BREAK "before return-list-p")
               (when return-list-p
                 (setf return-keylist return-nested-lists))

               ;;(afout 'out (format nil "AFTER LAST-KEY-FOUND: list-head= ~A~%return-keylist= ~A~%list-tail= ~A~%return-nested-lists= ~A" list-head return-keylist list-tail return-nested-lists))   
               (when last-key-found-p
                 (return)) 
               ;;(BREAK "RETURN? if  last-key-found-p")
               ;;END (not (equal key :NTH))
               )
                ;;was (setf old-keylist (list key old-value)))
                (t (setf old-keylist (list old-value))))
               ;;end null spec-list
               )
              ;;RECURSE TO NEXT KEY-SPEC
              ( T 
               ;;RECURSE ON  new-key-spec-lists new-nested-lists ;;same as nested lists
               ;; ADD TO RETURN NESTED-LISTS ETC
               ;;VARS SET ABOVE
              ;;(afout 'out (format nil "RECURSE TO NEXT KEY-SPEC: list-head= ~A~%return-keylist= ~A~%list-tail= ~A~%return-nested-lists= ~A next-key-in-value-p= ~a" list-head return-keylist list-tail return-nested-lists next-key-in-value-p))

               (when next-key-in-value-p
                 (setf topitem (nth (+ val-nth top-n) nested-lists)
                       list-head (butlast nested-lists (- length-nnl (+ top-n val-nth)))  
                                          ;;was (+ (- length-nnl top-n) val-nth))
                       list-tail (nthcdr  (+ top-n val-nth)  nested-lists))
               ;;set above  list-tail  (nthcdr top-n))
                 )

               ;;(BREAK "XX BEFORE RECURSE")
               ;;RECURSE ON SAME LIST WITH CDR  KEY-SPEC-LIST (set above)
               (multiple-value-setq (return-keylist new-return-nested-lists new-keylist
                                                    return-value   old-keylist last-key-found-p )
                   (get-set-append-keyvalue-in-nested-list new-value  new-key-spec-lists  
                                                           TOPITEM 
                                                           :return-list-p return-list-p
                                                           :append-keyvalue-p  append-keyvalue-p
                                                           :add-value-p add-value-p
                                                           :begin-items begin-items :end-items end-items
                                                           :max-list-length max-list-length
                                                           :last-key-found-p   last-key-found-p
                                                           ;;      :new-keylist new-keylist
                                                           ;;     :old-keylist old-keylist
                                                           ;;      :return-value return-value
                                                           :if-not-found-append-key-value-p NIL
                                                           :bottom-level-p nil
                                                           ;; :put-key-after-items put-key-after-items
                                                           ;; :put-value-after-items put-value-after-items
                                                           ;; :splice-key-value-in-list splice-key-value-in-list
                                                           :put-splice-keylist-p put-splice-keylist-p
                                                           ;;  :return-keylist return-keylist
                                                           ))

               (setf  return-nested-lists (append list-head 
                                                  put-key-after-items  ;; (list key)  ;;list key added
                                                  (list  new-return-nested-lists)   list-tail))

               ;;(afout 'out (format nil "IN TOP NUMBERP, AFTER RECURSE ON TOPITEM= ~a, top-n= ~A new-keylist= ~A~%return-value= ~A~%new-return-nested-lists= ~A~%list-head= ~A~%list-tail= ~A new-keylist= ~A return-keylist= ~A~%FINAL: return-nested-lists= ~A~% AND put-key-after-items= ~a" TOPITEM top-n new-keylist  return-value new-return-nested-lists list-head list-tail new-keylist return-keylist return-nested-lists put-key-after-items))

               (when return-list-p
                 (setf  return-keylist  new-return-nested-lists))
               ;;(BREAK "BEFORE RETURN")
               (when last-key-found-p
                 (return))
               ;;end key found, but not last key, cond
               ))
             ;;END KEY=ITEM
             )
            ;;KEY NOT= ITEM, and  (null recurse-for-key-p)
            ((null recurse-for-key-p)
             (setf return-nested-lists (append return-nested-lists (list topitem)))
             ;;(afout 'out (format nil "IN TOP NUMBERP, NO-RECURSE, KEY= ~a NOT= ITEM= ~a~%topitem= ~A~% (append return-nested-lists (list topitem))= ~A"  key item topitem return-nested-lists))
             ;;end (null recurse-for-key-p)
             )
            ;;KEY NOT=ITEM, RECURSE (recurse-for-key-p)
            (recurse-for-key-p
             ;;RECURSE ON  new-key-spec-lists new-nested-lists ;;same as nested lists
             ;; ADD TO RETURN NESTED-LISTS ETC
             ;;don't loop, done in recurse?    
             (setf list-head (butlast nested-lists (- length-nnl top-n))
                   list-tail (nthcdr (+ top-n 1) nested-lists))

             ;;(BREAK "XX BEFORE TOPITEM LIST RECURSE")
             ;;RECURSE ON SAME LIST WITH SAME  KEY-SPEC-LIST (set above)
             ;;  WAS ON CDR KEY-SPEC-LIST, BUT IF NOT FOUND, NO
             (multiple-value-setq (return-keylist new-return-nested-lists new-keylist
                                                  return-value   old-keylist last-key-found-p )
                 (get-set-append-keyvalue-in-nested-list new-value  key-spec-lists  
                                                         TOPITEM 
                                                         :return-list-p return-list-p
                                                         :append-keyvalue-p  append-keyvalue-p
                                                         :add-value-p add-value-p
                                                         :begin-items begin-items :end-items end-items
                                                         :max-list-length max-list-length
                                                         :last-key-found-p   last-key-found-p
                                                         ;;      :new-keylist new-keylist
                                                         ;;     :old-keylist old-keylist
                                                         ;;      :return-value return-value
                                                         :put-value-after-items put-value-after-items
                                                         :if-not-found-append-key-value-p NIL
                                                         :RECURSE-FOR-KEY-P T
                                                         :bottom-level-p nil
                                                         ;; :put-key-after-items put-key-after-items
                                                         ;; :splice-key-value-in-list splice-key-value-in-list
                                                         :put-splice-keylist-p put-splice-keylist-p
                                                         ;;  :return-keylist return-keylist
                                                         ))
             (setf  return-nested-lists (append list-head 
                                                put-key-after-items  ;; (list key)  ;;list key added
                                                (list  new-return-nested-lists)   list-tail))

             ;;(afout 'out (format nil "IN TOP NUMBERP, AFTER RECURSE ON TOPITEM LIST= ~a, top-n= ~A new-keylist= ~A~%return-value= ~A~%new-return-nested-lists= ~A~%list-head= ~A~%list-tail= ~A new-keylist= ~A return-keylist= ~A~%FINAL: return-nested-lists= ~A~% AND put-key-after-items= ~a" TOPITEM top-n new-keylist  return-value new-return-nested-lists list-head list-tail new-keylist return-keylist return-nested-lists put-key-after-items))

             (when return-list-p
               (setf  return-keylist  new-return-nested-lists))
             ;;(BREAK "BEFORE RETURN")
             (when last-key-found-p
               (return))
            ;;end recurse-for-key-p
            ))

           ;;END NUMBERP KEYLOC-N
           )
          ;;(LISTP TOPITEM), BUT  KEYLOC-N = T, NIL or other non-number
          ;;Recurse on this list to search for key?
          (recurse-for-key-p
           (setf list-head (butlast nested-lists (- length-nnl top-n)) ;;lll
                 list-tail (nthcdr (+ top-n 1) nested-lists))
         
           ;;put this here?
           (cond
            ((equal key :any-key)
             (setf key-spec-lists1 (cdr key-spec-lists)))
            (t (setf key-spec-lists1 key-spec-lists)))
           
           (multiple-value-setq (return-keylist new-return-nested-lists new-keylist
                                                return-value   old-keylist last-key-found-p )
               (get-set-append-keyvalue-in-nested-list new-value  key-spec-lists1  
                                                       TOPITEM 
                                                       :return-list-p return-list-p
                                                       :append-keyvalue-p append-keyvalue-p
                                                       :add-value-p add-value-p
                                                       :begin-items begin-items :end-items end-items
                                                       :max-list-length max-list-length
                                                       :last-key-found-p   last-key-found-p
                                                       ;;      :new-keylist new-keylist
                                                       ;;     :old-keylist old-keylist
                                                       ;;      :return-value return-value
                                                       :if-not-found-append-key-value-p NIL
                                                       :RECURSE-FOR-KEY-P  recurse-for-key-p
                                                       :bottom-level-p nil
                                                       ;; :put-key-after-items put-key-after-items
                                                       ;; :splice-key-value-in-list splice-key-value-in-list
                                                       ;; :put-value-after-items put-value-after-items
                                                        :put-splice-keylist-p put-splice-keylist-p
                                                       ;;  :return-keylist return-keylist
                                                       ))
           (setf  return-nested-lists (append list-head 
                                              put-key-after-items  ;; (list key)  ;;list key added
                                              (list  new-return-nested-lists)   list-tail))

           ;;(afout 'out (format nil "IN TOP NUMBERP LOOP, AFTER RECURSE, ITEM=new-keylist= ~A~%return-value= ~A~%new-return-nested-lists= ~A~%list-head= ~A~%list-tail= ~A new-keylist= ~A return-keylist= ~A~%FINAL: return-nested-lists= ~A~% AND put-key-after-items= ~a" ITEM  return-value new-return-nested-lists list-head list-tail new-keylist return-keylist return-nested-lists put-key-after-items))

           (when last-key-found-p
             (return))
           ;;end listp topitem, not numberp keyloc-n,  listp item, recurse-for-key-p
           ))
         ;;END (LISTP TOPITEM)
         )
        ;;TOPITEM NOT A LIST
        (t
         (cond
          ;;TOPITEM = KEY ( or NTH if :NTH)
          ((or (my-equal topitem key)
               (and (equal key :nth) (equal top-n keyloc-n)))
           ;;(afout 'out (format nil ">>>> 1. TOPITEM NOT A LIST, KEY= ~a  FOUND~% NESTED-LISTS=old-keylist= ~A, ~%FOR OLD: SPEC-LIST= ~a, TOP-N= ~a NEXT-KEY-IN-VALUE-P= ~a " key nested-lists spec-list top-n next-key-in-value-p))
          (setf key-found-p T              
                   new-key-spec-lists (cdr key-spec-lists)             
                   new-nested-lists (nth (+ top-n val-nth) nested-lists) ;;DIFFERENT
                   old-keylist nested-lists                  
                   list-head (butlast nested-lists (- length-nnl (+ top-n val-nth)))
                   list-tail (nthcdr (+ top-n val-nth 1) nested-lists))
           (when (not (numberp keyloc-n))
             (setf keyloc-n top-n)) 
           ;;(afout 'out (format nil ">>>> 2. TOPITEM NOT A LIST, KEY= ~a  FOUND~% NESTED-LISTS= ~A, ~%FOR NEW RECURSE: new-key-spec-lists ~a, TOP-N= ~a ~%LIST-HEAD= ~a~%LIST-TAIL= ~a" key nested-lists new-key-spec-lists top-n  list-head list-tail))
           ;;(BREAK "TOPITEM NOT LIST, FOR NEW RECURSE:")

           (cond
            ;;LAST-KEY-FOUND-P
            ((null new-key-spec-lists)
             ;;GET-SET
             (setf last-key-found-p T
                   old-value (nth (+ keyloc-n val-nth) nested-lists))

            ;;(afout 'out (format nil ">>>> 2. LAST-KEY-FOUND. OLD-VALUE= ~A" old-value))      

             ;;copied from old
             ;;Inserts items betw keyloc-n and val-nth into final returned list
             (cond            
              #|topitem not a list, clause moot
              ((and (listp topitem)(numberp keyloc-n)(> keyloc-n 0))
               (setf  put-key-after-items (subseq topitem 0  keyloc-n) ;; (+ keyloc-n val-nth))
                      old-value (nth (+ keyloc-n val-nth) topitem)
                      ;;was (+ keyloc-n 1) (+ keyloc-n val-nth))
                     ;;was  put-splice-keylist-p NIL ;;SSS or should be NIL?
                      splice-key-value-in-list nil))|#
              ((and (numberp keyloc-n) (> keyloc-n 0))
               (setf  put-key-after-items (subseq nested-lists 0  keyloc-n) 
                      ;;was (+ keyloc-n 1) (+ keyloc-n val-nth))
                     ;;was  put-splice-keylist-p NIL ;;SSS or should be NIL?
                      splice-key-value-in-list nil))
              ;;added
              (t (setf put-key-after-items NIL)))

             ;;(BREAK "BEFORE GET-SET-OR-APPEND")

             ;;USE GET-SET-OR-APPEND
             (multiple-value-setq (return-keylist new-keylist return-value)
                     (get-set-append-keyvalue key old-value new-value
                                              :keyloc-n keyloc-n :val-nth val-nth
                                               :append-value-p append-keyvalue-p
                                              :add-value-p add-value-p
                                              :put-key-after-items put-key-after-items
                                              :put-value-after-items put-value-after-items
                                              :begin-items begin-items :end-items end-items
                                              :splice-key-value-in-list splice-key-value-in-list
                                              :put-splice-keylist-p put-splice-keylist-p))
#|            was     (get-set-append-keyvalue key old-value new-value  :keyloc-n keyloc-n
                                          :append-value-p append-keyvalue-p
                                          :put-key-after-items put-key-after-items
                                          :splice-key-value-in-list splice-key-value-in-list
                                          :put-splice-keylist-p put-splice-keylist-p))|#
             (setf return-nested-lists  (append list-head
                                                (list return-value)   ;;was return-keylist
                                                list-tail))

             ;;(BREAK "before return-list-p")
             (when return-list-p
               (setf return-keylist return-nested-lists))

             ;;(afout 'out (format nil "TOPITEM= ~A NOT A LIST AFTER LAST-KEY-FOUND:~%GET-SET-APPEND; top-n= ~A list-head= ~A~%return-keylist= ~A~%list-tail= ~A~%return-nested-lists= ~A OLD-VALUE= ~a" TOPITEM top-n list-head return-keylist list-tail return-nested-lists old-value))
             (when last-key-found-p
               (return))
             ;;end (null new-key-spec-lists)
             )
            ;;LAST KEY NOT FOUND, RECURSE ON NEW-NESTED-LISTS
            (t
             ;;(BREAK "KEY-FOUND, RECURSE, TOPITEM NOT A LIST")
             ;;Arguments set above after key found
             (multiple-value-setq (return-keylist new-return-nested-lists new-keylist
                                                  return-value   old-keylist last-key-found-p )
                 (get-set-append-keyvalue-in-nested-list new-value  new-key-spec-lists  
                                                         NEW-NESTED-LISTS
                                                         :return-list-p return-list-p
                                                         :append-keyvalue-p append-keyvalue-p
                                                         :add-value-p add-value-p
                                                         :begin-items begin-items :end-items end-items
                                                         :max-list-length max-list-length
                                                         :last-key-found-p   last-key-found-p
                                                         ;;      :new-keylist new-keylist
                                                         ;;     :old-keylist old-keylist
                                                         ;;      :return-value return-value
                                                         :if-not-found-append-key-value-p NIL
                                                         :bottom-level-p nil
                                                         ;; :put-key-after-items put-key-after-items
                                                         ;; :splice-key-value-in-list splice-key-value-in-list
                                                         ;; :put-splice-keylist-p put-splice-keylist-p
                                                         :put-splice-keylist-p put-splice-keylist-p
                                                         ;;  :return-keylist return-keylist
                                                         ))
             ;;(afout 'out (format nil "IN LOOP, TOPITEM=KEY (NOT A LIST),, AFTER RECURSE, TOPITEM=new-keylist= ~A~%return-value= ~A~%new-return-nested-lists= ~A~%list-head= ~A~%list-tail= ~A new-keylist= ~A return-keylist= ~A~%FINAL: return-nested-lists= ~A~% AND put-key-after-items= ~a~%old-keylist= ~A last-key-found-p= ~A" TOPITEM  return-value new-return-nested-lists list-head list-tail new-keylist return-keylist return-nested-lists put-key-after-items old-keylist last-key-found-p))

             (cond
              ((numberp keyloc-n)
               (setf  return-nested-lists (append list-head 
                                                  put-key-after-items  ;; (list key)  ;;list key added
                                                  (list  new-return-nested-lists)   list-tail)))
              ;;If keyloc-n = T, return whole nested list
              (t (setf return-nested-lists new-return-nested-lists)))                         

             ;;(BREAK "IN LOOP, TOPITEM=KEY (NOT A LIST), RETURN? if  last-key-found-p")
             (when last-key-found-p
               (return))
             ))
           ;;end key found
           )
          ;;KEY NOT FOUND  AS ITEM IN TOPLIST
          (T
           (setf return-nested-lists (append return-nested-lists (list topitem)))
           ))
         ;;END TOPITEM NOT A LIST, COND
         ))

       ;;END TOPLEVEL LOOP
       )
      ;;END LISTP NESTED-LISTS
      )
     ;;NESTED-LISTS NOT A LIST
     (T (break "ERROR: NESTED-LISTS IS NOT A LIST")))
     

    ;;IF-NOT-FOUND-APPEND-KEY-VALUE-P
    (when (and if-not-found-append-key-value-p bottom-level-p
               (null last-key-found-p) (null new-keylist) (not (equal new-value :get)))
      (setf  added-key-value T
             key (caar (last key-spec-lists))
             new-keylist (list key new-value)
             return-keylist new-keylist
             return-value new-value)
      (cond
       (if-not-found-append-keyvalue-list-p
        (setf return-nested-lists (append nested-lists 
                                          (list new-keylist))))
       (t (setf return-nested-lists (append nested-lists 
                                            new-keylist))))

      ;;(BREAK "new-value if not found")      
      ;;(afout 'out (format nil "KEY NEVER FOUND, return-nested-lists= ~A" return-nested-lists))
      ;;end if-not-found-append-key-value-p clause
      )

    (when (and bottom-level-p (null return-orig-nested-list-p)
               (setf return-nested-lists  (list "return-orig-nested-list-p=NIL"))))

    ;;(BREAK "AT END")
    (values return-keylist return-nested-lists new-keylist return-value    
            old-keylist last-key-found-p )
    ;;end let, get-set-append-keyvalue-in-nested-list
    ))
;;TEST
;; COMPLEX TESTS (with added items, etc) -----------------------------------------
;;added items in all cases, append-list-p T, :append-keyvalue-p T, put-splice-keylist-p NIL
;; (progn (setf out nil)(get-set-append-keyvalue-in-nested-list  'NEW-VALUE '((:key3 2)(:key4  3 3)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  :KEY2 (0.55) (pre3a pre3b :KEY3 (44) (pre4a pre4b pre4c :KEY4 mid1 mid2 0.95 end1 end2) post3) :KEY5 (xxx)) :append-keyvalue-p t :return-list-p T :put-splice-keylist-p NIL))
;;works=  (PRE3A PRE3B :KEY3 (44) (PRE4A PRE4B PRE4C :KEY4 MID1 MID2 0.95 NEW-VALUE END1 END2) POST3)             ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) :KEY2 (0.55) (PRE3A PRE3B :KEY3 (44) (PRE4A PRE4B PRE4C :KEY4 MID1 MID2 0.95 NEW-VALUE END1 END2) POST3) :KEY5 (XXX))               (PRE4A PRE4B PRE4C :KEY4 MID1 MID2 0.95 NEW-VALUE END1 END2)                       (0.95 NEW-VALUE)                       (PRE3A PRE3B :KEY3 (44) (PRE4A PRE4B PRE4C :KEY4 MID1 MID2 0.95 END1 END2) POST3)      T
;;;;added items in all cases, append-list-p T, :append-keyvalue-p T, put-splice-keylist-p T
;;(progn (setf out nil)(get-set-append-keyvalue-in-nested-list  'NEW-VALUE '((:key3 2)(:key4  3 3)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  :KEY2 (0.55) (pre3a pre3b :KEY3 (44) (pre4a pre4b pre4c :KEY4 mid1 mid2 0.95 end1 end2) post3) :KEY5 (xxx)) :append-keyvalue-p t :return-list-p T :put-splice-keylist-p NIL)) 
;;works= (PRE3A PRE3B :KEY3 (44) (PRE4A PRE4B PRE4C :KEY4 MID1 MID2 (0.95 NEW-VALUE) END1 END2) POST3)              ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) :KEY2 (0.55) (PRE3A PRE3B :KEY3 (44) (PRE4A PRE4B PRE4C :KEY4 MID1 MID2 (0.95 NEW-VALUE) END1 END2) POST3) :KEY5 (XXX))                (PRE4A PRE4B PRE4C :KEY4 MID1 MID2 (0.95 NEW-VALUE) END1 END2)                (0.95 NEW-VALUE)                  (PRE3A PRE3B :KEY3 (44) (PRE4A PRE4B PRE4C :KEY4 MID1 MID2 0.95 END1 END2) POST3)  T

;; with T as keyloc-n AND  added items in all cases, append-list-p T, :append-keyvalue-p T, put-splice-keylist-p T
;;   (progn (setf out nil)(get-set-append-keyvalue-in-nested-list  'NEW-VALUE '((:key3 T)(:key4  3 3)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  :KEY2 (0.55) (pre3a pre3b :KEY3 (44) (pre4a pre4b pre4c :KEY4 mid1 mid2 0.95 end1 end2) post3) :KEY5 (xxx)) :append-keyvalue-p t :return-list-p T :put-splice-keylist-p T))
;;works=  (PRE3A PRE3B :KEY3 (44) (PRE4A PRE4B PRE4C :KEY4 MID1 MID2 (0.95 NEW-VALUE) END1 END2) POST3)           ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) :KEY2 (0.55) (PRE3A PRE3B :KEY3 (44) (PRE4A PRE4B PRE4C :KEY4 MID1 MID2 (0.95 NEW-VALUE) END1 END2) POST3) :KEY5 (XXX))                      (PRE4A PRE4B PRE4C :KEY4 MID1 MID2 (0.95 NEW-VALUE) END1 END2)                (0.95 NEW-VALUE)               (PRE3A PRE3B :KEY3 (44) (PRE4A PRE4B PRE4C :KEY4 MID1 MID2 0.95 END1 END2) POST3)      T

;;USES :NEXT-KEY-IN-VALUE (key3 not in a separate list here)
;;(progn (setf out nil)(get-set-append-keyvalue-in-nested-list  'NEW-VALUE '((:key3 T 2 :next-key-in-value)(:key4  3 3)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  :KEY2 (0.55) pre3a pre3b :KEY3 (44) (pre4a pre4b pre4c :KEY4 mid1 mid2 0.95 end1 end2) post3 :KEY5 (xxx)) :append-keyvalue-p t :return-list-p T :put-splice-keylist-p T))
;;works=  (PRE4A PRE4B PRE4C :KEY4 MID1 MID2 (0.95 NEW-VALUE) END1 END2)            ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) :KEY2 (0.55) PRE3A PRE3B :KEY3 (44) (PRE4A PRE4B PRE4C :KEY4 MID1 MID2 (0.95 NEW-VALUE) END1 END2) POST3 :KEY5 (XXX))                (:KEY4 (0.95 NEW-VALUE))                (0.95 NEW-VALUE)              (PRE4A PRE4B PRE4C :KEY4 MID1 MID2 0.95 END1 END2)            T      

;; ADDED ITEMS, KEY= T, NEXT-KEY-IN-VALUE
;; (progn (setf out nil) (get-set-append-keyvalue-in-nested-list 'NEW-VALUE  '((:PC-VALUES T 1 :NR :NEXT-KEY-IN-VALUE)(A3 T)) '(:PCSYM-ELM-LISTS ((A3 (MOTHER FATHER BEST-M-FRIEND)) (B3 (MOTHER FATHER BEST-F-FRIEND)) (C5 (MOTHER BEST-M-FRIEND BEST-F-FRIEND)) (D1 (FATHER BEST-M-FRIEND BEST-F-FRIEND))) :PC-VALUES ((A3 "a vs not a" :SINGLE "One of the most important things in my life" "0.917" 11 1 12 11 SCORED-NORMAL PRIORITY12)(A4 "this is A4") x y) (list z))))
;;works=  (A3 NEW-VALUE)          (:PCSYM-ELM-LISTS ((A3 (MOTHER FATHER BEST-M-FRIEND)) (B3 (MOTHER FATHER BEST-F-FRIEND)) (C5 (MOTHER BEST-M-FRIEND BEST-F-FRIEND)) (D1 (FATHER BEST-M-FRIEND BEST-F-FRIEND))) :PC-VALUES ((A3 NEW-VALUE :SINGLE "One of the most important things in my life" "0.917" 11 1 12 11 SCORED-NORMAL PRIORITY12) (A4 "this is A4") X Y) (LIST Z))               (A3 NEW-VALUE)            NEW-VALUE    (A3 "a vs not a" :SINGLE "One of the most important things in my life" "0.917" 11 1 12 11 SCORED-NORMAL PRIORITY12)    T

;USES :NEXT-KEY-IN-VALUE
;; (progn (setf out nil) (get-set-append-keyvalue-in-nested-list 'NEW-VALUE  '((:PC-VALUES T 1 :NR :NEXT-KEY-IN-VALUE)(A3 T)) '(:PCSYM-ELM-LISTS ((A3 (MOTHER FATHER BEST-M-FRIEND)) (B3 (MOTHER FATHER BEST-F-FRIEND)) (C5 (MOTHER BEST-M-FRIEND BEST-F-FRIEND)) (D1 (FATHER BEST-M-FRIEND BEST-F-FRIEND))) :PC-VALUES ((A3 "a vs not a" :SINGLE "One of the most important things in my life" "0.917" 11 1 12 11 SCORED-NORMAL PRIORITY12)(A4 "this is A4") x y) (list z))))
;;works=  (A3 NEW-VALUE)                  ((A3 NEW-VALUE "a vs not a" :SINGLE "One of the most important things in my life" "0.917" 11 1 12 11 SCORED-NORMAL PRIORITY12) (A4 "this is A4") X Y)              (A3 NEW-VALUE)             NEW-VALUE               (A3 "a vs not a" :SINGLE "One of the most important things in my life" "0.917" 11 1 12 11 SCORED-NORMAL PRIORITY12)           T 





;;xxx SIMPLIER TESTS ---------------------------------------------------
;; normal key-spec-lists
;; (progn (setf out nil)(get-set-append-keyvalue-in-nested-list  'NEW-VALUE '((:key2 0)(:key3 0)(:key4 0)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  (:KEY2 (0.55) (:KEY3 (44) (:KEY4 0.95))) :KEY5 (xxx)) :append-keyvalue-p t))
;;works= (:KEY4 (0.95 NEW-VALUE))          ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) (:KEY2 (0.55) (:KEY3 (44) (:KEY4 (0.95 NEW-VALUE)))) :KEY5 (XXX))               (:KEY4 (0.95 NEW-VALUE))        (0.95 NEW-VALUE)        (:KEY3 (44) (:KEY4 0.95))           T
;; (progn (setf out nil)(get-set-append-keyvalue-in-nested-list  'NEW-VALUE '((:key3 0)(:key4 0)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  :KEY2 (0.55) (:KEY3 (44) (:KEY4 0.95)) :KEY5 (xxx)) :append-keyvalue-p t))
;; works= (:KEY4 (0.95 NEW-VALUE))    ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) :KEY2 (0.55) (:KEY3 (44) :KEY4 (0.95 NEW-VALUE)) :KEY5 (XXX))    (:KEY4 (0.95 NEW-VALUE))    (0.95 NEW-VALUE)   (:KEY4 0.95)   T
;; set with return-list-p
;;  (progn (setf out nil)(get-set-append-keyvalue-in-nested-list  'NEW-VALUE '((:key3 0)(:key4 0)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  :KEY2 (0.55) (:KEY3 (44) (:KEY4 0.95)) :KEY5 (xxx)) :return-list-p T))
;; works= (:KEY3 (44) :KEY4 NEW-VALUE)    ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) :KEY2 (0.55) (:KEY3 (44) :KEY4 NEW-VALUE) :KEY5 (XXX))    (:KEY4 NEW-VALUE)    NEW-VALUE    (:KEY4 0.95)    T
;; keyloc-n > 0
;; (progn (setf out nil)(get-set-append-keyvalue-in-nested-list  'NEW-VALUE '((:key3 2)(:key4 4)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  :KEY2 (0.55) (A  B :KEY3 (44) (0 1 2 3 :KEY4 0.95)) :KEY5 (xxx)) :return-list-p T )) ;; :recurse-for-key-p T))
;;works= (A B :KEY3 (44) (0 1 2 3 :KEY4 NEW-VALUE))   ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) :KEY2 (0.55) (A B :KEY3 (44) (0 1 2 3 :KEY4 NEW-VALUE)) :KEY5 (XXX))    (:KEY4 NEW-VALUE)   NEW-VALUE   (:KEY4 0.95)   T
;; keyloc-n = T, keys not nested
;; IF KEY4 NOT NESTED IN SAME LIST AS KEY3, then must be specified in key3's VAL-NTH and include :NEXT-KEY-IN-VALUE as 4th key-spec item.
;;  (progn (setf out nil)(get-set-append-keyvalue-in-nested-list  'NEW-VALUE '((:KEY3 T 2 :next-key-in-value)(:key4 0)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95) :key5 (xxx)) :append-keyvalue-p NIL))
;;works= (:KEY4 NEW-VALUE)    ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) (:KEY2 (0.55)) ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) (:KEY2 (0.55)) :KEY3 (44) (:KEY4 NEW-VALUE) :KEY5 (XXX)) (44) (:KEY4 0.95) :KEY5 (XXX))    (:KEY4 NEW-VALUE)     NEW-VALUE    (:KEY4 0.95)     T
;; keyloc-n = T, keys nested, set/not append.
;;  (progn (setf out nil)(get-set-append-keyvalue-in-nested-list  'NEW-VALUE '((:KEY3 T)(:key4 0)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  (:KEY2 (0.55)) ( :KEY3 (44) (:KEY4 0.95) :key6 'this) :key5 (xxx)) :append-keyvalue-p NIL))
;;works=  (:KEY4 NEW-VALUE)    ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) (:KEY2 (0.55)) (:KEY3 (44) (:KEY4 NEW-VALUE) :KEY6 (QUOTE THIS)) :KEY5 (XXX))    (:KEY4 NEW-VALUE)    NEW-VALUE   (:KEY4 0.95)   T
;;   :return-list-p :append-keyvalue-p with above  keyloc-n = T, keys nested
;; (progn (setf out nil)(get-set-append-keyvalue-in-nested-list  'NEW-VALUE '((:KEY3 T)(:key4 0)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  (:KEY2 (0.55)) ( :KEY3 (44) (:KEY4 0.95) :key6 'this) :key5 (xxx)) :append-keyvalue-p T :return-list-p T))
;;works= (:KEY3 (44) (:KEY4 (0.95 NEW-VALUE)) :KEY6 (QUOTE THIS))     ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) (:KEY2 (0.55)) (:KEY3 (44) (:KEY4 (0.95 NEW-VALUE)) :KEY6 (QUOTE THIS)) :KEY5 (XXX))      (:KEY4 (0.95 NEW-VALUE))     (0.95 NEW-VALUE)    (:KEY4 0.95)   T

;; If KEY = T, keyloc-n = 0; key-spec = (T 0)
;; (progn (setf out nil)(get-set-append-keyvalue-in-nested-list  'NEW-VALUE '((T 0)(:key4 0)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) ( (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95)) :key5 (xxx)) :append-keyvalue-p NIL))
;;works= (:KEY4 NEW-VALUE)   ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) ((:KEY2 (0.55)) :KEY3 (44) (:KEY4 NEW-VALUE)) :KEY5 (XXX))   (:KEY4 NEW-VALUE)   NEW-VALUE   (:KEY4 0.95)   T
;; :GET
;; (progn (setf out nil)(get-set-append-keyvalue-in-nested-list  :GET '((:KEY3 t)(:key4 0)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95) :key5 (xxx)) :append-keyvalue-p NIL))
;;works= (:KEY4 0.95)   ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95) :KEY5 (XXX))  (:KEY4 0.95)  0.95  (:KEY4 0.95)  T
;; :get with return-list-p
;; (progn (setf out nil)(get-set-append-keyvalue-in-nested-list  :GET '((:KEY3 t)(:key4 0)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95) :key5 (xxx)) :append-keyvalue-p NIL :return-list-p T))
;;works= ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95) :KEY5 (XXX))   ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95) :KEY5 (XXX))   (:KEY4 0.95)   0.95  (:KEY4 0.95)   T

;; if key not found, append
;;  (progn (setf out nil)(get-set-append-keyvalue-in-nested-list  'NEW-VALUE '((:key2 0) (:KEY7  0)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95) :key5 (xxx)) :append-keyvalue-p NIL))
;;work=  (:KEY7 NEW-VALUE)   ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95) :KEY5 (XXX) (:KEY7 NEW-VALUE))    (:KEY7 NEW-VALUE)    NEW-VALUE    NIL   NIL
;;
;;(progn (setf out nil)(get-set-append-keyvalue-in-nested-list  'NEW-VALUE '((:key2 0) (:KEY7  0)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95) :key5 (xxx)) :append-keyvalue-p NIL :if-not-found-append-keyvalue-list-p NIL))
;;works= (:KEY7 NEW-VALUE)    ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95) :KEY5 (XXX) :KEY7 NEW-VALUE)   (:KEY7 NEW-VALUE)   NEW-VALUE   NIL   NIL

;; (progn (setf out nil)(get-set-append-keyvalue-in-nested-list  'NEW-VALUE '((:key3 T)(:key4 0)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  :KEY2 (0.55) (:KEY3 (44) (:KEY4 A)) :KEY5 (xxx)) :append-keyvalue-p t))
;;works=  (:KEY4 (A NEW-VALUE))     ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) :KEY2 (0.55) (:KEY3 (44) (:KEY4 (A NEW-VALUE))) :KEY5 (XXX))     (:KEY4 (A NEW-VALUE))     (A NEW-VALUE)    (:KEY4 A)    T
;;with items  after value, key3 = T
;;(progn (setf out nil)(get-set-append-keyvalue-in-nested-list  'NEW-VALUE '((:key3 T)(:key4 0)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  :KEY2 (0.55) (:KEY3 (44) (:KEY4 A B C D E)) :KEY5 (xxx)) :append-keyvalue-p t)))
;;works= (:KEY4 (A NEW-VALUE) B C D E)     ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) :KEY2 (0.55) (:KEY3 (44) (:KEY4 (A NEW-VALUE) B C D E)) :KEY5 (XXX))             (:KEY4 (A NEW-VALUE) B C D E)            (A NEW-VALUE)         (:KEY3 (44) (:KEY4 A B C D E))       T
;;nested keys
;;  (progn (setf out nil)(get-set-append-keyvalue-in-nested-list  'NEW-VALUE '((:key3 T )(:key4 1)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  (:KEY2 (0.55)) :KEY3 (44 (:KEY4 0.95))  :KEY5 (xxx)) :append-keyvalue-p t))
;;works= (:KEY4 (0.95 NEW-VALUE))   ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) (:KEY2 (0.55)) :KEY3 (44 (:KEY4 (0.95 NEW-VALUE))) :KEY5 (XXX))   (:KEY4 (0.95 NEW-VALUE))    (0.95 NEW-VALUE)   (:KEY4 0.95)   T
;;no append/replace old-value
;; (progn (setf out nil)(get-set-append-keyvalue-in-nested-list  'NEW-VALUE '((:key3 T 2)(:key4 0)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95) :key5 (xxx)) :append-keyvalue-p NIL))
;;works=(:KEY4 NEW-VALUE)   ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) (:KEY2 (0.55)) :KEY3 (44) (:KEY4 NEW-VALUE) :KEY5 (XXX))  (:KEY4 NEW-VALUE)   NEW-VALUE   (:KEY4 0.95)  T
;;If key = T
;; (progn (setf out nil)(get-set-append-keyvalue-in-nested-list  'NEW-VALUE '((T 0)(:key4 0)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95) :key5 (xxx)) :append-keyvalue-p NIL))

;;ORIG TESTS
;; append
;;(progn (setf out nil)(get-set-append-keyvalue-in-nested-list  'NEW-VALUE '((:key3 T 2)(:key4 0)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95) :KEY5 (xxx)) :append-keyvalue-p t))
;; works= (:KEY4 (0.95 NEW-VALUE))     ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) (:KEY2 (0.55)) :KEY3 (44) (:KEY4 (0.95 NEW-VALUE)) :KEY5 (XXX))    (:KEY4 (0.95 NEW-VALUE))    (0.95 NEW-VALUE)   (:KEY4 0.95)   T
;;no append/replace old-value
;; (progn (setf out nil)(get-set-append-keyvalue-in-nested-list  'NEW-VALUE '((:key3 T 2)(:key4 0)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95) :key5 (xxx)) :append-keyvalue-p NIL))
;;works=(:KEY4 NEW-VALUE)   ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) (:KEY2 (0.55)) :KEY3 (44) (:KEY4 NEW-VALUE) :KEY5 (XXX))  (:KEY4 NEW-VALUE)   NEW-VALUE   (:KEY4 0.95)  T
;;If key = T
;; (progn (setf out nil)(get-set-append-keyvalue-in-nested-list  'NEW-VALUE '((T 0)(:key4 0)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95) :key5 (xxx)) :append-keyvalue-p NIL))
;;works=  (:KEY4 NEW-VALUE)        ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) (:KEY2 (0.55)) :KEY3 (44) (:KEY4 NEW-VALUE) :KEY5 (XXX))          (:KEY4 NEW-VALUE)                NEW-VALUE          ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95) :KEY5 (XXX))           T
;;return-list-p NIL
;; (progn (setf out nil)(get-set-append-keyvalue-in-nested-list  'NEW-VALUE '((:key3 0)(:key4 0)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  :KEY2 (0.55) (:KEY3 (44) (:KEY4 0.95)) :KEY5 (xxx)) :append-keyvalue-p t) :return-list-p T)
;;works= (:KEY4 (0.95 NEW-VALUE))       ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) :KEY2 (0.55) (:KEY3 (44) (:KEY4 (0.95 NEW-VALUE))) :KEY5 (XXX))        (:KEY4 (0.95 NEW-VALUE))       (0.95 NEW-VALUE)     (:KEY3 (44) (:KEY4 0.95))     T
;;return-list-p T
;; (progn (setf out nil)(get-set-append-keyvalue-in-nested-list  'NEW-VALUE '((:key3 0)(:key4 0)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  :KEY2 (0.55) (:KEY3 (44) (:KEY4 0.95)) :KEY5 (xxx)) :append-keyvalue-p t :return-list-p T))
;;works= (:KEY3 (44) (:KEY4 (0.95 NEW-VALUE)))          ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) :KEY2 (0.55) (:KEY3 (44) (:KEY4 (0.95 NEW-VALUE))) :KEY5 (XXX))     (:KEY4 (0.95 NEW-VALUE))     (0.95 NEW-VALUE)    (:KEY3 (44) (:KEY4 0.95))    T
;; ;;return-list-p T with added
;;  (progn (setf out nil)(get-set-append-keyvalue-in-nested-list  'NEW-VALUE '((:key3 2)(:key4  3 3)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  :KEY2 (0.55) (pre3a pre3b :KEY3 (44) (pre4a pre4b pre4c :KEY4 mid1 mid2 0.95 end1 end2) post3) :KEY5 (xxx)) :append-keyvalue-p t :return-list-p T))
;; works= (PRE3A PRE3B :KEY3 (44) (PRE4A PRE4B PRE4C :KEY4 MID1 MID2 0.95 NEW-VALUE END1 END2) POST3)             ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) :KEY2 (0.55) (PRE3A PRE3B :KEY3 (44) (PRE4A PRE4B PRE4C :KEY4 MID1 MID2 0.95 NEW-VALUE END1 END2) POST3) :KEY5 (XXX))                    (PRE4A PRE4B PRE4C :KEY4 MID1 MID2 0.95 NEW-VALUE END1 END2)              (0.95 NEW-VALUE)                   (PRE3A PRE3B :KEY3 (44) (PRE4A PRE4B PRE4C :KEY4 MID1 MID2 0.95 END1 END2) POST3)              T

;;  keyloc-n = T, val-nth 2, where next key is in the top list in value slot
;;  (progn (setf out nil)(get-set-append-keyvalue-in-nested-list  'NEW-VALUE '((:key3 T 2)(:key4 0)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95 a b (c) d) :key5 (xxx)) :return-list-p NIL :append-keyvalue-p NIL))
;;works=  (:KEY4 NEW-VALUE A B (C) D)   ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) (:KEY2 (0.55)) :KEY3 (44) (:KEY4 NEW-VALUE A B (C) D) :KEY5 (XXX))   (:KEY4 NEW-VALUE)   NEW-VALUE   (:KEY4 0.95)   T
;; when return-list-p returns whole nested-list with new value
;;  (progn (setf out nil)(get-set-append-keyvalue-in-nested-list  'NEW-VALUE '((:key3 T 2)(:key4 0)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95 a b (c) d) :key5 (xxx)) :return-list-p T :append-keyvalue-p NIL))
;;works= ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) (:KEY2 (0.55)) :KEY3 (44) (:KEY4 NEW-VALUE A B (C) D) :KEY5 (XXX))                        ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) (:KEY2 (0.55)) :KEY3 (44) (:KEY4 NEW-VALUE A B (C) D) :KEY5 (XXX))                 (:KEY4 NEW-VALUE A B (C) D)                 NEW-VALUE    ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95 A B (C) D) :KEY5 (XXX))       T
;;
;;TRY SETTING VAL-NTH WITH :NTH
;; (progn (setf out nil)(get-set-append-keyvalue-in-nested-list  'NEW-VALUE '((:key3 T 2 :NEXT-KEY-IN-VALUE)(:NTH  2)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95 a b (c) d) :key5 (xxx)) :return-list-p NIL :append-keyvalue-p NIL))
;; NOTE: MUST USE :RETURN-LIST-P TO GET FULL NEW KEYLIST W/END
;;works= (:KEY4 0.95 NEW-VALUE)  ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95 NEW-VALUE B (C) D) :KEY5 (XXX))    (NEW-VALUE)    NEW-VALUE   (:KEY4 0.95 A B (C) D)    T
;;works= (:KEY4 0.95 NEW-VALUE)              ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95 NEW-VALUE B (C) D) :KEY5 (XXX))           (NEW-VALUE)            NEW-VALUE   (:KEY4 0.95 A B (C) D)   T
;;
;; NTH with :RETURN-LIST-P
;;   (progn (setf out nil)(get-set-append-keyvalue-in-nested-list  'NEW-VALUE '((:key3 T 2 :NEXT-KEY-IN-VALUE)(:NTH  2)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95 a b (c) d) :key5 (xxx)) :RETURN-LIST-P t :append-keyvalue-p NIL))
;;works= (:KEY4 0.95 NEW-VALUE B (C) D)             ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95 NEW-VALUE B (C) D) :KEY5 (XXX))            (NEW-VALUE)     NEW-VALUE       (:KEY4 0.95 A B (C) D)    T

;;GET
;; (progn (setf out nil)(get-set-append-keyvalue-in-nested-list  :GET '((:key3 T 2)(:key4 0)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95) :key5 (xxx)) :append-keyvalue-p NIL))
;; works= (:KEY4 0.95)   ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95) :KEY5 (XXX))    (:KEY4 0.95)    0.95   (:KEY4 0.95)  T

;; IF NO LAST KEY FOUND AND IF-NOT-FOUND-APPEND-KEY-VALUE-P
;; (progn (setf out nil)(get-set-append-keyvalue-in-nested-list  'NEW-VALUE '((:key3 T 2)(:keyX 0)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95) :KEY5 (xxx)) :append-keyvalue-p t  :if-not-found-append-key-value-p T))
;;works=  (:KEYX NEW-VALUE)      ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95) :KEY5 (XXX) (:KEYX NEW-VALUE))          (:KEYX NEW-VALUE)           NEW-VALUE           NIL         NIL
;; NO KEY FOUND FOR :GET
;; (progn (setf out nil)(get-set-append-keyvalue-in-nested-list  :GET '((:key3 T 2)(:keyX 0)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95) :KEY5 (xxx)) :append-keyvalue-p t  :if-not-found-append-key-value-p T))
;; works (:keyx not in list)=  NIL  ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) (:KEY2 (0.55)) :KEY3 (44) NIL :KEY5 (XXX))   NIL   NIL   NIL  NIL
;;
;;  
;; 2 equal higher-keys A3, must get first key :PC-VALUES right first
;; Also, double-list with NO key ((A3....)(A4 ...))
;;  (progn (setf out nil) (get-set-append-keyvalue-in-nested-list 'NEW-VALUE  '((:PC-VALUES T)(A3 T)) '(:PCSYM-ELM-LISTS ((A3 (MOTHER FATHER BEST-M-FRIEND)) (B3 (MOTHER FATHER BEST-F-FRIEND)) (C5 (MOTHER BEST-M-FRIEND BEST-F-FRIEND)) (D1 (FATHER BEST-M-FRIEND BEST-F-FRIEND))) :PC-VALUES ((A3 "a vs not a" :SINGLE "One of the most important things in my life" "0.917" 11 1 12 11 SCORED-NORMAL PRIORITY12)(A4 "this is A4") x y) (list z))))
;;works= (A3 NEW-VALUE)      (:PCSYM-ELM-LISTS ((A3 (MOTHER FATHER BEST-M-FRIEND)) (B3 (MOTHER FATHER BEST-F-FRIEND)) (C5 (MOTHER BEST-M-FRIEND BEST-F-FRIEND)) (D1 (FATHER BEST-M-FRIEND BEST-F-FRIEND))) :PC-VALUES ((A3 NEW-VALUE :SINGLE "One of the most important things in my life" "0.917" 11 1 12 11 SCORED-NORMAL PRIORITY12) (A4 "this is A4") X Y) (LIST Z))      (A3 NEW-VALUE)   NEW-VALUE    (A3 "a vs not a")   T




;;xxx
;;FOR TEST ON CSQ-RELATED SEARCHES ==================
;; (get-keyvalue-in-nested-list  (list (list T  0) (list 'mother  0)) (eval *pce-test-list) :return-list-p T)

;;WORKS
;;SEARCH ALL LISTS--time consuming BUT WORK & SIMPLE SPEC-LIST
;; (progn (setf out nil)(get-set-append-keyvalue-in-nested-list  :get  (list '((PCE-PEOPLE  (list 'motherQ  T))  *pce-test-list :return-list-p T :append-keyvalue-p t :RECURSE-FOR-KEY-P T)) 
;;works= (MOTHERQ ("Your Mother [or person most like a mother to you]") PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS)   [REST OF VALUES WORK TOO]
;;
;;FOR APPEND NEW-VALUE
;; (progn (setf out nil)(get-set-append-keyvalue-in-nested-list  'NEW-VALUE (list  (list 'motherQ  T))  *pce-test-list :return-list-p T :append-keyvalue-p t :RECURSE-FOR-KEY-P T))
;;works= 
#|(MOTHERQ ("Your Mother [or person most like a mother to you]" NEW-VALUE) PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS)
((PCE-PEOPLE (PCE-PEOPLE-INSTR ("People Important To You" *INSTR-NAME-ELEMENT)) (MOTHERQ ("Your Mother [or person most like a mother to you]" NEW-VALUE) PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS)   ETC )
(MOTHERQ ("Your Mother [or person most like a mother to you]" NEW-VALUE))
("Your Mother [or person most like a mother to you]" NEW-VALUE)
(MOTHERQ ("Your Mother [or person most like a mother to you]") PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS)   T|#
;; VAL-NTH 3
;;  (progn (setf out nil)(get-set-append-keyvalue-in-nested-list  'NEW-VALUE (list  (list 'motherQ  T 3))  *pce-test-list :return-list-p T :append-keyvalue-p t :RECURSE-FOR-KEY-P T))
#|works=  (MOTHERQ ("Your Mother [or person most like a mother to you]") PCE-PEOPLE-INSTR (*INPUT-BOX-INSTRS NEW-VALUE))
((PCE-PEOPLE (PCE-PEOPLE-INSTR ("People Important To You" *INSTR-NAME-ELEMENT)) (MOTHERQ ("Your Mother [or person most like a mother to you]") PCE-PEOPLE-INSTR (*INPUT-BOX-INSTRS NEW-VALUE))  ETC )
(MOTHERQ (*INPUT-BOX-INSTRS NEW-VALUE))
(*INPUT-BOX-INSTRS NEW-VALUE)
(MOTHERQ ("Your Mother [or person most like a mother to you]") PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS)   T|#
;; (progn (setf out nil)(get-set-append-keyvalue-in-nested-list  'NEW-VALUE (list  (list 'motherQ  0))  *pce-test-list :return-list-p T :append-keyvalue-p t :RECURSE-FOR-KEY-P T)) = DOESN'T WORK


;;  (setf  *pce-test-list ' ((PCE-PEOPLE (PCE-PEOPLE-INSTR ("People Important To You" *INSTR-NAME-ELEMENT)) (MOTHERQ ("Your Mother [or person most like a mother to you]") PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS) (FATHERQ ("Your Father [or person most like a father to you]") PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS) (BEST-M-FRIENDQ ("A best male friend") PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS) (BEST-F-FRIENDQ ("A best female friend") PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS)  (DIS-TEACHERQ ("A teacher you disliked the most") PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS))                       (PCE-GROUPS (PCE-GROUPS-INSTR ("Significant Groups" *INSTR-NAME-ELEMENT)) (TEACHERQ ("Teachers") PCE-GROUPS-INSTR *INPUT-BOX-INSTRS) (POLICEMANQ ("Policepersons") PCE-GROUPS-INSTR *INPUT-BOX-INSTRS)  PCE-GROUPS-INSTR *INPUT-BOX-INSTRS)                        (PCE-SELF (PCE-SELF-INSTR ("Important Self-Related" *INSTR-NAME-ELEMENT)) (MOST-IMPORTANT-VALUEQ ("Your most important goal or value") PCE-SELF-INSTR *INPUT-BOX-INSTRS) (MOST-IMPORTANT-ABILITYQ ("Your most important ability") PCE-SELF-INSTR *INPUT-BOX-INSTRS)  (YOUR-WORST-CHARACTERISTICQ ("Your worst characteristic") PCE-SELF-INSTR *INPUT-BOX-INSTRS PCE-SELF-INSTR *INPUT-BOX-INSTRS))))

;;  :return-list-p T
;; (progn (setf out nil)(get-set-append-keyvalue-in-nested-list  :get (list (list 'PCE-PEOPLE 0) (list 'motherQ  T))  *pce-test-list :return-list-p T :append-keyvalue-p t :RECURSE-FOR-KEY-P T))
;; works=  note: :return-list-p T
#|(PCE-PEOPLE (PCE-PEOPLE-INSTR ("People Important To You" *INSTR-NAME-ELEMENT)) (MOTHERQ ("Your Mother [or person most like a mother to you]") PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS) (FATHERQ ("Your Father [or person most like a father to you]") PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS) (BEST-M-FRIENDQ ("A best male friend") PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS) (BEST-F-FRIENDQ ("A best female friend") PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS) (DIS-TEACHERQ ("A teacher you disliked the most") PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS)) 
;;  :return-list-p NIL
;; (get-set-append-keyvalue-in-nested-list :get  (list (list 'pce-people 0 :R)                                              (list 'motherq  0 :R))      (eval  *cur-all-questions))  :return-list-p T)
;;works= MOTHERQ ("Your Mother [or person most like a mother to you]") PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS)                    ("return-orig-nested-list-p=NIL")              (MOTHERQ ("Your Mother [or person most like a mother to you]") PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS)             ("Your Mother [or person most like a mother to you]")                (PCE-PEOPLE (PCE-PEOPLE-INSTR ("People Important To You" *INSTR-NAME-ELEMENT)) (MOTHERQ ("Your Mother [or person most like a mother to you]") PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS) (FATHERQ ("Your Father [or person most like a father to you]") PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS) (BEST-M-FRIENDQ ("A best male friend") PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS)  etc   ....   etc   (FAV-SPIRITUALQ ("A favorite spiritual or inspirational person") PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS) (DIS-TEACHERQ ("A teacher you disliked the most") PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS))    T
;;
;;NOTE; return-list-p = T  not needed, returns too much
;; (progn (setf out nil) (get-set-append-keyvalue-in-nested-list :get  (list (list T 0) (list 'motherq  0 :R))      (eval  *cur-all-questions)))
;;works= (MOTHERQ ("Your Mother [or person most like a mother to you]") PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS)                ("return-orig-nested-list-p=NIL")                    (MOTHERQ ("Your Mother [or person most like a mother to you]") PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS)               ("Your Mother [or person most like a mother to you]")     (PCE-PEOPLE (PCE-PEOPLE-INSTR ("People Important To You" *INSTR-NAME-ELEMENT)) (MOTHERQ ("Your Mother [or person most like a mother to you]") PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS) (FATHERQ ("Your Father [or person most like a father to you]") PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS)  ETC  ETC    (FAV-TEACHERQ ("A favorite teacher") PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS) (FAV-SPIRITUALQ ("A favorite spiritual or inspirational person") PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS) (DIS-TEACHERQ ("A teacher you disliked the most") PCE-PEOPLE-INSTR *INPUT-BOX-INSTRS))       T



;;THESE WON'T WORK
;;  Reason: For :next-key-in-value to work must be a LIST of lists or keys in val-nth place--only looks in that one place.
;; (progn (setf out nil)(get-set-append-keyvalue-in-nested-list  :get (list (list 'PCE-PEOPLE 0 1  :next-key-in-value) (list 'motherQ  T))  *pce-test-list :return-list-p T :append-keyvalue-p t :RECURSE-FOR-KEY-P T))





























;;GET-SET-APPEND-KEYVALUE
;; Key function for get, set, or appending a list in almost any way. Used as final link to modify nested keys in nested-lists of any depth.
;;2016
;;ddd
(defun get-set-append-keyvalue (key old-value new-value
                                       &key (keyloc-n 0) (val-nth 1)
                                       old-keylist
                                       append-value-p
                                       add-value-p
                                       put-key-after-items
                                       put-value-after-items
                                       begin-items
                                       end-items
                                       splice-key-value-in-list
                                       (put-splice-keylist-p T))
  "In U-lists, if new-value = :get, just returns old-value. Otherwise replaces or appends old value.  If listp old- value, appends list; if not sets value to (list old-value new-value). RETURNS (values return-keylist new-keylist return-value). When KEYLOC-N > 0, If  PUT-KEY-AFTER-ITEMS is a list, puts put-key-after-items  before key. If a single object, puts keyloc-n objects key in keylist. If SPLICE-KEY-VALUE-IN-LIST, splices key and new-value at keyloc-n in that list which becomes the new-keylist. If PUT-SPLICE-KEYLIST-P, then puts/splices key-value LIST, if nil, appends key, value. RETURN-KEYLIST is the list possibly modified by outside items. The NEW-KEYLIST is the innermost keylist.
   If splice-key-value-in-list length < prev-valloc-n, then fills places with  put-key-after-items object to make new-keylist. If KEY = :NTH (keyloc-n becomes the number for nth and val-nth=0, then just ignores all keys and operates on old-value--otherwise like above. NOTE: Used by nesting functions to get, set, or append values in FINAL NESTED LIST. If want filled with NIL, put :NIL in put-... ADD-VALUE-P adds new-value after old-value without being in a list. APPEND-VALUE-P puts new-value in a list with old-value (even if old-value wasn't a list). [Note: IF OLD-KEYLIST, the don't need key or old-value.]  USE OLD-KEYLIST WITH NO-KEY."
  (let
      ((new-keylist)
       (return-keylist)
       (return-value)
       (splice-list-n)
       (put-n 0)
       (add-n 0)
       (len-old-keylist) 
       (n-to-val) 
       )
    ;;PRE-PROCESSING
    ;;WHEN :NTH
    (when (equal key :NTH)
      (setf val-nth 0))

    (when (and (numberp keyloc-n)(numberp val-nth))
      (setf  n-to-val  (+ keyloc-n val-nth)))

    ;;WHEN OLD-KEYLIST PROVIDED (overrides begin-items put-value-after-items end-items   
    (cond
     (old-keylist 
      (setf len-old-keylist (list-length old-keylist))    
      (when  (> keyloc-n 0)
        (setf begin-items (subseq old-keylist 0 keyloc-n)))
      (when  (> val-nth 1)
        (setf put-value-after-items (subseq old-keylist (+ keyloc-n 1) n-to-val)))
      (when (> len-old-keylist n-to-val)
        (setf end-items (subseq old-keylist (+ n-to-val 1) len-old-keylist)))
      (when (null old-value) 
        (setf old-value (nth  (+ keyloc-n val-nth) old-keylist)))
        )
     (t
      (cond
       (put-value-after-items
        (setf  old-keylist (list key put-value-after-items old-value))
        (when  (equal key :NTH)
          (setf begin-items (append begin-items put-value-after-items))))
       (t      
        (setf old-keylist (list key old-value))))))   

    ;;STEP 1: MAKE NEW-KEYLIST
    (cond
     ;;FOR GET
     ((equal new-value :get)
        (setf  new-keylist old-keylist)
       (when (equal key :NTH)
          (setf  new-keylist (list old-value)))
      ;;also
      (setf return-value  old-value)
      ;;end get
      )
     ;;FOR APPEND-VALUE-P
     (append-value-p  ;;was (and append-value-p (not (equal key :NTH)))
      (cond
       ((listp old-value)
        (cond
         (put-value-after-items
          (cond
           ((equal key :NTH)
            (setf  new-keylist (append  put-value-after-items
                                          (append old-value  (list new-value)))))
           (put-splice-keylist-p
            (setf  new-keylist (append  (list key)  put-value-after-items
                                        (list (append old-value  (list new-value))))))
           (t (setf  new-keylist (append  (list key)  put-value-after-items
                                          (append old-value  (list new-value))))))
          ;;(break "put-splice-keylist-p, LIST")
          ;;end put-value-after-items
          )
         (t (setf new-keylist (list key (append  old-value (list new-value))))))
        ;;in any case
        (setf return-value (append old-value (list new-value))))
       ;;OLD VALUE NOT A LIST
       (t
        (cond
         (put-value-after-items
          (cond
           ((equal key :NTH)
            (setf  new-keylist (append  put-value-after-items
                                          (list old-value new-value))))
           (put-splice-keylist-p
            (setf  new-keylist (append (list key)  put-value-after-items
                                       (list (list old-value  new-value)))))
           (t (setf  new-keylist (append (list key)  put-value-after-items
                                         (list old-value  new-value)))))
          ;;(break "put-splice-keylist-p, NOT-LIST")
          (setf return-value (list old-value  new-value)))
         (t (setf new-keylist (list key  (list old-value  new-value))
                  return-value  (list old-value  new-value))))))
  
      ;;later??(when begin-items (setf new-keylist (append begin-items new-keylist)))
      ;;end append-value-p
      )
     ;;FOR ADD-VALUE-P
     (add-value-p 
      (cond
       ((equal key :NTH)
        (setf new-keylist (list old-value new-value)
               return-value (list  old-value  new-value))
        (when put-value-after-items
          (setf new-keylist (append put-value-after-items new-keylist))))
       (put-value-after-items
        (setf  new-keylist (append  (list key)  put-value-after-items
                                    (list old-value)(list new-value))
               return-value (list old-value  new-value)))
       (t (setf new-keylist (list key old-value  new-value)
                return-value (list  old-value  new-value))))

      ;;later? (when begin-items (setf new-keylist (append begin-items new-keylist)))
      ;;end add-value-p
      )
     ;;FOR REPLACE OLD VALUE
     (t
      (cond
       ((equal key :NTH)
        (setf  new-keylist (list new-value)))
       (t
        (cond
         (put-value-after-items
          (cond
           ((not (equal key :NTH))
            (setf  new-keylist
                   (append (list key) put-value-after-items (list  new-value))))
           (t (setf  new-keylist
                 (append  put-value-after-items (list  new-value))))))
         (t (setf new-keylist (list key new-value))))
        ;;later? (when begin-items (setf new-keylist (append begin-items new-keylist)))
        ))
      ;;in any case of  T
      (setf return-value  new-value)
      ))

    ;;STEP 2: FINAL CHANGES TO KEYLIST?
    ;;was (unless (equal key :NTH)
      (when begin-items
        (setf new-keylist (append begin-items new-keylist)))
      (when end-items
        (setf  new-keylist (append new-keylist  end-items))) 

  
    ;;STEP 3: INSERT NEW-KEYLIST IN A LIST?
    (cond
     (splice-key-value-in-list 
      ;;is splice-list-n > keyloc-n?
      (cond
       ((>=  (setf splice-list-n (list-length splice-key-value-in-list)) keyloc-n)
        NIL )
       ;;if not, must modify new-keylist by putting in new elments 
       (t (setf return-keylist (append (make-list (- keyloc-n splice-list-n)  :initial-element put-key-after-items)  new-keylist))))

      ;;In either case, make new-keylist
      (cond
       (put-splice-keylist-p
        (setf return-keylist (append-nth (list new-keylist) keyloc-n splice-key-value-in-list :splice-list-p T)))
       (t  
        (setf  return-keylist (append-nth new-keylist keyloc-n splice-key-value-in-list :splice-list-p T))))       
      ;;end splice-key-value-in-list clause
      )

     ;;IF PUT-KEY-AFTER-ITEMS and NULL SPLICE-KEY-VALUE-IN-LIST
     (put-key-after-items
      (when (> keyloc-n 0)
        ;;if not a list, use put-key-after-items to fill a new list; IF :NIL fills with NIL
        (cond
         ((not (listp put-key-after-items))
          (cond
           ((equal put-key-after-items :NIL)
            (setf put-key-after-items (make-list keyloc-n :initial-element  NIL)))
           (t (make-list keyloc-n :initial-element  put-key-after-items))))
         ((listp put-key-after-items)
          ;;keyloc-n is num of items needed to fill in keyloc-n > 0
          ;;put-n is number of  items put-key-after-items is short
          (setf put-n (list-length put-key-after-items)
                add-n (- keyloc-n put-n))
           
          ;;cccc
          (cond
           ;; if keyloc-n = put-n, use entire list
           ((= add-n 0) NIL)
           ;;if put-n is negative, must only use part of list
           ((< add-n 0)
            (setf put-key-after-items (butlast put-key-after-items (abs add-n))))
           ;;keyloc-n > list length, must add elements
           ((> put-n 0)
            (setf put-key-after-items (append put-key-after-items 
                                              (make-list add-n :initial-element  NIL))))))
         (t nil))
        ;;(BREAK "AFTER PUT")
        ;;IN ANY CASE PUT KEYLIST AFTER PUT-KEY-AFTER-ITEMS
        (when (> (list-length put-key-after-items) 0)
          (cond
           (put-splice-keylist-p
            (setf  return-keylist (append  put-key-after-items  (list new-keylist))))
           (t (setf  return-keylist (append  put-key-after-items  new-keylist))))
          ;;end when
          )
        ;;end when keyloc-n > 0
        )
      ;;end put-key-after-items clause, cond
      ))   
     ;;Often the return-keylist = new-keylist
     (when (null return-keylist)
       (setf return-keylist new-keylist))

    (values return-keylist new-keylist return-value)
    ;;end let, get-set-append-keyvalue
    ))
;;TEST
;;current
;;(get-set-append-keyvalue :key4 0.95 'new-value :KEYLOC-N 0 :VAL-NTH 1 :old-keylist '( :key4 .95) :append-value-p T :put-splice-keylist-p NIL )
;;;;(get-set-append-keyvalue :key4 0.95 'new-value :keyloc-n 3 :val-nth 3 :old-keylist '(PRE3A PRE3B :KEY3 (44) (PRE4A PRE4B PRE4C :KEY4 MID1 MID2 0.95 END1 END2) POST3)  :append-value-p T :put-splice-keylist-p T)
;;
;;
;;COMPLEX FORMS WHEN ADD ITEMS TO RESULT
;;  :put-splice-keylist-p NIL 
;; (get-set-append-keyvalue :key4 0.95 'new-value :KEYLOC-N 3 :VAL-NTH 4 :old-keylist '(old begin items :key4  old mid items .95  old end items) :append-value-p T :put-splice-keylist-p NIL )
;;works=  (OLD BEGIN ITEMS :KEY4 OLD MID ITEMS 0.95 NEW-VALUE OLD END ITEMS)         (OLD BEGIN ITEMS :KEY4 OLD MID ITEMS 0.95 NEW-VALUE OLD END ITEMS)           (0.95 NEW-VALUE)
;;  :put-splice-keylist-p T
;; (get-set-append-keyvalue :key4 0.95 'new-value :KEYLOC-N 3 :VAL-NTH 4 :old-keylist '(old begin items :key4  old mid items .95  old end items) :append-value-p T :put-splice-keylist-p T )
;;works= (OLD BEGIN ITEMS :KEY4 OLD MID ITEMS (0.95 NEW-VALUE) OLD END ITEMS)           (OLD BEGIN ITEMS :KEY4 OLD MID ITEMS (0.95 NEW-VALUE) OLD END ITEMS)           (0.95 NEW-VALUE)
;;
;;when keyloc-n = 0
;; (progn (setf out nil)(get-set-append-keyvalue :k2 '(old-value) 'new-value :append-value-p T :begin-items '(begin items) :end-items '(end items) :put-value-after-items '(a b c) :put-key-after-items '(put key after items) :put-splice-keylist-p T ))
;;works= (BEGIN ITEMS :K2 A B C (OLD-VALUE NEW-VALUE) END ITEMS)    (BEGIN ITEMS :K2 A B C (OLD-VALUE NEW-VALUE) END ITEMS)   (OLD-VALUE NEW-VALUE)    >>> If keyloc-n = 0, no (put key after items) included. 
;;when keyloc-n > 0
;; (progn (setf out nil)(get-set-append-keyvalue :k2 '(old-value) 'new-value :keyloc-n 3 :append-value-p T :begin-items '(begin items) :end-items '(end items) :put-value-after-items '(a b c) :put-key-after-items '(put key after items) :put-splice-keylist-p T ))
;; works=  (PUT KEY AFTER (BEGIN ITEMS :K2 A B C (OLD-VALUE NEW-VALUE) END ITEMS))    (BEGIN ITEMS :K2 A B C (OLD-VALUE NEW-VALUE) END ITEMS)   (OLD-VALUE NEW-VALUE)
;; same with  :put-splice-keylist-p = NIL
;; (progn (setf out nil)(get-set-append-keyvalue :k2 '(old-value) 'new-value :keyloc-n 3 :append-value-p T :begin-items '(begin items) :end-items '(end items) :put-value-after-items '(a b c) :put-key-after-items '(put key after items) :put-splice-keylist-p nil ))
;; works= (PUT KEY AFTER BEGIN ITEMS :K2 A B C OLD-VALUE NEW-VALUE END ITEMS)      (BEGIN ITEMS :K2 A B C OLD-VALUE NEW-VALUE END ITEMS)      (OLD-VALUE NEW-VALUE)
;; with :splice-key-value-in-list instead of :put-key-after-items, :put-splice-keylist-p T
;; (progn (setf out nil)(get-set-append-keyvalue :k2 '(old-value) 'new-value :keyloc-n 3 :append-value-p T :begin-items '(begin items) :end-items '(end items) :put-value-after-items '(a b c) :splice-key-value-in-list '(splice key value in list) :put-splice-keylist-p T ))
;;works= (SPLICE KEY VALUE IN (BEGIN ITEMS :K2 A B C (OLD-VALUE NEW-VALUE) END ITEMS) LIST)      (BEGIN ITEMS :K2 A B C (OLD-VALUE NEW-VALUE) END ITEMS)      (OLD-VALUE NEW-VALUE)
;; with :splice-key-value-in-list instead of :put-key-after-items, :put-splice-keylist-p NIL
;; (progn (setf out nil)(get-set-append-keyvalue :k2 '(old-value) 'new-value :keyloc-n 3 :append-value-p T :begin-items '(begin items) :end-items '(end items) :put-value-after-items '(a b c) :splice-key-value-in-list '(splice key value in list) :put-splice-keylist-p NIL ))
;;works=  (SPLICE KEY VALUE IN BEGIN ITEMS :K2 A B C OLD-VALUE NEW-VALUE END ITEMS LIST)   (BEGIN ITEMS :K2 A B C OLD-VALUE NEW-VALUE END ITEMS)    (OLD-VALUE NEW-VALUE)


;;SIMPLIER FORMS
;;replace old-value
;;  (get-set-append-keyvalue :k2 'old-value 'new-value)
;;works= (:K2 NEW-VALUE)  (:K2 NEW-VALUE)  NEW-VALUE
;;  (get-set-append-keyvalue :k2 '(old-value) 'new-value)
;; works= (:K2 NEW-VALUE) (:K2 NEW-VALUE)  NEW-VALUE
;;append old-value= nonlist
;; (get-set-append-keyvalue :k2 'old-value 'new-value :append-value-p T)
;; works= (:K2 (OLD-VALUE NEW-VALUE)) (:K2 (OLD-VALUE NEW-VALUE))   (OLD-VALUE NEW-VALUE)
;;append old-value=list
;;  (get-set-append-keyvalue :k2 '(old-value) 'new-value :append-value-p T)
;; works= (:K2 (OLD-VALUE NEW-VALUE)) (:K2 (OLD-VALUE NEW-VALUE))  (OLD-VALUE NEW-VALUE)
;; add-value-p
;; (get-set-append-keyvalue :k2 '(old-value) 'new-value :add-value-p T)
;; works= (:K2 (OLD-VALUE) NEW-VALUE)     (:K2 (OLD-VALUE) NEW-VALUE)     ((OLD-VALUE) NEW-VALUE)
;; :put-splice-keylist-p has no effect on :add-value-p
;; (get-set-append-keyvalue :k2 '(old-value) 'new-value :add-value-p T :put-splice-keylist-p NIL)
;; result= (:K2 (OLD-VALUE) NEW-VALUE)      (:K2 (OLD-VALUE) NEW-VALUE)    ((OLD-VALUE) NEW-VALUE)
;; put-value-after-items
;; (get-set-append-keyvalue :k2 '(old-value) 'new-value :append-value-p T :put-value-after-items '(a b c) )
;; works= (:K2 A B C (OLD-VALUE NEW-VALUE))    (:K2 A B C (OLD-VALUE NEW-VALUE))    (OLD-VALUE NEW-VALUE)
;; ::put-value-after-items WITH :put-splice-keylist-p NIL, 
;;(get-set-append-keyvalue :k2 '(old-value) 'new-value :append-value-p T :put-value-after-items '(a b c) :put-splice-keylist-p NIL )
;; works= (:K2 A B C OLD-VALUE NEW-VALUE)    (:K2 A B C OLD-VALUE NEW-VALUE)     (OLD-VALUE NEW-VALUE)
;;
;; (get-set-append-keyvalue :k2 '(old-value) 'new-value :append-value-p T :put-value-after-items '(a b c) :BEGIN-ITEMS '(BEGIN 1 2 ) :END-ITEMS '(END X Y))
;; works=  (BEGIN 1 2 :K2 A B C (OLD-VALUE NEW-VALUE) END X Y)     (BEGIN 1 2 :K2 A B C (OLD-VALUE NEW-VALUE) END X Y)    (OLD-VALUE NEW-VALUE)
;; :get
;; (get-set-append-keyvalue :k2 '(old-value) :get)
;; works= (:K2 (OLD-VALUE))  (:K2 (OLD-VALUE)) (OLD-VALUE)
;;
;; If :put-key-after-items, with NIL
;; (get-set-append-keyvalue :k2 '(old-value) 'new-value :append-value-p T :keyloc-n 4 :put-key-after-items '(x) :put-splice-keylist-p NIL)
;; works= (X NIL NIL NIL :K2 (OLD-VALUE NEW-VALUE))   (:K2 (OLD-VALUE NEW-VALUE))   (OLD-VALUE NEW-VALUE)
;; (get-set-append-keyvalue :k2 '(old-value) 'new-value :append-value-p T :keyloc-n 4 :put-key-after-items '(x) :put-splice-keylist-p T)
;; works= (X NIL NIL NIL (:K2 (OLD-VALUE NEW-VALUE)))   (:K2 (OLD-VALUE NEW-VALUE))  (OLD-VALUE NEW-VALUE)
;; If :put-key-after-items, with list
;;   (get-set-append-keyvalue :k2 '(old-value) 'new-value :append-value-p T :keyloc-n 2 :put-key-after-items '(a b c) :put-splice-keylist-p t)
;;  works= (A B (:K2 (OLD-VALUE NEW-VALUE)))  (:K2 (OLD-VALUE NEW-VALUE))  (OLD-VALUE NEW-VALUE)
;; (get-set-append-keyvalue :k2 '(old-value) 'new-value :append-value-p T :keyloc-n 2 :put-key-after-items '(a b c) :PUT-SPLICE-KEYLIST-P NIL)
;; works= (A B :K2 (OLD-VALUE NEW-VALUE))   (:K2 (OLD-VALUE NEW-VALUE))   (OLD-VALUE NEW-VALUE)
;;NOTE: keyloc-n = 0, so A B C shouldn't be added
;; (get-set-append-keyvalue :k2 '(old-value) 'new-value :put-key-after-items '(a b c))
;; works= (:K2 NEW-VALUE)  (:K2 NEW-VALUE) NEW-VALUE

;; if set not append
; ; (get-set-append-keyvalue :k2 '(old-value) 'new-value :keyloc-n 4 :put-key-after-items '(a b c) )
;; works= (A B C NIL (:K2 NEW-VALUE))  (:K2 NEW-VALUE)  NEW-VALUE


;; :NTH  TESTS
;;set
;; (get-set-append-keyvalue :NTH  NIL  'new-value :put-key-after-items '(a b c) :OLD-KEYLIST '(1 2 3 4 5 (6 7)  8 9 ) :KEYLOC-N 2 )
;;works= (A B (1 2 NEW-VALUE 4 5 (6 7) 8 9))      (1 2 NEW-VALUE 4 5 (6 7) 8 9)     NEW-VALUE
;; append
;; (get-set-append-keyvalue :NTH  NIL  'new-value :put-key-after-items '(a b c) :OLD-KEYLIST '(1 2 3 4 5 (6 7)  8 9 ) :KEYLOC-N 2 :APPEND-VALUE-P T)
;;works= (A B (1 2 :NTH (3 NEW-VALUE) 4 5 (6 7) 8 9))    (1 2 :NTH (3 NEW-VALUE) 4 5 (6 7) 8 9)         (3 NEW-VALUE)
;;get
;; (get-set-append-keyvalue :NTH  NIL  :get  :put-key-after-items '(a b c) :OLD-KEYLIST '(1 2 3 4 5 (6 7)  8 9 ) :KEYLOC-N 2 )
;; works= (A B (1 2 3 4 5 (6 7) 8 9))     (1 2 3 4 5 (6 7) 8 9)     3











;;GET-SET-APPEND-KEYVALUE-IN-ORDERLY-NESTED-LIST
;;2016
;;ddd
(defun get-set-append-keyvalue-in-orderly-nested-list (new-value  key-spec-lists  
                                                                  nested-lists 
                                                        &key append-keyvalue-p 
                                                        return-list-p
                                                        (max-list-length 1000)  
                                                        if-not-found-append-key-value-p
                                                        ;;cur-level-list
                                                        (keyloc-n 0)
                                                        put-key-after-items
                                                        splice-key-value-in-list
                                                        last-key-found-p 
                                                        new-keylist old-keylist return-value) 
  "In U-lists. Efficient way to get or set keyvalue for ORDERLY LISTS; but (1) MUST have exact loc of all keys keyloc-n MUST be a number.  Does not check for keys in any other location; (2) each new level MUST BE INSIDE THE VALUE PLACE OF THE PREVIOUS KEY;  (3) MUST be a key AT EVERY LEVEL; and (4) VALUE MUST be item in list following key.  RETURNS (values return-keylist return-nested-lists new-keylist return-value old-keylist last-key-found-p). If RETURN-LIST-P, return-keylist is the ENTIRE list containing key, otherwise same as new-keylist. KEY-SPEC-LISTS are lists of (key keyloc-n) from outermost to innermost (last) key. Eg of proper list (:k1 (a) :key1 (k2 (b) :key2 (k3 (c) k4 (d) :key3 (:key5 OLD-VALUE)...)); key-spec-list= ((:key1 2)(:key2  2)(:key3 4)(:key5 0)) .IF-NOT-FOUND-APPEND-KEY-VALUE-P adds a key and keyvalue to innermost list if not found (but preceding key must be found). If last keyloc-n = 0, puts old previous keyvalue at end of new keylist. If last keyloc-n > 0, puts it first, then fills in nils before new key in last list which is new value of previous key. IF KEY = :NTH, then gets, sets, or appends number in keyloc-n place. Note: If second item in spec-list isn't number, uses default keyloc-n"

  (let*
      ((return-nested-lists)
       (new-return-nested-lists)
       (match-item)
       (spec-list (car key-spec-lists))
       (KEY (first spec-list))
       (new-spec-lists)
       (new-nested-list1)
       (new-nested-lists)
       (add-new-value-p)
       (cur-level-list)
       (list-head)
       (list-tail)
       (length-nnl)
       (key-not-found-p)
       )
    ;;If second item in spec-list isn't number, use default keyloc-n
    (when (numberp (second spec-list))
      (setf  keyloc-n (second spec-list)))

    ;;(afout 'out (format nil "key-spec-lists= ~A~%nested-lists= ~A"   key-spec-lists nested-lists))

    (when (and nested-lists (listp nested-lists) (numberp keyloc-n))
      (when (setf item (nth keyloc-n nested-lists))

        (setf length-nnl (list-length nested-lists)
              list-head  (butlast nested-lists  (- length-nnl  keyloc-n))
              list-tail (nthcdr  (+ keyloc-n  2) nested-lists))
  
        ;;afout 'out (format nil "HEAD-TAIL:keyloc-n ~A length-nnl= ~A~% new-nested-lists= ~A~%list-head= ~A ~%list-tail= ~A" keyloc-n length-nnl new-nested-lists list-head    list-tail))
        
        (cond
         ((or (equal key item)(equal key :NTH))
          ;;(afout 'out (format nil "ITEM=~A = KEY=~A" item key))

          (setf new-spec-lists (cdr key-spec-lists))
          ;;(afout 'out (format nil "new-spec-lists= ~A" new-spec-lists  ))

          (cond
           (new-spec-lists
            (setf  new-nested-lists (car (nthcdr (+ keyloc-n 1) nested-lists)))
            ;;(afout 'out (format nil " new-nested-lists= ~A" new-nested-lists ))

            (multiple-value-setq (return-keylist new-return-nested-lists new-keylist
                                              return-value   old-keylist last-key-found-p )
                (get-set-append-keyvalue-in-orderly-nested-list new-value  new-spec-lists  
                                                      new-nested-lists 
                                                      :return-list-p return-list-p
                                                      :append-keyvalue-p append-keyvalue-p
                                                      :max-list-length max-list-length
                                                      :last-key-found-p   last-key-found-p
                                                      :new-keylist new-keylist
                                                      :old-keylist old-keylist
                                                      :return-value return-value))

            (setf  return-nested-lists (append  ;;return-nested-lists 
                                                list-head (list key)
                                                (list new-return-nested-lists)
                                                list-tail))

            ;;(afout 'out (format nil "After RECURSE, new-keylist= ~A~%return-value= ~A~%new-return-nested-lists= ~A~%list-head= ~A~%list-tail= ~A new-keylist= ~A~%FINAL: return-nested-lists= ~A" new-keylist  return-value new-return-nested-lists list-head list-tail new-keylist return-nested-lists))
            ;;end new-spec-lists
            )
           ;;last-key-found-p
           (t
            (setf old-value (nth (+ keyloc-n 1) nested-lists)
                  last-key-found-p T)
            (cond 
             ((not (equal key :NTH))
              (setf old-keylist (list key old-value)))
             (t (setf old-keylist (list old-value))))
            ;;(afout 'out (format nil "LAST KEY FOUND= key= ~A  old-keylist= ~A" key old-keylist))
            (multiple-value-setq (new-keylist return-value)
                (get-set-append-keyvalue key old-value new-value  :keyloc-n keyloc-n
                                            :append-value-p append-keyvalue-p
                                            :put-key-after-items put-key-after-items
                                            :splice-key-value-in-list splice-key-value-in-list))

            (setf return-nested-lists  (append list-head
                                               new-keylist
                                               list-tail))
            (when return-list-p
              (setf return-keylist return-nested-lists))
            ;;(afout 'out (format nil "AFTER LAST-KEY-FOUND: list-head= ~A~%new-keylist= ~A~%list-tail= ~A~%return-nested-lists= ~A" list-head new-keylist list-tail return-nested-lists))    
            ;;end null spec-list, cond
            ))
          ;;end key = item
          )
         ;;KEY NOT= ITEM
         ((and (= (list-length new-spec-lists) 1)  if-not-found-append-key-value-p)
          (multiple-value-setq (new-keylist return-value)
                (get-set-append-keyvalue key old-value new-value :keyloc-n keyloc-n
                                            :append-value-p append-keyvalue-p
                                            :put-key-after-items put-key-after-items
                                            :splice-key-value-in-list splice-key-value-in-list))

          ;;(BREAK "new-value if not found")
          (setf return-nested-lists  (append list-head 
                                             (list item key return-value)
                                             list-tail))
          (when return-list-p
            (setf return-keylist return-nested-lists))
          )
         (t
          ;;(BREAK "NADA")
          (setf key-not-found-p T)
          ;;end not=item, cond
          ))
        ;;end when item= list (and should be because it is in the place of the key value for less than final key)
        )
      ;;end when  (and nested-list (numberp keyloc-n))
      )
    (when (and (not (and (listp nested-lists)(numberp keyloc-n)))
               (= (list-length key-spec-lists) 1))
      (multiple-value-setq (new-keylist return-value)
                (get-set-append-keyvalue key old-value new-value :keyloc-n keyloc-n
                                            :append-value-p append-keyvalue-p
                                            :put-key-after-items put-key-after-items
                                            :splice-key-value-in-list splice-key-value-in-list))

      ;;(BREAK "(and (not (and (listp nested-lists)(numberp keyloc-n)))(= (list-length key-spec-lists) 1))")
      (cond 
       ((= keyloc-n 0)
        (setf new-keylist (append new-keylist (list nested-lists))))
       (t (setf new-keylist (replace new-keylist (list nested-lists)))))

      (setf return-nested-lists  (append list-head
                                          new-keylist
                                         list-tail))
      ;;end when
      )      

    (values return-keylist return-nested-lists new-keylist return-value    
            old-keylist last-key-found-p )
    ;;end let, get-set-append-keyvalue-in-orderly-nested-list
    ))
;;TEST
;;  (progn (setf out nil) (get-set-append-keyvalue-in-orderly-nested-list 'NEW-VALUE    '((:key1 2)(:key2  2)(:key3 4)(:key4 0))  '(:k1 (a) :key1 (k2 (b) :key2 (k3 (c) k4 (d) :key3 (:key4 OLD-VALUE) k5 e) k6 f) xx) :if-not-found-append-key-value-p nil ))
;; works= 
;;  (:KEY4 NEW-VALUE)    (:K1 (A) :KEY1 (K2 (B) :KEY2 (K3 (C) K4 (D) :KEY3 (:KEY4 NEW-VALUE) K5 E) K6 F) XX)    NEW-VALUE   (:KEY4 OLD-VALUE)    T
;; for return-list-p
;; (progn (setf out nil) (get-set-append-keyvalue-in-orderly-nested-list 'NEW-VALUE    '((:key1 2)(:key2  2)(:key3 4)(:key4 0))  '(:k1 (a) :key1 (k2 (b) :key2 (k3 (c) k4 (d) :key3 (:key4 OLD-VALUE rest of list) k5 e) k6 f) xx) :return-list-p T :if-not-found-append-key-value-p nil ))
;; works= (:KEY4 NEW-VALUE REST OF LIST)    (:K1 (A) :KEY1 (K2 (B) :KEY2 (K3 (C) K4 (D) :KEY3 (:KEY4 NEW-VALUE REST OF LIST) K5 E) K6 F) XX)   (:KEY4 NEW-VALUE)   (:KEY4 NEW-VALUE)   (:KEY4 OLD-VALUE)   T
;; append-keyvalue-p
;; (progn (setf out nil) (get-set-append-keyvalue-in-orderly-nested-list 'NEW-VALUE    '((:key1 2)(:key2  2)(:key3 4)(:key4 0))  '(:k1 (a) :key1 (k2 (b) :key2 (k3 (c) k4 (d) :key3 (:key4 OLD-VALUE) k5 e) k6 f) xx) :if-not-found-append-key-value-p nil  :append-keyvalue-p T))
;; works= 
;;  (:KEY4 (OLD-VALUE NEW-VALUE))   (:K1 (A) :KEY1 (K2 (B) :KEY2 (K3 (C) K4 (D) :KEY3 (:KEY4 (OLD-VALUE NEW-VALUE)) K5 E) K6 F) XX)    (OLD-VALUE NEW-VALUE)   (:KEY4 OLD-VALUE)    T
;; if no old key or value and :if-not-found-append-key-value-p
;; keyloc-n > 0
;; (progn (setf out nil) (get-set-append-keyvalue-in-orderly-nested-list 'NEW-VALUE    '((:key1 2)(:key2  2)(:key3 4)(:key4 3))  '(:k1 (a) :key1 (k2 (b) :key2 (k3 (c) k4 (d) :key3  k5 e) k6 f) xx) :if-not-found-append-key-value-p T ))
;; keyloc-n = 0
;; (progn (setf out nil) (get-set-append-keyvalue-in-orderly-nested-list 'NEW-VALUE    '((:key1 2)(:key2  2)(:key3 4)(:key4 0))  '(:k1 (a) :key1 (k2 (b) :key2 (k3 (c) k4 (d) :key3  k5 e) k6 f) xx) :if-not-found-append-key-value-p T ))
;; TEST FOR :NTH
;; append
;; (progn (setf out nil) (get-set-append-keyvalue-in-orderly-nested-list 'NEW-VALUE    '((:NTH 2)(:NTH  2)(:NTH 4)(:NTH 0))  '(:k1 (a) :key1 (k2 (b) :key2 (k3 (c) k4 (d) :key3 (:key4 OLD-VALUE) k5 e) k6 f) xx) :if-not-found-append-key-value-p nil  :append-keyvalue-p T))
;; works=  (OLD-VALUE NEW-VALUE)     (:K1 (A) :KEY1 (K2 (B) :KEY2 (K3 (C) K4 (D) :KEY3 (:KEY4 (OLD-VALUE NEW-VALUE)) K5 E) K6 F) XX)     (OLD-VALUE NEW-VALUE)      (OLD-VALUE)     T
;; set
;;  (progn (setf out nil) (get-set-append-keyvalue-in-orderly-nested-list 'NEW-VALUE    '((:NTH 2)(:NTH  2)(:NTH 4)(:NTH 0))  '(:k1 (a) :key1 (k2 (b) :key2 (k3 (c) k4 (d) :key3 (:key4 OLD-VALUE) k5 e) k6 f) xx) :if-not-found-append-key-value-p nil  :append-keyvalue-p NIL))
;; works= (NEW-VALUE)    (:K1 (A) :KEY1 (K2 (B) :KEY2 (K3 (C) K4 (D) :KEY3 (:KEY4 NEW-VALUE) K5 E) K6 F) XX)   NEW-VALUE   (OLD-VALUE)   T







;SET-KEY-SINGLEVALUE-IN-NESTED-LISTS
;;  (partially replaced by set-key-value and append-key-value) 
;;   This function can search deep nested lists 
;;
;;ddd
(defun set-singlekey-value-in-nested-lists (new-value  key  nested-lists 
                                                          &key append-keyvalue-p  (set-nth 1)
                                                          return-list-p
                                                          (max-list-length 1000)
                                                          (keyloc-n 0)
                                                          splice-key-value-in-list
                                                          put-key-after-items
                                                          new-keylist old-keylist return-value key-found-p) 
  "In U-lists.lisp, SETS key-value FOR EVERY OCCURANCE OF KEY. Use set-key-value-in-nested-lists for key-spec-lists.   RETURNS (values new-keylist return-nested-lists  return-value  old-keylist) that matches key.  If RETURN-LIST-P, returned new-keylist is the ENTIRE list containing key..If APPEND-KEYVALUE-P, appends the new-value to the value following the key [If old value not a list, makes a list with both values.]  If set-nth, sets nth item after key to value (starts with 1).  [If  KEY NOT FOUND, RETURNS NILS for all values except return-nested-lists. [Note: this version does exhaustive search of tree including branches and items past the found key.  Not efficient if know lower level keys.]. SPLICE-KEY-VALUE-IN-LIST is a list to put the key/new-value (or if :NTH) at position keyloc-n.  put-key-after-items is a list of items to put before key/value. IF KEY = :NTH, then uses keyloc-n to find the item located at that position in the last list in recursion to operate on (not very useful?)."
  (let*
      ((return-nested-lists)
       (new-nested-lists)
       (new-nested-list)
       (new-nested-lists)
       (add-new-value-p)
       (old-value)
       )
    (cond
     ((listp nested-lists)
      (loop
       for item in nested-lists
       for item-n from 0 to max-list-length
       do
       ;;(afout 'out (format nil "item=  ~A key-found-p= ~A" item key-found-p))
       (cond
        ;;ADD-NEW-VALUE-P  SET VALUE FOR ITEM FOLLOWING LAST KEY  [only for next item after last key]
        (add-new-value-p  
         (setf add-new-value-p nil
               old-value item)
         (cond
          ((not (equal key :NTH))
           (setf old-keylist (list key item)))
          (t (setf old-keylist (list item))))

         (multiple-value-setq (new-keylist return-value)
             (get-set-append-keyvalue key old-value new-value
                                         :keyloc-n keyloc-n
                                         :append-value-p append-keyvalue-p
                                         :put-key-after-items put-key-after-items
                                         :splice-key-value-in-list splice-key-value-in-list))

         (setf  return-nested-lists (append return-nested-lists 
                                            new-keylist))
         ;;(BREAK "after add new value")
         ;;end add-new-value-p
         )
        ((listp item)
         (multiple-value-setq (new-keylist new-nested-lists 
                                           return-value   old-keylist key-found-p)
             (set-singlekey-value-in-nested-lists new-value  key  item
                                                  :append-keyvalue-p append-keyvalue-p
                                                  :max-list-length max-list-length
                                                  :key-found-p key-found-p
                                                  :new-keylist new-keylist 
                                                  :old-keylist old-keylist :return-value return-value
                                                  ))
         (setf return-nested-lists (append return-nested-lists (list  new-nested-lists)))
         ;;end listp item
         )
        ;;NOT A LIST 
        (t (cond
            ((or (my-equal item key) 
                 (and (equal key :NTH)
                      (= item-n keyloc-n)))
             (setf key-found-p T
                   add-new-value-p T)
             ;;(break "1b EQUAL")
             ;;(afout 'out (format nil "ITEM=KEY= ~a" ITEM))
             ;;end item = key
             )
            ;;not item = key
            (t (setf return-nested-lists (append return-nested-lists (list  item)))))
           ;;end item not listp, cond
           ))
       ;;end loop, nested-lists clause
       ))
     ;;NULL NESTED-LISTS (end recursions)
     (t  NIL
         ;;end null nested-lists
         ))
    ;;(afout 'out (format nil "return-nested-lists= ~A~%key-found-p= ~A" return-nested-lists key-found-p))
    (values  new-keylist return-nested-lists return-value    
            old-keylist key-found-p)
    ;;end let, set-singlekey-value-in-nested-lists
    ))
;;TEST
;; APPEND
;;(progn (setf out nil)(set-singlekey-value-in-nested-lists  'NEW-VALUE ':key2 '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95)) :append-keyvalue-p t))
;;works=(:KEY2 (0.55 NEW-VALUE))    ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) (:KEY2 (0.55 NEW-VALUE)) :KEY3 (44) (:KEY4 0.95))    (:KEY2 (0.55 NEW-VALUE))   (:KEY2 (0.55))    T
;;SET
;;   (set-singlekey-value-in-nested-lists  'NEW-VALUE ':key2 '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95)))
;;works=  (:KEY2 NEW-VALUE)     ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) (:KEY2 NEW-VALUE) :KEY3 (44) (:KEY4 0.95))     (:KEY2 NEW-VALUE)    (:KEY2 (0.55))   T 
;; GET
;;  (set-singlekey-value-in-nested-lists  :get  ':key2 '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95)))
;; works= (:KEY2 (0.55))   ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95))    (:KEY2 (0.55))   (:KEY2 (0.55))   T
;; 2 SAME KEYS, PICKS FIRST KEY ONLY--Use get-set-append-keyvalue-in-nested-list instead
;; set not append
;;(progn (setf out nil) (set-singlekey-value-in-nested-lists 'NEW-VALUE  'A3  '(:PCSYM-ELM-LISTS ((A3 (MOTHER FATHER BEST-M-FRIEND)) (B3 (MOTHER FATHER BEST-F-FRIEND)) (C5 (MOTHER BEST-M-FRIEND BEST-F-FRIEND)) (D1 (FATHER BEST-M-FRIEND BEST-F-FRIEND))) :PC-VALUES ((A3 "a vs not a" :SINGLE "One of the most important things in my life" "0.917" 11 1 12 11 SCORED-NORMAL PRIORITY12)(A4 "this is A4") x y) (list z))))
;;NOTE:-SETS BOTH A3 =  (A3 NEW-VALUE)    (:PCSYM-ELM-LISTS ((A3 NEW-VALUE) (B3 (MOTHER FATHER BEST-F-FRIEND)) (C5 (MOTHER BEST-M-FRIEND BEST-F-FRIEND)) (D1 (FATHER BEST-M-FRIEND BEST-F-FRIEND))) :PC-VALUES ((A3 NEW-VALUE :SINGLE "One of the most important things in my life" "0.917" 11 1 12 11 SCORED-NORMAL PRIORITY12) (A4 "this is A4") X Y) (LIST Z))    (NEW-VALUE)    (A3 "a vs not a")    T
;;
;;with 2 keys in nested list, sets BOTH VALUES.
;;WORKS= (A3 NEW-VALUE)    (:PCSYM-ELM-LISTS ((A3 NEW-VALUE) (B3 (MOTHER FATHER BEST-F-FRIEND)) (C5 (MOTHER BEST-M-FRIEND BEST-F-FRIEND)) (D1 (FATHER BEST-M-FRIEND BEST-F-FRIEND))) :PC-VALUES ((A3 NEW-VALUE :SINGLE "One of the most important things in my life" "0.917" 11 1 12 11 SCORED-NORMAL PRIORITY12)))    (NEW-VALUE)     (A3 "a vs not a")    T




;;SET-KEY-VALUE-IN-NESTED-LISTS
;; DEPRECIATED--REPLACED BY GET-SET-APPEND-KEYVALUE-IN-NESTED-LIST
;;  (partially replaced by set-key-value and append-key-value) 
;;   This function can more specifically search deep nested lists 
;;  ESPECIALLY GOOD FOR MULTI-LEVEL KEYS
;;
;;ddd
(defun set-key-value-in-nested-lists (new-value  key-spec-lists  nested-lists 
                                                &key append-item  append-keyvalue-p
                                                final-key-inside-keylist-p 
                                                subst-new-keylist-p no-return-extra-p) 
  "In U-lists.lisp, DEPRECIATED (but works ok?)--use get-set-append-keyvalue-in-nested-list SETS key-value. USE SET-KEY-VALUE (newer) or APPEND-KEY-VALUE for many cases. USE THIS FOR MULTI-LEVEL KEYS.  RETURNS (values new-keylist return-nested-lists  return-value  final-spec-list old-keylist) that matches key-spec. The spec-lists is a list of 2 item spec lists. (key set-nth)  If  key = T, searches entire list of lists for key  If set-nth = T, searches entire sublist for the key.. Extra items are extra non-list items on all level lists that might contain important info. Can process infinite? levels of key lists. Can match with equal or string-equal automatically. The spec-lists are arranged FROM OUTER-MOST LIST to inner-most list. FINAL-KEY-INSIDE-KEYLIST-P means that the last key is inside a list--so items in outer list won't cause a false match. APPEND-ITEM is appended to the end of the list containing the final key.If APPEND-KEYVALUE-P, appends the new-value to the value following the key [If old value not a list, makes a list with both values.]     NOTE: final keylists should contain a value--even if nil in the set-nth position so it can be replaced UNLESS the new-value is a new keylist and subst-new-keylist-p = t. If value is in place, replaces it with new-value. "
  (let*
      ((new-keylist)
       (return-key)
       (return-value)
       (return-nested-lists)
       (old-keylist)
       (match-item)
       (spec-list (car key-spec-lists))
       (KEY (first spec-list))
       (SET-NTH (second spec-list))
       (new-spec-lists (cdr key-spec-lists))
       (final-spec-list)
       (nested-list-length (list-length nested-lists))
       (new-nested-lists)
       (match-item-lo)
       (new-value-lo  )
       (length-item-lo)
       (new-nested-lists)
       (item-loval)
       )
    (unless  set-nth (setf set-nth 0))
    (cond
     (nested-lists
      (cond
       ;;SPEC IS TO SEARCH EVERY LIST AT THIS LEVEL
       ((or (equal key t) (equal set-nth t))
        ;;(afout 'out (format nil "NEW CALL TO T  key= ~A~% (car of list= ~A~%" key (car nested-lists)))   
        (loop
         for item  in nested-lists
         ;;with new-value2 
         with new-nested-lists 
         do
         ;;(afout 'out (format nil "T OUTER-LOOP key= ~A~%  item= ~A~%" key item))
         ;;test to see what the spec-list indicates
         (cond
          ((and item (listp item))

           ;;note: this may call other recursive calls
           (multiple-value-setq (new-keylist  new-nested-lists return-value 
                                              final-spec-list old-keylist)
               (set-key-value-in-nested-lists new-value new-spec-lists item
                                              :append-item append-item 
                                              :final-key-inside-keylist-p final-key-inside-keylist-p 
                                              :subst-new-keylist-p subst-new-keylist-p
                                              :no-return-extra-p no-return-extra-p
                                              ))
           ;;works?
                (setf return-nested-lists (append return-nested-lists (list  new-nested-lists)))
           )
          ;;may be non-list items such as other keys providing info re: found outer lis.
          (t (setf return-nested-lists (append return-nested-lists (list  item))))) 
         ;;end loop set-nth = t
         ))
       ;;AT LOWEST LIST, SEARCH FOR KEY-VALUE
       ((and (null new-spec-lists) key)
        (loop
         for item-lo in nested-lists
         for n from 0 to nested-list-length
         with match-item-lo 
         with new-value-lo  
         with length-item-lo 
         do
         ;;(afout 'out (format  nil "1 LOWEST LEVEL TEST key= ~A  match-item-lo= ~A~% set-nth= ~A~%nested-lists= ~A~% IN find-key-value-in-lists " key match-item-lo set-nth nested-lists))
         (cond
          ;;SEARCH INSIDE A NESTED LIST -- NOT IN OUTER LIST FOR KEY
          ((and (listp item-lo) (null final-key-inside-keylist-p))
           (setf  length-item-lo (list-length item-lo))       
           (unless (>= set-nth  length-item-lo))
           (setf match-item-lo (nth set-nth item-lo))
           (cond
            ((my-equal key match-item-lo)
             ;;was (or (equal key match-item-lo) (if (stringp match-item-lo) (string-equal key match-item-lo)))

             ;;(afout 'out (format  nil "2 LOWEST LEVEL TEST key= ~A  match-item-lo= ~A~% set-nth= ~A~%nested-lists= ~A~% item-lo= ~A~%IN find-key-value-in-lists " key match-item-lo set-nth nested-lists item-lo))

             ;;use RETURN-VALUE because will be NIL IF NO MATCH (and return NIL)
             ;;  also set old-keylist to item-lo
             (setf return-value new-value
                   old-keylist item-lo)
             ;;PUT NEW-VALUE IN PROPER PLACE AND/OR ADD APPEND-ITEM-LO
             ;;do I replace an item-lo or just add it.
             (cond
              ;;if subst-new-keylist-p subst new-value (a list) for old keylist.
              ((and subst-new-keylist-p (listp new-value))
               (setf new-keylist new-value))
              ;;if  set-nth, replace that item-lo with new-value
              (set-nth
               ;;problem with replace modifying item-lo permenantly and therefore old-keylist
               ;;zzzz
               ;;modified 2016-04 to add append-keyvalue-p option
               (cond
                (append-keyvalue-p
                 (setf item-loval (second item-lo))
                 (cond
                  ((listp item-loval)
                   (setf new-value-lo (append item-loval (list new-value))))
                  (t (setf new-value-lo (list item-loval new-value)))))
                (t (setf  new-value-lo new-value)))

               ;;2015-05 was (list new-value)= extra parens
               (setf new-keylist (replace-list item-lo (+ set-nth 1) new-value-lo)))
              (t nil))  ;;was (setf new-keylist nil)))
             ;;if append-item (= a value) append to the new-keylist
             (if append-item
                 (setf final-spec-list (append final-spec-list (list append-item)))) 
             ;;set final return items
             (setf final-spec-list (list key set-nth)
                   return-nested-lists (replace-list nested-lists n new-keylist)) 
             ;;2015 was (list new-keylist))) => extra parens around list
             ;;  (return)
             )
            (t nil))
           ;;end listp item-lo
           )
          ;;if not list, search current list for item-lo, just check to see if item-lo = key
          ;;problem if an item-lo matches key but real key is inside a later list
          ;;SEARCHES OUTER LIST FOR KEY MATCH--NOT AN INNER LIST
          (final-key-inside-keylist-p
           (cond
            ((my-equal key item-lo)
             ;;was (or (equal key item-lo) (if (stringp item-lo) (string-equal key item-lo)))
             ;;PUT NEW-VALUE IN PROPER PLACE AND/OR ADD APPEND-ITEM-LO
             ;;do I replace an item-lo or just add it.
             (cond
              ;;if subst-new-keylist-p subst new-value (a list) for old keylist.
              ((and subst-new-keylist-p (listp new-value))
               (setf new-keylist new-value))
              ;;if  set-nth, replace that item-lo with new-value
              (set-nth
               (setf  new-value-lo (list new-value)
                      ;;xxx causes a problem perm changing item-lo value??
                      new-keylist (replace-list item-lo    (+ set-nth 1) new-value-lo)))
              #|                    (replace item-lo new-value-lo :start1
                                         (+ set-nth 1) :end1 (+ set-nth 2))))|#
              (t (setf new-keylist nil)))

             ;;if append-item (= a value) append to the new-keylist
             (if append-item
                 (setf new-keylist (append new-keylist (list append-item))))
             ;;added works
             (setf return-nested-lists (append return-nested-lists (list  new-keylist)))
             (return))
            (t  nil)))    ;; (setf new-nested-list (append new-nested-list (list item-lo))))))
          (t nil) ;; (setf new-nested-list (append new-nested-list (list item-lo))))
          ;;end cond, lo loop, equal null new-spec-lists
          )))
       ;;SPEC IS TO SEARCH THIS LEVEL (not last level) BY KEY at SET-NTH
       ((and  new-spec-lists  key)
        ;;(afout 'out (format nil "OUTER-LOOP SEARCH new-spec-lists= ~A~%" new-spec-lists))         
        ;;check each list at this level
        (loop
         for item in nested-lists
         with list-length  
         with match-item 
         with new-nested-lists 
         do
         ;;for each sublist, check set-nth
         ;;(afout 'out (format nil "KEY OUTER-LOOP key= ~A~% item= ~A~%new-spec-lists= ~A~%" key item new-spec-lists))
         (cond
          ((listp item)
           (setf  list-length (list-length item))     
           (unless (>= set-nth  list-length))
           (setf match-item (nth set-nth item))
          ;; (setf *out (append *out  (format  nil "OUTER LOOP TESTING key= ~A  match-item= ~A~%  IN find-key-value-in-lists" key match-item)))
           (cond
            ((my-equal key match-item)
             ;;was (or (equal key match-item) (if (stringp match-item) (string-equal key match-item)))
             ;;;yyy
             (multiple-value-setq (new-keylist  new-nested-lists return-value 
                                                final-spec-list old-keylist)
                 (set-key-value-in-nested-lists new-value new-spec-lists item
                                                :append-item append-item 
                                                :final-key-inside-keylist-p final-key-inside-keylist-p 
                                                :subst-new-keylist-p subst-new-keylist-p
                                                :no-return-extra-p no-return-extra-p))
             ;;works
             (setf return-nested-lists (append return-nested-lists (list new-nested-lists)))
   ;;zzzz sss start here
             ;;(setf *out (append *out  (format nil "ON RECURSE RETURN return-value= ~A~% return-key=~A~%" return-value return-key)))
             ;;was   (return)
             )
            (t (setf return-nested-lists (append return-nested-lists (list item)))))
           ;;end listp item
           )
          (t 
           (setf return-nested-lists (append return-nested-lists (list (list item))))))
         ;;end loop, equal new-spec-lists
         ))
       ;;IF TOP LEVEL NONE-OF-ABOVE
       (t (setf return-nested-lists (append return-nested-lists (list (list :NTH new-value))))))
      ;;end nested-lists
      )
     (t (cond
         (key-spec-lists
          (if (listp key-spec-lists)
              (setf new-keylist (list (caar  key-spec-lists) new-value)
                    return-nested-lists (list new-keylist))
            (setf new-keylist (list  key-spec-lists  new-value)
                  nested-lists (list new-keylist))))
         (t (setf new-keylist (list :NTH new-value)
              return-nested-lists (list new-keylist))))
        ;;end outer t, cond
        ))
    ;;end find-key-value-in-lists

;;2015-05 added bec wouldn't add keylist to a simple list with no key already in list
   (if (and (null return-value) (null new-keylist) key new-value )
        (setf new-keylist  (list key new-value)
              return-nested-lists (append nested-lists (list new-keylist))))
             ;; return-nested-lists nil)
    ;;end set-key-value-in-nested-lists
    (values new-keylist return-nested-lists  return-value  final-spec-list old-keylist) 
    ))
  
;;TEST
;;2016-04 added :append-keyvalue-p functionality
;; (set-key-value-in-nested-lists  77 '((:key2 0)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95)) :append-keyvalue-p t)
;;works= (:KEY2 (0.55 77))   ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) (:KEY2 (0.55 77)) :KEY3 (44) (:KEY4 0.95))   77   (:KEY2 0)   (:KEY2 (0.55))
;;
;; (set-key-value-in-nested-lists 0.50 '((:PCSYM-ELM-LISTS  0)(B3 0)) '(:PCSYM-ELM-LISTS ((A3 (MOTHER FATHER BEST-M-FRIEND)) (B3 (MOTHER FATHER BEST-F-FRIEND)) (C5 (MOTHER BEST-M-FRIEND BEST-F-FRIEND)) (D1 (FATHER BEST-M-FRIEND BEST-F-FRIEND))) :PC-VALUES ((A3 "a vs not a" :SINGLE "One of the most important things in my life" "0.917" 11 1 12 11 SCORED-NORMAL PRIORITY12))) :final-key-inside-keylist-p T)

;;SSS START HERE MAKING IT APPEND INSIDE FIRST LEVEL OR
;;  USE ANOTHER FUNCTION (APPEND-KEY-VALUE OR SET-KEY-VALUE)
;; (setf mother '("MOTHER" "mother" CS1-1-1-1 (datalist this :key1 "that") NIL :info "this is info" :key3 (a b c)))
;; (set-key-value-in-nested-lists  "new-value"  `((:key1  0 T)) mother :append-keyvalue-p T :final-key-inside-keylist-p T) =
;;doesn't work right, results =  (:KEY1 "new-value")  ("MOTHER" "mother" CS1-1-1-1 (DATALIST THIS :KEY1 "that") NIL :INFO "this is info" :KEY3 (A B C) (:KEY1 "new-value"))


;;  (set-key-value-in-nested-lists 77 '((:key1 0)) '("classX1end" 0.22 (:NTH 0.55) (:NTH 0.55) (:NTH 0.55)))

;;test append-item -- only replaces items after the key??
;; (set-key-value-in-nested-lists  77 '((:key2 0)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  (:KEY2 (0.55)) :KEY3 (44) (:KEY4 0.95)) :append-item t)
;; (set-key-value-in-nested-lists  77 '((:key4 0)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  (:KEY2 0.55) :KEY3 (44) (:KEY4 0.95)) :append-item t)
;;(:KEY4 77)   ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) (:KEY2 0.55) :KEY3 (44) (:KEY4 77))   77   (:KEY4 0)  (:KEY4 0.95)

;;note key-spec-list wrong below, so appends list with key1; MUST BE A NESTED LIST
;; (set-key-value-in-nested-lists  77 '((:key1 0)) '("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33)  (:KEY2 0.55) :KEY3 (44) (:KEY4 0.95)))
;; result= ("classX1end" 0.22 (:KEY0 0.25) :KEY1 (33) (:KEY2 0.55) :KEY3 (44) (:KEY4 0.95) (:KEY1 77))

;;first key NOT in main list
;;  (progn (setf out nil) (set-key-value-in-nested-lists 'this-value '((a 0)(b 0)(c 0)) '( (s 1 2 (t 3 4 (u 5 6 7 )))(a (n1 2) (b (mm 33 ) (c 1 2 3  (l i s t )) (nn 22)) ( extra ))))  )
;;works, returns 1- (C (THIS-VALUE) 2 3 (L I S T))  2-  ((S 1 2 (T 3 4 (U 5 6 7))) (A (N1 2) (B (MM 33) ((C (THIS-VALUE) 2 3 (L I S T))) (NN 22)) (EXTRA)))  3-  THIS-VALUE  4-  (C 0)    5- (C 1 2 3 (L I S T))
;; first key IN MAIN list
;; (progn (setf out nil) (set-key-value-in-nested-lists 'this-value '((a T)(b 0)(c 0)) '( s 1 2 (t 3 4 (u 5 6 7 )) a ((n1 2) (b (mm 33 ) (c 1 2 3  (l i s t ))) (nn 22)) ( extra )))  )  
;;SSS = doesn't work (ME-USE GET-SET-APPEND-KEYVALUE-IN-NESTED-LIST.
;;
;;  (progn (setf out nil)  (set-key-value-in-nested-lists 'this-value  '(("iAcademicMotivation.java" 1)( "acmESOCSTudy" 0)) '((PC-INSTANCES  "iAcademicMotivation.java"      ("[]questionInstancesArray1)")      ("acmNDROPcourses" "30" "acmNDROPcoursesQ" "int" "FrAnswerPanel.LikeMe7" "questionInstancesArray" "frameTitle" "frameDimWidth" "frameDimHeight")      ("acmESOCSTudy" "3" "acmESOCSTudyQ" "int" "FrAnswerPanel.LikeMe7" "questionInstancesArray" "frameTitle" "frameDimWidth" "frameDimHeight"))))) ;;(fout out)) ;; :return-list-p  t))
;;WORKS, returns 1-("acmESOCSTudy" (THIS-VALUE) "acmESOCSTudyQ" "int" "FrAnswerPanel.LikeMe7" "questionInstancesArray" "frameTitle" "frameDimWidth" "frameDimHeight")   2-((PC-INSTANCES "iAcademicMotivation.java" ("[]questionInstancesArray1)") ("acmNDROPcourses" "30" "acmNDROPcoursesQ" "int" "FrAnswerPanel.LikeMe7" "questionInstancesArray" "frameTitle" "frameDimWidth" "frameDimHeight") (("acmESOCSTudy" (THIS-VALUE) "acmESOCSTudyQ" "int" "FrAnswerPanel.LikeMe7" "questionInstancesArray" "frameTitle" "frameDimWidth" "frameDimHeight"))))   3- THIS-VALUE  4- ("acmESOCSTudy" 0)   5-("acmESOCSTudy" "3" "acmESOCSTudyQ" "int" "FrAnswerPanel.LikeMe7" "questionInstancesArray" "frameTitle" "frameDimWidth" "frameDimHeight")
;;
;;  (set-key-value-in-nested-lists 'this '((5 0))  '((1 0 (1 2 A)) (5 1 (1 6 (QUOTE (A B))))))
;; returns (THIS 1 (1 6 (QUOTE (A B))))  ((THIS 1 (1 6 (QUOTE (A B))))) (THIS)  (5 0) NIL

;; (progn (setf out nil) (set-key-value-in-nested-lists 'that-value '((this 0))  '((a b)(c (1 2))(this (3 4 5))(x y))))
;;  (set-key-value-in-nested-lists '((x 0))  '((a b)(c (1 2))(this (3 4 5))(x y)) )
;; (set-key-value-in-nested-lists  '((5 0)) '((1 0 (1 2 A)) (5 1 (1 6 (QUOTE (A B))))) :return-list-p t)
;;  use (replace-list  new-value2   (+ set-nth 1)) instead
;;(replace   '(5 1 (1 6 (QUOTE (A B)))) '(this) :start1 1)
;; (progn (setf out nil) (set-key-value-in-nested-lists  '((5 0)(1 0)) '((1 0 (1 2 A)) (5 1 (1 6 (QUOTE (A B))))) :return-list-p t))(fout out))
;; (set-key-value-in-nested-lists  '((5 0)(1 0))  '((1 0 (1 2 A)) (5 1 (1 6 (QUOTE (A B))))) :return-nth 2) 
;; (nth 0 '((1 0 (1 2 A)) (5 1 (1 6 (QUOTE (A B)))))) (1 0 (1 2 A))
;; (progn (setf out nil)  ( set-key-value-in-nested-lists '(( "iWorldviewFears.java" 1)("wovNoLove" 0)) *all-shaq-pc-instances  :return-list-p t))  
;; (progn (setf out nil)  (set-key-value-in-nested-lists "wovNoLove" *all-shaq-pc-instances :find-outer-key "iWorldviewFears.java" :find-nth-first 1)) ;;  :return-list-p t))
;;
;; (progn (setf out nil) (multiple-value-setq (*testfn1 *testfn2 *testfn3 *testfn4) (set-key-value-in-nested-lists "wovNoLove" *all-shaq-pc-instances  :find-outer-key  t   :return-list-p t) )(fout out)))
 
;;  (progn (setf out nil)  (set-key-value-in-nested-lists    "acmESOCSTudy" '(PC-INSTANCES  "iAcademicMotivation.java"      ("[]questionInstancesArray1)")      ("acmNDROPcourses" "30" "acmNDROPcoursesQ" "int" "FrAnswerPanel.LikeMe7" "questionInstancesArray" "frameTitle" "frameDimWidth" "frameDimHeight")      ("acmESOCSTudy" "3" "acmESOCSTudyQ" "int" "FrAnswerPanel.LikeMe7" "questionInstancesArray" "frameTitle" "frameDimWidth" "frameDimHeight")) :return-list-p  t))
;;works, returns ("acmESOCSTudy" "3" "acmESOCSTudyQ" "int" "FrAnswerPanel.LikeMe7" "questionInstancesArray" "frameTitle" "frameDimWidth" "frameDimHeight")  "acmESOCSTudy"  NIL  (PC-INSTANCES "iAcademicMotivation.java")

;;  (set-key-value-in-nested-lists 'this  '((a b)(c (1 2))(this (3 4 5))(x y)))
;; works, returns= (3 4 5)  THIS NIL NIL
;;  (set-key-value-in-nested-lists 'x '((a b)(c (1 2))(this (3 4 5))(x y)))
;; works, returns Y X NIL NIL
;;   (set-key-value-in-nested-lists 5  '((1 0 (1 2 A)) (5 1 (1 6 (QUOTE (A B)))))) = 1 5 NIL NIL
;; (set-key-value-in-nested-lists 5  '((1 0 (1 2 A)) (5 1 (1 6 (QUOTE (A B))))) :return-list-p t)
;; works, returns (5 1 (1 6 (QUOTE (A B))))  5 NIL NIL
;; (set-key-value-in-nested-lists 5  '((1 0 (1 2 A)) (5 1 (1 6 (QUOTE (A B))))) :return-nth 2) 
;;works, returns (1 6 (QUOTE (A B)))  5 NIL NIL
;;(replace  '(0 1 2 3 4 5 6 7 8 9) '(a b c) :start1 5 :end1 7 :start2 1 :end2 3) = (0 1 2 3 4 B C 7 8 9)
;; (replace  '(0 1 2 3 4 5 (x y z) 7 8 9) '(a b (l m)) :start1 5 :end1 7 :start2 1 :end2 3) = (0 1 2 3 4 B (L M) 7 8 9)
;; (replace  '(0 1 2 3 4 5 (x y z) 7 8 9) '(a b (l m)) :start1 6 :end1 9 :start2 1 :end2 3) = (0 1 2 3 4 5 B (L M) 8 9)
;; (set-class-symval "classXX" .22 :key1)
;; ;;    (set-key-value-in-nested-lists 55  '((key1 T))  nil) = (KEY1 55) ((KEY1 55)) NIL NIL NIL
;;  (set-key-value-in-nested-lists 55 nil  nil) = (:NTH 55) ((:NTH 55)) NIL NIL NIL
;;  (set-key-value-in-nested-lists 55  '((key2 0)) '(1 2 (key1 22)(key2 33)(key3 0))) 
;; works= (KEY2 55)  (1 2 (KEY1 22) (KEY2 55) (KEY3 0))   55  (KEY2 0)  (KEY2 33)













                 


;; hhh ***************************** HELP ****************************8
;;
;;USEFUL CL FUNCTIONS
;; FIND-SYMBOL
;;Lambda List: (STRING &OPTIONAL (PACKAGE *PACKAGE*))
#|Returns the symbol named String in Package.  If such a symbol is found
  then the second value is :intern, :external or :inherited to indicate
  how the symbol is accessible.  If no symbol is found then both values
  are NIL.|#
;;TEST
;;  (find-symbol "setf") = nil  ;;doesn't work
;;  (find-symbol "newtest") = nil ;;doesn't work



#|
(append nil (list 5)) = (5)
(tx '(1 2 3 (4) 5))  = (1 2 3 (4) 5)
(tx '(a b c)) = (A B C)
(defun tx (list)
  (let
      ((newlist)
       )
  (dolist (x list)
    (setf newlist (append newlist (list x)))    
    )
      newlist))
|#



#|(random 10)
(nth 3 '(a b c d e))
(elt  '(a b c d e) 2) = C
(elt  "abcde" 2) = #\c 
(elt  '((a)(b)(c) d) 1) = (B) |#


#|
;;DELETE ONCE NEW ONE WORKS
(defun find-key-value-in-nested-lists (key list-of-lists &key return-list-p
                                 (find-nth 0) (return-nth 1) find-key-in-nested (find-nth-first 0))
  "In U-lists.lisp, RETURNS first value that matches key (values value key outer-items). FIND-NTH searches for nth item in the list for the first key. if RETURN-NTH, returns nth instead of second item in list. :FIND-KEY-IN-NESTED looks for the first key in the nth-key of the second order nested lists--not in first list set. It IGNORES keys in the first list order if = T,  otherwise it searchs only the list with the key set to :find-key-in-nested.  If  RETURN-LIST-P, returns entire sublist.  :FIND-NTH-FIRST is used only if :find-key-in-nested is set to a value."
  (let
      ((search-list list-of-lists)
       (list-length (list-length list-of-lists))
       (result-value)
       (result-key)
       (outer-extra-items)
       (inner-extra-items)
       )
    ;;  (afout 'out (format nil "NEW CALL TO FUNCTION key= ~A~% list~A~%" key list-of-lists))
    ;;finds a list to search in outer/first order set of lists (by key)
    ;;SSS debug here
    (cond
     ;;if  T all first-order lists must be searched
     ((equal find-key-in-nested t)
    ;;  (afout 'out (format nil "NEW CALL TO T  key= ~A~% (car of list= ~A~%" key (car list-of-lists)))   
      (loop
       for first-order-item  in list-of-lists
       with inner-extra-items1
       with outer-extra-items1
       do
     ;; (afout 'out (format nil "T OUTER-LOOP key= ~A~% first-order-item= ~A~%" key first-order-item))
       (cond
        ((and first-order-item (listp first-order-item))
         (multiple-value-setq (result-list result-key inner-extra-items1 outer-extra-items1)
             (find-key-value-in-nested-lists  key  first-order-item :return-list-p t))
                                           ;;not needed?   :find-nth find-nth 
         ;;these are the extra items want to return (bec inside of target containing list)
         (if inner-extra-items1
             (setf inner-extra-items (append inner-extra-items (list inner-extra-items1))))
         (if outer-extra-items
             outer-extra-items (append outer-extra-items (list outer-extra-items1)))
         (return))
        ;;may be non-list items such as other keys providing info re: found outer list.
        (t (setf outer-extra-items (append outer-extra-items (list first-order-item)))))
       ;;end loop equal
       ))

     ;;if first-order key is specified by find-key-in-nested, find that sublist first
     (find-key-in-nested
      (loop
       for first-order-item in list-of-lists
       with match-key1
       with result-key1
       with outer-extra-items1
       with inner-extra-items1
       do
      ;;(afout 'out (format nil "OUTER-LOOP #2 first-order-item= ~A~%" first-order-item))
       (multiple-value-setq 
           (search-list result-key1 outer-extra-items1 inner-extra-items1)
           (find-key-value-in-nested-lists  find-key-in-nested  list-of-lists
                                            :find-nth find-nth-first :return-list-p t))
       (cond
        ((and result-key1 inner-extra-items1)
           (setf  outer-extra-items  (append outer-extra-items (list inner-extra-items1)))
           (return))
        (t nil))
       ;;note needed here?? with inner-extra-items1
       ;;end loop, find-key-in-nested
       ))
      ;;if find-key-in-nested = NIL do nothing here--
      ;;MAY NOT NEED FIRST CLAUSE ABOVE SSS
        (t nil))
 
    ;;(afout 'out (format nil "MAIN PROCESSING key= ~A~%  (car search-list= ~A~%" key (car search-list)))
    ;;main work searching final search list (may be outer or nested)
    (unless (equal find-key-in-nested t)
      (loop
       for item in search-list
       with match-item
       with item-length
       do
       ;;(afout 'out (format nil "SECOND-LOOP item ~A~%" item))   
       (cond
        ((listp item)
         (setf item-length (list-length item))       
         (unless (>= find-nth  item-length))
         (setf match-item (nth find-nth item))
         ;;(afout 'out (format  nil "TESTING key= ~A  match-item= ~A~%  IN find-key-value-in-nested-lists" key match-item))
         (cond
          ((or (equal key match-item)(string-equal key match-item))
           (cond
            (return-list-p
             (setf result-value item
                   result-key match-item))
            (t
             (setf result-value (nth return-nth item)
                   result-key key)))
           (return))
          (t nil))
         ;;end listp item
         )
        ;;if item is not a list, then item may be outer keys or info want to keep
        (t (if  item (setf inner-extra-items (append inner-extra-items (list item))))))
       ;;end loop, unless
       ))
    ;;end find-key-value-in-nested-lists
    (values result-value  result-key outer-extra-items inner-extra-items)
    ))|#


;;ORIGINAL-ERROR
#|(defun delete-duplicate-nth (nth list1 list2 &key compare-nth return-lessp )
  "In U-lists. Compares same nth item in list of lists and deletes lists with duplicates. RETURNS (values new-list duplicate-list duplicatep) new-list is a list of both lists unless nth item is my-equal.  If compare-nth, then the one with the largest value of compare-nth will be returned (unless return-less-p)."
  (let
      ((new-list)
       (result)
       (duplicate-list)
       (duplicatep)
       )
    (cond
     ((my-equal (nth nth list1)(nth nth list2))
      (setf duplicatep t)
      (cond
       (compare-nth
        (setf ordered-list  (my-list-nth-lessp compare-nth list1 list2))
        (cond
         (return-lessp
          (setf new-list (car ordered-list)
                duplicate-list (second ordered-list)))
         (t (setf new-list (second ordered-list)
                  duplicate-list (first ordered-list))))
        ;;end compare-nth
        )
       (t (setf new-list (list list1)
                duplicate-list (list list2))))
      ;;ene my-equal
      )
     (t (setf new-list (list list1 list2))))
    (values new-list duplicate-list duplicatep)
    ;;end let, delete-duplicate-nth
    ))|#

#|(defun set-key-value-in-nested-lists (new-value key-spec-lists nested-lists 
                                                &key append-item final-key-inside-keylist-p 
                                                subst-new-keylist-p no-return-extra-p) 
  "In U-lists.lisp, SETS key-value . RETURNS (values new-keylist return-nested-lists return-value  final-spec-list) that matches key-spec. The spec-lists is a list of 2 item spec lists. (key set-nth)  If  key = T, searches entire list of lists for key  If set-nth = T, searches entire sublist for the key.. Extra items are extra non-list items on all level lists that might contain important info. Can process infinite? levels of key lists. Can match with equal or string-equal automatically. The spec-lists are arranged from outer-most list to inner-most list. final-key-inside-keylist-p means that the last key is inside a list--so items in ourer list won't cause a false match. append-item is appended to the end of the list containing the final key. NOTE: final keylists should contain a value--even if nil in the set-nth position so it can be replaced UNLESS the new-value is a new keylist and subst-new-keylist-p = t."
  (let*
      ((new-keylist)
       (return-key)
       (return-value)
       (return-nested-lists)
       (old-keylist)
       (spec-list (car key-spec-lists))
       (key (first spec-list))
       (set-nth (second spec-list))
       (new-spec-lists (cdr key-spec-lists))
       (final-spec-list)
       (list-length (list-length nested-lists))
       (new-value2)
       (new-nested-lists)
       (match-item )
       ( new-value2 )
       (length-item )
      ;; (list-length  )
       ;;with match-item )
       ;;with new-nested-lists )
       )
    (unless  set-nth (setf set-nth 0))
    (cond
     ;;SPEC IS TO SEARCH EVERY LIST AT THIS LEVEL
     ((or (equal key t) (equal set-nth t))
      ;;(afout 'out (format nil "NEW CALL TO T  key= ~A~% (car of list= ~A~%" key (car nested-lists)))   
      (loop
       for item  in nested-lists
       do
       ;;(afout 'out (format nil "T OUTER-LOOP key= ~A~%  item= ~A~%" key item))
       ;;test to see what the spec-list indicates
       (cond
        ((and item (listp item))

         ;;note: this may call other recursive calls
         (multiple-value-setq (new-keylist  new-nested-lists return-value 
                                            final-spec-list old-keylist)
             (set-key-value-in-nested-lists new-value new-spec-lists item
                                            :append-item append-item 
                                            :final-key-inside-keylist-p final-key-inside-keylist-p 
                                            :subst-new-keylist-p subst-new-keylist-p
                                            :no-return-extra-p no-return-extra-p
                                            ))
         (setf return-nested-lists (append return-nested-lists (list  new-nested-lists)))
         )
        ;;may be non-list items such as other keys providing info re: found outer lis.
        (t (setf return-nested-lists (append return-nested-lists (list  item))))) 
       ;;end loop set-nth = t
       ))
     ;;AT LOWEST LIST, SEARCH FOR KEY-VALUE
     ((and (null new-spec-lists) key)
      (loop
       for item in nested-lists
       for n from 0 to list-length
       do
       ;;(afout 'out (format  nil "1 LOWEST LEVEL TEST key= ~A  match-item= ~A~% set-nth= ~A~%nested-lists= ~A~% IN find-key-value-in-lists " key match-item set-nth nested-lists))
       (cond
        ;;SEARCH INSIDE A NESTED LIST -- NOT IN OUTER LIST FOR KEY
        ((and (listp item) (null final-key-inside-keylist-p))
         (setf  length-item (list-length item))       
         (unless (>= set-nth  length-item))
         (setf match-item (nth set-nth item))
         (cond
          ((my-equal key match-item)
       ;;was (or (equal key match-item) (if (stringp match-item) (string-equal key match-item)))

           ;;(afout 'out (format  nil "2 LOWEST LEVEL TEST key= ~A  match-item= ~A~% set-nth= ~A~%nested-lists= ~A~% item= ~A~%IN find-key-value-in-lists " key match-item set-nth nested-lists item))

           ;;use RETURN-VALUE because will be NIL IF NO MATCH (and return NIL)
           ;;  also set old-keylist to item
           (setf return-value new-value
                 old-keylist item)
           ;;PUT NEW-VALUE IN PROPER PLACE AND/OR ADD APPEND-ITEM
           ;;do I replace an item or just add it.
           (cond
            ;;if subst-new-keylist-p subst new-value (a list) for old keylist.
            ((and subst-new-keylist-p (listp new-value))
             (setf new-keylist new-value))
            ;;if  set-nth, replace that item with new-value
            (set-nth
             ;;problem with replace modifying item permenantly and therefore old-keylist
             (setf  new-value2 (list new-value)
                    new-keylist (replace-list item (+ set-nth 1) new-value2)))
            (t nil))  ;;was (setf new-keylist nil)))
           
           ;;if append-item (= a value) append to the new-keylist
           (if append-item
               (setf final-spec-list (append final-spec-list (list append-item)))) 
           ;;set final return items
           (setf final-spec-list (list key set-nth)
                 return-nested-lists (replace-list nested-lists n (list new-keylist)))
         ;;  (return)
           )
          (t nil))
         ;;end listp item
         )
        ;;if not list, search current list for item, just check to see if item = key
        ;;problem if an item matches key but real key is inside a later list
        ;;SEARCHES OUTER LIST FOR KEY MATCH--NOT AN INNER LIST
        (final-key-inside-keylist-p
         (cond
          ((my-equal key item)
           ;;was (or (equal key item) (if (stringp item) (string-equal key item)))
           ;;PUT NEW-VALUE IN PROPER PLACE AND/OR ADD APPEND-ITEM
           ;;do I replace an item or just add it.
           (cond
            ;;if subst-new-keylist-p subst new-value (a list) for old keylist.
            ((and subst-new-keylist-p (listp new-value))
             (setf new-keylist new-value))
            ;;if  set-nth, replace that item with new-value
            (set-nth
             (setf  new-value2 (list new-value)
                    ;;xxx causes a problem perm changing item value??
                    new-keylist (replace-list  new-value2   (+ set-nth 1))))
#|                    (replace item new-value2 :start1
                                         (+ set-nth 1) :end1 (+ set-nth 2))))|#
            (t (setf new-keylist nil)))

           ;;if append-item (= a value) append to the new-keylist
           (if append-item
               (setf new-keylist (append new-keylist (list append-item))))
           ;;added
           (setf return-nested-lists (append return-nested-lists (list  new-keylist)))
           (return))
          (t  nil)))    ;; (setf new-nested-list (append new-nested-list (list item))))))
        (t nil) ;; (setf new-nested-list (append new-nested-list (list item))))
        ;;end cond, loop, equal null new-spec-lists
        )))
     ;;SPEC IS TO SEARCH THIS LEVEL (not last level) BY KEY at SET-NTH
     ((and  new-spec-lists  key)
      ;;(afout 'out (format nil "OUTER-LOOP SEARCH new-spec-lists= ~A~%" new-spec-lists))         
      ;;check each list at this level
      (loop
       for item  in nested-lists
       do
       ;;for each sublist, check set-nth
       ;;(afout 'out (format nil "KEY OUTER-LOOP key= ~A~% item= ~A~%new-spec-lists= ~A~%" key item new-spec-lists))
       (cond
        ((listp item)
         (setf  list-length (list-length item))     
         (unless (>= set-nth  list-length))
         (setf match-item (nth set-nth item))
         ;;(afout 'out (format  nil "OUTER LOOP TESTING key= ~A  match-item= ~A~%  IN find-key-value-in-lists" key match-item))
         (cond
          ((my-equal key match-item)
           ;;was (or (equal key match-item) (if (stringp match-item) (string-equal key match-item)))
           ;;;yyy
           (multiple-value-setq (new-keylist  new-nested-lists return-value 
                                              final-spec-list old-keylist)
               (set-key-value-in-nested-lists new-value new-spec-lists item
                                              :append-item append-item 
                                              :final-key-inside-keylist-p final-key-inside-keylist-p 
                                              :subst-new-keylist-p subst-new-keylist-p
                                              :no-return-extra-p no-return-extra-p))
           (setf return-nested-lists (append return-nested-lists (list new-nested-lists)))

           ;;(afout 'out (format nil "ON RECURSE RETURN return-value= ~A~% return-key=~A~%" return-value return-key))
        ;;was   (return)
           )
          (t (setf return-nested-lists (append return-nested-lists (list item)))))
         ;;end listp item
         )
        (t (setf return-nested-lists (append return-nested-lists (list item)))))
       ;;end loop, equal new-spec-lists
       ))
     ;;IF TOP LEVEL NONE-OF-ABOVE
     (t (setf return-nested-lists (append return-nested-lists (list item)))))
    ;;end find-key-value-in-lists

;;UNQUOTE THIS AFTER DEBUGGING
#|    (if (null return-value)
        (setf new-keylist nil
              return-nested-lists nil))|#
    ;;end set-key-value-in-nested-lists
    (values new-keylist return-nested-lists  return-value  final-spec-list old-keylist) 
    ))|#

;;DELETE WHEN ABOVE DEBUGGED
#|(defun set-key-value-in-nested-lists (new-value key-spec-lists nested-lists 
                                                &key append-item final-key-inside-keylist-p 
                                                subst-new-keylist-p no-return-extra-p) 
  "In U-lists.lisp, SETS key-value . RETURNS (values new-keylist return-nested-lists return-value  final-spec-list) that matches key-spec. The spec-lists is a list of 2 item spec lists. (key set-nth)  If  key = T, searches entire list of lists for key  If set-nth = T, searches entire sublist for the key.. Extra items are extra non-list items on all level lists that might contain important info. Can process infinite? levels of key lists. Can match with equal or string-equal automatically. The spec-lists are arranged from outer-most list to inner-most list. final-key-inside-keylist-p means that the last key is inside a list--so items in ourer list won't cause a false match. append-item is appended to the end of the list containing the final key. NOTE: final keylists should contain a value--even if nil in the set-nth position so it can be replaced UNLESS the new-value is a new keylist and subst-new-keylist-p = t. If value is in place, replaces it with new-value."
  (let*
      ((new-keylist)
       (return-key)
       (return-value)
       (return-nested-lists)
       (old-keylist)
       (spec-list (car key-spec-lists))
       (key (first spec-list))
       (set-nth (second spec-list))
       (new-spec-lists (cdr key-spec-lists))
       (final-spec-list)
       (nested-list-length (list-length nested-lists))
       (new-nested-lists)
       )
    (unless  set-nth (setf set-nth 0))
    (cond
     ;;SPEC IS TO SEARCH EVERY LIST AT THIS LEVEL
     ((or (equal key t) (equal set-nth t))
      ;;(afout 'out (format nil "NEW CALL TO T  key= ~A~% (car of list= ~A~%" key (car nested-lists)))   
      (loop
       for item  in nested-lists
       ;;with new-value2 
       with new-nested-lists 
       do
       ;;(afout 'out (format nil "T OUTER-LOOP key= ~A~%  item= ~A~%" key item))
       ;;test to see what the spec-list indicates
       (cond
        ((and item (listp item))

         ;;note: this may call other recursive calls
         (multiple-value-setq (new-keylist  new-nested-lists return-value 
                                            final-spec-list old-keylist)
             (set-key-value-in-nested-lists new-value new-spec-lists item
                                            :append-item append-item 
                                            :final-key-inside-keylist-p final-key-inside-keylist-p 
                                            :subst-new-keylist-p subst-new-keylist-p
                                            :no-return-extra-p no-return-extra-p
                                            ))
         (setf return-nested-lists (append return-nested-lists (list  new-nested-lists)))
         )
        ;;may be non-list items such as other keys providing info re: found outer lis.
        (t (setf return-nested-lists (append return-nested-lists (list  item))))) 
       ;;end loop set-nth = t
       ))
     ;;AT LOWEST LIST, SEARCH FOR KEY-VALUE
     ((and (null new-spec-lists) key)
      (loop
       for item-lo in nested-lists
       for n from 0 to nested-list-length
       with match-item-lo 
       with new-value-lo  
       with length-item-lo 
       do
       ;;(afout 'out (format  nil "1 LOWEST LEVEL TEST key= ~A  match-item-lo= ~A~% set-nth= ~A~%nested-lists= ~A~% IN find-key-value-in-lists " key match-item-lo set-nth nested-lists))
       (cond
        ;;SEARCH INSIDE A NESTED LIST -- NOT IN OUTER LIST FOR KEY
        ((and (listp item-lo) (null final-key-inside-keylist-p))
         (setf  length-item-lo (list-length item-lo))       
         (unless (>= set-nth  length-item-lo))
         (setf match-item-lo (nth set-nth item-lo))
         (cond
          ((my-equal key match-item-lo)
       ;;was (or (equal key match-item-lo) (if (stringp match-item-lo) (string-equal key match-item-lo)))

           ;;(afout 'out (format  nil "2 LOWEST LEVEL TEST key= ~A  match-item-lo= ~A~% set-nth= ~A~%nested-lists= ~A~% item-lo= ~A~%IN find-key-value-in-lists " key match-item-lo set-nth nested-lists item-lo))

           ;;use RETURN-VALUE because will be NIL IF NO MATCH (and return NIL)
           ;;  also set old-keylist to item-lo
           (setf return-value new-value
                 old-keylist item-lo)
           ;;PUT NEW-VALUE IN PROPER PLACE AND/OR ADD APPEND-ITEM-LO
           ;;do I replace an item-lo or just add it.
           (cond
            ;;if subst-new-keylist-p subst new-value (a list) for old keylist.
            ((and subst-new-keylist-p (listp new-value))
             (setf new-keylist new-value))
            ;;if  set-nth, replace that item-lo with new-value
            (set-nth
             ;;problem with replace modifying item-lo permenantly and therefore old-keylist
             (setf  new-value-lo (list new-value)
                    new-keylist (replace-list item-lo (+ set-nth 1) new-value-lo)))
            (t nil))  ;;was (setf new-keylist nil)))
           
           ;;if append-item (= a value) append to the new-keylist
           (if append-item
               (setf final-spec-list (append final-spec-list (list append-item)))) 
           ;;set final return items
           (setf final-spec-list (list key set-nth)
                 return-nested-lists (replace-list nested-lists n (list new-keylist)))
         ;;  (return)
           )
          (t nil))
         ;;end listp item-lo
         )
        ;;if not list, search current list for item-lo, just check to see if item-lo = key
        ;;problem if an item-lo matches key but real key is inside a later list
        ;;SEARCHES OUTER LIST FOR KEY MATCH--NOT AN INNER LIST
        (final-key-inside-keylist-p
         (cond
          ((my-equal key item-lo)
           ;;was (or (equal key item-lo) (if (stringp item-lo) (string-equal key item-lo)))
           ;;PUT NEW-VALUE IN PROPER PLACE AND/OR ADD APPEND-ITEM-LO
           ;;do I replace an item-lo or just add it.
           (cond
            ;;if subst-new-keylist-p subst new-value (a list) for old keylist.
            ((and subst-new-keylist-p (listp new-value))
             (setf new-keylist new-value))
            ;;if  set-nth, replace that item-lo with new-value
            (set-nth
             (setf  new-value-lo (list new-value)
                    ;;xxx causes a problem perm changing item-lo value??
                    new-keylist (replace-list item-lo    (+ set-nth 1) new-value-lo)))
#|                    (replace item-lo new-value-lo :start1
                                         (+ set-nth 1) :end1 (+ set-nth 2))))|#
            (t (setf new-keylist nil)))

           ;;if append-item (= a value) append to the new-keylist
           (if append-item
               (setf new-keylist (append new-keylist (list append-item))))
           ;;added
           (setf return-nested-lists (append return-nested-lists (list  new-keylist)))
           (return))
          (t  nil)))    ;; (setf new-nested-list (append new-nested-list (list item-lo))))))
        (t nil) ;; (setf new-nested-list (append new-nested-list (list item-lo))))
        ;;end cond, lo loop, equal null new-spec-lists
        )))
     ;;SPEC IS TO SEARCH THIS LEVEL (not last level) BY KEY at SET-NTH
     ((and  new-spec-lists  key)
      ;;(afout 'out (format nil "OUTER-LOOP SEARCH new-spec-lists= ~A~%" new-spec-lists))         
      ;;check each list at this level
      (loop
       for item  in nested-lists
       with list-length  
       with match-item 
       with new-nested-lists 
       do
       ;;for each sublist, check set-nth
       ;;(afout 'out (format nil "KEY OUTER-LOOP key= ~A~% item= ~A~%new-spec-lists= ~A~%" key item new-spec-lists))
       (cond
        ((listp item)
         (setf  list-length (list-length item))     
         (unless (>= set-nth  list-length))
         (setf match-item (nth set-nth item))
         ;;(afout 'out (format  nil "OUTER LOOP TESTING key= ~A  match-item= ~A~%  IN find-key-value-in-lists" key match-item))
         (cond
          ((my-equal key match-item)
           ;;was (or (equal key match-item) (if (stringp match-item) (string-equal key match-item)))
           ;;;yyy
           (multiple-value-setq (new-keylist  new-nested-lists return-value 
                                              final-spec-list old-keylist)
               (set-key-value-in-nested-lists new-value new-spec-lists item
                                              :append-item append-item 
                                              :final-key-inside-keylist-p final-key-inside-keylist-p 
                                              :subst-new-keylist-p subst-new-keylist-p
                                              :no-return-extra-p no-return-extra-p))
           (setf return-nested-lists (append return-nested-lists (list new-nested-lists)))

           ;;(afout 'out (format nil "ON RECURSE RETURN return-value= ~A~% return-key=~A~%" return-value return-key))
        ;;was   (return)
           )
          (t (setf return-nested-lists (append return-nested-lists (list item)))))
         ;;end listp item
         )
        (t (setf return-nested-lists (append return-nested-lists (list item)))))
       ;;end loop, equal new-spec-lists
       ))
     ;;IF TOP LEVEL NONE-OF-ABOVE
     (t (setf return-nested-lists (append return-nested-lists (list item)))))
    ;;end find-key-value-in-lists

;;UNQUOTE THIS AFTER DEBUGGING
#|    (if (null return-value)
        (setf new-keylist nil
              return-nested-lists nil))|#
    ;;end set-key-value-in-nested-lists
    (values new-keylist return-nested-lists  return-value  final-spec-list old-keylist) 
    ))|#

;;ORIGINAL-OLDER VERSION
;;GET-KEY-VALUE
;;
;;ddd
#|(defun get-key-value  (key list)
  "In U-lists, searches a flat list for a key, RETURNS (values result key) the value following key if found, nil if not. Uses my-equal which will even match symbols and strings. NOTE: Differs from GETF which requires a keyword. Any string or symbol can be used for key in get-key-value."
  (let
      ((result)
       (n 0)
       (keylist)
       )
    (unless (null (listp list))
    (loop
     for item in list
     do
     (incf n)
     (cond
      ((my-equal key item)
        (setf result (nth n list))
        (return))
      (t nil))
     ;;end loop
     )
    (values result key)
    ;;end unless,let, get-key-value
    )))|#
;;TEST
;;  (get-key-value "this" '(a b c this (1 2 3) d e f))  =  (1 2 3)   "this"
;;  (getf  '(a b c this (1 2 3) d e f) "this") = NIL
;;  (getf  '(a b c this (1 2 3) d e f) 'this) = nil
;;  (getf  '(a b c :this (1 2 3) d e f) :this) = nil
;;  (getf  '(a b c :this (1 2 3) d e f)  1) = NIL


;;MAKE-COMBOS
;;2016
;;ddd

#| WORKS, BUT DOESN'T MAKE ALL POSSIBLE COMBOS
(defun make-combos (items items-per-combo &key (first-n-items 'all)
                          (return-n-combos-p T))
  "In U-lists. Creates a list of all combinations of items taken items-per-combo at a time for first num-sets. RETURNS (values all-combos all-rest-items n-combos) .  Only take FIRST-N-ITEMS of the items to create all-combos. RETURN-N-COMBOS-P returns n-combos as last value."
  (let
      ((combo)
       (combos-list)
       (n-combo)
       (rest-items)
       (all-rest-items)
       (all-combos)
       (n-combos)
       )

    (loop
     for item1 in items
     for n from 1 to (list-length items)
     do
     (when 
         (or (equal first-n-items 'all)
             (>= first-n-items n))

       (setf combo (list item1)
             rest-items (nthcdr n items))
       (multiple-value-bind (combos rest)
           (append-items-to-list combo rest-items
                                 :items-per-append (- items-per-combo 1))
         (setf all-combos (append all-combos combos)
               all-rest-items (append all-rest-items (list rest)))

         ;;end when,mvb,loop
         )))

    (cond
     (return-n-combos-p 
      (setf n-combos (list-length all-combos))
      (values all-combos all-rest-items  n-combos))
     (t (values all-combos all-rest-items )))
    ;;end let, make-combos
    ))|#

