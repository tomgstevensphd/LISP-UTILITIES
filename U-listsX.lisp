;;******************************** U-lists.lisp ****************************
;;
;;SSS NOTE:  INSERT VALUE EITHER 1.AFTER INNER KEY, 2. NTH IN LIST, OR 3. APPEND VALUE TO LIST CONTAINING KEY=FIRST
;;(KEY  XXX :KEY VALUE  ...) OR (KEY .. NTH= VALUE..) OR (KEY .. (APPEND VALUE)
(defun set-key-value-in-nested-lists (new-value key-spec-lists nested-lists 
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
       (nested-list-length (list-length nested-lists))
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
    ))

;;tests

;;  (progn (setf out nil) (set-key-value-in-nested-lists 'this-value '((a 0)(b 0)(c 0)) '( (s 1 2 (t 3 4 (u 5 6 7 )))(a (n1 2) (b (mm 33 ) (c 1 2 3  (l i s t )) (nn 22)) ( extra ))))  )
;;works, returns 1- (C (THIS-VALUE) 2 3 (L I S T))  2-  ((S 1 2 (T 3 4 (U 5 6 7))) (A (N1 2) (B (MM 33) ((C (THIS-VALUE) 2 3 (L I S T))) (NN 22)) (EXTRA)))  3-  THIS-VALUE  4-  (C 0)    5- (C 1 2 3 (L I S T))
;;
;;  (progn (setf out nil)  (set-key-value-in-nested-lists 'this-value  '(("iAcademicMotivation.java" 1)( "acmESOCSTudy" 0)) '((PC-INSTANCES  "iAcademicMotivation.java"      ("[]questionInstancesArray1)")      ("acmNDROPcourses" "30" "acmNDROPcoursesQ" "int" "FrAnswerPanel.LikeMe7" "questionInstancesArray" "frameTitle" "frameDimWidth" "frameDimHeight")      ("acmESOCSTudy" "3" "acmESOCSTudyQ" "int" "FrAnswerPanel.LikeMe7" "questionInstancesArray" "frameTitle" "frameDimWidth" "frameDimHeight"))))) ;;(fout out)) ;; :return-list-p  t))
;;works, returns 1-("acmESOCSTudy" (THIS-VALUE) "acmESOCSTudyQ" "int" "FrAnswerPanel.LikeMe7" "questionInstancesArray" "frameTitle" "frameDimWidth" "frameDimHeight")   2-((PC-INSTANCES "iAcademicMotivation.java" ("[]questionInstancesArray1)") ("acmNDROPcourses" "30" "acmNDROPcoursesQ" "int" "FrAnswerPanel.LikeMe7" "questionInstancesArray" "frameTitle" "frameDimWidth" "frameDimHeight") (("acmESOCSTudy" (THIS-VALUE) "acmESOCSTudyQ" "int" "FrAnswerPanel.LikeMe7" "questionInstancesArray" "frameTitle" "frameDimWidth" "frameDimHeight"))))   3- THIS-VALUE  4- ("acmESOCSTudy" 0)   5-("acmESOCSTudy" "3" "acmESOCSTudyQ" "int" "FrAnswerPanel.LikeMe7" "questionInstancesArray" "frameTitle" "frameDimWidth" "frameDimHeight")
;;
;;  (set-key-value-in-nested-lists 'this '((5 0))  '((1 0 (1 2 A)) (5 1 (1 6 (QUOTE (A B))))))
;; returns (THIS 1 (1 6 (QUOTE (A B))))  ((THIS 1 (1 6 (QUOTE (A B))))) (THIS)  (5 0) (5 1 (1 6 (QUOTE (A B))))
;;NOW RETURNS (5 (THIS) (1 6 (QUOTE (A B))))
;;((1 0 (1 2 A)) ((5 (THIS) (1 6 (QUOTE (A B))))))    THIS   (5 0)   (5 1 (1 6 (QUOTE (A B))))

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


(defun get-key-list (key list)
  "In U-lists, searches a flat list for a sublist that begins with key, RETURNS (values list  key) if found, nil if not. Uses my-equal which will even match symbols and strings."
  (let
      ((result)
       )
    (loop
     for sublist in list
     with first-item 
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


(defun get-key-value  (key list)
  "In U-lists, searches a flat list for a key, RETURNS (values result key) the value following key if found, nil if not. Uses my-equal which will even match symbols and strings. NOTE: Differs from GETF which requires a keyword. Any string or symbol can be used for key in get-key-value."
  (let
      ((result)
       (n 0)
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
    )))
;;TEST
;;  (get-key-value "this" '(a b c this (1 2 3) d e f))  =  (1 2 3)   "this"
;;  (getf  '(a b c this (1 2 3) d e f) "this") 
;;  (getf  '(a b c this (1 2 3) d e f) 'this) 
;;  (getf  '(a b c :this (1 2 3) d e f) :this) 


;;GET-KEY-VALUE-IN-NESTED-LISTS
;; gets values in all sorts of lists and nested lists
;;
;;ddd
(defun get-key-value-in-nested-lists (key-spec-lists nested-lists 
                                                      &key return-list-p no-return-extra-p
                                                      final-key-inside-keylist-p) ;; return-nth (now in spec)
  "In U-lists.lisp, RETURNS first value that matches key (values return-value key extra-return-nth-items extra-items). The spec-lists is a list of 2 or 3 item spec lists. (key find-nth return-nth)  If  key = T, searches entire list of lists for key  If find-nth = T, searches entire sublist for the key.  If return-list-p, RETURNS entire key-list as return-value. Extra items are extra non-list items on all level lists that might contain important info. Can process infinite? levels of key lists. Can match with equal or string-equal automatically. The spec-lists are arranged from outer-most list to inner-most list. final-key-inside-keylist-p means that the last key is inside a list--so items in ourer list won't cause a false match."
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
       with extra-items1 
       with extra-return-nth-items1 
       with current-level-return-extra-value 
       do
       ;;(afout 'out (format nil "T OUTER-LOOP new-spec-lists= ~S~%  item= ~S~%" new-spec-lists item))
       ;;Should it return a  value from this level (eg name))
       (if (and return-nth (listp item))
           (setf current-level-return-extra-value (nth return-nth item)))

       ;;test to see what the spec-list indicates
       (cond
        ((and item (listp item))
         ;;note: this may call other recursive calls
         (multiple-value-setq (return-value return-key extra-return-nth-items1 extra-items1)
             (get-key-value-in-nested-lists   new-spec-lists  item
                                              :return-list-p return-list-p
                                              #|:return-nth return-nth|# :no-return-extra-p no-return-extra-p
                                              :final-key-inside-keylist-p final-key-inside-keylist-p))

         ;;these are the extra items wnt to return (bec inside of target containing list)         
         (cond
          (return-key  
           #|(cond
            (return-list-p
               (setf return-value item))
            (t (setf return-value return-value)))|#
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
       with match-item 
       with extra-items1 
       with length-item 
       do
       ;;(afout 'out (format  nil "1 LOWEST LEVEL TEST key= ~S  match-item= ~S~% find-nth= ~S~% " key match-item find-nth ))
       ;;;;(afout 'out (format  nil "1 LOWEST LEVEL TEST key= ~S  match-item= ~S~% find-nth= ~S~%nested-lists= ~S~% IN find-key-value-in-lists " key match-item find-nth nested-lists))

       (cond
        ;;SEARCH INSIDE A NESTED LIST -- NOT IN OUTER LIST FOR KEY
        ((and (listp item) (null final-key-inside-keylist-p))
         (setf  length-item (list-length item))       
         (unless (>= find-nth  length-item))
         (setf match-item (nth find-nth item))
         (cond
          ((my-equal key match-item)
           ;;was (or (equal key match-item) (unless (or (numberp key)(number (stringp match-item) (string-equal key match-item)))
           ;; (equal (string 'this) "this")
           ;;(afout 'out (format  nil "2 LOWEST LEVEL TEST key= ~S  match-item= ~S~% find-nth= ~S~% IN find-key-value-in-lists " key match-item find-nth))
 ;;;;(afout 'out (format  nil "2 LOWEST LEVEL TEST key= ~S  match-item= ~S~% find-nth= ~S~%nested-lists= ~S~% item= ~S~%IN find-key-value-in-lists " key match-item find-nth nested-lists item))
           (cond
            (return-list-p
             (setf return-value item
                   return-key match-item))
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
      ;;check each list at this level
      (loop
       for item  in nested-lists
       ;;  with new-spec-list1
       with extra-return-nth-items1 
       with extra-items1 
       with list-length  
       with match-item 
       do
       ;;for each sublist, check find-nth
       ;;(afout 'out (format nil "KEY OUTER-LOOP key= ~S~% item= ~S~%new-spec-lists= ~S~%" key item new-spec-lists))
       (cond
        ((listp item)
         (setf  list-length (list-length item))     
         (unless (>= find-nth  list-length))
         (setf match-item (nth find-nth item))
         ;;(afout 'out (format  nil "OUTER LOOP TESTING key= ~S  match-item= ~S~%  IN find-key-value-in-lists" key match-item))
         (cond
          ((my-equal key match-item)
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

;;yyy
;; (progn (setf out nil) (get-key-value-in-nested-lists '((t 0)("smtsdevelopment" 0)) *all-shaq-pc-instances :return-list-p t))
;;  (progn (setf out nil)  (get-key-value-in-nested-lists  '(("iAcademicMotivation.java" 1)( "acmESOCSTudy" 0)) '((PC-INSTANCES  "iAcademicMotivation.java"      ("[]questionInstancesArray1)")      ("acmNDROPcourses" "30" "acmNDROPcoursesQ" "int" "FrAnswerPanel.LikeMe7" "questionInstancesArray" "frameTitle" "frameDimWidth" "frameDimHeight")      ("acmESOCSTudy" "3" "acmESOCSTudyQ" "int" "FrAnswerPanel.LikeMe7" "questionInstancesArray" "frameTitle" "frameDimWidth" "frameDimHeight"))))) ;;(fout out)) ;; :return-list-p  t))
;;works, returns"acmESOCSTudyQ"  "acmESOCSTudy"  ((PC-INSTANCES "iAcademicMotivation.java"))
;;
;; (progn (setf out nil) (get-key-value-in-nested-lists (list '(T 0)'(smtExercizeQ 0)) *testq-vars3)));; (fout out) *all-shaq-questions)))
;; (progn (setf out nil) (get-key-value-in-nested-lists '((T 0) (thvUncondCareQ 0)) *all-shaq-questions))
;;(get-key-value-in-nested-lists '((T 0) (ugoals 0)) *shaq-question-variable-lists :return-list-p t)

;;  (get-key-value-in-nested-lists '((5 0))  '((1 0 (1 2 A)) (5 1 (1 6 (QUOTE (A B))))))
;;  (get-key-value-in-nested-lists '((this 0))  '((a b)(c (1 2))(this (3 4 5))(x y)) :return-nth 1)
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
(defun print-list (list &key stream no-newline-p incl-quotes-p incl-parens-p incl-label sort-string-test (num-spaces 1))
  "in U-lists.lisp, Prints each list item on newline (or not) with quotes (or not) for strings (only). If sort-string-test, use it (eg. #'sting<) to sort the list. ALSO see FORMAT-STRING-LIST--generally a better choice."
  (let
      ((string "")
       (item-string "")
       (space " " )
       )
    (cond
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
   with space1 
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
;;  (print-list '("a" "b" C "d") :no-newline-p t :num-spaces 0)
;;  works = "abCd"
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
;;  (my-lessp 7  3) 
;;  (my-lessp 3 7) = T
;;  (my-lessp "THIS" "apple") 
;;  (my-lessp "APPLE" "this")  = 0
;;  (my-lessp 'this 'apple) 
;;  (my-lessp 'apple 'this) = 0


;;MY-GREATERP
;;
;;ddd
(defun my-greaterp (item1 item2)
  "In U-lists, items can be numbers, strings, or symbols. If both numbers, compares using >, if symbols, converts to strings, then compares by string-greaterp."
  (let
      ((result)
       (item1-str)
       (item2-str)
       )
    (cond
     ((and (null item1)(null item2)) nil)
     ((null item2)(setf result item1))
     ((null item1)(setf result item2))
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
;; ;; (my-greaterp nil 'a ) = A
;; (my-greaterp nil nil) 
           


;;MY-SORT-LISTS
;;
;;ddd
(defun my-sort-lists (nth lists  &key ascending-p from-end) 
  "In U-lists.  Sorts by largest item first only.  RETURNS (values descending-lists ascending-lists)  ascending-lists  unless ASCENDING-P. If from-end, counts from end and returns both lists."
  (let
      ((descending-lists)
       (ascending-lists)
       (ordered-list)
       (temp-list)
       )
   ;;no (if from-end   (setf lists (reverse lists)))

   (loop
    for list in lists
    with length-list = (list-length list)
    with greaterp 
    with first-lists 
    with item-deleted-p 
    with last-item 
    do
    ;;(setf  length-list (list-length list))
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
    ;;(afout 'out (format nil "1 Nth= ~A~%  list= ~A~% descending-lists= ~A~%first-lists= ~A~%last-item= ~A~%" nth list descending-lists first-lists last-item))
    ;;end loop
    )
    (cond
     (ascending-p
      (setf ascending-lists (reverse descending-lists)))
     (t nil))

    (values descending-lists ascending-lists)
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


;;  (my-sort-lists 0 '((("life_goals_and_meaning.htm" "Life Goals-Values" "Life Goals and Meaning ") ("test_anxiety.htm"  "Perform Anxiety" "Reducing Test or Performance Anxiety") .5)(("assert req.html" "Assertive Request" "How to Make an Assertive Request for a Behavior Change" )("c14-lisn.htm" "Intimacy" "Assertive Communication Skills to Create Understanding and Intimacy" ) .2)) :from-end t)
;; works

;;
;;DO IN SEPARATE FUNCTION
;;TEST delete-duplicates-nth    delete-largest-p
;;  (my-sort-lists 0 '((a 3  x)(m 1 mm)(z 9 l)(m 4 llm)(a  5  nn))) 
;;works = ((Z 9 L) (M 1 MM) (M 4 LLM) (A 3 X) (A 5 NN))
;;  (my-sort-lists 0 '((a 3  x)(m 1 mm)(z 9 l)(m 4 llm)(a  5  nn)) :ascending-p t) 
;; works = ((Z 9 L) (M 1 MM) (M 4 LLM) (A 3 X) (A 5 NN)) ((A 5 NN) (A 3 X) (M 4 LLM) (M 1 MM) (Z 9 L))



;;REMOVE-NTH-DUPLICATES
;;
;;ddd
(defun remove-nth-duplicates (nth lists &key  delete-largest-p)
  "In U-lists.  DELETE-DUPLICATES-NTH (if duplicate by delete-duplicates-nth item, deletes the item with smallest nth item (unless delete-largest-p). RETURNS (values new-list duplicates-list."
  (let
      ((new-list)
       (sorted-list.)
       )
    (loop
     for list in lists
     with greaterp 
     with first-lists 
     with item-deleted-p 
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
  "In U-lists RETURNS (values sorted-list result) where result is T if the nth item in list1 is my-greater than the one in list2."
  (let
      ((sorted-list)
       (result)
       (nth1 nth)
       (nth2 nth)
       )
    (when from-end
      (setf nth1 (- (list-length list1) nth 1)
            nth2 (- (list-length list2) nth 1)))

    (cond
     ((my-greaterp (nth nth1 list1) (nth nth2 list2))
      (setf sorted-list (list list1 list2)
            result T))
     (t (setf sorted-list (list list2 list1)
              result NIL)))
    (values sorted-list result)
  ;;end let, my-list-nth-greaterp
  ))
;;; TEST
;; (my-list-nth-greaterp 1 '(A 2 XX) '(B 1 MM N)) = ((A 2 XX) (B 1 MM N)) T
;; (my-list-nth-greaterp 0 '(A 2 XX) '(B 1 MM N)) = ((B 1 MM N) (A 2 XX)) NIL
;; (my-list-nth-greaterp 2 '(A 2 XX) '(B 1 MM y)) = ((A 2 XX) (B 1 MM Y))  T
;; (my-list-nth-greaterp 0 '(A 2 XX) '(B 1 MM Y) :from-end t) = ((B 1 MM Y) (A 2 XX)) NIL
;;;; if one is nil
;;(my-list-nth-greaterp 2 '(A 2 XX) '(B 1 nil N)) = ((A 2 XX) (B 1 NIL N))
T

;;MY-COMPARE-LIST-NTH
;;
;;ddd
(defun my-compare-list-nth (nth list1 list2 &key (test 'my-greaterp))
  "In U-lists RETURNS (values sorted-list result) where result is T if the nth item in list1 is TEST (usually 'my-greaterp or 'my-lessp than the one in list2."
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
          )
       (t (setf return-list list))))
     ;;end outer loop
     )
    (values return-list duplicates-list)
    ;;end let, delete-all-duplicate-nth-lists
    ))
;;TEST
;;SSSS START HERE DEBUGGING USE TO DELETE EXTRA HELP-LINKS
;;  (delete-all-duplicate-nth-lists 1 '((x a  3)(a a 4)(b u 3)(m d 6)(x m 0)))
        
      




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
        (setf ordered-list (my-list-nth-lessp compare-nth list1 list2))
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
  "In U-lists.lisp, compares the nth-item1 in each sublist in the  list-of-lists1 (OR if nth-item1  to plain items) to each of nth-item2 in sublists (or simple items) of list-of-lists2. RETURNS (values match-items matched-sublists unmatched-items unmatched-sublists). Compares using 'equal  If items 1 and 2 are strings, can use fuzzy-match-p. n-matched-cutoff  = num of chars must have in common to specifiy a fuzzy match.  If NIL, test = 'equal is used. auto-cutoff default is 0.6 means 0.6 of the length of the item1 is the cutoff. Must be betw 0 < 1.0.  If want auto-cutoff default must set it to T or 0.6. n-match-length specifies that a substring match of at least that many chars MUST happen or no match."
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
              (fuzzy-string-matcher  item1 item2 :auto-cutoff auto-cutoff
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
       ((equal item old-item)
        (cond
         ((null delete-instead-p)
          (setf  item-found-p t
                 new-list (append new-list (list new-item))))
         (t nil)))
       (t (setf new-list (append new-list (list item)))))
      )
   (values  new-list item-found-p)
    ))
;;test
#|(defun testrli ()
 ;; (replace-list-item 'x NIL '(a b c d NIL e) :delete-instead-p t) ;;works, returns (A B C D E)
  (replace-list-item 'x "" '(a b c d "" e) :delete-instead-p t) ;;works, returns (A B C D E)
  )|#

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
  "In U-lists.lisp, replaces nth item in a list with new-item--item can be sublist or not"
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
;;works
#|(defun testnl ()
  (values
  (replace-list '((a b)(c d)(e f)(g (1 2) h)(i j))  3 '(x y))
  (replace-list '((a b)(c d)(e f)(g (1 2) h)(i j))  3 "this"))
  )|#


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
;; doesn't work (find-keys-and-values-in-list '(2)  '((1 (a b))(2 (c d))))  

;;NOTE: RANDOMIZE WORKS WELL, BUT THE 'LESSP AND 'GREATERP ARE
;; UNRELIABLE--SOMETHING TO DO WITH HOW THE STRINGS ARE COMPARED??


;;FIND-KEYS-IN-LISTS
;;
;;ddd
(defun find-keys-in-lists (key-lists)
  "In U-lists. RETURNS list of keys/cars of all first-order lists in key-lists."
  (let
      ((keys-list)
       )
    (loop
     for list in key-lists
     with key 
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
         )
      ;;eliminate extra quote--added for use with read
      (if (and (listp nested-list) (equal (car nested-list) 'quote))
          (setf nested-list (second nested-list)))

      (loop
       for sublist in nested-list
       with new-sublist-string 
       do
       (setf  new-sublist-string  "(")
       (cond
        ((listp sublist)
         (loop
          for item in sublist
          with key2 
          with key2-next 
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





;;ddd
;;works
#|(defun testsk ()
  (setf  xxyy 'this)
  (set-key-to-value-in-plists '((a 7)(b 'this)(c xxyy)))  ;; b = 'this c= xxyy
  (set-key-to-value-in-plists '((d 8)(e 'that)(f xxyy)) :eval-value t)) ;; e= that, f= this|#
;;
(defun set-key-to-value-in-plists (plist-list &key eval-value)
  "In U.lists.lisp, sets key to value eg. '((a 7)(b 'this)....), If eval-value is T, sets the key = (eval value), otherwise sets key = value."
  (let ((key)
        (value)
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
    ))
    


;;SSS START HERE MAKING THIS NON-DISTRUCTIVE, also see Seibel p145 and before on destructive modifs.  It seems to work, but ORGANIZE-SUBLISTS STILL DOESN'T EVEN THO I BASE IT ON THIS FUNCTION
;;MY-SORT
;;  A function to NON-DESTRUCTIVELY sort lists (REPLACES SORT for lists)
;;ddd


(defun my-sort (sequence predicate &key key)
  "In U-lists.lisp, function to NON-DESTRUCTIVELY sorts sequences. REPLACES SORT.Eg.  (my-sort list-of-lists #'test-greaterp  :key 'car) also (my-sort "xylmno" #'char>)"
  (let
      ((new-sequence (copy-seq sequence))
       )
    (setf new-sequence (sort new-sequence predicate :key key))
    (values new-sequence sequence)
    ))
;;works
;;   (my-sort "xylmno" #'char>) ;; returns "yxonml" "xylmno"

#|was only for lists
(defun my-sort (list predicate &key key)
  "In U-lists.lisp, function to NON-DESTRUCTIVELY sorts sequences. REPLACES SORT for lists . Eg.  (my-sort list-of-lists #'test-greaterp  :key 'car) also (my-sort "xylmno" #'char>)"
  (let
      ((new-list (copy-list list))
     ;;  (result)
       )
    (setf new-list (sort new-list predicate :key key))
    (values new-list list)
    ))
;;works
;; (my-sort "xylmno" #'char>) returns "yxonml"
|# 
  
(defun testsl2 ()
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
    ))




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
       (n)
       )
   ;;  (afout 'out (format nil "rest-n= ~A~%" rest-n))
     (loop
      for i from 1 to list-length
      with item 
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
      new-list))
;;test
#|(defun testmr ()
  (setf out nil)
  (let
      ((list '(x a (1 2) (l m n) 99  3))
       )
  ;;  (fout out)
    (my-randomize-lists list)))|#
;;works returns (A (L M N) X 99 (1 2) 3),  ((L M N) 99 X (1 2) A 3), then ((1 2) A 99 (L M N) X 3)



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
(string-lessp  "xyz" "abc") 
(string-lessp  "9" "2") 
(string-lessp   "2" "9") = 0
|#
;;works
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
  (show-text (format nil "FINAL, flat-list= ~A~% simple-list= ~A~%"flat-list  simple-list) 200 t)
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
;;(flat-list-equal '(a b c) '(a d  c))  B
;;(flat-list-equal '(a b c) '(a b  c d))  nil
;;(flat-list-equal '(a b c d e) '(a b  c d))  E
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


;;MAKE-LIST-N-LONG
; 
;;ddd
(defun make-list-n-long ( length list &key default-element second-list)
  "In U-list.lisp, makes a list length long by starting with list and either adding a second-list or default-element (s) or both to fill in extra length.  NIL used otherwise.  If  length < list length, then it cuts off items."
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
(defun find-greatest-max-subtract-list (list-of-lists)
  "In U-lists.lisp, subtracts each first-item from second-item and returns the entire list from the list-of-lists. RETURNS (values results-list max-dif). Used in fuzzy-string-matcher."
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
    (setf  list-n (find-key-value-in-nested-lists max-dif new-results-lists)
           result-list (third (nth n list-of-lists)))
 ;;end find-greatest-max-subtract-list  
 (values result-list max-dif)
 ))
;;test
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
;;works, returns ((A X) (B Y) (C Z) (D NIL)) ;;NOTE EXTRA ITEM 
;;  (join-parallel-list-items '(a b c d) '(x y z l m n o))    
;;works, returns ((A X) (B Y) (C Z) (D L))


;; JOIN-NESTED-PARALLEL-LIST-ITEMS
;;
;;ddd
(defun join-nested-parallel-list-items (nlist1 nlist2 &key nlist1-start  nlist2-start)
  (let
      ((nitem1)
       (nitem2)
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
  "In U-lists, first item= 0. If append-to-short-list-p = T, appends new-item to the end of a list that is too short."
  (let
      ((length-list (list-length list))
       (new-list)
       )
    (loop
     for n from 0 to (- length-list 1)
     for item in list
     do
     (cond
      ((or (= nth length-list)
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
  "In U-lists, first item= 0"
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

;;ddd
(defun append-nth-item-in-2nd-nested-lists (nth new-item double-nested-list) 
  "In U-lists, first item= 0"
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
     



;;APPEND-NO-DUPLICATES
;;
;;ddd
(defun append-no-duplicates (list &rest items)
  "In U-lists.lisp. USE INSTEAD OF APPEND sometimes. Appends items to list that neither the item or ANY subitem match an item in list. Note: subitems of LIST are NOT searched. Uses string-equal for strings. Otherwise works like append."
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
;; (find-item-or-subitem-in-list '(m y) '(a b c 3 "this" 7 (x y)))  
;; (find-item-or-subitem-in-list '(m "this") '(a b c 3 "this" 7 (x y))) = ("this" 7 (X Y))


;;
(defun my-member (item list &key (ignore-case-p T))
   "In U-lists.lisp, REPLACE MEMBER,  tests item whether it is a string, number, symbols, or list to see if it is a member of list. Good for UNKNOWN items."
   (let
       ((result)
        )
     (cond
      ((or (symbolp item)(listp item)(numberp item)(null ignore-case-p))
       (setf result (member item list :test 'equal)))
      ((stringp item)
       (loop
        for element in list
        for n from 0 to (- (length list) 1)
        do
        (cond
         ((stringp element)
          (if (string-equal element item)
              (setf result (nthcdr n list)))
          (return))
         (t nil))
        ;;end loop, clause
        ))
      (t nil))
      result
      ;;end my-member
      ))
;;TEST works
;; (my-member 7  '(a b c 3 "this" 7 (a b c))) = (7 (A B C))
;; (my-member '(x y) '(a b c 3 "this" 7 (x y))) = ((X Y))
;;  (my-member "this"  '(a b c 3 "this" 7 (x y)))





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
;;  (find-symbol "setf")   ;;doesn't work
;;  (find-symbol "newtest")  ;;doesn't work



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


;;xxx
;;6.1.2.2 LOCAL VARIABLE INITIALIZATIONS
#|
When a loop form is executed, the local variables are bound and are initialized to some value. These local variables exist until loop iteration terminates, at which point they cease to exist. Implicit variables are also established by iteration control clauses and the into preposition of accumulation clauses.

The WITH construct initializes variables that are local to a loop. The variables ARE INITIALIZED ONE TIME ONLY. If the OPTIONAL TYPE-SPEC ARGUMENT IS SUPPLIED for the variable var, but there is no related expression to be evaluated, var is initialized to an appropriate default value for its type. For example, for the types t, number, and float, the default values are nil, 0, and 0.0 respectively. The consequences are undefined if a type-spec argument is supplied for var if the related expression returns a value that is not of the supplied type. 
BY DEFAULT, THE WITH CONSTRUCT INITIALIZES VARIABLES SEQUENTIALLY; that is, one variable is assigned a value before the next expression is evaluated. 
However, by using the loop keyword AND to join several with clauses, INITIALIZATIONS CAN BE FORCED TO OCCUR IN PARALLEL; that is, all of the supplied forms are evaluated, and the results are bound to the respective variables simultaneously.
Sequential binding is used when it is desireable for the initialization of some variables to depend on the values of previously bound variables. For example, suppose the variables a, b, and c are to be bound in sequence:

;; These bindings occur in sequence.
 (loop with a = 1 
       with b = (+ a 2) 
       with c = (+ b 3)
       return (list a b c))
=>  (1 3 6)
 
;; These bindings occur in parallel.
 (setq a 5 b 10)
=>  10
 (loop with a = 1
       and b = (+ a 2)
       and c = (+ b 3)
       return (list a b c))
=>  (1 7 13)
 
;; This example shows a SHORTHAND way to declare local variables 
;; that are of DIFFERENT TYPES.
 (loop with (a b c) of-type (float integer float)
       return (format nil "~A ~A ~A" a b c))
=>  "0.0 0 0.0"
 
;; This example shows a SHORTHAND way to declare local variables 
;; that are the SAME TYPE.
 (loop with (a b c) of-type float 
       return (format nil "~A ~A ~A" a b c))
=>  "0.0 0.0 0.0"
|#


;;DELETE OLD
#|
(defun set-key-value-in-nested-lists (new-value key-spec-lists nested-lists 
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
       )
    (unless  set-nth (setf set-nth 0))
    (cond
     ;;SPEC IS TO SEARCH EVERY LIST AT THIS LEVEL
     ((or (equal key t) (equal set-nth t))
      ;;(afout 'out (format nil "NEW CALL TO T  key= ~A~% (car of list= ~A~%" key (car nested-lists)))   
      (loop
       for item  in nested-lists
       with new-value2
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
       for item in nested-lists
       for n from 0 to list-length
       with match-item
       with new-value2
       with length-item
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
    ))
|#