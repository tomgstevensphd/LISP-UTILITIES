;;******************************** U-lists.lisp ****************************
;;

;;NOTE: RANDOMIZE WORKS WELL, BUT THE 'LESSP AND 'GREATERP ARE
;; UNRELIABLE--SOMETHING TO DO WITH HOW THE STRINGS ARE COMPARED???
#|
(defun testsl ()
  (let
      ;;must use flat lists
      ((sublists '((x 1 2)(f 3)(2 6 5)(b 3 2) (99 6 2)))
                  )
  (organize-sublists sublists  :randomize t))) ;; :ascend-sort t))) ;; :randomize t))) ;; :descend-alpha-sort t))) ;; :randomize t)))
|#
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
     (:randomize   ;;works
      (dolist (sublist sublists)
        (setf n (random list-length)
              sublist (nth n sublists)
              sublists (remove sublist sublists :test 'equal)
              organized-sublists (append organized-sublists (list sublist)))
        (decf list-length)))
     (:descend-sort 
      (setf organized-sublists (sort sublists #'test-lessp :key 'car)))
     (:ascend-sort
      (setf organized-sublists (sort sublists #'test-greaterp :key 'car)))  
     )
    organized-sublists))

;;works (sort '(c  x 99 d 3 f 1) #'test-lessp)

;;(sort '((x 1 2)(f 3)(2 6 5)(b 3 2) (99 6 2))  #'test-lessp :key 'car )
#|
(char "xyz" 0) = #\x
(char  897 0) = error
(char  (format nil "~A" 897) 0) = #\8 
;; works (test-car 'this '(this that))
|#
#| test works
(defun tcl ()
  (test-car-lessp 9 '(5 a b)))|#
;;
#|
(test-lessp 9 (car '(1 21)))
(car '(11 1))
|#
;;ddd
(defun test-lessp (item1 item2)
  "In U-lists.lisp, tests item vs (car list) for any type items for 'lessp"
  (test-any-type item1 item2 'lessp))

;;ddd
(defun test-greaterp (item1 item2)
  "In U-lists.lisp, tests item vs (car list) for any type items for 'greaterp"
  (test-any-type item1 item2 'greaterp))

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
  "In U-lists.lisp, tests item vs (car list) for any type items for 'lessp 'greaterp or 'equalp"
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
        (setf result (string-equal item1-char item2-char)))
       (t nil))
      ))
    result
    ))

#|
;;works
(defun testflt ()
  (flatten-list-tree '((a b c) (d (e f (g)) h) (i (j) k))))  
=> FINAL, flat-list= ((A B C) (D) (E F) (G H) (I) (J K))
 flat-sublist= (J K)
 simple-list= (A B C D E F G H I J K)
|#
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


;; hhh ***************************** HELP ****************************8
;;


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
