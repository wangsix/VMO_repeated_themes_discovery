

;(in-package :common-lisp-user)
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "File conversion")
   :name "csv-files"
   :type "lisp")
  *lisp-code-root*))
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "File conversion")
   :name "director-musices"
   :type "lisp")
  *lisp-code-root*))
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "File conversion")
   :name "midi-save"
   :type "lisp")
  *lisp-code-root*))
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "Pattern rating")
   :name "projection"
   :type "lisp")
  *lisp-code-root*))
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "Pattern metrics")
   :name "robust-metrics"
   :type "lisp")
  *lisp-code-root*))
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "Maths foundation")
   :name "set-operations"
   :type "lisp")
  *lisp-code-root*))
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "File conversion")
   :name "text-files"
   :type "lisp")
  *lisp-code-root*))

#| Load the piece and the pattern. |#
(progn
  (setq
   *path&name*
   (merge-pathnames
    (make-pathname
     :directory
     '(:relative "gibbonsSilverSwan1612" "monophonic"))
    *music-data-root*))
  (setq
   *pattname*
   (merge-pathnames
    (make-pathname
     :directory
     '(:relative
       "gibbonsSilverSwan1612" "monophonic" "repeatedPatterns"
       "tomCollins" "C"))
    *music-data-root*))
  (setq
   D
   (read-from-file
    (merge-pathnames
     (make-pathname
      :directory '(:relative "lisp") :name "silverswan"
      :type "txt")
     *path&name*)))
  (setq
   D1 (orthogonal-projection-unique-equalp D '(1 1 0 0 0)))
  (setq
   D2 (orthogonal-projection-unique-equalp D '(1 0 1 0 0)))
  (setq
   P
   (read-from-file
    (merge-pathnames
     (make-pathname
      :directory '(:relative "lisp") :name "silverswan"
      :type "txt")
     *pattname*)))
  (setq
   P1 (orthogonal-projection-unique-equalp P '(1 1 0 0 0)))
  (setq
   P2 (orthogonal-projection-unique-equalp P '(1 0 1 0 0)))
  "Yes!")

#| Define all occurrences. |#
(setq *scale* 1000)
(setq T2 (translators-of-pattern-in-dataset P2 D2))
(setq m (length T2))
(loop for i from 0 to (- m 1)
  do
  (progn
    #| occi. Save as csv, lisp, and midi. |#
    (setq
     Idx
     (indices-lookup
      (translation P2 (nth i T2)) D '(1 0 1 0 0)))
    #| Definition of ontime-pitch pairs. |#
    (setq
     Q
     (mapcar
      #'(lambda (x)
          (if (> (length x) 1)
            ;"Datapoint refers to multiple indices."
            (nth-list '(0 1) (nth (first x) D))
            (nth-list '(0 1) (nth (first x) D)))) Idx))
    #| Definition for MIDI file. |#
    (setq
     R
     (mapcar
      #'(lambda (x)
          (if (> (length x) 1)
            ;"Datapoint refers to multiple indices."
            (nth (first x) D)
            (nth (first x) D))) Idx))
    (setq
     *fname*
     (concatenate 'string "occ" (write-to-string (+ i 1))))
    #| Save MIDI file (have to alter any anacrusis). |#
    (saveit
     (merge-pathnames
      (make-pathname
       :directory
       '(:relative "occurrences" "midi") :name *fname*
       :type "midi")
      *pattname*)
     (modify-to-check-dataset
      (mapcar
       #'(lambda (x)
           (append
            (list (- (first x) (first (first R))))
            (rest x))) R)
      *scale*))
    #| Save lisp-format text file. |#
    (write-to-file
     Q
     (merge-pathnames
      (make-pathname
       :directory
       '(:relative "occurrences" "lisp") :name *fname*
       :type "txt")
      *pattname*))
    #| Save CSV. |#
    (dataset2csv
     (merge-pathnames
      (make-pathname
       :directory
       '(:relative "occurrences" "lisp") :name *fname*
       :type "txt")
      *pattname*)
     (merge-pathnames
      (make-pathname
       :directory
       '(:relative "occurrences" "csv") :name *fname*
       :type "csv")
      *pattname*))))
