

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
     '(:relative "bachBWV889Fg" "monophonic"))
    *music-data-root*))
  (setq
   *pattname*
   (merge-pathnames
    (make-pathname
     :directory
     '(:relative
       "bachBWV889Fg" "monophonic" "repeatedPatterns"
       "bruhn" "A"))
    *music-data-root*))
  (setq
   D
   (read-from-file
    (merge-pathnames
     (make-pathname
      :directory '(:relative "lisp") :name "wtc2f20"
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
      :directory '(:relative "lisp") :name "wtc2f20"
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
            "Datapoint refers to multiple indices."
            (nth-list '(0 1) (nth (first x) D)))) Idx))
    #| Definition for MIDI file. |#
    (setq
     R
     (mapcar
      #'(lambda (x)
          (if (> (length x) 1)
            "Datapoint refers to multiple indices."
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

#| The tonal answer can be defined by lowering the pitch of
the first point in P2. We will label these as occurrences 5
and 6. |#
(setq j* '(6 7))
(setq P* (append (list (list 1 61)) (rest P2)))
(setq T* (translators-of-pattern-in-dataset P* D2))
(setq m (length T*))
(loop for i from 0 to (- m 1)
  do
  (progn
    #| occi. Save as csv, lisp, and midi. |#
    (setq
     Idx*
     (indices-lookup
      (translation P* (nth i T*)) D '(1 0 1 0 0)))
    #| Definition of ontime-pitch pairs. |#
    (setq
     Q
     (mapcar
      #'(lambda (x)
          (if (> (length x) 1)
            "Datapoint refers to multiple indices."
            (nth-list '(0 1) (nth (first x) D)))) Idx*))
    #| Definition for MIDI file. |#
    (setq
     R
     (mapcar
      #'(lambda (x)
          (if (> (length x) 1)
            "Datapoint refers to multiple indices."
            (nth (first x) D))) Idx*))
    (setq
     *fname*
     (concatenate
      'string "occ" (write-to-string (nth i j*))))
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

#| There is one extra partial occurrence, which can be defined
by removing the first point in P2. We will label this as
occurrence 8. |#
(setq j* 8)
(setq P* (rest P2))
(setq T* (translators-of-pattern-in-dataset P* D2))
(setq m (length T*))
(loop for i from 5 to 5
  do
  (progn
    #| occi. Save as csv, lisp, and midi. |#
    (setq
     Idx*
     (indices-lookup
      (translation P* (nth i T*)) D '(1 0 1 0 0)))
    #| Definition of ontime-pitch pairs. |#
    (setq
     Q
     (mapcar
      #'(lambda (x)
          (if (> (length x) 1)
            "Datapoint refers to multiple indices."
            (nth-list '(0 1) (nth (first x) D)))) Idx*))
    #| Definition for MIDI file. |#
    (setq
     R
     (mapcar
      #'(lambda (x)
          (if (> (length x) 1)
            "Datapoint refers to multiple indices."
            (nth (first x) D))) Idx*))
    (setq *fname* (concatenate 'string "occ8"))
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

