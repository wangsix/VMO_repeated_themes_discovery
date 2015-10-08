

;(in-package :common-lisp-user)
(load
 (concatenate
  'string
  *lisp-code-root* "/File conversion"
  "/csv-files.lisp"))
(load
 (concatenate
  'string
  *lisp-code-root* "/File conversion"
  "/humdrum-by-col.lisp"))
(load
 (concatenate
  'string
  *lisp-code-root* "/File conversion"
  "/midi-save.lisp"))
(load
 (concatenate
  'string
  *lisp-code-root* "/Pattern rating"
  "/musical-properties.lisp"))

(setq
 *music-data-root*
 (make-pathname
  :directory
  '(:absolute
    "Users" "tomthecollins" "Shizz" "Data" "Music")))

(setq
 *path&name*
 (merge-pathnames
  (make-pathname
   :directory
   '(:relative
     "bachBWV889Fg" "monophonic"))
  *music-data-root*))

(setq
 csv-destination
 (merge-pathnames
  (make-pathname
   :directory
   '(:relative "csv") :name "wtc2f20" :type "csv")
  *path&name*))
(setq
 MIDI-destination
 (merge-pathnames
  (make-pathname
   :directory
   '(:relative "midi") :name "wtc2f20" :type "mid")
  *path&name*))
(setq
 dataset-destination
 (merge-pathnames
  (make-pathname
   :directory
   '(:relative "lisp") :name "wtc2f20" :type "txt")
  *path&name*))

(progn
  (setq *scale* 1000)
  (setq *anacrusis* 0)
  (setq *beats-in-bar* 4)
  (setq
   dataset
   (kern-file2dataset-by-col
    (merge-pathnames
     (make-pathname
      :directory
      '(:relative "kern") :name "wtc2f20"
      :type "krn")
     *path&name*)))
  (setq
   dataset-ana
   (mapcar
    #'(lambda (x)
        (append
         (list (+ (first x) *anacrusis*))
         (rest x)))
    dataset))
  (setq dataset-mono (monophonise dataset-ana *beats-in-bar*))
  "Yes!"
  (setq
   dataset-mna ; mono, no anacrusis.
   (mapcar
    #'(lambda (x)
        (append
         (list (- (first x) *anacrusis*))
         (rest x)))
    dataset-mono))
  (saveit
   MIDI-destination
   (modify-to-check-dataset dataset-mna *scale*))
  (write-to-file
   dataset-mono
   dataset-destination)
  (dataset2csv
   dataset-destination csv-destination))





