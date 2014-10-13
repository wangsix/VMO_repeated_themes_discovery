

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
   :name "humdrum-by-col"
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
   :name "musical-properties"
   :type "lisp")
  *lisp-code-root*))

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
     '(:relative "chopinOp24No4" "monophonic"))
    *music-data-root*))

(setq
   csv-destination
   (merge-pathnames
    (make-pathname
     :directory
     '(:relative "csv") :name "mazurka24-4" :type "csv")
    *path&name*))
(setq
   MIDI-destination
   (merge-pathnames
    (make-pathname
     :directory
     '(:relative "midi") :name "mazurka24-4" :type "mid")
    *path&name*))
(setq
   dataset-destination
   (merge-pathnames
    (make-pathname
     :directory
     '(:relative "lisp") :name "mazurka24-4" :type "txt")
    *path&name*))
(progn
  (setq *scale* 1000)
  (setq *anacrusis* -1)
  (setq *beats-in-bar* 3)
  (setq
   dataset
   (kern-file2dataset-by-col
    (merge-pathnames
     (make-pathname
      :directory
      '(:relative "kern") :name "mazurka24-4" :type "krn")
     *path&name*)))
  (setq
   dataset-ana
   (mapcar
    #'(lambda (x)
        (append
         (list (+ (first x) *anacrusis*))
         (rest x)))
    dataset))
  (setq dataset-mono (sky-line-clipped dataset-ana))
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





