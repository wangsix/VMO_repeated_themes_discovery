

;(in-package :common-lisp-user)
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "File conversion")
   :name "csv-files"
   :type "lisp")
  *MCStylistic-Mar2013-functions-path*))
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "File conversion")
   :name "kern-by-col"
   :type "lisp")
  *MCStylistic-Mar2013-functions-path*))
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "File conversion")
   :name "midi-export"
   :type "lisp")
  *MCStylistic-Mar2013-functions-path*))
(load
 (merge-pathnames
  (make-pathname
   :directory '(:relative "Pattern rating")
   :name "musical-properties"
   :type "lisp")
  *MCStylistic-Mar2013-functions-path*))

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
     "beethovenOp2No1Mvt3" "monophonic"))
  *music-data-root*))

(setq
 csv-destination
 (merge-pathnames
  (make-pathname
   :directory
   '(:relative "csv") :name "sonata01-3" :type "csv")
  *path&name*))
(setq
 MIDI-destination
 (merge-pathnames
  (make-pathname
   :directory
   '(:relative "midi") :name "sonata01-3" :type "mid")
  *path&name*))
(setq
 dataset-destination
 (merge-pathnames
  (make-pathname
   :directory
   '(:relative "lisp") :name "sonata01-3" :type "txt")
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
      '(:relative "kern") :name "sonata01-3"
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
  (setq dataset-mono (sky-line-clipped dataset-ana))
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





