

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

(setq
 *path&name*
 (concatenate
  'string *music-data-root*
  "/beethovenOp2No1Mvt3/polyphonic/repeatedPatterns"
  "/barlowAndMorgenstern/A"))

(setq
 csv-destination
 (concatenate
  'string *path&name* "/csv/sonata01-3.csv"))
(setq
 MIDI-destination
 (concatenate
  'string *path&name* "/midi/sonata01-3.mid"))
(setq
 dataset-destination
 (concatenate
  'string *path&name* "/lisp/sonata01-3.txt"))
(progn
  (setq *scale* 1000)
  (setq *anacrusis* -1)
  (setq
   dataset
   (humdrum-file2dataset-by-col
    (concatenate
     'string *path&name* "/kern/sonata01-3.krn")))
  (saveit
   MIDI-destination
   (modify-to-check-dataset dataset *scale*))
  (write-to-file
   (mapcar
    #'(lambda (x)
        (append
         (list (+ (first x) *anacrusis*))
         (rest x)))
    dataset)
   dataset-destination)
  (dataset2csv
   dataset-destination csv-destination))





