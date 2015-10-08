

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
  "/bachBWV889Fg/monophonic/repeatedPatterns/bruhn/B"))

(setq
 csv-destination
 (concatenate
  'string *path&name* "/csv/wtc2f20.csv"))
(setq
 MIDI-destination
 (concatenate
  'string *path&name* "/midi/wtc2f20.mid"))
(setq
 dataset-destination
 (concatenate
  'string *path&name* "/lisp/wtc2f20.txt"))
(progn
  (setq *scale* 1000)
  (setq *anacrusis* 0)
  (setq
   dataset
   (humdrum-file2dataset-by-col
    (concatenate
     'string *path&name* "/kern/wtc2f20.krn")))
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





