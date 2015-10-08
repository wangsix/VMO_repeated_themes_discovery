

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
  "/chopinOp24No4/polyphonic/repeatedPatterns"
  "/tomCollinsMisc/E"))

(setq
 csv-destination
 (concatenate
  'string *path&name* "/csv/mazurka24-4.csv"))
(setq
 MIDI-destination
 (concatenate
  'string *path&name* "/midi/mazurka24-4.mid"))
(setq
 dataset-destination
 (concatenate
  'string *path&name* "/lisp/mazurka24-4.txt"))
(progn
  (setq *scale* 1000)
  (setq *anacrusis* 0)
  (setq
   dataset
   (humdrum-file2dataset-by-col
    (concatenate
     'string *path&name* "/kern/mazurka24-4.krn")))
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





