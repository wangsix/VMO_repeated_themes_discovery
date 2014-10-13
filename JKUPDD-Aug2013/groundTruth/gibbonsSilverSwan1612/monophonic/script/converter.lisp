

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
 *path&name*
 (concatenate
  'string *music-data-root*
  "/gibbonsSilverSwan1612/monophonic"))

(setq
 csv-destination
 (concatenate
  'string *path&name* "/csv/silverswan.csv"))
(setq
 MIDI-destination
 (concatenate
  'string *path&name* "/midi/silverswan.mid"))
(setq
 dataset-destination
 (concatenate
  'string *path&name* "/lisp/silverswan.txt"))
(progn
  (setq *scale* 1000)
  (setq *anacrusis* 0)
  (setq *beats-in-bar* 4)
  (setq
   dataset
   (humdrum-file2dataset-by-col
     (concatenate
      'string *path&name* "/kern/silverswan.krn")))
  (setq
   dataset-ana
   (mapcar
    #'(lambda (x)
        (append
         (list (+ (first x) *anacrusis*))
         (rest x)))
    dataset))
  (setq dataset-mono (monophonise dataset-ana *beats-in-bar*))
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





