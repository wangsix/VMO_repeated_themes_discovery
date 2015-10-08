#| Copyright Tom Collins 16/9/2013.

Script for converting Sebastian Boeck's transcription
algorithm output to a dataset, midi file, spelling
the pitches, and then outputing for quantisation and
pattern discovery in Matlab. |#

(setq
 *piece-path*
 (make-pathname
  :directory
  '(:absolute
    "Users" "tomthecollins" "Shizz" "JKU"
    "ConferencesPresentations" "AES" "2014"
    "collinsEtalAES2014" "2013-09-16-submission"
    "mozartK282Mvt2" "deadAud")))
(setq *piece-name* "sonata04-2")
; (setq *piece-fifths-step-mode* '(-2 0))
; Parameters for writing to MIDI file.
(setq *dur* .50)
(setq *scale* 1000)
(setq *channel* 1)

; Set some paths.
(setq
 *tab-fname*
 (merge-pathnames
  (make-pathname
   :directory '(:relative "multiF0")
   :name *piece-name* :type "txt") *piece-path*))
(setq
 csv-destination
 (merge-pathnames
  (make-pathname
   :directory
   '(:relative "pitchSpelling")
   :name *piece-name* :type "csv")
  *piece-path*))
(setq
 MIDI-destination
 (merge-pathnames
  (make-pathname
   :directory
   '(:relative "midi")
   :name
   (concatenate
    'string *piece-name* "-multiF0") :type "mid")
  *piece-path*))
(setq
 dataset-destination
 (merge-pathnames
  (make-pathname
   :directory
   '(:relative "pitchSpelling")
   :name *piece-name* :type "txt")
  *piece-path*))

(progn
  ; Load (onset, MIDI note) pairs.
  (setq
   onMNN (sort-dataset-asc (tab2dataset *tab-fname*)))
  ; Guess key using Sapp's (2005) approach.
  (setq
   key-corr
   (key-correlations
    (mapcar
     #'(lambda (x) (list (first x) (second x) *dur*))
     onMNN) *Aarden-key-profiles*))
  (setq
   quasi-key-MNN
   (second
    (max-argmax
     (nth-list-of-lists 1 key-corr))))
  (setq
   *piece-fifths-step-mode*
   (list
    (nth
     (mod quasi-key-MNN 12)
     (list 0 -5 2 -3 4 -1 6 1 -4 3 -2 5))
    (if (< quasi-key-MNN 12) 0 5)))
  ; Use estimated key to guess MPNs.
  (setq
   dataset
   (mapcar
    #'(lambda (x)
        (list
         (first x) (second x)
         (guess-morphetic
          (second x) *piece-fifths-step-mode*) *dur*))
    onMNN))
  ; Output MIDI, text, and csv files.
  (saveit
   MIDI-destination
   (modify-to-check-dataset
    dataset *scale* *channel*))
  (write-to-file
   (mapcar #'(lambda (x) (butlast x)) dataset)
   dataset-destination)
  (dataset2csv
   dataset-destination csv-destination)
  "Yes!")
