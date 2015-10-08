#| Copyright Tom Collins 3/12/2013.

Script for converting a beat-tracking ground-truth
midi file into a text file for the purposes of
evaluation. |#

(setq
 *piece-path*
 (make-pathname
  :directory
  '(:absolute
    "Users" "tomthecollins" "Shizz" "JKU"
    "ConferencesPresentations" "AES" "2014"
    "collinsEtalAES2014" "collinsEtalAES2014"
    "JKUPDD-Oct2013" "groundTruth"
    "beethovenOp2No1Mvt3" "polyphonic" "perfAud"
    "gulda")))

; Set some paths.
(setq
 *midi-in*
 (merge-pathnames
  (make-pathname
   :directory '(:relative "midi")
   :name "sonata01-3-beatTracking-gt" :type "mid")
  *piece-path*))
(setq
 *text-out*
 (merge-pathnames
  (make-pathname
   :directory '(:relative "beatTracking")
   :name "sonata01-3-gt" :type "txt")
  *piece-path*))

(progn
  (setq beats (load-midi-file *midi-in*))
  (dataset2csv
   (mapcar
    #'(lambda (x)
        (list (first x)))
    beats) *text-out*)
  "Yes!")
