#| Copyright Tom Collins 16/9/2013.

Script for converting beat-tracking algorithm output
to a dataset and midi file for loading into Logic and
auditioning. |#

(setq
 *beats-path*
 (make-pathname
  :directory
  '(:absolute
    "Users" "tomthecollins" "Shizz" "JKU"
    "ConferencesPresentations" "AES" "2014"
    "collinsEtalAES2014" "2013-09-16-submission"
    "beethovenOp2No1Mvt3" "deadAud")))
; Parameters for writing to MIDI file.
(setq *scale* 1000)
(setq *channel* 1)

; Set some paths.
(setq
 *beats-fname*
 (merge-pathnames
  (make-pathname
   :directory '(:relative "beatTracking")
   :name "sonata01-3-fk" :type "txt") *beats-path*))
(setq
 *midi-out*
 (merge-pathnames
  (make-pathname
   :directory '(:relative "midi")
   :name "sonata01-3-beatTracking-fk" :type "mid")
  *beats-path*))

(progn
  (setq beats (csv2dataset *beats-fname*))
  (setq
   dataset
   (mapcar
    #'(lambda (x) (list (first x) 37 46 .5)) beats))
  (saveit
   *MIDI-out*
   (modify-to-check-dataset
    dataset *scale* *channel*))
  "Yes!")
