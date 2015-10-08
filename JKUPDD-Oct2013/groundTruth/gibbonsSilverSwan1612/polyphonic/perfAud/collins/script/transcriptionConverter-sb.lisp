#| Copyright Tom Collins 16/9/2013.

Script for converting a transcription (pitches
estimated and onsets quantised) contained in a text
file into a midi file for auditioning. |#

(setq
 *piece-path*
 (make-pathname
  :directory
  '(:absolute
    "Users" "tomthecollins" "Shizz" "JKU"
    "ConferencesPresentations" "AES" "2014"
    "collinsEtalAES2014" "2013-09-16-submission"
    "gibbonsSilverSwan1612" "perfAud" "collins")))
(setq *piece-name* "silverswan")
; Parameters for writing to MIDI file.
(setq *dur* .5)
(setq *scale* 1000)
(setq *channel* 1)

; Set some paths.
(setq
 *transcr-fname*
 (merge-pathnames
  (make-pathname
   :directory '(:relative "transcription")
   :name "silverswan-sb" :type "txt") *piece-path*))
(setq
 *midi-out*
 (merge-pathnames
  (make-pathname
   :directory '(:relative "midi")
   :name "silverswan-transcription-sb" :type "mid")
  *piece-path*))

(progn
  ; Load the dataset.
  (setq dataset (csv2dataset *transcr-fname*))
  ; Output MIDI, text, and csv files.
  (saveit
   *midi-out*
   (modify-to-check-dataset
    (mapcar
     #'(lambda (x)
         (list
          (- (first x) (first (first dataset)))
          (floor (second x)) (floor (third x)) *dur*))
     dataset)
    *scale* *channel*)))
