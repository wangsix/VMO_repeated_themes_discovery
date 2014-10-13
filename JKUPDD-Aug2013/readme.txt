JKUPDD-Aug2013
The Johannes Kepler University Patterns Development Database

This package of music data and code is intended to help participants develop and try out their algorithms for the 2013 MIREX task, Discovery of Repeated Themes & Sections:

http://www.music-ir.org/mirex/wiki/2013:Discovery_of_Repeated_Themes_%26_Sections

or

http://www.tomcollinsresearch.net/mirex-pattern-discovery-task.html


Users are encouraged to run their algorithms on either the text file representations of pieces (contained in "lisp" folders) and their constituent patterns, or the csv file representations (beware rounding errors). The columns represent ontime (measured from zero in crotchet beats), MIDI note number; morphetic pitch number, duration (measure in crotchet beats), and staff number (integers from zero for the top staff). Users are discouraged from running their algorithms on the midi file representations. The midi files were created and included in the distribution for the purposes of mistake checking, but do not necessarily begin in the correct bar position and contain an extra quiet note at the end to avoid clipping.

If you are writing your own code for iterating over the ground truth patterns, the annotation folders to include for the polyphonic version of the task are:

o barlowAndMorgensternRevised;
o bruhn;
o schoenberg;
o sectionalRepetitions;
o tomCollins.

The annotation folders to include for the monophonic version of the task are:

o barlowAndMorgenstern;
o barlowAndMorgensternRevised;
o bruhn;
o schoenberg;
o sectionalRepetitions;
o tomCollins.

In particular, a faithful barlowAndMorgenstern folder is included in the polyphonic ground truth for the sake of comparison with the revised folder, but it should not be iterated over for the evaluation. This is because the barlowAndMorgenstern originals contain some monophonic patterns that ought to be polyphonic (e.g., because the right hand never occurs independently of the left) and some patterns have erroneous length (e.g., a theme is curtailed at five bars because it fits nicely on the page, but actually the repetition extends for one or two more bars).

Occurrences of patterns consist of (ontime, MIDI note number) pairs. For example, see bachBWV889Fg -> polyphonic -> repeatedPatterns -> bruhn -> A -> occurrences. Inexact occurrences of a pattern are handled as follows: the prototypical version of a pattern is defined at the top level, e.g., bachBWV889Fg -> polyphonic -> repeatedPatterns -> bruhn -> A -> lisp. This definition is shifted in time towards the beginning of the piece, but is in the correct bar position. The prototypical version of a pattern is always defined as "occ1" in the occurrences folder. All of the definitions in the occurrences folder correspond exactly to (ontime, MIDI note number) pairs from the piece (i.e., none of these are shifted in time).


Changes since JKUPDD-Jul2013

The Lisp implementation of the evaluation metrics has been replaced with a Matlab implementation. This Matlab implementation is the version that will be used to evaluate the task.

The ossia has been removed from bachBWV889Fg because it led to duplicate notes.

Removed the prelude from the score of bachBWV889Fg to avoid confusion.

A pattern in chopinOp24No4 has been altered so that the second occurrence of pattern B (bars 13 to 20 beat 1) is the prototype.

A new example evaluation has been added to JKUPDD-Jul2013 for seven algorithms run on mozartK282Mvt2. The outputs are located in examples -> MIREX2013 -> pattDiscOut, and the results in examples -> MIREX2013 -> results.txt. The script I used to generate the results is pattDiscExampleEval.m, and with the paths in this script corrected for a different file system, it should be possible to regenerate results.txt if desired.

The first two algorithm outputs, algo1 and algo2 are sanity checks. I defined the output of algo1 to be exactly equal to the ground truth, to make sure all metrics were at ceiling. For algo2 I defined the output to be exactly equal to the ground truth for the Gibbons piece, to make sure all metrics were at or close to floor. Outputs for algo3 - algo7 were motivated as follows:
3. Removed first two and last two points of each occurrence of patterns 1-4 to demonstrate P_est and R_est remain high while P and R fall;
4. As in 3, but removed patterns 1 and 5 completely to demonstrate R and R_est fall while P and P_est remain;
5. As in 3, but introduced all patterns from the Gibbons piece (erroneous) to demonstrate P and P_est fall while R and R_est remain. P_occ and R_occ should also remain, as the Gibbons patterns are below threshold .75 and so do not get included in this calculation;
6. As in 4, and also removed all but occurrences 1 and 2 of each pattern to demonstrate R_occ falls;

7. As in 6, filling occurrences 3 onwards of each pattern with erroneous Gibbons patterns to demonstrate P_occ falls while R_occ remains.
These algorithm outputs capture a lot of the different dimensions along which the task ought to be evaluated, and the metrics are responding appropriately to changes in algorithm output.


Tom Collins,
Linz, home of Anton Bruckner
August 2013
