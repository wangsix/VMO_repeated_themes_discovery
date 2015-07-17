VMO_repeated_themes_discovery
=============================

Code repository for ICASSP 2015 & ISMIR 2015 submission on repeated themes discovery from audio recordings and MIDI with VMO. Created and maintained by Cheng-i Wang. Implementation by Cheng-i Wang and Jennifer Hsu. 

email:chw160@ucsd.edu

## Dependencies
* [vmo](https://github.com/wangsix/vmo) 
* Numpy, Scipy, matplotlib
* [mir_eval](https://github.com/craffel/mir_eval)

## Contents 
* bin
    * JKUPDD_experiment.py

    The experiment script generating pattern discovery algorithm output files. 
    * JKUPDD_time.py

    The timing script.
* notebook/JKUPDD_demo.ipynb

    A python notebook demonstrating how [vmo](https://github.com/wangsix/vmo) is used to find repeated themes from audio recordings.  
* JKUPDD-Aug2013

    The [MIREX dataset](http://www.music-ir.org/mirex/wiki/2014:Discovery_of_Repeated_Themes_%26_Sections).

* estimation_results

    The experiment results using the scripts. 

* supplementPlots

    Folders corresponding to the five music pieces from the dataset have supplement plots not shown in the submitted paper.

    * **irProfile** The information rate of different VMO created by different threshold values. The vmo with the highest information rate is used to find the repeated themes.

    * **jkupdd_patterns** Beat-synchronous chromagram, ground truth and the algorithm outputs.

    * **chromagramAndRecurrencePlot** Beat-synchrounous chromagram and a recurrence plot extracted directly from the VMO symbolization results. The recurrence plot is depicted only for visualization purpose, and is not used during the pattern finding algorithm. The pattern finding algorithm relies only on the suffix structure and values of longest repeated suffices from VMO.

