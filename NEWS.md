# SwissASR 0.5.1

* Corrected the version of the word template, 2.1 instead of 1.1. 

# SwissASR 0.5.0

* Modified the template of the ASR to align with the newest template available on the swissethics website. Main changes: update of the text in some parts of the template and inclusion of the transplant typology of trial.
* Addition of an RStudio add-in which pastes a template `asr` call into the active R script. In RStudio, put the cursor in the desired file, click "Addins" and then "Add asr call template" and the template should appear. Using the guiding text, edit the arguments as appropriate.

# SwissASR 0.4.0

* Modified some bookmarks into word fields such that the replacement can still happen in tables and not after them, as per recent update of officer package. This resolves issue #9.
* The above change requires use of the `doconv` package, which may require **setting some permissions on MAC OS** (for details see [the doconv documentation](https://github.com/ardata-fr/doconv)), and **may not work on Ubuntu** systems.

# SwissASR 0.3.0

* Additional option to include an intervention (random allocation) variable into the line listing (option `var_tx`), which is sometimes requested by ethics bodies.
* Addition of a logo

# SwissASR 0.2.0

* Addition of `asr_dataprep` and `asr_safety_summary` functions allowing for production of tables without generation of the MS Word report.
* Reworking of the internals of `asr` to utilise the above mentioned functions.
* Addition of demonstration dataset.
* Line listing pages in the word template set to landscape.

# SwissASR 0.1.0

* Added a `NEWS.md` file to track changes to the package.
