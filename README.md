# edfR [![Build Status](https://travis-ci.org/jashubbard/edfR.svg?branch=master)](https://travis-ci.org/jashubbard/edfR)
A package for importing SR-Research EDF files into R.

**NOTE:** This package may not install correctly on Mac and there is no support for Windows.  See [issue 12](https://github.com/jashubbard/edfR/issues/12) and [issue 15](https://github.com/jashubbard/edfR/issues/15) for details.  If someone has a fix, we’d be happy to include it.  Linux should work okay.

This contains basic functions for importing event data (fixations, saccades, blinks), sample data (gaze position, pupil diameter), and messages directly from .edf files. There are also basic utilities for quickly plotting fixation data (`edf.plot`), for time-locking sample data relative to some event (`epoch.samples`), and for combining data across multiple subjects (`edf.batch` and `combine.eyedata`).

For more high-level analyses (e.g., merging with behavioral data, creating regions of interest), please install the package `itrackR`: http://github.com/jashubbard/itrackR. 

This requires the shared libraries from the Eyelink Developer's Kit (EDF API) to be installed (outside of R). The API can be downloaded from: https://www.sr-support.com/forumdisplay.php?17-EyeLink-Display-Software  Click on “*EyeLink Developers Kit for Mac OS X (Mac OS X Display Software)*” or “*EyeLink Developers Kit for Linux (Linux Display Software)*” and follow the instructions.

The package also depends on the R packages Rcpp, RcppArmadillo, and data.table which should be installed automatically. Efforts were made to write as much as possible in base R. For saving .mat files for Matlab (using `edf.batch`), it also requires the package R.matlab (this will not be installed automatically).


To install, simply run:

```r
install.packages('devtools')
library(devtools)
install_github('jashubbard/edfR')
```

