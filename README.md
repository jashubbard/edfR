# edfR
A package for importing SR-Research EDF files into R.

This contains basic functions for importing event (fixations, saccades, blinks) and sample (pupil diameter) data directly from .edf files. There are also basic utilities for quickly plotting fixation data (edf.plot), for time-locking sample data relative to some event (epoch.samples), and for combining data across multiple subjects (edf.batch and combine.eyedata). 

This requires the shared libraries from the Eyelink Developer's Kit (EDF API) to be installed (outside of R). The API can be downloaded from: https://www.sr-support.com/forumdisplay.php?17-EyeLink-Display-Software. 

This also depends on the R packages Rcpp, RcppArmadillo, and data.table which should be installed automatically. Efforts were made to write as much as possible in base R. For saving .mat files for Matlab (using edf.batch), it also requires the package R.matlab (this will not be installed automatically). 

Currently works with Mac or Linux only. Windows support coming soon.

To install, simply run:

```r
install.packages('devtools')
library(devtools)
install_github('jashubbard/edfR')
```

