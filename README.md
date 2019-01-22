# BEQR
## Purpose  

This package provides a collection of functions for analysing and reporting bioequivalence trials for insulin-biosimilars.

## Installation  

### To build vignette and install the package   

devtools::install_github("Eliford/BEQR", build = TRUE, build_opts = c("--no-resave-data", "--no-manual"))

### To install without building the vignette  

devtools::install_github("Eliford/BEQR")  

## Introduction  

The BEQR vignette provides examples on how to use different BEQR functions. 
The vignette can be accessed by the `utils:vignette` function `vignette(topic="beqr_overview", package = "BEQR"). 
Alternatively, `BEQR::make_examplesfile()` function will create an rmarkdown file with examples on how to use the different BEQR functions.
Note that `make_examplefile(dir_path=?, file_name=?)` function requires an existing file directory where an example rmarkdown file will be saved.

The BEQR functions are documented. The documentation can be accessed through help("functionname") or ?functionname.
