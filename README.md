# LFApp
The repository includes the development version of R package LFApp

[![License: LGPL v3](https://img.shields.io/badge/License-LGPL%20v3-blue.svg)](https://www.gnu.org/licenses/lgpl-3.0)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)


## Testing our apps: shinyapps.io

Our apps can also be tested on shinyapps.io. The desktop version of our full
purpose analysis app is at

https://lfapp.shinyapps.io/LFAnalysis/

The mobile version is at

https://lfapp.shinyapps.io/mobile_app/


## Installation

The package requires Bioconductor package EBImage, which should be installed
first via

```{r, eval = FALSE}
## Install package BiocManager
if(!requireNamespace("BiocManager", quietly = TRUE)) 
  install.packages("BiocManager")
## Use BiocManager to install EBImage
BiocManager::install("EBImage", update = FALSE)
```

Our package depends on the most recent version of package shinyMobile, which 
must be installed from github (https://github.com/RinteRface/shinyMobile) by

```{r}
## Install package remotes
if(!requireNamespace("remotes", quietly = TRUE)) 
  install.packages("remotes")
## Install package shinyMobile
remotes::install_github("RinteRface/shinyMobile")
```

Finally, one can install package LFApp, where all remaining dependencies will
be installed automatically.

```{r, eval = FALSE}
## Install package LFApp
remotes::install_github("fpaskali/LFApp", build_vignette=TRUE)
```

## Start App
LFApp consist of four different modules where there is a desktop and a 
mobile version for each module. They can be started with one of the following 
commands: 

```{r}
## desktop versions
## LFA App core
LFApp::run_core()

## LFA App quantification
LFApp::run_quan()

## LFA App calibration
LFApp::run_cal()

## LFA App full analysis
LFApp::run_analysis()

## mobile versions
## LFA App core
LFApp::run_mobile_core()

## LFA App quantification
LFApp::run_mobile_quan()

## LFA App calibration
LFApp::run_mobile_cal()

## LFA App full analysis
LFApp::run_mobile_analysis()
```
