# LFApp
The repository includes the development version of R package LFApp

## Installation

The package requires Bioconductor package EBImage, which should be installed
first via

```{r, eval = FALSE}
## Install package BiocManager
if(!require(BiocManager)) install.packages("BiocManager")
## Use BiocManager to install EBImage
BiocManager::install("EBImage", update = FALSE)
```

Next, one can install package LFApp, where all remaining dependencies will
be installed automatically.

```{r, eval = FALSE}
## Install package remotes
if(!require(remotes)) install.packages("remotes")
## Install package LFApp
remotes::install_github("fpaskali/LFApp")
```

## Start App
LFApp consist of four different modules. They can be started with one of the following commands: 

```{r}
# LFA App core
LFApp::run_core()

# LFA App quantification
LFApp::run_quan()

# LFA App calibration
LFApp::run_cal()

# LFA App full analysis
LFApp::run_analysis()
```
