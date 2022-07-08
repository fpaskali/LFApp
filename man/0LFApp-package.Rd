\name{LFApp-package}
\alias{LFApp-package}
\alias{LFApp}
\docType{package}
\title{
Shiny Apps for Lateral Flow Assays.
}
\description{
Shiny apps for the quantitative analysis of images from lateral flow assays (LFAs). 
The images are segemented and background corrected and color intensities are extracted. 
The apps can be used to import and export intensity data and to calibrate LFAs by 
means of linear, loess, or gam models. The calibration models can further be saved 
and applied to intensity data from new images for determining concentrations.
}
\details{
\tabular{ll}{
Package: \tab LFApp\cr
Type: \tab Package\cr
Version: \tab 1.3\cr
Date: \tab 2021-07-27\cr
Depends: \tab R(>= 4.0.0)\cr
Imports: \tab stats, utils, graphics, methods, mgcv, shiny, shinyjs, shinythemes, shinyFiles, shinyMobile (>= 0.9), EBImage, DT, ggplot2, fs\cr
Suggests: \tab knitr, rmarkdown, remotes\cr
License: \tab LGPL-3\cr
URL: \tab https://github.com/fpaskali/LFApp\cr
BugReports: \tab https://github.com/fpaskali/LFApp/issues\cr
}
library(LFApp)
}
\author{ Filip Paskali, Weronika Schary, Matthias Kohl

Maintainer: Filip Paskali  \email{F.Paskali@hs-furtwangen.de}}
%\references{
%~~ Literature or other references for background information ~~
%}
%~~ Optionally other keywords from doc/KEYWORDS, one per line
%\seealso{
%~~ Optional links to other man pages, e.g. ~~
%~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
%}
%\examples{
%~~ simple examples of the most important functions ~~
%}
\keyword{package}
