\name{threshold_li}
\alias{threshold_li}
\title{Li Thresholding Algorithm}
\description{
  The function computes a background theshold of an image by using Li's 
  iterative minimum cross entropy method.
}
\usage{
threshold_li(image, tolerance = NULL, initial_guess = NULL, iter_callback = NULL)
}
\arguments{
  \item{image}{ object of class \code{Image} from package EBImage. }
  \item{tolerance}{ optional tolerance threshold.}
  \item{initial_guess}{ optional initial value for the minimization. }
  \item{iter_callback}{ optional function applied to the minimizsation criterion. }
}
\details{
  For more details about the method see Li and Lee (1993) as well as 
  Li and Tam (1998).
}
\value{
  \code{numeric} vector with the computed threshold.
}
\references{
  C.H. Li and C.K. Lee (1993). Minimum cross entropy thresholding. 
  \emph{Pattern Recognition} \bold{26} (4): 617-25. 
  https://doi.org/10.1016/0031-3203(93)90115-D.

  C.H. Li and P.K.S. Tam (1998). An iterative algorithm for minimum cross entropy 
  thresholding. \emph{Pattern Recognition Letters} \bold{19} (8): 771-76. 
  https://doi.org/10.1016/S0167-8655(98)00057-9.

}
\author{Filip Paskali \email{Filip.Paskali@gmail.de}}
\examples{
library(EBImage)
x <- readImage(system.file("images", "sample.TIF", package="LFApp"))
threshold_li(x)
}
\keyword{univar}
