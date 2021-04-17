\name{triangle}
\alias{triangle}
\title{Triangle Thresholding Algorithm}
\description{
  The function computes a background theshold of an image using the triangle 
  algorithm.
}
\usage{
triangle(image, offset = 0.2, breaks = 256)
}
\arguments{
  \item{image}{ object of class \code{Image} from package EBImage. }
  \item{offset}{ numeric, additional offset added to the computed threshold. }
  \item{breaks}{ integer, number of breaks used in the histogram. }
}
\details{
  The Triangle method is based on the histogram of the intensities. Based on 
  the range of intensities and the maximum peak a threshold is determined. 
  The method was proposed in Zack et al. (1977).
}
\value{
  \code{numeric} vector with the computed threshold.
}
\references{
  G.W. Zack, W. E. Rogers, and S. A. Latt (1977). Automatic measurement of sister 
  chromatid exchange frequency. \emph{The journal of histochemistry and cytochemistry: 
  official journal of the Histochemistry Society} \bold{25} (7): 741-53. 
  https://doi.org/10.1177/25.7.70454.
}
\author{Matthias Kohl \email{Matthias.Kohl@stamats.de}}
\examples{
library(EBImage)
x <- readImage(system.file("images", "sample.TIF", package="LFApp"))
triangle(x)
}
\keyword{univar}
