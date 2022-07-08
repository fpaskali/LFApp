\name{run_functions}
\alias{run_analysis}
\alias{run_cal}
\alias{run_core}
\alias{run_quan}
\alias{run_mobile_analysis}
\alias{run_mobile_cal}
\alias{run_mobile_core}
\alias{run_mobile_quan}
\title{Run Analysis Shiny Apps}
\description{
  Function start the Analysis Shiny App.
}
\usage{
run_analysis()
run_cal()
run_core()
run_quan()
run_mobile_analysis()
run_mobile_cal()
run_mobile_core()
run_mobile_quan()
}
\details{
  The functions start the various shiny apps included in the package.
}
\value{
  An object that represents the app. Printing the object will run the app.
}
\author{Filip Paskali \email{F.Paskali@hs-furtwangen.de}, 
        Weronika Schary \email{W.Schary@hs-furtwangen.de}, 
        Matthias Kohl \email{Matthias.Kohl@stamats.de}}
\examples{
if(interactive()){
  ## start full analysis app
  run_analysis()
  ## start mobile version of full analysis app
  run_mobile_analysis()
}
}
\keyword{dynamic}
