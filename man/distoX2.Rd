\name{distoX2}
\alias{distoX2}
\docType{data}
\title{
Data example file
}
\description{
This data example file contains fabrics and coordinates coming from Le Moustier Lower shelter. It can be downladed into the application in various formats.
}
\usage{data("distoX2")}
\format{
  A data frame with 159 observations on the following 8 variables.
  \describe{
    \item{\code{id}}{a numeric vector}
    \item{\code{x}}{a numeric vector}
    \item{\code{y}}{a numeric vector}
    \item{\code{z}}{a numeric vector}
    \item{\code{code}}{a character vector}
    \item{\code{sample}}{a character vector}
    \item{\code{orientation}}{a numeric vector}
    \item{\code{dip}}{a numeric vector}
  }
}
\details{
Here are the various formats this example file takes into the application:
'Case 1: Only angles'
'Case 2: Only angles from DistoX2'
'Case 3: Angles from DistoX2 with coordinates'
'Case 4: Angles and coordinates'
'Case 5: Two shots data without angles'
}
\source{
Data from Le Moustier Lower Shelter (excavations E. Discamps, B. Gravina and M. Thomas)
}
\references{
Thomas, M., Discamps, E., Gravina, B., Texier, J.-P. (2019). Analyse taphonomique et spatiale de palimpsestes d'occupations moustériennes de l'abri inférieur du Moustier (Dordogne, France). Paléo 30(1), p. 278-299. https://doi.org/10.4000/paleo.4897
Texier, J.-P., Discamps, E., Gravina, B., Thomas, M. (2020). Les dépots de remplissage de l'abri inférieur du Moustier (Dordogne, France) : lithostratigraphie, processus de formation et évolution du système géomorphologique. Paléo 30(2), p. 320-345. https://doi.org/10.4000/paleo.5826
}
\examples{
data(distoX2)
}
\keyword{datasets}
