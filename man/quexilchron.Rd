\name{quexilchron}
\alias{quexilchron}
\docType{data}
\title{ Chronology Lake Quexil}
\description{
  This data set provides the age basis for the chronolgy of Core 80-1 from Lake Quexil (Leyden 1984; Leyden et al 1993, 1994).
}
\usage{data(quexilchron)}
\format{
 A matrix with 3 rows (dates) and 3 columns (depth, age and uncertainty). Present age is assumed for depth 0.
 
 }

\source{
  http://www.ncdc.noaa.gov/paleo
}
\references{
  Leyden, B.W. 1984. Guatemalan forest synthesis after Pleistocene aridity. Proceedings of the National Academy of Sciences USA 81:4856-4859.

Leyden, B.W., M. Brenner, D.A. Hodell, and J.H. Curtis. 1993. Late Pleistocene climate in the Central American lowlands. Pages 165-178 in P.K. Swart, K.C. Lohmann, J. McKenzie, and S. Savin, editors. Climate change in continental isotopic records. American Geophyysical Union Geophysical Monograph 78, Washington, DC, USA.

Leyden, B.W., M. Brenner, D.A. Hodell, and J.H. Curtis. 1994. Orbital and internal forcing of climate on the Yucatan Peninsula for the past ca. 36 ka. Palaeogeography, Palaeoclimatology, Palaeoecology 109:193-210.

}
\examples{
data(quexilchron)
plot(quexilchron[,1:2],type="l")
}
\keyword{datasets}
