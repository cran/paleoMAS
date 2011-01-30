\name{run.mean}
\alias{run.mean}

\title{ Moving averages }
\description{
  This function calculates moving averages for data series.  
}

\usage{
run.mean(x, window)
}

\arguments{
  \item{x}{ A vector with the data series. }
  \item{window}{ Number of observations to be taken into account in the estimation of the local mean. }
  
}

\value{
  Returns a vector with the estimated local averages.}

\author{ Alexander Correa-Metrio. }

\examples{
run.mean(c(1:10),window=3)
}

\keyword{ manip }
\keyword{ arith } 
