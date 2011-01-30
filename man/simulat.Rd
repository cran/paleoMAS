\name{simulat}
\alias{simulat}

\title{ Sample simulation }
\description{
 This function simulates new samples based on known percentages 
}
\usage{
simulat(x, pop = 10000, nsamples = 10, ssample = 300)
}

\arguments{
  \item{x}{ Relative frequencies of taxa in the sample of interest. It should correspond to a row in a matrix where species are in columns and samples in rows. }
  \item{pop}{ Size of the population to be simulated. }
  \item{nsamples}{ Number of samples to be drawn. }
  \item{ssample}{ Sample size. }
}
\details{
  A population of size \code{pop} and relative frequencies \code{x} is simulated, and randomly sampled to produce \code{nsamples} new samples. This procedure assumes that the percentages contained in \code{x} represent the real population.
}
\value{
Returns a matrix \code{samples} that contains simulated samples in rows and species in columns.
  
}

\author{ Alexander Correa-Metrio, Kenneth R. Cabrera.}

\references{ 
Correa-Metrio, A., K.R. Cabrera, and M.B. Bush. 2010. Quantifying ecological change through discriminant analysis: a paleoecological example from the Peruvian Amazon. \emph{Journal of Vegetation Science} 21: 695-704.	
}



\examples{
# data from lake Quexil (Leyden et al 1994)
data(quexilper)
simulat(quexilper[1,])
}

\keyword{manip}
\keyword{datagen}
