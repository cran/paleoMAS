\name{simulat.t}
\alias{simulat.t}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Model desaturation }
\description{
  This function simulates new samples for a sequence of fossil data to desaturate models.
}
\usage{
simulat.t(x, pop = 1000, nsamples = 10, ssample = 300,
 percenta = TRUE, last)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ A data frame containing species relative abundance in columns and samples in rows}
  \item{pop}{ Size of the population to be simulated }
  \item{nsamples}{ Number of samples to be drawn }
  \item{ssample}{ Sample size }
  \item{percenta}{ Type of data desired. If \code{percenta=TRUE} the data are presneted as percentage. Otherwise data are presented as absolute counts. }
  \item{last}{ If \code{percenta=TRUE}, last referes to the column where the last species to be taken into account is located. Only relevant if \code{percenta=TRUE}. See \code{\link{percenta}} for details.}
}
\details{
  When all species are taken into the pollen count, \code{last} corresponds to the number of columns. If only some species are to be taken into the pollen count, they must be contiguous and start in teh first column.}
\value{
  When \code{percenta=FALSE}, \code{samples.t} is returned. When \code{percenta=TRUE} an additional element \code{percentages} is returned.
  \item{samples.t }{An absolute abundance matrix with samples in rows and species in columns. The first column contains sample number.}
  \item{percentages }{A relative abundance matrix with samples in rows and species in columns. Each species is repeated \code{nsamples} times.}

}

\author{ Alexander Correa-Metrio, Kenneth R. Cabrera.}

\references{ Correa-Metrio, A., K.R. Cabrera, and M.B. Bush. Submitted. Deriving millenial-scale moisture availability curves from pollen data: an example from the Peruvian Amazon.
}


\seealso{ This function is an iteration of \code{\link{simulat}}}
\examples{
data(quexilper)
# Cyperaceae and spores are left out of the pollen sum.
# For absolute abundance data
simulat.t(quexilper,percenta=FALSE,last=73)
# or
simulat.t(quexilper,last=73)$samples.t
# For percentages data
simulat.t(quexilper,last=73)$percentages
}

\keyword{manip}
\keyword{datagen}

