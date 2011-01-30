\name{simulat.t}
\alias{simulat.t}

\title{ Model desaturation by samples simulation }
\description{
  This function simulates new samples for a sequence of fossil data to desaturate models.
}
\usage{
simulat.t(x, pop = 1000, nsamples = 10, ssample = 300,
 percenta = TRUE, last)
}

\arguments{
  \item{x}{ A percentage matrix containing samples in rows and taxa in columns.}
  \item{pop}{ Size of the population to be simulated. }
  \item{nsamples}{ Number of samples to be drawn. }
  \item{ssample}{ Sample size. }
  \item{percenta}{ Logical indicating whether or not the outcome matrix should be one of percentages. }
  \item{last}{ If \code{percenta=TRUE}, last refers to the column where the last species to be taken into account is located. Only relevant if \code{percenta=TRUE}. See \code{\link{percenta}} for details.}
}
\details{
  When all species are taken into the pollen count, \code{last} corresponds to the number of columns. If only some species are to be taken into the pollen count, they must be contiguous and start in the first column.}

\value{
  When \code{percenta=FALSE}, \code{samples.t} is returned. When \code{percenta=TRUE} an additional element \code{percentages} is returned.
  \item{samples.t }{An absolute abundance matrix with samples in rows and species in columns. The first column contains sample number.}
  \item{percentages }{A relative abundance matrix with samples in rows and species in columns. Each species is repeated \code{nsamples} times.}

}

\author{ Alexander Correa-Metrio, Kenneth R. Cabrera.}

\references{
Correa-Metrio, A., K.R. Cabrera, and M.B. Bush. 2010. Quantifying ecological change through discriminant analysis: a paleoecological example from the Peruvian Amazon. \emph{Journal of Vegetation Science} 21: 695-704.
}


\seealso{ This function is an iteration of \code{\link{simulat}}}
\examples{
data(quexilper)
colnames(quexilper)
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

