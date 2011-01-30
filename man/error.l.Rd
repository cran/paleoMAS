\name{error.l}
\alias{error.l}

\title{
LOESS regression error estimation for multiple taxa
}
\description{
This fucntion applies bootstrap crossvalidation (CV) of LOESS regressions of multiple taxa. The regression parameters for each taxon are previously selected.
}

\usage{
error.l(x, y, z, trials = c(100, 0.25))
}

\arguments{
  \item{x}{
A vector containing the environmental gradient value for each sample or location.
}
  \item{y}{
A matrix containing the observed taxa abundances along the environmental gradient. Taxa are in columns and samples in rows.}
  \item{z}{
A matrix containig the parameters for the LOESS regression of each taxon (\eqn{\alpha} and degree in that order). Objects produced by \code{\link{bcrossv.all}} and \code{\link{akaike.all}} could be used as \code{z}.
}
  \item{trials}{
Values for number of repetitions of the crossvalidation (100 by default), and proportion of observations to be left out each time the CV is repeated. The observations left out each time are randomly selected with replacement.
}
}
\details{
See \code{\link{bcrossv.l1}} for details.
}
\value{
A matrix of 6 columns showing error diagnostics for each taxon LOESS regression (root of the predictive squared error (rse), and root of the mean squared error (rmse) see \code{\link{bcrossv.l1}} for details). The proportion of the error relative to the data range and mean is presented in the last two columns.
}
\references{
Correa-Metrio, A., M.B. Bush, L.Perez, A. Schwalb, and K. Cabrera. Accepted. Pollen distribution along climatic and biogeographic gradients in northern Central America. The Holocene

Cleveland, W.S., and S.J. Devlin. 1988. Locally weighted regression: An approach to regression analysis by local fitting. \emph{Journal of the American Statistical Association} 83: 596-610.
}

\author{
Alexander Correa-Metrio, Kenneth R. Cabrera.
}

\seealso{
\code{\link[stats]{loess}} and \code{\link{bcrossv.l1}}
}
\examples{
data(modernq)
# Calculate percentages
perq<-percenta(modernq,first=2,last=39)[,2:55]
#if alpha and degree are selected using AIC
a.d<-akaike.all(modernq[,1],perq[,1:10])
error.l(modernq[,1],perq[,1:10],a.d)

}

\keyword{ models }
\keyword{ robust } 