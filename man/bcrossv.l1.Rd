\name{bcrossv.l1}
\alias{bcrossv.l1}

\title{
LOESS regression bootstrap cross-validation with fixed parameters
}

\description{
This function performs v-fold cross-validation (CV) of a LOESS regression of known \eqn{\alpha} and degree for a single taxon.
}

\usage{
bcrossv.l1(x, y, trials = c(100, 0.25), span = 0.75, degree = 2,
          plot = TRUE, estimated = FALSE)
}

\arguments{
  \item{x}{
A vector containing the environmental gradient value for each sample or location.
}
  \item{y}{
A vector containing the observed taxa abundances along the environmental gradient.
}
  \item{trials}{
Values for number of repetitions of the crossvalidation (100 by default), and proportion of observations to be left out each time the CV is repeated (0.25 by default). The observations left out each time are randomly selected with replacement.
}
  \item{span}{
\eqn{\alpha} value for the LOESS regression.
}
  \item{degree}{
Degree of the LOESS regression. Limited to 1 and 2.
}
  \item{plot}{
Logical indicating whether or not an observed vs. error plot is desired
}
\item{estimated}{
Logical indicating whether or not the matrix with the estimated values is desired.
}
}

\details{
The LOESS regression (Cleveland and Devlin, 1988) CV is repeated as many times as indicated in the first component of \code{trials}. Each repetition leaves out a fraction of the total number of observations (indicated in the second component of \code{trials}). \eqn{\alpha} and degree values for the LOESS regression are fixed and indicated in \code{span} and \code{degree} respectively. Returns the results of the crossvalidation and an error matrix containing the predictive squared error (se, 1/n*sum(est-obs)^2), the root predictive squared error (rse, pse^0.5), and root mean squared error (rmse, 1/n*sum(abs(obs-est))).

If the remaining number of observations after leaving out a certain number of elements is too low, the fitting of the LOESS regression is not possible. Therefore, it is recommended to keep the second component of \code{trials} low, unless the number of observations is large enough.
}

\value{
A matrix \code{error} containing the predictive squared error (se), the root predictive squared error (rse), and the root mean squared error (rmse). If \code{estimated=TRUE}, returns an additional matrix of three columns containing the values of the environmental parameter (\code{"x"}), and observed (\code{"observed"}) and predicted (\code{"predicted"}) percentages.
}

\references{
Cleveland, W.S., and S.J. Devlin. 1988. Locally weighted regression: An approach to regression analysis by local fitting. \emph{Journal of the American Statistical Association} 83: 596-610.
}

\author{
Alexander Correa-Metrio, Kenneth R. Cabrera.
}

\seealso{
\code{\link[stats]{loess}} for details on LOESS regression.
}

\examples{
data(modernq)
# Calculate percentages
perq<-percenta(modernq,first=2,last=39)[,2:55]
# Cross-validation for Pinus
bcrossv.l1(modernq[,1],perq[,3])
}

\keyword{ models }
\keyword{ robust } 