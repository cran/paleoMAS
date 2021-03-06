\name{bcrossv.l}
\alias{bcrossv.l}

\title{
LOESS regression parameters selection through bootstrap cross-validation for one taxon
}
\description{
This function applies v-fold crossvalidation (CV) of LOESS regressions with varying \eqn{\alpha} and degree for a single taxon.
}
\usage{
bcrossv.l(x, y, interval = c(0.15, 1, 0.05), 
         trials = c(10, 0.25), plot = TRUE)
}

\arguments{
  \item{x}{
A vector containing the environmental gradient value for each sample or location.
}
  \item{y}{
A vector containing the observed taxa abundances along the environmental gradient.
}
  \item{interval}{
\eqn{\alpha} values to be evaluated. Three numbers in the following order: lower and upper limits of the \eqn{\alpha}-values sequenceto be evaluated, and the increment of the sequence.  
}
  \item{trials}{
Values for number of repetitions of the crossvalidation (100 by default), and proportion of observations to be left out each time the CV is repeated (0.25 by default). The observations left out each time are randomly selected with replacement.
}
  \item{plot}{
Logical indicating whether or not an observed vs. error is desired
}
}
\details{
A sequence of \eqn{\alpha} values is generated according to the range and increments declared in \code{interval} (from 0.15 to 1 in 0.05 increments). The sequence of \eqn{\alpha} values is combined with degree 1 and 2, and the LOESS regression produced using each combination is cross-validated as explained in \code{\link{bcrossv.l1}}. See Cleveland and Devlin (1988) for details on loess regression.
}

\value{
A matrix of 4 columns. All possible combinations of \eqn{\alpha} values generated by the sequence \code{interval} and degree 1 and 2 are contained in the first and second columns. The third column contains the root of the predictive squared error (rse, (1/n*sum(est-obs)^2)^0.5). The fourth column contains the root mean squared error (rmse, 1/n*sum(abs(obs-est))). If \code{plot=TRUE}, a scatter plot of the rse and the rmse conditioned to the degree of the loess regression is produced.

This function involves very intensive computations. Therefore, the user must find a balance between values of \code{interval} and \code{trials}, and the computational capabilities. Large number of trials may require long computation times.
}
\references{
Cleveland, W.S., and S.J. Devlin. 1988. Locally weighted regression: An approach to regression analysis by local fitting. \emph{Journal of the American Statistical Association} 83: 596-610.
}

\author{
Alexander Correa-Metrio, Kenneth R. Cabrera.
}


\seealso{
\code{\link[stats]{loess}} for details on loess regression. \code{\link{bcrossv.l1}} for details on the bootstrap cross-validation.
}
\examples{
data(modernq)
# Calculate percentages
perq<-percenta(modernq,first=2,last=39)[,2:55]
# Cross-validation for Pinus
bcrossv.l(modernq[,1],perq[,3],trials=c(10,0.1))

}

\keyword{ models }
\keyword{ robust } 
