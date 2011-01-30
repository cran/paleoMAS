\name{bcrossv.all}
\alias{bcrossv.all}

\title{
LOESS regression parameters selection through bootstrap cross-validation for multiple taxa}

\description{
This function applies v-fold crossvalidation (CV) of loess regressions with varying \eqn{\alpha} and degree for multiple taxa. The final result contains  \eqn{\alpha} and degree values that minimize the error of the loess regression for each taxon.
}
\usage{
bcrossv.all(x, y, interval = c(0.15, 1, 0.05), 
           trials = c(10, 0.25), target = c("rse","rmse"))
}

\arguments{
  \item{x}{
A vector containing the environmental gradient value for each sample or location.
}
  \item{y}{
A matrix containing the observed taxa abundances along the environmental gradient. Taxa are in columns and samples in rows.
}
  \item{interval}{
\eqn{\alpha} values to be evaluated. Three numbers in the following order: lower and upper limits of the sequence of \eqn{\alpha} values to be evaluated, and increment of the sequence.  
}
  \item{trials}{
Values for number of repetitions of the crossvalidation (100 by default), and proportion of observations to be left out each time the CV is repeated (25 percent of observations by default). The observations left out each time are randomly selected with replacement.
}
  \item{target}{
If \code{target="rse"} the values of alpha and degree that minimize the LOESS regression root of the predictive squared error are selected. If \code{target="rmse"} the values of  \eqn{\alpha} and degree that minimize the LOESS regression root mean squared error are selected. 
}
}

\value{
A matrix containing  \eqn{\alpha} and degree values that minimize the LOESS regression rse or rmse (depending on the choice in \code{target}.)
}
\references{
Correa-Metrio, A., M.B. Bush, L.Perez, A. Schwalb, and K. Cabrera. Accepted. Pollen distribution along climatic and biogeographic gradients in northern Central America. The Holocene

Cleveland, W.S., and S.J. Devlin. 1988. Locally weighted regression: An approach to regression analysis by local fitting. \emph{Journal of the American Statistical Association} 83: 596-610.
}

\author{
Alexander Correa-Metrio, Kenneth R. Cabrera.
}

\seealso{
\code{\link[stats]{loess}} for details on loess regression. \code{\link{bcrossv.l1}} and \code{\link{bcrossv.l}} for details on the bootstrap cross-validation.
}
\examples{
data(modernq)
# Calculate percentages
perq<-percenta(modernq,first=2,last=39)[,2:55]
bcrossv.all(modernq[,1],perq[,1:5],trials=c(10,0.1),target="rse")
bcrossv.all(modernq[,1],perq[,1:5],trials=c(10,0.1),target="rmse")
}

\keyword{ models }
\keyword{ robust } 