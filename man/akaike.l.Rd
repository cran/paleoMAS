\name{akaike.l}
\alias{akaike.l}

\title{
Loess regression parameters selection through AIC minimization for one taxon
}
\description{
This function calculates the Akaike Information Criteron (AIC) of LOESS regressions with varying \eqn{\alpha} and degree for a single taxon.
}
\usage{
akaike.l(x, y, interval = c(0.15, 1, 0.05), plot = TRUE,
        parameters = FALSE)
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
  \item{plot}{
Logical indicating whether or not a figure displaying the behavior of AIC through \eqn{\alpha} values conditioned to degree is plotted.
}
  \item{parameters}{
Logical indicating whether or not a matrix with the parameters of each LOESS regression is desired.
}

}
\details{
AIC is calculated according to Hurvich & Simonoff (1998) for LOESS regressions (Cleveland and Devlin, 1988) generated through all possible combinations between degree (1 and 2) and the generated series of \eqn{\alpha} values. 
}

\value{
A list with two components containing:
  \item{aic.loess }{A matrix with the AIC values for each combination of \eqn{\alpha} and degree.}
  \item{minimum }{The values of \eqn{\alpha} and degree that minimize AIC.}
  \item{minimum }{A matrix containing all the parameters of the LOESS regression for each combination of \eqn{\alpha} and degree (see Cleveland and Devlin (1988) and Hurvich & Simonoff (1998) for details). Only relevant if \code{parameters=TRUE}.}
}

\references{
Correa-Metrio, A., M.B. Bush, L.Perez, A. Schwalb, and K. Cabrera. Accepted. Pollen distribution along climatic and biogeographic gradients in northern Central America. The Holocene

Hurvich, C.M., and J.S. Simonoff. 1998. Smoothing parameters selection in nonparametric regression using an improved Akaike information criterion. \emph{Journal of the Royal Society, Series B} 60: 271-293.

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
akaike.l(modernq[,1],perq[,3])
akaike.l(modernq[,1],perq[,3],parameters=TRUE)
}

\keyword{ models }
\keyword{ robust } 
