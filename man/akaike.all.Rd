\name{akaike.all}
\alias{akaike.all}

\title{
LOESS regression parameters selection through AIC minimization for multiple taxa
}
\description{
This function applies Akaike Information Criterion (AIC) to varying \eqn{\alpha} and degree in LOESS regression for multiple taxa. The final result contains alpha and degree values that minimize AIC for each taxon LOESS regression as a function of the environmental parameter.
}
\usage{
akaike.all(x, y, interval = c(0.15, 1, 0.05))
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
}

\details{
See \code{\link{akaike.l}} for details. This procedure is less computation intensive than cross-validation, but the resulting parameters do not provide the LOESS regression that best fit the  data. Instead, it selects the parameters that best fit the data conditioned to simplicity of the model.
}

\value{
A matrix containing the values of \eqn{\alpha} and degree that minimize AIC of the LOESS regression for each taxon, and the value of the AIC.
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
\code{\link[stats]{loess}} for details on LOESS regression, and \code{\link{akaike.l}} for details on AIC.
}
\examples{
data(modernq)
# Calculate percentages
perq<-percenta(modernq,first=2,last=39)[,2:55]
akaike.all(modernq[,1],perq[,1:10])

}
\keyword{ models }
\keyword{ robust } 