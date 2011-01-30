\name{dispersion.all}
\alias{dispersion.all}

\title{
Taxon response to environmental variables through LOESS regression
}
\description{
This function generates a family of responses of 1 or several taxa to an environmental gradient. A bootstrap procedure is performed to calculate the dispersion of the species response.
}
\usage{
dispersion.all(x, y, z, delta, trials = c(100, 0.25), 
              conf = c(0.025, 0.975), outfile = "Dispersion.pdf")
}

\arguments{
  \item{x}{
A vector containing the environmental gradient value for each sample or location.
}
  \item{y}{
A matrix containing the observed taxa abundances along the environmental gradient. Taxa are in columns and samples in rows.
}
  \item{z}{
A matrix containing the parameters for the LOESS regression of each species (\eqn{\alpha} and degree in that order). Objects produced by \code{\link{bcrossv.all}} and \code{\link{akaike.all}} could be used as \code{z}.
}
  \item{delta}{
Increments for the loess regression prediction. See \code{\link[stats]{predict.loess}} for details.
}
  \item{trials}{
Values for number of repetitions of the cross-validation (100 by default), and proportion of observations to be left out each time the CV is repeated (0.25 by default). The observations left out each time are randomly selected with replacement.
}
  \item{conf}{
Quantiles for the confidence intervals of the LOESS curves.
}
\item{outfile}{
The name of a file (format pdf) where the resulting plots will be saved. The file is saved in the working directory.
}

}

\details{
The response of each taxa to the environmental gradient is calculated through a LOESS regression (Cleveland and Devlin, 1988). To capture the variability the LOESS is recalculated as many times as indicated in the first component of \code{trials} leaving out a proportion equal to the second component of \code{trials}. The LOESS regression is used to predict the response of each taxon in a sequence that goes through the range of the observed environmental gradient in increments equal to \code{delta}. For each delta the minimum, maximum, average and \code{conf} quantiles are calculated.
The coefficient of determination (R2) is calculated as in linear regression, where R2=SSregression/SStotal. SSregression = sum(ESTIMATE - mean(OBSERVED))^2, and SStotal = sum(OBSERVED - mean(OBSERVED))^2.
}

\value{
The function returns plots of taxa responses to the environmental gradient. The plots are saved in file named after \code{outfile} in pdf format. The total variation detected through the CV is signaled by the shaded area. Dashed lines indicate the confidence intervals, and a continuous line indicates the mean response of the taxon to the environmental gradient. Filled diamonds represent data points, whereas red crosses represent data points with percentage values higher than those displayed. Additionally, Q-Q plots of the LOESS regression residuals of each taxon are saved in the same file. Solid line represents the ideal residual distribution (theoretical quantiles equal to observed quantiles), and dashed line is built as a line passing through the first and third observed quartiles.

Additionally, a list is returned containing:
\item{limits }{A list containing the mean response of each taxon to the environmental gradient, and the limits of its dispersion.}
\item{coef.det }{A matrix containing the sum of squares of the LOESS regression (SS regression), the total sum of squares (SS total), and the coefficient of determination (R2) for each taxon.}
}

\references{
Correa-Metrio, A., M.B. Bush, L.Perez, A. Schwalb, and K. Cabrera. Accepted. Pollen distribution along climatic and biogeographic gradients in northern Central America. The Holocene

Cleveland, W.S., and S.J. Devlin. 1988. Locally weighted regression: An approach to regression analysis by local fitting. \emph{Journal of the American Statistical Association} 83: 596-610.
}

\author{
Alexander Correa-Metrio, Kenneth R. Cabrera.
}

\seealso{
\code{\link[stats]{loess}} for details on LOESS regression. \code{\link{bcrossv.all}} and \code{\link{akaike.all}} for details related to \code{z}.
}
\examples{
data(modernq)
# Calculate percentages
perq<-percenta(modernq,first=2,last=39)[,2:55]
# filter data set to include only samples with at least 0.5
# percent in 20 percent of the samples
perq1<-filter.p(perq,presen=0.5,persist=0.2)$filtered
# calculate alpha and degree for each taxon through AIC
a.d<-akaike.all(modernq[,1],perq1)
# evaluate dispersion and residuals for each loess regression
dispersion.all(modernq[,1],perq1,a.d,trials=c(50,0.1),delta=25)
}
\keyword{ models }
\keyword{ robust } 
\keyword{ smooth }