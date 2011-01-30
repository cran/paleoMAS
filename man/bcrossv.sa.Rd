\name{bcrossv.sa}
\alias{bcrossv.sa}

\title{
Bootstrap cross-validation of synthetic assemblages fossil estimates
}

\description{
This function applies repeated v-fold cross-validation to the synthetic assemblages fossil estimations, producing an estimation of the error associated with the reconstruction.
}

\usage{
bcrossv.sa(x, y, z, delta, method = "canberra", quant = 0.05,
          trials = c(10, 0.05), detrend = FALSE)
}

\arguments{
  \item{x}{
A vector containing the environmental gradient value for each sample or location.
}
  \item{y}{
A matrix containing the observed taxa abundances along the environmental gradient. Taxa are in columns and samples in rows. See \link{synthetic} for details
}
  \item{z}{
A matrix containing the parameters for the LOESS regression of each species (\eqn{\alpha} and degree in that order). Objects produced by \code{\link{bcrossv.all}} and \code{\link{akaike.all}} could be used as \code{z}.
}
  \item{delta}{
Increments for the LOESS regression prediction. See \code{\link[stats]{predict.loess}} for details.
}
  \item{method}{
Corresponds to methods available in \code{vegdist} of \pkg{vegan}. Available methods are \code{"manhattan"},
    \code{"euclidean"}, \code{"canberra"}, \code{"bray"}, \code{"kulczynski"},
     \code{"jaccard"}, \code{"gower"}, \code{"morisita"}, \code{"horn"},
     \code{"mountford"}, \code{"raup"} , \code{"binomial"} or \code{"chao"}. See \code{\link[vegan]{vegdist}} (package \pkg{vegan}) for details.
}
  \item{quant}{
The quantile of the distances that will be used to obtain the mean environmental parameter. See \code{\link{fossil.values}} for details.
}
  \item{trials}{
Values for number of repetitions of the crossvalidation (10 by default), and proportion of observations to be left out each time the CV is repeated (5 percent of observations by default). The observations left out each time are randomly selected with replacement.
}
  \item{detrend}{
Whether or not a detrending procedure is desired.
}
}

\details{
Detrending is used when the distribution of residuals along estimated values produces a heteroscedastic pattern. The evaluation of such a pattern is done by fitting a linear regression of residuals as a function of estimated values (RES=a+b*ESTIMATE). The significance of the regression is given in terms of its intercept, slope, and p-value, which are shown in the \code{coef} component of the list produced when applying the function with \code{detrend=FALSE}. If the regression is significant, it is recommended to run the analysis again using \code{detrend=TRUE}. The detrending is done translating the estimated values using the found regression line (DETRENDED ESTIMATE = ESTIMATE + (a + b*ESTIMATE)).
}

\value{
A list containing:
\item{estimated }{A matrix with three columns: observed, estimated and error. If \code{detrend=TRUE}, additional columns provide estimated error thorugh regression line (a + b*ESTIMATE, \code{Est error}), rotated estimate (ESTIMATE + (a + b*ESTIMATE), \code{Rotated}), translated error (RES + MEAN(RES), \code{Translated}), and detrended error (OBSERVED - TRANSLATED ESTIMATE, \code{Det error}).}
\item{error }{A matrix displaying squared sum of errors (\code{sse}), and root squared mean error (\code{rmse}). If \code{detrend=TRUE}, these two parameters are also provided for detrended estimation.}
\item{coef }{A matrix containing intercept, slope and p-value of the residuals vs estimates regression.}
\item{transl }{The average error that is added or substracted to each estimate. Only relevant if \code{detrend=TRUE}.}

If \code{detrend=FALSE}, a figure shows residuals distribution along estimated (upper panel) and observed values (lower panel). If \code{detrend=FALSE}, a figure shows residuals distribution along estimates (upper panel) and detrended estimates (lower panel).
}

\references{
Correa-Metrio, A., M.B. Bush, L.Perez, A. Schwalb, and K. Cabrera. Accepted. Pollen distribution along climatic and biogeographic gradients in northern Central America. The Holocene
}

\author{
Alexander Correa-Metrio.
}

\seealso{
\code{\link{synthetic}}, \code{\link{fossil.dist}}, \code{\link{fossil.values}}, \code{\link{syas}}.
}

\examples{
data(modernq,fossilq)
# Calculate percentages
perq<-percenta(modernq,first=2,last=39)[,2:55]
# filter data set to include only samples with at least 0.5
# percent in 20 percent of the samples
perq1<-filter.p(perq,presen=0.5,persist=0.2)$filtered
# calculate alpha and degree for each taxon through AIC
a.d<-akaike.all(modernq[,1],perq1)
# select taxa that have acceptable dispersion and normally
# distributed residuals in percentages and AIC matrices
perq2<-perq1[,-c(3:5,8,17,19)]
#cross-validation without detrending
bcrossv.sa(modernq[,1],perq2,a.d,delta=25,method="canberra",quant=0.05,
trials=c(5, 0.05),detrend=FALSE)
#cross-validation with detrending
bcrossv.sa(modernq[,1],perq2,a.d,delta=25,method="canberra",quant=0.05,
trials=c(5, 0.05),detrend=TRUE)

}

\keyword{ manip }
\keyword{ arith }
\keyword{ math }
\keyword{ multivariate }
\keyword{ robust }