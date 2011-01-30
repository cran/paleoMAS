\name{syas}
\alias{syas}

\title{
Synthetic assemblages
}
\description{
This function performs all the procedures for synthetic assemblages construction, and estimation of fossil environmental parameters.}

\usage{
syas(x, y, z, fossil, age, delta, percen, method = "canberra",
    quant = 0.05, plot = TRUE, window = 3, detrend = FALSE, 
    crossv, maximum = 0.99, classes = 200, colors = c("red","blue"), 
    line = TRUE, ylabel = "Age", xlabel = "Parameter")
}

\arguments{
  \item{x}{
A vector containing the environmental gradient value for each sample or location.
}
  \item{y}{
A matrix containing the observed taxa abundances along the environmental gradient. Taxa are in columns and samples in rows. Taxa included in this analysis should be selected by evaluating dispersion and residuals distribution through the function \code{\link{dispersion.all}}.
}
  \item{z}{
A matrix containing the parameters for the LOESS regression of each species (\eqn{\alpha} and degree in that order). Objects produced by \code{\link{bcrossv.all}} and \code{\link{akaike.all}} could be used as \code{z}.
}
  \item{fossil}{
A matrix containing the fossil pollen percentages with samples in rows and taxa in columns.
}
  \item{age}{
A vector containing the age or depth of each fossil sample.
}
  \item{delta}{
Increments for the LOESS regression prediction. See \code{\link[stats]{predict.loess}} for details.
}
  \item{percen}{
A vector containing total percentage of pollen in each modern sample. See \code{\link{synthetic}} for details.
}
  \item{method}{
Corresponds to methods available in \code{vegdist} of \pkg{vegan}. Available methods are \code{"manhattan"},
    \code{"euclidean"}, \code{"canberra"}, \code{"bray"}, \code{"kulczynski"},
     \code{"jaccard"}, \code{"gower"}, \code{"morisita"}, \code{"horn"},
     \code{"mountford"}, \code{"raup"} , \code{"binomial"} or \code{"chao"}. See \code{\link[vegan]{vegdist}} (package \pkg{vegan}) for details.
}
  \item{quant}{
The quantile of the distances that will be used to obtain the mean environmental parameter.
}
  \item{plot}{
Logical indicating whether or not a plot of the fossil reconstruction is desired.
}
  \item{window}{
The plot is done using a moving average along the estimated values to avoid the excess of noise. This parameter refers to the size of the moving average. Only relevant if \code{plot=TRUE}.
}
  \item{detrend}{
Logical indicating whether or not a detrending procedure is desired. See \code{bcrossv.sa} for details.
}
  \item{crossv}{
An object produced by \code{bcrossv.sa}, only relevant when \code{detrend=TRUE}.
}
  \item{maximum}{Percentage of data used to build the color tables, as extreme high distance values can make all values fall into low classes, showing only two colors. Only relevant if \code{plot=TRUE}.}
  \item{classes}{Number of classes in the color ramp. See \code{\link[grDevices]{colorRamp}} for details. Only relevant if \code{plot=TRUE}.}
  \item{colors}{Color to interpolate for the color ramp. See \code{\link[grDevices]{colorRamp}} for details. Only relevant if \code{plot=TRUE}.}
  \item{line}{Logical indicating whether or not a line showing the estimated values is desired. Only relevant if \code{plot=TRUE}.}
  \item{ylabel}{Label for y axis, which is depth or age. Only relevant if \code{plot=TRUE}.}
  \item{xlabel}{Label for x axis, which corresponds to the environmental variable. Only relevant if \code{plot=TRUE}.}
}


\value{
A list containing:
\item{syn.as }{A list containing the elements \code{loess.f} and \code{predicted } produced by the function \link{synthetic}.}
\item{values }{Fossil environmental reconstruction values. If \code{detrend=TRUE}, both estimates and detrended estimates are returned in this component.}
Additionally, if \code{plot=TRUE} a plot of the environmental estimation through time or depth is produced.
}

\references{
Correa-Metrio, A., M.B. Bush, L.Perez, A. Schwalb, and K. Cabrera. Accepted. Pollen distribution along climatic and biogeographic gradients in northern Central America. The Holocene
}

\author{
Alexander Correa-Metrio.
}

\seealso{
See \code{\link{synthetic}}, \code{\link{fossil.dist}}, and \code{\link{fossil.values}}, for details on the fossil reconstruction through synthetic assemblages. See \code{\link{bcrossv.sa}} for details on detrending procedure and error estimation. 
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
# distributed residuals in percentages matrix
perq2<-perq1[,-c(3:5,8,17,19)]
#cross-validation of the method
bcrossv.sa(modernq[,1],perq2,a.d,delta=25,method="canberra",quant=0.05,trials=c(5, 0.05),detrend=FALSE)
#synthetic assemblages fossil estimation without detrending
syas(modernq[,1],perq2,a.d,fossilq,fossilq[,1],delta=25,percen=rep(100,53),
plot=TRUE,window=3,detrend=FALSE,xlabel="Elevation (m asl)")
#synthetic assemblages fossil estimation with detrending
#first, estimation of the detrending parameters
crossv.q<-bcrossv.sa(modernq[,1],perq2,a.d,delta=25,method="canberra",
quant=0.05,trials=c(5, 0.05),detrend=TRUE)
#second, application of the synthetic assemblages using the
# results of the cross-validation for the detrending.
syas(modernq[,1],perq2,a.d,fossilq,fossilq[,1],delta=25,percen=rep(100,53),
plot=TRUE,detrend=TRUE,crossv=crossv.q,xlabel="Elevation (m asl)")
}

\keyword{ manip }
\keyword{ arith }
\keyword{ math }
\keyword{ multivariate }