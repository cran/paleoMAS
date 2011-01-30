\name{synthetic}
\alias{synthetic}

\title{
Calculation of synthetic assemblages
}

\description{
This fucntion calculates the ideal pollen assemblages associated with a series of values of a given environmental factor.
}

\usage{
synthetic(x, y, z, delta, plot = FALSE, percen, upmargin = 0.1,
         widths = c(1, 4), steps = 0.05)
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
  \item{delta}{
Increments for the LOESS regression prediction. See \code{\link[stats]{predict.loess}} for details.
}
  \item{plot}{
Logical indicating whether or not a plot of the synthetic assemblages is desired.
}
  \item{percen}{
A vector containing total percentage of pollen in each modern sample. As some taxa might have been excluded from the pollen sum, this value could be larger than 100. See details below.
}
  \item{upmargin}{
A number indicating the margin size for the upper part of each panel. \code{par} function, \code{mai} parameter.
}
  \item{widths}{
A vector of values for the widths of columns in the plotting device. \code{layout} parameter.
}
  \item{steps}{
The proportion of the range to draw lines separating each synthetic assemblage.
}
}

\details{
When applying the synthetic assemblages technique, it is desirable to know the proportion of data from each sample that was used in the the construction of the modern ideal assemblages. The \code{avg.info} returns the proportion of pollen data that was used from each sample. If no taxa are excluded from the pollen sum, such proportion will coincide with the sum of the percentages of the taxa included. In this case, the argument \code{percent} can be filled by repeating 100 as many times as number of of modern samples (\code{percent = rep(100, nrow(y))}). When one or more taxa are excluded from the pollen sum, samples might have total percentages that are larger than 100. Therefore, the \code{percent} has to be calculated by adding all the percentages in each sample (\code{percent = apply (Y, 1, sum)}, where \code{Y} is the original percentage matrix including all taxa).	
}

\value{
This function returns a list containing:
\item{loess.f}{A list with the LOESS regression for each taxon.}
\item{predicted}{A matrix containing the predicted percentages of each taxon along the evaluated environmetal gradient. The first column contains the values of the environmental variable, and taxa are contained in the subsequent columns.}
\item{avg.info}{if \code{plot=TRUE}, this value is the average pollen percentage used from each sample in the construction of the synthetic assemblages.}
Additionally, if \code{plot=TRUE}, a figure containing the synthetic assemblages is produced. Given differences in pollen representativity among species, individual taxa are standardized to facilitate illustration.
}

\references{
Correa-Metrio, A., M.B. Bush, L.Perez, A. Schwalb, and K. Cabrera. Accepted. Pollen distribution along climatic and biogeographic gradients in northern Central America. The Holocene
}

\author{
Alexander Correa-Metrio, Kenneth R. Cabrera.
}


\seealso{
\code{\link[graphics]{par}} and \code{\link[graphics]{layout}} for details on graphic parameters. 
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
# select taxa that have acceptable dispersion and normally 
# distributed residuals in percentages and AIC matrices
perq2<-perq1[,-c(3:5,8,17,19)]
a.d1<-a.d[-c(3:5,8,17,19),]
#build synthetic assemblages
syas.q<-synthetic(modernq[,1],perq2,a.d1,delta=25,plot=TRUE,
percen=rep(100,53))
# predic percentages of Cyperacea for an elevation sequence from
# 100 to 550 in 25-m increments
predict(syas.q[[1]]$Cyperaceae,seq(100,550,25))
}

\keyword{ models }
\keyword{ graphs }
\keyword{ smooth }
 