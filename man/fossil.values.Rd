\name{fossil.values}
\alias{fossil.values}

\title{
Fossil environmental parameters calculation
}
\description{
This function recovers the values of the fossil environmental parameters by evaluating the distance of each fossil sample to each modern synthetic assemblage.
}
\usage{
fossil.values(distan, age, quant = 0.05, detrend = FALSE, crossv)
}

\arguments{
  \item{distan}{
A matrix containing the distance of each fossil sample to each modern synthetic assemblage, produced by \code{fossil.dist}.
}
  \item{age}{
A vector containing the age or depth of each fossil sample.
}
  \item{quant}{
The quantile of the distances that will be used to obtain the mean environmental parameter. See details below.
}
  \item{detrend}{
Whether or not a detrending procedure is desired. See \code{bcrossv.sa} for details.
}
  \item{crossv}{
An object produced by \code{bcrossv.sa}. Only relevant when \code{detrend=TRUE}.
}
}

\details{
The fossil environmental value is assumed to be the value associated with the modern synthetic assemblage that displays the least distance to the fossil sample. To counteract possible bias produced by local resemblances, the value is calculated as the average among the values associated with the synthetic assemblages that display distances within the quantile defined in \code{quant}.
}

\value{
A matrix with ages or depths in the first column and values of the environmental parameter in the second column.
}

\references{
Correa-Metrio, A., M.B. Bush, L.Perez, A. Schwalb, and K. Cabrera. Accepted. Pollen distribution along climatic and biogeographic gradients in northern Central America. The Holocene
}

\author{
Alexander Correa-Metrio, Kenneth R. Cabrera.
}

\seealso{
\code{\link{bcrossv.sa}}
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
a.d1<-a.d[-c(3:5,8,17,19),]
#build synthetic assemblages
syas.q<-synthetic(modernq[,1],perq2,a.d1,delta=25,plot=TRUE,
percen=rep(100,53))
#calculate distance from synthetic assemblages to each fossil sample
distances<-fossil.dist(syas.q,fossilq)
#calculate the fossil values of the environmental variable
# without detrending
fossil.values(distances,fossilq[,1],quant=0.05,detrend=FALSE)
#if detrending is desired, the detrending parameters have to be obtained
crossv.q<-bcrossv.sa(modernq[,1],perq2,a.d1,delta=25,method="canberra",
quant=0.05,trials=c(5, 0.05),detrend=TRUE)
#calculate the fossil values of the environmental variable
# with detrending
fossil.values(distances,fossilq[,1],quant=0.05,detrend=TRUE,
crossv=crossv.q)

}

\keyword{ manip }
\keyword{ arith }
\keyword{ math }
\keyword{ multivariate }