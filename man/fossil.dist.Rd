\name{fossil.dist}
\alias{fossil.dist}

\title{
Distance calculation between fossil samples and modern synthetic assemblages
}
\description{
This function calculates the distance between each fossil sample and the synthetic assemblages, according to the selected distance method.
}
\usage{
fossil.dist(modern, fossil, method = "canberra")
}

\arguments{
  \item{modern}{
An object containing the synthetic assemblages produced by the function \code{synthetic}.
}
  \item{fossil}{
A matrix containing the fossil pollen percentages with samples in rows and taxa in columns.
}
  \item{method}{
Corresponds to methods available in \code{vegdist} of \pkg{vegan}. Available methods are \code{"manhattan"},
    \code{"euclidean"}, \code{"canberra"}, \code{"bray"}, \code{"kulczynski"},
     \code{"jaccard"}, \code{"gower"}, \code{"morisita"}, \code{"horn"},
     \code{"mountford"}, \code{"raup"} , \code{"binomial"} or \code{"chao"}. See \code{\link[vegan]{vegdist}} (package \pkg{vegan}) for details.
}
}

\value{
A matrix containing the distance or dissimilarity between fossil samples and each one of the modern synthetic assemblages. Fossil samples are in rows and modern synthetic assemblages are in columns. The first row contains the values of the environmental gradient associated with each synthetic assemblage.}

\references{
Correa-Metrio, A., M.B. Bush, L.Perez, A. Schwalb, and K. Cabrera. Accepted. Pollen distribution along climatic and biogeographic gradients in northern Central America. The Holocene
}

\author{
Alexander Correa-Metrio, Kenneth R. Cabrera.
}

\seealso{
See \code{\link{synthetic}} for details on the construction of modern synthetic assemblages. See \code{\link[vegan]{vegdist}}  for details on dissimilarity indices.
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
fossil.dist(syas.q,fossilq)
}

\keyword{ manip }
\keyword{ arith }
\keyword{ math }
\keyword{ multivariate }