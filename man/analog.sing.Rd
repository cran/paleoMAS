\name{analog.sing}
\alias{analog.sing}

\title{Single analog comparison}

\description{
 This function calculates and plots distance or dissimilarity indexes (analogy degree) between all samples in a fossil record and a single sample from the same record.
}

\usage{
analog.sing(fossil, base, age, dca = FALSE, method = "euclidean",
           binary = FALSE)
}

\arguments{
  \item{fossil}{A matrix of fossil data containing samples in rows and species in columns.}
  \item{base}{ Row number where the analog sample is.}
  \item{age}{ A vector with samples ages. }
  \item{dca}{ Logical indicating whether or not distances should be calculated based on detrended correspondence analysis scores (DCA performed according to \code{decorana} of \pkg{vegan}). If \code{dca=TRUE}, the dissimilarity between samples is calculated using scores of the firt 4 axes of a DCA. Some DCA scores are negative, therefore caution is needed to select an appropriate dissimilarity method when \code{dca=TRUE}. If \code{method="schord"} DCA cannot be used.}
  \item{method}{ Corresponds to squared chord distance (\code{"schord"}), and methods available in \code{vegdist} of \pkg{vegan}. Available methods are \code{"manhattan"},
    \code{"euclidean"}, \code{"canberra"}, \code{"bray"}, \code{"kulczynski"},
     \code{"jaccard"}, \code{"gower"}, \code{"morisita"}, \code{"horn"},
     \code{"mountford"}, \code{"raup"} , \code{"binomial"} or \code{"chao"}. See \code{\link[vegan]{vegdist}} (package \pkg{vegan}) for details.}
  \item{binary}{ Argument of the function \code{\link[vegan]{vegdist}} (package \pkg{vegan}) that standardizes the data into presence/absence before calculating dissimilarities or distances.}
}

\value{
  Returns a matrix with sample ages and resulting distance or dissimilarity index between each sample and the selected analog.
}

\author{ Dunia H. Urrego, Alexander Correa-Metrio, Mark B. Bush.}

\references{
Correa-Metrio, A. 2010. Climate and vegetation of the Yucatan Peninsula during the late Pleistocene. PhD Dissertation, Florida Institute of Technology, Melbourne, FL. 194 p.
		
Overpeck, J.T. T.I. Webb, and I.C. Prentice. 1985, Quantitative interpretation of fossil pollen spectra: Dissimilarity coefficients and the method of modern analogs. \emph{Quaternary Research} 23: 87-708.
}



\seealso{ 
\code{\link{analog.mult}}. See \code{\link[vegan]{vegdist}} for details on the dissimilarity indices. }

\examples{
data(quexilper,quexilchron,quexildepths)
age<-quexilchron
d<-quexildepths
ages<-chron(age,d,max.depth=1957,dates="bars")$chronology[,2]
#distance to the first sample without DCA, using canberra metric
analog.sing(quexilper,base=1,ages,method="canberra")
#distance to the first sample with DCA, using euclidean distance
analog.sing(quexilper,base=1,ages,dca=TRUE)
}

\keyword{ manip }
\keyword{ arith }
\keyword{ math }
\keyword{ multivariate }