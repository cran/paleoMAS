\name{change}
\alias{change}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Dissimilarities, distances and rates of change }
\description{
  This function calculates dissimilarity or distances between contoguous samples (timeslices), as well as rates of ecological change when a robust age model is available.  
}
\usage{
change(x, age, dca = FALSE, meth = "euclidean", bin = FALSE,
 roc = FALSE, digits = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ Numeric matrix with samples in rows and species in columns. }
  \item{age}{ Vector with sample ages. }
  \item{dca}{ Basis for dissimilarity computation (\code{TRUE}: uses DCA axis scores, \code{FALSE}: uses the distance measure specified in \code{meth}). }
  \item{meth}{ Corresponds to methods available in \code{vegdist} of \pkg{vegan}. Available methods are \code{"manhattan"},
    \code{"euclidean"}, \code{"canberra"}, \code{"bray"}, \code{"kulczynski"},
     \code{"jaccard"}, \code{"gower"}, \code{"morisita"}, \code{"horn"},
     \code{"mountford"}, \code{"raup"} , \code{"binomial"} or \code{"chao"}. See \code{\link[vegan]{vegdist}} (package \pkg{vegan}) for details.}
  \item{bin}{ Argument of the function \code{\link[vegan]{vegdist}} (package \pkg{vegan}) that standardizes the data into presence/absence before claculating the dissimilarity. }
  \item{roc}{ Logical argument of whether or not the calculation of rates of change is desired.}
  \item{digits}{ Number that specifies the digits desired for the rounded-up ages. }
}
\details{
  Rates of change are calculated as: RoC[jk] = vegdist[jk]/"res"). For further discussion on assumptions involved, see Urrego et al (2009). DCA is calculated according to \code{decorana} (package \pkg{vegan}.)
}

\value{
  Returns a matrix with ages and their corresponding dissimilarity, distances or RoC. As calculations between subsequent samples return n-1 observarions, the distance, dissimilarity, or RoC are assigned to the youngest of the two contiguous samples. When \code{roc=TRUE}, it also returns a vector \code{resolution} with time steps between samples.
}

\author{ Dunia H. Urrego, Alexander Correa-Metrio. }

\references{ Urrego DH, Bush M, Silman MR, Correa-Metrio A, Ledru M-P, Mayle FE, Valencia BG (2009) Millennial-scale ecological changes in tropical South America since the Last Glacial Maximum. Past climate variability from the Last Glacial Maximum to the Holocene in South America and surrounding regions. (eds. Vimeux F, Sylvestre F, Khodri M). Springer.

Oksanen, J., R. Kindt, P. Legendre, B. OHara, G.L. Simpson, P. Solymos, M. Henry H. Stevens and H. Wagner (2008). vegan: Community Ecology Package. R package version 1.15-1.
}

\seealso{ See \code{\link[vegan]{vegdist}} and \code{\link[vegan]{decorana}} for details on the dissimilarity indexes. }

\examples{
data(quexilchron,quexildepths,quexilper)
ages<-chron(quexilchron,quexildepths,max.depth=1957)$chronology[,2]
#Absolute change
change(quexilper,ages,meth="bray")
change(quexilper,ages,dca=TRUE)
#Rate of change
change(quexilper,ages,meth="bray",roc=TRUE)
change(quexilper,ages,dca=TRUE,roc=TRUE)
}

\keyword{ manip }
\keyword{ arith }
\keyword{ math }
\keyword{ multivariate }
 
