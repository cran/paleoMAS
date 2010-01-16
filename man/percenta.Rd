\name{percenta}
\alias{percenta}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Computation of percentage matrices }
\description{
  This function allows computation of percentage matrices based on a dataframe with species in columns and samples in rows. 
}
\usage{
percenta(x, first, last)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ Dataframe cotaining raw data with species in columns and samples in rows.}
  \item{first}{ Number of first column in \code{x} (taxon) to be included in the sum that serves as basis for the percentage calculation (e.g. pollen sum, see Birks & Birks (1980)). }
  \item{last}{ Number of last column in \code{x} (taxon) to be included in the sum that serves as basis for the percentage calculation (e.g. pollen sum, see Birks & Birks (1980)). }
}
\details{  
  Percentages can be calculated based on a selected group of columns or taxa (e.g. terrestrial taxa included in the pollen sum for palynological analyses, see Birks & Birks (1980) for ilustration on pollen sum). For the rest of the columns (taxa) percentages are calculated based on totals from included columns (taxa), e.g. spores and aquatics.
  
  The dataframe should be organized in such a way that taxa or columns to be included in the percentage calculation are contiguous. Percentages of other columns outside that range (first to last) are calculated based on the total sum of elements included (e.g. pollen sum).
}
\value{
  The function returns a dataframe with species as columns and samples as rows. The entries in the dataframe are calculated percentages.
 }

\author{ Akexander Correa-Metrio, Dunia H. Urrego. }

\references{ Birks, H. J. B., and Birks, H. H. (1980). Quaternary Palaeoecology. University Park Press, Baltimore.
	 }

\seealso{ \code{\link{scale}} 
	}
\examples{
pollen<-matrix(nrow=10,sample(c(1:100),50))
colnames(pollen)<-c("Sp1","Sp2","Sp3","Sp4","Sp5")
percenta(pollen,first=1,last=5)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
% 