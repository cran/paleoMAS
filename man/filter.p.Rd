\name{filter.p}
\alias{filter.p}

\title{ Filter for Presence and Persistence }
\description{
  This function reduces a given dataset based on filters for  minimum presence (abundance) AND minimum persistence (number of samples), or maximum persitence.
}
\usage{
filter.p(x, rare=TRUE, presen = 1, persist = 0.05)
}

\arguments{
  \item{x}{ Matrix or dataframe with taxa in columns and samples in rows. }
  \item{rare}{ Argument indicating if filter aims rare or common taxa. If \code{rare=TRUE}, the filter eliminates rare taxa with presence and persitence indicated in the respective arguments. If \code{rare=FALSE}, the filter eliminates taxa with a persistence higher than that indicated in teh argument \code{persist}. }
  \item{presen}{ Criterion for minimum percentual presence, 1 percent by default. Only relevant if \code{rare=TRUE}.}
  \item{persist}{ Criterion for minimum persistence as a fraction of the number of samples where the taxon is expected to occur, 0.05 of the total number of samples by default. }
}

\details{
  This function applies both the presence and persistence filters when \code{rare=TRUE}. If the user desires to apply only one of the filters at a given time, a criterion that is met by all elements (taxa) should be chosen, e.g. \code{persist=0}. If \code{rare=FALSE}, only the criterion of being under the given persitence threshold is applied.
}

\value{
  Returns a list with three components  
  \item{filtered }{Reduced dataset after both filters are applied. }
  \item{filter }{This component is returned only when \code{rare=TRUE}. Matrix with three columns: \code{n}, number of samples where taxon is present; \code{n over minimum presence}, number of samples where percentage is greater than the defined minimum or filter; \code{quality}, binary that lets the user know whether  or not a taxon meets the filter criteria. }
  \item{result }{List with two or three components: \code{percentage}, minimum presence, only relevant when \code{rare=TRUE}; \code{minimum} or \code{maximum}, minimum or maximum persistence; and \code{number of taxa}, total number of taxa that meet the filter criteria. }
}

\author{ Alexander Correa-Metrio, Kenneth R. Cabrera, Dunia H. Urrego.}

\references{ 
Correa-Metrio, A., K.R. Cabrera, and M.B. Bush. 2010. Quantifying ecological change through discriminant analysis: a paleoecological example from the Peruvian Amazon. Journal of Vegetation Science 21: 695-704.
}


\examples{
#For a minimum presence of 2 percent in 20 percent of the samples
data(quexilper)
filter.p(quexilper)
quexilfil<-filter.p(quexilper,presen=2,persist=0.2)
#Filtered database
quexilfil$filtered
}

\keyword{ manip }
