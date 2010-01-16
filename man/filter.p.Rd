\name{filter.p}
\alias{filter.p}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Filter for Presence and Persistence }
\description{
  This function reduces a given dataset based on filters for  minimum presence (abundance) AND minimum persistence (number of samples).
}
\usage{
filter.p(x, presen = 1, persist = 0.05)
}

\arguments{
  \item{x}{ Matrix or dataframe with species in columns and samples in rows. }
  \item{presen}{ Criterium for minimum percentual presence, 1 percent by default. }
  \item{persist}{ Criterium for minimum persistence as a fraction of the number of samples where the taxon is expected to occur, 0.05 of the total number of samples by default. }
}

\details{
  This function applies both the presence and persistence filters automatically. If the user desires to apply only one of the filters at a given time, a criterium that is met by all elements (taxa) should be chosen, e.g. \code{persist=0}.
}

\value{
  Returns a list with three components  
  \item{filtered }{Reduced dataset after both filters are applied. }
  \item{filter }{Matrix with three columns: \code{n}, number of samples where taxon is present; \code{n over minimum presence}, number of samples where percentage is greater than the defined minimum or filter; \code{quality}, binary that lets the user know whether a taxon meets or not the filter criteria. }
  \item{result }{List with three components: \code{percentage}, minimum presence; \code{minimum}, minimum persistence; and \code{number of taxa}, total number of taxa that meet the filter criteria. }
}

\author{ Alexander Correa-Metrio, Kenneth R. Cabrera, Dunia H. Urrego.}

\references{ Correa-Metrio, A., K.R. Cabrera, and M.B. Bush. In review. Deriving millenial-scale moisture availability curves from pollen data: an example from the Peruvian Amazon. Journal of Vegetation Science.
}


\examples{
#For the default criteria (1 percent in at least 5 percent of the samples)
data(quexilper)
filter.p(quexilper)
#For a minimum presence of 2 percent in 20 percent of the samples
quexilfil<-filter.p(quexilper,presen=2,persist=0.2)
#Filtered database
quexilfil$filtered
#Taxa that meet the last criteria
colnames(quexilfil$filtered)
}

\keyword{ manip }
