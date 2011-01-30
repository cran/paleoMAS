\name{vcrossv.da}
\alias{vcrossv.da}

\title{
V-fold cross-validation for discriminant analysis
}
\description{
This function v-fold cross-validates a discriminant analysis through the leave-v-out procedure.
}
\usage{
vcrossv.da(x, f, fold, nsimulat, funct)
}

\arguments{
  \item{x}{ A matrix with samples in columns and taxa in rows. The rows must be named after taxa names (see \code{\link{rownames}}).

}
  \item{f}{
An object of class \code{factor} containing the discriminant factor (See Venables & Ripley (2002) for details on discriminant analysis).
}
  \item{fold}{
Value of v, i.e. number of elements to be left out in each validation. 
}
  \item{nsimulat}{ Number of samples simulated to desaturate the model (see Correa-Metrio et al (in review) for details). If no samples were simulated \code{nsimulat=1}.
}
  \item{funct}{
\code{lda} for linear discriminant analysis, and \code{qda} for quadratic discriminant analysis.
}
}
\details{
The function was designed for discrimination of pollen taxa into dichotomous ecological groups (only admits two factors). The prior information corresponds to the affinity of certain taxa to known environmental conditions. Therefore, while the taxa corrrespond to the objects to classify, the percentages through the fossil dataset correspond to the attributes. Each time the discriminant function is adjusted, v elements are left out with no replacements. Therefore, it is recommended that v be smaller than half of the total taxa, unless there is a considerable number of species. Take also into consideration that each time a taxon is left out for the crossvalidation, all the samples that were simulated for such taxon are left out too.
}
\value{
A list containing:
  \item{posterior}{The \emph{a posteriori} probability of each taxa belonging to each one of the defined groups.}
  \item{comp2 }{Binary classification of the taxa.}
  \item{accuracy}{The percentage of cases well classified in the cross-validation.}
}

\author{
Alexander Correa-Metrio, Kenneth R. Cabrera.
}


\references{
Correa-Metrio, A., K.R. Cabrera, and M.B. Bush. 2010. Quantifying ecological change through discriminant analysis: a paleoecological example from the Peruvian Amazon. \emph{Journal of Vegetation Science} 21: 695-704.

Venables, W.N., and B.D. Ripley. 2002. "Modern applied statistics with S". Springer, New York.
}


\seealso{
\code{\link{vcrossv.all}}.\code{\link[MASS]{lda}} and \code{\link[MASS]{qda}} (package \pkg{MASS}) for details on the discriminant functions. \code{\link{simulat}} and \code{\link{simulat.t}} for details on samples simulations.
}

\examples{
data(quexilper)
# Taking only a fraction of the data base so the model is not saturated
a<-quexilper[1:10,1:20]
a<-t(a)
# build a dummy factor assuming that the first 10 species belong
# to group1 and the send ten belong to group 2
b<-as.factor(rep(c("group1","group2"),each=10))
#to apply ordinary crossvalidation (leave-one-out)
vcrossv.da(a,b,fold=1,nsimulat=1,funct=lda)
#to apply 3-fold cross-validation
vcrossv.da(a,b,fold=3,nsimulat=1,funct=lda)
}

\keyword{ models }
\keyword{ robust }
