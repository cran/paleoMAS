\name{vcrossv.all}
\alias{vcrossv.all}

\title{
V-fold iterative cross-validation for discriminant analysis
}

\description{
This function v-fold cross-validates a discriminant analysis through the leave-v-out procedure, with v varying from 1 to v. It also does repetitions of the cross-validation at each value of v to make estimates of the confidence limits for the accuracy of the function. This function involves very intensive computations. Therefore, if only specific values of v need to be evaluated, it is recommended to use \code{vcrossv.da} instead.
}

\usage{
vcrossv.all(x, f, to, nsimulat, funct, ntrials, plot = TRUE)
}

\arguments{
  \item{x}{A matrix with samples in columns and taxa in rows. The rows must be named after taxa names (see \code{\link{rownames}}).
}
  \item{f}{
An object of class \code{factor} containing the discriminant factor (See Venables & Ripley (2002) for details on discriminant analysis).
}
  \item{to}{
The upper value of v. The v-fold crossvalidation is performed for each value from 1 to v.
}
  \item{nsimulat}{
Number of samples simulated to desaturate the model (see Correa-Metrio et al (2010) for details). If no samples were simulated \code{nsimulat=1}.
}
  \item{funct}{
\code{lda} for linear discriminant analysis, and \code{qda} for quadratic discriminant analysis.
}
  \item{ntrials}{
Number of desired repetitions for the cross-validation at each value of v.
}
  \item{plot}{
Whether or not a plot of the behavior of the accuracy estimated for the discriminant function at each value of v is desired.
}
}

\details{
The function was designed for discrimination of pollen taxa into dichotomous ecological groups (only admits two factors). The prior information corresponds to the affinity of certain taxa to known environmental conditions. Therefore, while the taxa corrrespond to the objects to classify, the percentages through the fossil dataset correspond to the attributes. Each time the discriminant function is adjusted, v elements are left out with no replacements. Therefore, it is recommended that v be smaller than half of the total taxa, unless there is a considerable number of species. Take also into consideration that each time a taxon is left out for the crossvalidation, all the samples that were simulated for such taxon are left out too.

}
\value{
\code{vcrossv.all} returns a matrix with four columns. \code{fold} contains the values of v. \code{mean accuracy} contains the average discriminant function accuracy obtained from repeating the cross-validation \code{ntrials} times at the given value of v. \code{lower (0.025)} and \code{upper (0.975)} contain the 0.025 and 0.975 quantiles of the discriminant function accuracy obtained from the same procedure. Note that for \code{v=1} the results are the same for all repetitions given that leaving only one element out has no random component associated.
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
#build a dummy factor assuming that the first 10 species belong to group1 and the send ten belong to group 2
b<-as.factor(rep(c("group1","group2"),each=10))
#apply the function
vcrossv.all(a,b,to=5,nsimulat=1,funct=lda,ntrials=20,plot=TRUE)
}
\keyword{ models }
\keyword{ robust } 