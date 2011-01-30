\name{analog.mult}
\alias{analog.mult}

\title{
Multiple analogs comparison
}
\description{
This function calculates and plots distance or dissimilarity indices (degree of analogy) between all samples in a fossil record and a set of modern or fossil analog samples.
}
\usage{
analog.mult(base, fossil, age, res = 100, method = "euclidean",
           binary = FALSE, plot = TRUE, dimension = TRUE, 
           wire = FALSE, aspect = c(0.5, 0.3), drape = TRUE)
}

\arguments{
  \item{base}{
A matrix containing analog samples with samples in rows and taxa in columns. Rows must be named after each analog sample that they contain (see \code{\link{rownames}}).
}
  \item{fossil}{
A matrix with the fossil dataset containing samples in rows and species in columns.
}
  \item{age}{
A vector with the age or depth of each sample.
}
  \item{res}{
Temporal resolution for the interpolation needed to plot the results. 
}
  \item{method}{
Corresponds to squared chord distance (\code{"schord"}) and the methods available in \code{vegdist} of \pkg{vegan}. Available methods are \code{"manhattan"},
    \code{"euclidean"}, \code{"canberra"}, \code{"bray"}, \code{"kulczynski"},
     \code{"jaccard"}, \code{"gower"}, \code{"morisita"}, \code{"horn"},
     \code{"mountford"}, \code{"raup"} , \code{"binomial"} or \code{"chao"}. See \code{\link[vegan]{vegdist}} (package \pkg{vegan}) for details.
}
  \item{binary}{
Logical indicating whether a transformation of the data into presence/absence is desired. See \code{\link[vegan]{vegdist}} (package \pkg{vegan}) for details.
}
  \item{plot}{
Logical indicating whether or not a plot is desired.
}
  \item{dimension}{
An indicator of whether or not a three-dimensional plot is desired. If \code{TRUE} a 3D figure is plotted according to the argument specified in \code{wire}. If \code{FALSE} a set of bidimensional panels is produced. 
}
  \item{wire}{
An indicator of the kind of 3D figure that is desired. If \code{TRUE} a figure type \code{\link[lattice]{wireframe}} (package \pkg{lattice}) is produced. If \code{FALSE} a figure type \code{\link[lattice]{levelplot}} (package \pkg{MASS}) is produced. Only relevant if \code{dimension=TRUE}.}
  \item{aspect}{
Argument of the function \code{\link[lattice]{wireframe}}. Only relevant if \code{wire=TRUE}.
}
  \item{drape}{
Argument of the function \code{\link[lattice]{wireframe}}. Only relevant if \code{wire=TRUE}.}
}

\details{
When 3D plots are desired, they are based on a grid with a uniform time resolution. Therefore, a linear interpolation is performed, which requires the definition of the desired time resolution through the argument \code{res}.
}

\value{
This function returns a list containing:
  \item{plot.data }{A matrix with distances between fossil samples and analogs interpolated into regular time intervals. The size of the interval was defined in the argument \code{res}.}
  \item{distances }{A matrix with the age of fossil samples, and their distance or dissimilarity to each analog sample.
}}

\references{
Correa-Metrio, A. 2010. Climate and vegetation of the Yucatan Peninsula during the late Pleistocene. PhD Dissertation, Florida Institute of Technology, Melbourne, FL. 194 p.	
}

\author{
Alexander Correa-Metrio.
}

\seealso{
\code{\link{analog.sing}}. See \code{\link[vegan]{vegdist}} for details on the dissimilarity indexes.
}

\examples{
data(quexilchron,quexildepths,quexilper)
#Define chronolgy
age<-quexilchron
ages<-chron(age,quexildepths,max.depth=1957)$chronology[,2]
#If samples are used as representative of time periods
quexilbase<-quexilper[c(1,27,68),]
rownames(quexilbase)<-c("Late Pleistocene","LGM","MIS3")
#comparison of all fossil samples with the three periods 
analog.mult(quexilbase,quexilper,ages,res=500)
#three-dimensional figure
analog.mult(quexilbase,quexilper,ages,res=500,plot=TRUE,dimension=TRUE,wire=TRUE)
#multipanel figure
analog.mult(quexilbase,quexilper,ages,res=500,plot=TRUE,dimension=FALSE)

}

\keyword{ manip }
\keyword{ arith }
\keyword{ math }
\keyword{ multivariate }