\name{chron}
\alias{chron}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Age-depth computation }
\description{
  This function computes and returns interpolated ages for specified sample depths based on a matrix with depths and calibrated ages.
}
\usage{
chron(age, depths, max.depth, type = "l", linear = TRUE,
 method = "fmm", ci = TRUE, dates = "points" , length = 0.05,
 digits = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{age}{ A numeric matrix with three columns: depths of dated intervals, calibrated ages and standard deviation. The date or suspected date of the uppermost sample must be provided (usually 0 cm are present). }
  \item{depths}{ A vector with a list of depths for which ages will be interpolated. The depths MUST be in ascendant order.}
  \item{max.depth}{ Maximum depth of the stratrigraphic sequence (Including analyzed and dated depths). It is used to extrapolate ages when the bottom of the sequence goes beyond the deepest dated interval. }
  \item{type}{ Graphic parameter for the output plot. See \link{plot} for details.}
  \item{linear}{ Interpolation method desired. If TRUE, ages are calculated using linear interpolation. If FALSE, an spline is fitted to the dataset.}
  \item{method}{ Argument of the function \code{\link{spline}}. This must be one of \code{"fmm"}, \code{"natural"}, \code{"periodic"} or \code{"monoH.FC"}. Only relevant if \code{linear=FALSE}. See \link{spline} for details. }
  \item{ci}{ A logical value indicating whether confidence intervals are desired in the output plot.}
  \item{dates}{ Graphical argument. \code{"points"} to display dates as red points, or \code{"bars"} to display dates as error bars spanning the provided standard deviation of calibrated ages.}
  \item{length}{ Graphical parameter. Leght in inches for the horizontal bar heads.}
  \item{digits}{ Number of digits desired for interpolated ages.}
}
\details{
  \code{chron} calculates interpolated ages using linear interpolation or spline as \code{method}. It is based on a three-column matrix with  depth of dated interval, calibrated age, and calibrated standard deviation. The interpolated ages are calculated for a set of depths (defined by the user in \code{depths}). Whenever the stratrigraphic sequence goes beyond the last dated point, the age for the bottom samples is extrapolated. \code{max.depth} is the maximum depth of the stratigraphic sequence, including dates and sampled points.
  
Confidence intervals correspond to the provided chronology deviation. Therefore, it does not necessarily correspond to 0.95 interval.
}
\value{
  \code{chron} returns a list and a plot. The list contains
  \item{base }{A matrix with the base chronology.}
  \item{chronology }{A matrix with interpolated ages up to \code{max depth} and with deviation if \code{ci=TRUE}. the deviation is calculated according to the error provided by the user, therefore it does not necessaryly corresponds to 0.95 intervals.}
}

\author{ Alexander Correa-Metrio, Dunia H. Urrego }

\seealso{ See \link{plot} for details on graphic parameters, \link{approx} for details on linear interpolation, and \link{spline} for details on non-linear interpolation.}
\examples{
data(quexilchron,quexildepths)
#linear
chron(quexilchron,quexildepths,max.depth=1957,dates="points")
# Spline
chron(quexilchron,quexildepths,max.depth=1957,linear=FALSE)

}

\keyword{ manip }
\keyword{ chron }
\keyword{ arith }
\keyword{ math }
