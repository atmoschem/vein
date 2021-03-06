% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Speed.R
\name{Speed}
\alias{Speed}
\alias{print.Speed}
\alias{summary.Speed}
\alias{plot.Speed}
\title{Construction function for class "Speed"}
\usage{
Speed(x, ...)

\method{print}{Speed}(x, ...)

\method{summary}{Speed}(object, ...)

\method{plot}{Speed}(x, ...)
}
\arguments{
\item{x}{Object with class "data.frame", "matrix" or "numeric"}

\item{...}{ignored}

\item{object}{Object with class "Speed"}
}
\value{
Constructor for class "Speed" or "units"
}
\description{
\code{Speed} returns a tranformed object with class "Speed" and units
 km/h. This functions includes two arguments, distance and time. Therefore,
 it is posibel to change the units of the speed to "m" to "s" for example.
 This function returns a dataframe with units for speed. When this function
 is applied to numeric vectors it add class "units".
}
\examples{
\dontrun{
data(net)
data(pc_profile)
speed <- Speed(net$ps)
class(speed)
plot(speed, type = "l")
pc_week <- temp_fact(net$ldv+net$hdv, pc_profile)
df <- netspeed(pc_week, net$ps, net$ffs, net$capacity, net$lkm)
summary(df)
}
}
\seealso{
\code{\link{units}}
}
