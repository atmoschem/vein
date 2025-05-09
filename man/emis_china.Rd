% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ef_china.R
\name{emis_china}
\alias{emis_china}
\title{Estimation with Chinese factors}
\usage{
emis_china(
  x,
  lkm,
  tfs,
  v = "PV",
  t = "Small",
  f = "G",
  standard,
  s,
  speed,
  te,
  hu,
  h,
  yeardet = 2016,
  p,
  verbose = TRUE,
  array = FALSE
)
}
\arguments{
\item{x}{Vehicles data.frame}

\item{lkm}{Length of each link in km}

\item{tfs}{temporal factor}

\item{v}{Character; category vehicle: "PV" for Passenger Vehicles or 'Trucks"}

\item{t}{Character; sub-category of of vehicle: PV Gasoline:  "Mini", "Small","Medium",
"Large", "Taxi", "Motorcycles", "Moped", PV Diesel: "Mediumbus", "Largebus",
 "3-Wheel". Trucks: "Mini", "Light" , "Medium", "Heavy"}

\item{f}{Character;fuel: "G", "D", "CNG", "ALL"}

\item{standard}{Character vector; "PRE", "I", "II", "III", "IV", "V".}

\item{s}{Sulhur in ppm}

\item{speed}{Speed  (length nrow x)}

\item{te}{Temperature (length tfs)}

\item{hu}{Humidity (length tfs)}

\item{h}{Altitude (length nrow x)}

\item{yeardet}{Year, default 2016}

\item{p}{Character; pollutant: "CO", "NOx","HC", "PM", "Evaporative_driving"
or "Evaporative_parking"}

\item{verbose}{Logical to show more info}

\item{array}{Logical to return EmissionsArray or not}
}
\value{
long data.frame
}
\description{
Emissions estimates
}
\examples{
{
ef_china_h(h = 1600, p = "CO")
}
}
\seealso{
Other China: 
\code{\link{ef_china}()},
\code{\link{ef_china_det}()},
\code{\link{ef_china_h}()},
\code{\link{ef_china_hu}()},
\code{\link{ef_china_long}()},
\code{\link{ef_china_s}()},
\code{\link{ef_china_speed}()},
\code{\link{ef_china_te}()},
\code{\link{ef_china_th}()},
\code{\link{emis_long}()}
}
\concept{China}
