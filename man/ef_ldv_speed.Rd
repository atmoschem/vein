% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ef_ldv_speed.R
\name{ef_ldv_speed}
\alias{ef_ldv_speed}
\title{Emissions factors for Light Duty Vehicles and Motorcycles}
\usage{
ef_ldv_speed(
  v,
  t = "4S",
  cc,
  f,
  eu,
  p,
  x,
  k = 1,
  speed,
  show.equation = FALSE,
  fcorr = rep(1, 8)
)
}
\arguments{
\item{v}{Character; category vehicle: "PC", "LCV", "Motorcycle" or "Moped}

\item{t}{Character; sub-category of of vehicle: PC:  "ECE_1501", "ECE_1502",
"ECE_1503", "ECE_1504" , "IMPROVED_CONVENTIONAL", "OPEN_LOOP", "ALL",
"2S"  or "4S". LCV: "4S", Motorcycle: "2S" or "4S". Moped: "2S" or "4S"}

\item{cc}{Character; size of engine in cc:  PC: "<=1400", ">1400", "1400_2000", ">2000",
"<=800", "<=2000". Motorcycle:  ">=50" (for "2S"), "<=250", "250_750", ">=750".
Moped: "<=50". LCV :  "<3.5" for gross weight.}

\item{f}{Character; type of fuel: "G", "D", "LPG" or "FH" (Gasoline Full Hybrid).
Full hybrid vehicles cannot be charged from the grid and recharge; only its own engine
may recharge tis batteries.}

\item{eu}{Character or data.frame of characters; euro standard:
 "PRE", "I", "II", "III", "III+DPF", "IV", "V", "VI" or "VIc".
When the pollutan is active surface or number of particles, eu can also be "III+DISI"}

\item{p}{Character; pollutant: "CO", "FC", "NOx", "NO", "NO2", "HC", "PM", "NMHC", "CH4",
"CO2",  "SO2" or "Pb". Only when p is "SO2" pr "Pb" x is needed. Also
polycyclic aromatic hydrocarbons (PAHs), persistent organi pollutants (POPs),
and Number of particles and Active Surface.}

\item{x}{Numeric; if pollutant is "SO2", it is sulphur in fuel in ppm, if is
"Pb", Lead in fuel in ppm.}

\item{k}{Numeric; multiplication factor}

\item{speed}{Numeric; Speed to return Number of emission factor and not a function.}

\item{show.equation}{Logical; option to see or not the equation parameters.}

\item{fcorr}{Numeric; Correction by fuel properties by euro technology.
See \code{\link{fuel_corr}}. The order from first to last is
"PRE", "I", "II", "III", "IV", "V", VI, "VIc". Default is 1}
}
\value{
An emission factor function which depends of the average speed V  g/km
}
\description{
\code{\link{ef_ldv_speed}} returns speed dependent emission factors, data.frames or
list of emission factors. The emission factors
 comes from the guidelines  EMEP/EEA air pollutant emission inventory guidebook
http://www.eea.europa.eu/themes/air/emep-eea-air-pollutant-emission-inventory-guidebook
}
\details{
The argument of this functions have several options which results in
different combinations that returns emission factors. If a combination of
any option is wrong it will return an empty value. Therefore, it is
important ti know the combinations.
}
\note{
t = "ALL" and cc == "ALL" works for several pollutants because emission
fators are the same. Some exceptions are with NOx and FC because size of engine.

\strong{Hybrid cars}: the only cover "PC" and according to EMEP/EEA air pollutant emission inventory
guidebook 2016 (Ntziachristos and Samaras, 2016) only for euro IV. When new literature
is available, I will update these factors.

\strong{Pollutants (g/km)}: "CO", "NOx", "HC", "PM", "CH4", "NMHC", "CO2", "SO2",
"Pb", "FC".

\strong{Black Carbon and Organic Matter (g/km)}: "BC", "OM"

\strong{PAH and POP (g/km)}:  \code{\link{speciate}}
\strong{Dioxins and furans(g equivalent toxicity / km)}:  \code{\link{speciate}}
\strong{Metals (g/km)}:  \code{\link{speciate}}

\strong{NMHC (g/km)}: \code{\link{speciate}}

\emph{Active Surface (cm2/km)}:  \code{\link{speciate}}"AS_urban", "AS_rural", "AS_highway"

\emph{Total Number of particles (N/km)}: \code{\link{speciate}} "N_urban", "N_rural", "N_highway",
"N_50nm_urban", "N_50_100nm_rural", "N_100_1000nm_highway".

The available standards for Active Surface or number of particles are Euro I,
II, III, III+DPF dor diesle and III+DISI for gasoline. Pre euro vehicles
has the value of Euro I and  euro IV, V, VI and VIc the value of euro III.
}
\examples{
\dontrun{
# Passenger Cars PC
# Emission factor function
V <- 0:150
ef1 <- ef_ldv_speed(v = "PC",t = "4S", cc = "<=1400", f = "G", eu = "PRE",
p = "CO")
efs <- EmissionFactors(ef1(1:150))
plot(Speed(1:150), efs, xlab = "speed[km/h]", type = "b", pch = 16, col = "blue")

# Quick view
pol <- c("CO", "NOx", "HC", "NMHC", "CH4", "FC", "PM", "CO2", "SO2",
"1-butyne", "propyne")
f <- sapply(1:length(pol), function(i){
ef_ldv_speed("PC", "4S", "<=1400", "G", "PRE", pol[i], x = 10)(30)
})
f
# PM Characteristics
pol <- c("AS_urban", "AS_rural", "AS_highway",
"N_urban", "N_rural", "N_highway",
"N_50nm_urban", "N_50_100nm_rural", "N_100_1000nm_highway")
f <- sapply(1:length(pol), function(i){
ef_ldv_speed("PC", "4S", "<=1400", "D", "PRE", pol[i], x = 10)(30)
})
f
# PAH POP
ef_ldv_speed(v = "PC",t = "4S", cc = "<=1400", f = "G", eu = "PRE",
p = "indeno(1,2,3-cd)pyrene")(10)
ef_ldv_speed(v = "PC",t = "4S", cc = "<=1400", f = "G", eu = "PRE",
p = "napthalene")(10)

# Dioxins and Furans
ef_ldv_speed(v = "PC",t = "4S", cc = "<=1400", f = "G", eu = "PRE",
p = "PCB")(10)

# NMHC
ef_ldv_speed(v = "PC",t = "4S", cc = "<=1400", f = "G", eu = "PRE",
p = "hexane")(10)

# List of Copert emission factors for 40 years fleet of Passenger Cars.
# Assuming a euro distribution of euro V, IV, III, II, and I of
# 5 years each and the rest 15 as PRE euro:
euro <- c(rep("V", 5), rep("IV", 5), rep("III", 5), rep("II", 5),
          rep("I", 5), rep("PRE", 15))
speed <- 25
lef <- lapply(1:40, function(i) {
ef_ldv_speed(v = "PC", t = "4S", cc = "<=1400", f = "G",
          eu = euro[i], p = "CO")
ef_ldv_speed(v = "PC", t = "4S", cc = "<=1400", f = "G",
          eu = euro[i], p = "CO", show.equation = FALSE)(25) })
# to check the emission factor with a plot
efs <- EmissionFactors(unlist(lef)) #returns 'units'
plot(efs, xlab = "age")
lines(efs, type = "l")
euros <- c("VI", "V", "IV", "III", "II")
ef_ldv_speed(v = "PC", t = "4S", cc = "<=1400", f = "G",
          eu = euros, p = "CO")
a <- ef_ldv_speed(v = "PC", t = "4S", cc = "<=1400", f = "G",
          eu = euros, p = "CO", speed = Speed(0:120))
head(a)
filled.contour(as.matrix(a)[1:10, 1:length(euros)], col = cptcity::cpt(n = 18))
filled.contour(as.matrix(a)[110:120, 1:length(euros)], col = cptcity::cpt(n = 16))
filled.contour(as.matrix(a)[, 1:length(euros)], col = cptcity::cpt(n = 21))
filled.contour(as.matrix(a)[, 1:length(euros)],
col = cptcity::cpt("mpl_viridis", n = 21))
filled.contour(as.matrix(a)[, 1:length(euros)],
col = cptcity::cpt("mpl_magma", n = 21))
persp(as.matrix(a)[, 1:length(euros)], phi = 0, theta = 0)
persp(as.matrix(a)[, 1:length(euros)], phi = 25, theta = 45)
persp(as.matrix(a)[, 1:length(euros)], phi = 0, theta = 90)
persp(as.matrix(a)[, 1:length(euros)], phi = 25, theta = 90+45)
persp(as.matrix(a)[, 1:length(euros)], phi = 0, theta = 180)
new_euro <- c("VI", "VI", "V", "V", "V")
euro <- c("V", "V", "IV", "III", "II")
old_euro <- c("III", "II", "I", "PRE", "PRE")
meuros <- rbind(new_euro, euro, old_euro)
aa <- ef_ldv_speed(v = "PC", t = "4S", cc = "<=1400", f = "G",
          eu = meuros, p = "CO", speed = Speed(10:11))
# Light Commercial Vehicles
V <- 0:150
ef1 <- ef_ldv_speed(v = "LCV",t = "4S", cc = "<3.5", f = "G", eu = "PRE",
p = "CO")
efs <- EmissionFactors(ef1(1:150))
plot(Speed(1:150), efs, xlab = "speed[km/h]")
lef <- lapply(1:5, function(i) {
ef_ldv_speed(v = "LCV", t = "4S", cc = "<3.5", f = "G",
          eu = euro[i], p = "CO", show.equation = FALSE)(25) })
# to check the emission factor with a plot
efs <- EmissionFactors(unlist(lef)) #returns 'units'
plot(efs, xlab = "age")
lines(efs, type = "l")

# Motorcycles
V <- 0:150
ef1 <- ef_ldv_speed(v = "Motorcycle",t = "4S", cc = "<=250", f = "G",
eu = "PRE", p = "CO",show.equation = TRUE)
efs <- EmissionFactors(ef1(1:150))
plot(Speed(1:150), efs, xlab = "speed[km/h]")
# euro for motorcycles
eurom <- c(rep("III", 5), rep("II", 5), rep("I", 5), rep("PRE", 25))
lef <- lapply(1:30, function(i) {
ef_ldv_speed(v = "Motorcycle", t = "4S", cc = "<=250", f = "G",
eu = eurom[i], p = "CO",
show.equation = FALSE)(25) })
efs <- EmissionFactors(unlist(lef)) #returns 'units'
plot(efs, xlab = "age")
lines(efs, type = "l")
a <- ef_ldv_speed(v = "Motorcycle", t = "4S", cc = "<=250", f = "G",
eu = eurom, p = "CO", speed = Speed(0:125))
a$speed <- NULL
filled.contour(as.matrix(a), col = cptcity::lucky(),
xlab = "Speed", ylab = "Age")
persp(x = as.matrix(a), theta = 35, xlab = "Speed", ylab = "Euros",
zlab = "CO [g/km]", col = cptcity::lucky(), phi = 25)

ef <- ef_ldv_speed(v = "LCV",
                   t = "4S",
                   cc = "<3.5",
                   f = "G",
                   p = "FC",
                   eu = c("I", "II"),
                   speed = Speed(10))
}
}
\seealso{
\code{\link{fuel_corr}} \code{\link{emis}} \code{\link{ef_ldv_cold}}
}
\keyword{emission}
\keyword{factors}
\keyword{speed}
