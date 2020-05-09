#' Emission factors deoending on accumulated mileage
#'
#' \code{\link{ef_im}} calculate the theoretical emission factors of vehicles.
#' The approache is different from including deterioration factors
#' (\code{\link{emis_det}})  but similar, because they represent how much emits
#' a vehicle with a normal deterioration, but that it will pass the
#' Inspection and Manteinance program.
#' @param ef Numeric; emission factors of vehicles with \strong{0 mileage}
#' (new vehicles).
#' @param tc Numeric; rate of growth of emissions by year of use.
#' @param amileage Numeric; Accumulated mileage by age of use.
#' @param max_amileage Numeric; Max accumulated mileage. This means that after
#' this value, mileage is constant.
#' @param max_ef Numeric; Max ef. This means that after this value, ef is constant.
#' @param verbose Logical; if you want detailed description.
#' @return An emission factor of a deteriorated vehicle under normal conditions
#' which would be approved in a inspection and mantainence program.
#' @keywords mileage emission factors
#' @export
#' @examples \dontrun{
#' # Do not run
#' # Passenger Cars PC
#' data(fkm)
#' # cumulative mileage from 1 to 50 years of use, 40:50
#' mil <- cumsum(fkm$KM_PC_E25(1:10))
#' ef_im(ef = seq(0.1, 2, 0.2), seq(0.1, 1, 0.1), mil)
#' }
ef_im <- function(ef,
                  tc,
                  amileage,
                  max_amileage,
                  max_ef,
                  verbose = TRUE){
  if(missing(ef)) stop("No ef")
  if(missing(tc)) stop("No tc")
  if(!missing(max_amileage)) {
    if(verbose) cat("adjusting amileage\n")
    amileage <- ifelse(amileage > max_amileage, max_amileage, amileage)
  }
  if(!missing(max_ef)) {
    if(verbose) cat("adjusting ef\n")
    ef <- ifelse(ef > max_ef, max_ef, ef)
  }
  nef <- ef * (1 + amileage * tc)
  return(EmissionFactors(nef))
}
