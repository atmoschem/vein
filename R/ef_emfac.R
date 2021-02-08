#' EMFAC2017 emission factors for Statewide California, Calendar Year 2020
#'
#' \code{\link{ef_emfac}} returns emission factors reflecting California, US,
#' conditions. If the user enter speeds more emission factors are returned.
#' See details.
#'
#' @param veh Character; "one of the 40 vehicle categories shown below.
#' @param fuel Character; "Diesel", "Gasoline", "Electricity" or "Natural Gas"
#' @param mph Numeric; Speed in miles per hour (optional).
#' @param pol Character;
#' \itemize{
#' \item{if the user enter mph}{"NOx_RUNEX", "PM2.5_RUNEX", "PM10_RUNEX",
#' "CO2_RUNEX", "CH4_RUNEX", "N2O_RUNEX", "ROG_RUNEX", "TOG_RUNEX", "CO_RUNEX",
#' "SOx_RUNEX"}
#' \item{if the user do not enter mph}{
#' "NOx_RUNEX", "NOx_IDLEX", "NOx_STREX",
#' "PM2.5_RUNEX", "PM2.5_IDLEX", "PM2.5_STREX",
#' "PM2.5_PMTW" , "PM2.5_PMBW", "PM10_RUNEX",
#' "PM10_IDLEX", "PM10_STREX", "PM10_PMTW",
#' "PM10_PMBW", "CO2_RUNEX", "CO2_IDLEX",
#' "CO2_STREX", "CH4_RUNEX", "CH4_IDLEX",
#' "CH4_STREX", "N2O_RUNEX", "N2O_IDLEX",
#' "N2O_STREX", "ROG_RUNEX", "ROG_IDLEX",
#' "ROG_STREX", "ROG_HOTSOAK", "ROG_RUNLOSS",
#' "ROG_RESTLOSS", "ROG_DIURN", "TOG_RUNEX",
#' "TOG_IDLEX", "TOG_STREX", "TOG_HOTSOAK",
#' "TOG_RUNLOSS", "TOG_RESTLOSS", "TOG_DIURN",
#' "CO_RUNEX", "CO_IDLEX", "CO_STREX",
#' "SOx_RUNEX", "SOx_IDLEX", "SOx_STREX"
#' }
#' }
#' @param season Character: "winter" or "summer".
#' @param full Logical: To return the whole data.table or not.
#' @return data.table with emission factors.
#' @keywords speed emission factors emfac
#' @references https://arb.ca.gov/emfac/emissions-inventory
#' @importFrom data.table fifelse
#' @export
#' @examples \dontrun{
#' #do not run
#' pols <- c("CO_RUNEX", "NOx_RUNEX")
#' dfef2 <- ef_emfac(full = TRUE)
#' colplot(df = dfef2,
#'         x = dfef2$Model_Year,
#'         cols = pols,
#'         main = "EF from LDT1 with Gasoline on Winter",
#'         ylab = units(dfef2[[pols[1]]][1]))
#' }
ef_emfac <- function(veh = "LDT1",
                     fuel = "Gasoline",
                     mph,
                     pol = "CO_RUNEX",
                     season = "winter",
                     full = FALSE){
  units::install_symbolic_unit("trip", warn = FALSE)
  units::install_symbolic_unit("veh", warn = FALSE)

  Vehicle_Category <- NULL
  Model_Year <- NULL
  Speed <- NULL
  Fuel <- NULL
  Season <- NULL
  # pol <- NULL

  if(missing(mph)) {
    ef <-sysdata$emfac_agg
    dt <- ef[Vehicle_Category %in% veh &
               Fuel %in% fuel &
               Season %in% season, ]
    if(full) {
      return(dt)
    } else {
      x <- cbind(dt[, c(1:9,52:54)], dt[[pol]])
      data.table::setnames(x, c(names(dt[, c(1:9,52:54)]), pol))
      return(x)
    }
  } else {
    mph <- as.numeric(mph)
    mph <- data.table::fifelse(
      mph <= 5, 5L,
      data.table::fifelse(
        mph>5 & mph <=10, 10L,
        data.table::fifelse(
          mph>10 & mph <=15, 15L,
          data.table::fifelse(
            mph>15 & mph <= 20, 20L,
            data.table::fifelse(
              mph>20 & mph<=25, 25L,
              data.table::fifelse(
                mph>25 & mph<=30, 30L,
                data.table::fifelse(
                  mph>30 & mph<=35, 35L,
                  data.table::fifelse(
                    mph>35 & mph<=40, 40L,
                    data.table::fifelse(
                      mph>40 & mph<=45, 45L,
                      data.table::fifelse(
                        mph>45 & mph<=50, 50L,
                        data.table::fifelse(
                          mph>50 & mph<=55, 55L,
                          data.table::fifelse(
                            mph>55 & mph<=60, 60L,
                            data.table::fifelse(
                              mph>60 & mph<=65, 65L,
                              data.table::fifelse(
                                mph>65 & mph<=70, 70L,
                                data.table::fifelse(
                                  mph>70 & mph<=75, 75L,
                                  data.table::fifelse(
                                    mph>75 & mph<=80, 80L,
                                    data.table::fifelse(
                                      mph>80 & mph<=85, 85L,
                                      90L)))))))))))))))))
    ef <-sysdata$emfac_speed

    dt <- ef[Vehicle_Category %in% veh &
               Fuel %in% fuel &
               Speed %in% mph &
               Season %in% season, ]
    if(full) {
      return(dt)
    } else {
      x <- cbind(dt[, c(1:7, 18:20)], x)
      data.table::setnames(x, c(names(dt[, c(1:7,18:20)]), pol))
      return(x)

    }
  }

}
