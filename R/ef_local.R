#' Local Emissions factors
#'
#' \code{\link{ef_local}} process an data.frame delivered by the user, but adding
#' similar funcionality and arguments as \code{\link{ef_cetesb}}, which are classification, filtering
#' and projections
#'
#' returns a vector or data.frame of Brazilian emission factors.
#'
#' @param p Character; pollutant delivered by the user. the name of the column of the data.frame must be
#' \strong{Pollutant}.
#'
#' @param veh Character; Vehicle categories available in the data.frame provided by the user
#' @param year Numeric; Filter the emission factor to start from a specific base year.
#' If project is 'constant' values above 2017 and below 1980 will be repeated
#' @param agemax Integer; age of oldest vehicles for that category
#' @param ef data.frame, for local the emission factors. The names of the ef must
#' be `Age`	`Year`	`Pollutant` and all the vehicle categories...
#' @param full Logical; To return a data.frame instead or a vector adding
#' Age, Year, Brazilian emissions standards and its euro equivalents.
#' @param project Character showing the method for projecting emission factors in
#' future. Currently the only value is "constant"
#' @param verbose Logical; To show more information
#' @return A vector of Emission Factor or a data.frame
#' @keywords  emission factors
#' @note The names of the ef must be `Age`	`Year`	`Pollutant` and all the vehicle categories...
#' @seealso  \code{\link{ef_cetesb}}
#' @export
#' @examples {
#' #do not run
#' }
ef_local <- function(p, veh, year = 2017, agemax = 40, ef, full = FALSE, project = "constant", verbose = TRUE){
  ef[is.na(ef)] <- 0


  # Check names
  if(verbose) {
    cat("\nNames of the ef data.frame: \n")
    print(names(ef))
  }

  # Check column Pollutant
  iss <- unique(grepl(pattern = "Year", x = names(ef)))
  if(length(iss) == 1) stop("Please, add a column named `Year` in your ef data.frame")
  year1 <- ef$Year[1]

  # Check names
  if(verbose) {
    cat("\nRange Year:", range(ef$Year), "\n")
  }
  if(year < min(ef$Year)) stop("Min Year:", min(ef$Year), "\n")

  # Check column Pollutant
  iss <- unique(grepl(pattern = "Pollutant", x = names(ef)))
  if(length(iss) == 1) stop("Please, add a column named `Pollutant` in your ef data.frame")

  pols <- as.character(unique(ef$Pollutant))
  if(verbose) cat("\nNames of the Pollutants: ", unique(ef$Pollutant), "\n")

  # Selecting
  ef <- ef[ef$Year <= year, ]

  if(!p %in% pols){
    stop(cat("\nPlease, choose one of the following pollutants:\n", pols, "\n"))
  }

  if(full) {

    df <- cbind(ef[ef$Pollutant %in% p, 1:3],
                EmissionFactors(ef[ef$Pollutant == p, veh]))
    names(df)[ncol(df)] <- p

  } else{

    df <- vein::EmissionFactors(ef[ef$Pollutant == p, veh])
    row.names(df)
  }


  if(is.data.frame(df)){
    # project future EF
    if(project == "constant"){
      if(year > year1){
        dif <- year - year1

        eff <- do.call("rbind",(lapply(1:dif, function(i){
          df[1, ]
        })))
        edff <- rbind(eff, df[1:(agemax - dif), ])
      }
    }

    #Filling older ef
    if(!missing(agemax)){
      if(nrow(df) < agemax){
        dif <- agemax - nrow(df)
        df[nrow(df):(nrow(df)+dif), ] <- df[nrow(df), ]
      }
      df <-  df[1:agemax, ]
    }

  } else {
    # project future EF
    if(project == "constant"){
      if(year > year1){
        dif <- year - year1
        eff <- rep(df[1], dif)
        df <- c(eff, df[1:(agemax - dif)])
      }
    }

    #Filling older ef
    if(!missing(agemax)){
      if(length(df) < agemax){
        dif <- agemax - length(df)
        df[length(df):(length(df)+dif)] <- df[length(df)]
      }
      df <-  df[1:agemax]
    }

  }
  return(df)
}
