#' Aggregate emissions
#'
#' @description \code{\link{emis_chem}} aggregates emissions by chemical mechanism
#' and convert grams to mol. This function reads all hydrocarbos and respective
#' criteria polluants specified in \code{\link{ef_ldv_speed}} and \code{\link{ef_hdv_speed}}.
#'
#' @param dfe data.frame with column `emissions` in grams and `pollutant`.
#' @param mechanism Character, any of "SAPRC", "RACM", "RADM2", "CBMZ",
#' "MOZART", "SAPRC99", "CB05", "CB06CMAQ", "RACM2CMAQ", "SAPRC99CMAQ",
#' "SAPRC07CMAQ" or "SAPRC07A".
#' @param colby Character indicating column name for aggregating extra column.
#' For instance, region or province
#' @return data.frame with lumped groups by chemical mechanism. It transform
#' emissions in grams to mol.
#' @importFrom data.table setDT setDF :=
#' @importFrom utils data
#' @seealso \code{\link{ef_ldv_speed}} \code{\link{ef_hdv_speed}} \code{\link{speciate}}
#' @export
#' @note This feature is experimental and the mapping of pollutants and lumped
#' species may change in future.
#' This function is converting the intial data.frame input into data.table
#' @examples {
#' # CO
#' df <- data.frame(emission = 1:10)
#' df$pollutant = "CO"
#' df$emission <- units::set_units(df$emission, "g")
#' emis_chem(df, "CBMZ")
#' # hexanal
#' df$pollutant = "hexanal"
#' emis_chem(df, "CBMZ")
#' # propadiene and NO2
#' df2 <- df1 <- df
#' df1$pollutant = "propadiene"
#' df2$pollutant = "NO2"
#' dfe <- rbind(df1, df2)
#' emis_chem(dfe, "RADM2")
#' dfe$region <- rep(letters[1:2], 10)
#' emis_chem(dfe, "RADM2", "region")
#' }
emis_chem <- function(dfe, mechanism, colby) {
  #Check column pollutant
  if(!any(grepl(pattern = "pollutant", x = names(dfe)))){
    stop("The column 'pollutant' is rm(list = ls(not present in 'dfe'")
  }
  #Check column emissions
  if(!any(grepl(pattern = "emission", x = names(dfe)))){
    stop("The column 'emission' is not present in 'dfe'")
  }
  # Check units
  if(class(dfe$emission) != "units"){
    stop("dfe$emission neeeds to has class 'units' in 'g'. Check '?units::set_units'")
  }
  # loading mechanisms data-base
  df <- sysdata$mech
  data.table::setDT(df, key = "pollutant")
  df2 <- df[, c("pollutant", mechanism)]

  # loading pollutants with g_mol
  utils::data("pollutants")
  pollutants <- pollutants[, c("pollutant", "g_mol")]
  data.table::setDT(pollutants, key = "pollutant")

  # data.tabeling and keying
  data.table::setDT(dfe, key = "pollutant")

  # merging and filtering g_mol
  dfe <- dfe[pollutants][!is.na(get("g_mol"))]

  # converting to mol
  dfe$mol <- dfe$emission/dfe$g_mol

  # merging with mechanism
  df_mech <- dfe[df[df$MECH == mechanism]]
  df_mech$mol <- df_mech$mol*df_mech$k
  if(!missing(colby)){
    ss <- df_mech[, lapply(.SD, sum, na.rm=TRUE),
                  keyby =  list(df_mech$LUMPED,
                                df_mech[[colby]]),
                  .SDcols = "mol" ]
  } else {
    ss <- df_mech[, lapply(.SD, sum, na.rm=TRUE),
                  keyby =  list(df_mech$LUMPED),
                  .SDcols = "mol" ]
  }
  data.table::setDF(ss)
  dfe <- as.data.frame(dfe)
  pollutants <- as.data.frame(pollutants)
  return(ss)

}
