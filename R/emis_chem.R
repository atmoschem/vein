#' Aggregate emissions by lumped groups in chemical mechanism
#'
#' @description \code{\link{emis_chem}} aggregates emissions by chemical mechanism
#' and convert grams to mol. This function reads all hydrocarbos and respective
#' criteria polluants specified in \code{\link{ef_ldv_speed}} and \code{\link{ef_hdv_speed}}.
#'
#' @param dfe data.frame with column `emissions` in grams and `pollutant` in long format.
#' @param mechanism Character, "RADM2_SORG", "CBMZ_MOSAIC", "CPTEC", "GOCART_CPTEC", "MOZEM",
#' "MOZCEM", "CAMMAM", "MOZMEM", "MOZC_T1_EM", "CB05_OPT1" or "CB05_OPT2"
#' @param colby Character indicating column name for aggregating extra column.
#' For instance, region or province
#' @param long Logical. Do you want data in long format?
#' @return data.frame with lumped groups by chemical mechanism. It transform
#' emissions in grams to mol.
#' @importFrom data.table setDF as.data.table
#' @importFrom utils data
#' @importFrom units as_units
#' @seealso \code{\link{ef_ldv_speed}} \code{\link{ef_hdv_speed}} \code{\link{speciate}} \code{\link{ef_evap}}
#' @export
#' @note This feature is experimental and the mapping of pollutants and lumped
#' species may change in future.
#' This function is converting the intial data.frame input into data.table.
#' To have a comprehensive speciation is necessary enter with a data.frame
#' with colum 'emission' in long format including another column named 'pollutant' with
#' species of NMHC, CO, NO, NO2, NH3, SO2, PM2.5 and coarse PM10.
#'
#' Groups derived from gases has units 'mol' and from aersols 'g'. The aersol
#' units for WRF-Chem are ug/m^2/s while for CMAQ and CAMx are g/s. So,
#' leaving the units just in g, allow to make further change while
#' providing flexibility for several models.
#' @examples {
#' # CO
#' df <- data.frame(emission = Emissions(1:10))
#' df$pollutant = "CO"
#' emis_chem(df, "CBMZ_MOSAIC")
#' # hexanal
#' df$pollutant = "hexanal"
#' emis_chem(df, "CBMZ_MOSAIC")
#' # propadiene and NO2
#' df2 <- df1 <- df
#' df1$pollutant = "propadiene"
#' df2$pollutant = "NO2"
#' (dfe <- rbind(df1, df2))
#' emis_chem(dfe, "CBMZ_MOSAIC")
#' dfe$region <- rep(letters[1:2], 10)
#' emis_chem(dfe, "CBMZ_MOSAIC", "region")
#' emis_chem(dfe, "CBMZ_MOSAIC", "region", TRUE)
#' }
emis_chem <- function(dfe, mechanism, colby, long = FALSE) {
  dfe <- as.data.frame(dfe)
  #Check column pollutant
  if(!any(grepl(pattern = "pollutant", x = names(dfe)))){
    stop("The column 'pollutant' is present in 'dfe'")
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
  df <- df[df$MECH == mechanism, ]

  # loading pollutants with g_mol
  utils::data("pollutants")
  pollutants <- pollutants[, c("pollutant", "g_mol")]

  # merging and filtering g_mol
  dfe <- merge(dfe, pollutants, by = "pollutant", all = TRUE)

  # converting to mol
  dfe$mol <- ifelse(!is.na(dfe$g_mol),
                    dfe$emission/dfe$g_mol,
                    dfe$emission)

  # merging with mechanism
  df_mech <-  merge(dfe, df, by = "pollutant", all = TRUE)
  df_mech <- data.table::as.data.table(df_mech)

  df_mech$mol <- ifelse(df_mech$type == "gas",
                        df_mech$mol*df_mech$k,
                        df_mech$emission*df_mech$k)
  if(!missing(colby)){
    ss <- df_mech[, lapply(.SD, sum, na.rm=TRUE),
                  keyby =  list(df_mech$LUMPED,
                                df_mech[[colby]]),
                  .SDcols = "mol" ]
    names(ss) <- c("group", colby,"emission")
  } else {
    ss <- df_mech[, lapply(.SD, sum, na.rm=TRUE),
                  keyby =  list(df_mech$LUMPED),
                  .SDcols = "mol" ]
    names(ss) <- c("group", "emission")
  }
  data.table::setDF(ss)
  gases <- unique(df[df$type == "gas", "LUMPED"])
  ss$units <- ifelse(ss$group %in% gases, "mol", "g")
  ss <- ss[!is.na(ss$group), ]
  pollutants <- as.data.frame(pollutants)

  if(long){
    return(ss)
  } else {
    if(missing(colby)){
      ss <- long_to_wide(df = ss,
                         column_with_new_names = "group",
                         column_with_data = "emission")
    } else {
      stop("emis_chem with colby and long = FALSE not supported yet")
      # ss <- long_to_wide(df = ss,
      #                    column_with_new_names = "group",
      #                    column_with_data = "emission",
      #                    column_fixed = colby)

    }
    for(i in 1:ncol(ss)){
      if(names(ss)[i] %in% gases) {
        ss[, i] <-  units::as_units(ss[, i], "mol")
      } else {
        ss[, i] <-  units::as_units(ss[, i], "g")
      }
    }
    return(ss)
  }
}
