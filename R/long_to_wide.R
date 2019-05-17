#' Transform data.frame from long to wide format
#'
#' @description \code{\link{long_to_wide}} aggregates emissions by chemical mechanism
#' and convert grams to mol. This function reads all hydrocarbos and respective
#' criteria polluants specified in \code{\link{ef_ldv_speed}} and \code{\link{ef_hdv_speed}}.
#'
#' @param df data.frame with three column.
#' @param column_with_new_names Character, column that has new column names
#' @param column_with_data Character column with data
#' @param column_fixed Character,  column that will remain fixed
#' @param geometry To return a sf
#' @return wide data.frame.
#' @importFrom sf st_sf
#' @seealso \code{\link{emis_hot_td}}  \code{\link{emis_cold_td}}
#' @export
#' @examples {
#' df <- data.frame(pollutant = rep(c("CO", "propadiene", "NO2"), 10),
#' emission = vein::Emissions(1:30),
#' region = rep(letters[1:2], 15))
#' df
#' long_to_wide(df)
#' long_to_wide(df, column_fixed = "region")
#' }
long_to_wide <- function(df,
                         column_with_new_names = names(df)[1],
                         column_with_data = "emission",
                         column_fixed,
                         geometry) {
  a <- as.data.frame(df)
  la <- split(a, a[[column_with_new_names]])
  if(missing(column_fixed)) {
    aa <- do.call("cbind", lapply(1:length(la), function(i){
      la[[i]][[column_with_data]]
    }))
    aa <- as.data.frame(aa)
    names(aa) <- names(la)

  } else {
  xa <- as.data.frame(df)
  xa$id <- xa[[column_fixed]]
  la <- split(xa, xa[[column_with_new_names]])
  aa <- do.call("cbind", lapply(1:length(la), function(i){
    dfid <- data.frame(id = unique(xa[[column_fixed]]))
    merge(dfid, la[[i]][, c("id", column_with_data)], by = "id", all.x = T)
  }))
  aa[is.na(aa)] <- 0
  aa <- cbind(aa$id, aa)
  names(aa)[1] <- column_fixed
  aa[, grepl("id", names(aa))] <- NULL
  }
  if(missing(geometry)) {
    return(aa)
  } else {
    aa <- sf::st_sf(aa, geometry = geometry)
    return(aa)
  }
}
