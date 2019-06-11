#' Transform data.frame from wide to long format
#'
#' @description \code{\link{wide_to_long}} transform data.frame from wide to
#' long format
#'
#' @param df data.frame with three column.
#' @param column_with_data Character column with data
#' @param column_fixed Character,  column that will remain fixed
#' @param geometry To return a sf
#' @return long data.frame.
#' @importFrom sf st_sf
#' @seealso \code{\link{emis_hot_td}} \code{\link{emis_cold_td}}  \code{\link{long_to_wide}}
#' @export
#' @examples {
#' data(net)
#' net <- net@data
#' df <- wide_to_long(df = net)
#' head(df)
#' }
wide_to_long <- function(df,
                         column_with_data = names(df),
                         column_fixed,
                         geometry) {
  a <- as.data.frame(df)
  if(!missing(column_fixed)){
    df2 <- data.frame(V1 = unlist(a[, column_fixed]))
    df2$V2 <- unlist(a[, column_with_data])
    df2$V3 <- rep(column_with_data, each = nrow(a))
  } else {
    df2 <- data.frame(V1 = unlist(a[, column_with_data]))
    df2$V2 <- rep(column_with_data, each = nrow(a))
  }


  if(missing(geometry)) {
    return(df2)
  } else {
    df2 <- sf::st_sf(df2, geometry = geometry)
    return(df2)
  }
}
