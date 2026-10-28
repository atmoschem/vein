#' Get ef reference data
#'
#' @description Get the reference data used to build the emission factor (ef)
#' model applied by vein.
#'
#' @param ref Character; The ef model required (e.g. "eea" for ef_eea)
#' @return A data.table copy of the reference table. Modifying it does not
#' change the tables used by vein.
#' @importFrom data.table as.data.table
#' @note This function is a shortcut to access unexported ef model information
#' in vein.
#'
#' @export
#' @examples \dontrun{
#' get_ef_ref("eea")
#' }
get_ef_ref <- function(ref){
  #to access vein:::sysdata outside vein
  if (!ref %in% names(sysdata)) {
    stop("'ref' must be one of: ", paste(names(sysdata), collapse = ", "))
  }
  data.table::as.data.table(sysdata[[ref]])
}


