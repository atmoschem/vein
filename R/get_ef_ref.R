#' Get ef reference data
#'
#' @description Get the reference data used to build the emission factor (ef)
#' model applied by vein.
#'
#' @param ref Character; The ef model required (e.g. "eea" for ef_eea)
#' @importFrom data.table setDT
#' @note This function is a shortcut to access unexported ef model information
#' in vein.
#'
#' @export
#' @examples \dontrun{
#' get_ef_ref("eea")
#' }
get_ef_ref <- function(ref){
  #to access vein:::sysdata outside vein
  data.table::setDT(sysdata[[ref]])
}


