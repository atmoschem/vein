## vein-deprecated.r
#' @title Deprecated functions in package \pkg{vein}.
#' @description The functions listed below are deprecated and will be defunct in
#'   the near future. When possible, alternative functions with similar
#'   functionality are also mentioned. Help pages for deprecated functions are
#'   available at \code{help("-deprecated")}.
#' @name vein-deprecated
#' @keywords internal
NULL


#' @rdname vein-deprecated
#' @export
emis_chem <- function(...) {
  .Deprecated("emis_chem2")
  "emis_chem2"
}


#' @rdname vein-deprecated
#' @export
long_to_wide <- function(...) {
  .Deprecated("data.table::dcast.data.table")
  "Use data.table::dcast.data.table"
}

#' @rdname vein-deprecated
#' @export
wide_to_long <- function(...) {
  .Deprecated("melt::dcast.data.table")
  "Use data.table::melt.data.table"
}

#' @rdname vein-deprecated
#' @export
invcop <- function(...) {
  .Deprecated("get_project")
  "Use projects"
}


#' @rdname vein-deprecated
#' @export
EmissionsList <- function(...) {
  .Deprecated("Emissions")
  "EmissionsList"
}


#' @rdname vein-deprecated
#' @export
running_losses <- function() {
  .Deprecated("emis_evap")
  "Evaporative emissions"
}

#' @rdname vein-deprecated
#' @export
hot_soak <- function() {
  .Deprecated("emis_evap")
  "Evaporative emissions"
}




#' @rdname vein-deprecated
#' @references Mellios G and Ntziachristos 2016. Gasoline evaporation. In:
#' EEA, EMEP. EEA air pollutant emission inventory guidebook-2009. European
#' Environment Agency, Copenhagen, 2009
#' @export
Evaporative <- function() {
  .Deprecated("emis_evap")
  "Evaporative emissions"
}


