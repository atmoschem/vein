#' Printing class "Emissionslist"
#'
#' @description A simple method for printing objects with class "EmissionsList".
#' It prints \code{\link{head}} of the first numeric element of the list.
#'
#' @param e Object with class "EmissionsList"
#' @param default when T call method print.listof
#' \code{\link{print.listof}}. When F returns messages with list structure
#' @param ... ignored
#' @return Print method
#' @rdname print.EmissionsList
#' @export
EmissionsList <- function(e, ...) {
  UseMethod("EmissionsList", e)
}
#' @examples \dontrun{
#' # Do not run
#' }
print.EmissionsList <- function(e, default = F) {
  if ( default == TRUE ) {
    print.listof(e)
  } else if ( is.list(e) && is.numeric(e[[1]]) ){ #dont work
    cat("This EmissionsList has\n", length(e),
        "vehicle categories\n")
    cat(length(e[[1]]), "streets\n")
    print(utils::head(e[[1]]))
    cat(" ...\n")
  } else if ( is.list(e) && is.list(e[[1]]) && is.numeric(e[[1]][[1]]) ) {
    cat("This EmissionsList has\n", length(e), "hours\n")
    cat(length(e[[1]]), "vehicle categories\n")
    cat(length(e[[1]][[1]]), "streets\n")
    print(utils::head(e[[1]][[1]]))
    cat(" ...")
  } else if ( is.list(e) && is.list(e[[1]]) && is.list(e[[1]][[1]]) &&
              is.numeric(e[[1]][[1]][[1]]) ) {
    cat("This EmissionsList has\n", length(e), "days\n")
    cat(length(e[[1]]), "hours\n")
    cat(length(e[[1]][[1]]), "vehicle categories\n")
    cat(length(e[[1]][[1]][[1]]), "streets\n")
    print(utils::head(e[[1]][[1]][[1]]))
    cat(" ...")
  }
}
