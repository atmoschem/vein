#' Summary of EmissionsList
#'
#' @description The object with class "EmissionsList" is list of emissions.
#' The summary method consists in plotting a mesage with the structure of
#' the list and indicating the summary of the first numeric element
#'
#' @return Summary for class "EmissionsList"

#' @param e Objecto of class "EmissionsArray"
#' @param default Logical value. Whe  default is TRUE it returns the default
#' summary. When FALSE it return the structure of the list
#'
#' @seealso \code{\link{head}}
#' @name summary.EmissionsList
#' @rdname summary.EmissionsList
#' @name summary.EmissionList
#' @title Summary
#' @aliases NULL
NULL
#' @examples \dontrun{
#' #Do not run
#'}
#' @export
summary.EmissionsList <- function(e, default = F) {
  if ( default == TRUE ) {
    summary(e)
  } else if ( is.list(e) && is.numeric(e[[1]]) ){ #dont work
    cat("This EmissionsList has\n", length(e),
        "vehicle categories\n")
    cat(length(e[[1]]), "streets\n")
    print(summary(e[[1]]))
    cat(" ...\n")
  } else if ( is.list(e) && is.list(e[[1]]) && is.numeric(e[[1]][[1]]) ) {
    cat("This EmissionsList has\n", length(e), "hours\n")
    cat(length(e[[1]]), "vehicle categories\n")
    cat(length(e[[1]][[1]]), "streets\n")
    print(summary(e[[1]][[1]]))
    cat(" ...")
  } else if ( is.list(e) && is.list(e[[1]]) && is.list(e[[1]][[1]]) &&
              is.numeric(e[[1]][[1]][[1]]) ) {
    cat("This EmissionsList has\n", length(e), "days\n")
    cat(length(e[[1]]), "hours\n")
    cat(length(e[[1]][[1]]), "vehicle categories\n")
    cat(length(e[[1]][[1]][[1]]), "streets\n")
    print(summary(e[[1]][[1]][[1]]))
    cat(" ...")
  }
}
