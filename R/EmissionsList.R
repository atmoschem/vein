#' DEPRECATED Construction function for class "EmissionsList"
#'
#' @description \code{EmissionsList} returns a tranformed object with class "EmissionsList".
#'
#' @return Objects of class "EmissionsList" and numeric elements as "units"
#'
#' @param x object with class "EmissionList"
#' @param object object with class "EmissionList"
#' @param ... ignored
#' @aliases EmissionsList print.EmissionsList summary.EmissionsList
#' plot.EmissionsList
#' @name vein-deprecated
#' @seealso \code{\link{vein-deprecated}}
#' @keywords internal
NULL

#' @rdname vein-deprecated
#'
#' @export
#' @examples \dontrun{
#' # do not run
#' # DEPRECATED
#' }
EmissionsList <- function(x,  ...) {
  .Deprecated("EmissionsList")
  "EmissionsList"
}


#' @rdname vein-deprecated
#' @method print EmissionsList
#' @export
print.EmissionsList <- function(x,  ...) {
  e <- x
  if ( is.list(e) && is.numeric(e[[1]]) ){
    cat("This EmissionsList has\n", length(e),
        "vehicle categories\n")
    for ( i in 1:length(e)  ) {
      e[[i]] <- e[[i]]*units::as_units("g h-1")
    }
    cat(length(e[[1]]), "streets\n")
    print(utils::head(e[[1]]))
    cat(" ...\n")
  } else if ( is.list(e) && is.list(e[[1]]) && is.numeric(e[[1]][[1]]) ) {
    cat("This EmissionsList has\n", length(e), "hours\n")
    cat(length(e[[1]]), "vehicle categories\n")
    cat(length(e[[1]][[1]]), "streets\n")
    for ( i in 1:length(e)  ) {
      e[[i]] <- e[[i]]*units::as_units("g h-1")
    }
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


#' @rdname vein-deprecated
#' @method summary EmissionsList
#' @export
summary.EmissionsList <- function(object, ...) {
  e <- object
  if ( is.list(e) && is.numeric(e[[1]]) ){ #dont work
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

#' @rdname vein-deprecated
#' @method plot EmissionsList
#' @export
plot.EmissionsList <- function(x, ...) {
  e <- x
  if ( is.list(e) && is.numeric(e[[1]]) ){
    graphics::plot(e[[1]], type = "l",  ...)
  } else if ( is.list(e) && is.list(e[[1]]) && inherits(e[[1]][[1]], "units") ) {
    graphics::plot(e[[1]][[1]], type = "l", ...)
  } else if ( is.list(e) && is.list(e[[1]]) && is.list(e[[1]][[1]]) &&
              inherits(e[[1]][[1]][[1]], "units") ) {
    graphics::plot(e[[1]][[1]][[1]], type = "l", ...)
  }
}

