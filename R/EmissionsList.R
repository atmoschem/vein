#' DEPRECATED Construction function for class "EmissionsList"
#' Not exported
#'
#' @description \code{EmissionsList} returns a tranformed object with class "EmissionsList".
#'
#' @return Objects of class "EmissionsList" and numeric elements as "units"
#'
#' @param x object with class "EmissionList"
#' @param object object with class "EmissionList"
#' @param ... ignored
#' @rdname EmissionsList
#' @aliases EmissionsList print.EmissionsList summary.EmissionsList
#' plot.EmissionsList
#' @export
#' @examples \dontrun{
#' # do not run
#' # DEPRECATED
#' }
EmissionsList <- function(x,  ...) {
  e <- x
  if ( !is.list(e) ) {
    stop("Class of e must b 'list'")
  } else if ( is.list(e) && is.numeric(e[[1]]) ){
    ex <-  lapply(1:length(e), function(i)  {
      e[[i]] <- e[[i]]*units::as_units("g h-1")
    })
    class(ex) <- c("EmissionsList",class(e))
  } else if ( is.list(e) && is.list(e[[1]]) && is.numeric(e[[1]][[1]]) ) {
    ex <-  lapply(1:length(e), function(i)  {
      lapply(1:length(e[[1]]), function(j)  {
        e[[i]][[j]] <- e[[i]][[j]]*units::as_units("g h-1")
      }) })
    class(ex) <- c("EmissionsList",class(e))
    class(ex[[1]]) <- c("EmissionsList",class(e))
  } else if ( is.list(e) && is.list(e[[1]]) && is.list(e[[1]][[1]] ) &&
              is.numeric(e[[1]][[1]][[1]]) ){
    ex <-  lapply(1:length(e), function(i)  {
      lapply(1:length(e[[1]]), function(j) {
        lapply(1:length(e[[1]][[1]]), function(k) {
          e[[i]][[j]][[k]] <- e[[i]][[j]][[k]]*units::as_units("g h-1")
        }) }) })
    class(ex[[1]][[1]]) <- c("EmissionsList",class(e))
    class(ex[[1]]) <- c("EmissionsList",class(e))
    class(ex) <- c("EmissionsList",class(e))
  }
  return(ex)
}

#' @rdname EmissionsList
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


#' @rdname EmissionsList
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

#' @rdname EmissionsList
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

