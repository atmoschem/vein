#' Matrix and vector multiplication
#'
#' @description \code{matvect} it is a helper function to multiply matrices
#' with vector by rows or columns
#'
#' @param df Numeric Data-frame or matrix.
#' @param x Numeric vector.
#' @param by Character, with two value "row" or "col"
#' @return data-frame
#' @note This function multiplies matrices with vectors by rows or colums.
#' If by = "row" all values of each row will be multiplied with each value
#' of the vector x.
#' If by = "col" all values of each column will be multiplied with each value
#' of the vector x.
#' @export
#' @examples {
#' # Do not run
#' data(net)
#' veh <- age_ldv(net$ldv[1:4], agemax = 4)
#' matvect(veh, 1:4)
#' }
matvect <- function(df, x, by = "row"){
  if(by == "row") {
    if(nrow(df) != length(x)) stop("Rows of df must be the same as length of ef")
    a <- do.call("rbind",lapply(1:nrow(df), function(i) {
      df[i, ] * as.numeric(x)[i]
    }))
    a <- as.data.frame(a)
    return(a)
  } else  if (by == "col"){
    if(ncol(df) != length(x)) stop("Cols of df must be the same as length of ef")
    a <- t(do.call("rbind",lapply(1:ncol(df), function(i) {
      df[, i] * as.numeric(x)[i]
    })))
    a <- as.data.frame(a)
    return(a)

  }
}

