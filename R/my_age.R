#' Returns amount of vehicles at each age
#'
#' @description Returns amount of vehicles at each age using a numeric vector
#'
#' @param x numerical vector of vehicles
#' @param y Age dustribution of vehicles
#' @param name of vehicle assigned to columns of dataframe
#' @param k multiplication factor
#' @return dataframe of age distrubution of vehicles
#' @export
#' @examples \dontrun{
#' # Do not run
#' pc <- rnorm(100, 300, 10)
#' dpc <- rnorm(10, 100, 1)
#' PC_E25_1400 <- my_age(x = pc,y = dpc, name = "PC_E25_1400")
#' }
my_age <- function (x, y, name,  k = 1){
  if (missing(x) | is.null(x)) {
    stop (print("Missing vehicles"))
    } else if (missing(y) | is.null(y)) {
      stop (print("Missing distributed vehicles"))
    } else {
    d <- matrix(data = y/sum(y), nrow = 1, ncol=length(y))
    df <- as.data.frame(as.matrix(x) %*%d)
    names(df) <- paste(name,seq(1,length(y)),sep="_")
    message(paste("Average age of",name, "is",
                  round(sum(seq(1,length(y))*base::colSums(df)/sum(df),na.rm = T), 2),
                  sep=" "))
  }
  df <- as.Vehicles(df*k)
  return(df)
}
