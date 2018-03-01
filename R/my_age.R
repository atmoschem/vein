#' Returns amount of vehicles at each age
#'
#' @description \code{my_age} returns amount of vehicles at each age using a
#' numeric vector.
#'
#' @param x numerical vector of vehicles.
#' @param y Age dustribution of vehicles.
#' @param name of vehicle assigned to columns of dataframe.
#' @param k multiplication factor.
#' @param message message with average age and total numer of vehicles.
#' @return dataframe of age distrubution of vehicles.
#' @export
#' @examples {
#' # Do not run
#' pc <- rnorm(100, 300, 10)
#' dpc <- c(rnorm(10, 99, 1), NA, NA, NA)
#' PC_E25_1400 <- my_age(x = pc, y = dpc, name = "PC_E25_1400")
#' plot(PC_E25_1400)
#' }
my_age <- function (x, y, name,  k = 1, message = TRUE){
  if (missing(x) | is.null(x)) {
    stop (print("Missing vehicles"))
    } else if (missing(y) | is.null(y)) {
      stop (print("Missing distributed vehicles"))
    } else {
      y <- y[!is.na(y)]
    d <- matrix(data = y/sum(y), nrow = 1, ncol=length(y))
    df <- as.data.frame(as.matrix(x) %*%d)
    names(df) <- paste(name,seq(1,length(y)),sep="_")
    if(message){
      message(paste("Average age of",name, "is",
                  round(sum(seq(1,length(y))*base::colSums(df)/sum(df),na.rm = T), 2),
                  sep=" "))
    message(paste("Number of",name, "is",
                  round(sum(df*k, na.rm = T)/1000, 3),
                  "* 10^3 veh",
                  sep=" ")
            )
    cat("\n")
    }}
  df <- Vehicles(df*k)
  return(df)
}
