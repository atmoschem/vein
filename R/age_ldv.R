#' Returns amount of vehicles at each age
#'
#' @description Returns amount of vehicles at each age
#'
#' @param x numerical vector of vehicles
#' @param name of vehicle assigned to columns of dataframe
#' @param a parameter of survival equation
#' @param b parameter of survival equation
#' @param agemin age of newest vehicles for that category
#' @param agemax age of oldest vehicles for that category
#' @param k multiplication factor
#' @param bystreet when TRUE it is expecting that 'a' and 'b' are numeric vectors with length equal to x
#' @param message message with average age and total numer of vehicles
#' @return dataframe of age distrubution of vehicles
#' @export
#' @examples \dontrun{
#' # Do not run
#' pc <- rnorm(100, 300, 10)
#' PC_E25_1400 <- age_ldv(x = pc,name = "PC_E25_1400")
#' plot(PC_E25_1400)
#' }
age_ldv <- function (x, name, a = 1.698, b = -0.2, agemin = 1, agemax = 50, k = 1,
                     bystreet = F, message = TRUE){
  if (missing(x) | is.null(x)) {
    stop (print("Missing vehicles"))
  } else if (bystreet == T){
    if(length(x) != length(a)){
      stop((print("Lengths of veh and age must be the same")))
    }
    d <- suca <- list()
    for (i in seq_along(x)) {
      suca[[i]] <- function (t) {1 - exp(-exp(a[i] + b[i]*t))}
      anos <- seq(agemin,agemax)
      d[[i]] <- (-1)*diff(suca[[i]](anos))
      d[[i]][length(d[[i]])+1] <- d[[i]][length(d[[i]])]
      d[[i]] <- d[[i]] + (1 - sum(d[[i]]))/length(d[[i]])
      d[[i]] <- d[[i]]*x[i]
    }
    df <- as.data.frame(matrix(0,ncol=length(anos), nrow=1))
    for (i in seq_along(x)) {
      df[i,] <- d[[i]]
    }
    if (agemin > 1) {
      df <- cbind(as.data.frame(matrix(0,ncol=agemin-1, nrow=length(x))),
                  df)
    } else {
      df <- df
      }
    if(message){
    names(df) <- paste(name,seq(1,agemax),sep="_")
    message(paste("Average age of",name, "is",
                  round(sum(seq(1,agemax)*base::colSums(df)/sum(df)), 2),
                  sep=" "))
    message(paste("Number of",name, "is",
                  round(sum(df*k, na.rm = T)/1000, 2),
                  "* 10^3 veh",
                  sep=" ")
    )
    cat("\n")
    }
  } else {
    suca <- function (t) {1 - exp(-exp(a + b*t))}
    anos <- seq(agemin,agemax)
    d <- (-1)*diff(suca(anos))
    d[length(d)+1] <- d[length(d)]
    d <- d + (1 - sum(d))/length(d)
    df <- as.data.frame(as.matrix(x) %*%matrix(d,ncol=length(anos), nrow=1))
    if (agemin > 1) {
      df <- cbind(as.data.frame(matrix(0,ncol=agemin-1, nrow=length(x))),
                  df)
    } else {df <- df}
    if(message){
    names(df) <- paste(name,seq(1,agemax),sep="_")
    message(paste("Average age of",name, "is",
                  round(sum(seq(1,agemax)*base::colSums(df)/sum(df)), 2),
                  sep=" "))
    message(paste("Number of",name, "is",
                  round(sum(df*k, na.rm = T)/1000, 2),
                  "* 10^3 veh",
                  sep=" ")
    )
    cat("\n")
}
  }
  df <- Vehicles(df*k)
  return(df)
}
