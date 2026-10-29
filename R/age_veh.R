#' Returns amount of vehicles at each age
#'
#' @family age
#'
#' @description \code{\link{age_veh}} returns amount of vehicles at each age
#' @param x Numeric; numerical vector of vehicles with length equal to lines features of road network
#' @param type "ldv", "hdv", or "mc" representing light vehicles, heavy vehicles or motorcycles
#' @param name Character; of vehicle assigned to columns of dataframe
#' @param a Numeric; parameter of survival equation
#' @param b Numeric; parameter of survival equation
#' @param agemin Integer; age of newest vehicles for that category
#' @param agemax Integer; age of oldest vehicles for that category
#' @param k Numeric; multiplication factor. If its length is > 1, it must match the length of x
#' @param bystreet Logical; when TRUE it is expecting that 'a' and 'b' are numeric vectors with length equal to x
#' @param net SpatialLinesDataFrame or Spatial Feature of "LINESTRING"
#' @param verbose Logical;  message with average age and total numer of vehicles
#' @param namerows Any vector to be change row.names. For instance, name of
#' regions or streets.
#' @param time Character to be the time units as denominator, eg "1/h"
#' @return dataframe of age distrubution of vehicles
#' @importFrom sf st_sf st_as_sf
#' @note
#' The functions age* produce distribution of the circulating fleet by age of use.
#' The order of using these functions is:
#'
#' 1. If you know the distribution of the vehicles by age of use , use:  \code{\link{my_age}}
#' 2. If you know the sales of vehicles, or the registry of new vehicles,
#' use \code{\link{age}} to apply a survival function.
#' 3. If you know the theoretical shape of the circulating fleet use
#' \code{\link{age_veh}} (\code{\link{age_ldv}}, \code{\link{age_hdv}} and
#' \code{\link{age_moto}} are deprecated wrappers of \code{\link{age_veh}}).
#' For instance, you dont know the sales or registry of vehicles, but somehow
#' you know the shape of this curve.
#' 4. You can use/merge/transform/adapt any of these functions.
#'
#' The shape is built from a survival curve S(t), evaluated at the ages
#' \code{agemin:agemax}: for type "ldv" a Gompertz curve
#' S(t) = 1 - exp(-exp(a + b*t)) (defaults a = 1.698, b = -0.2, from the
#' 1st Brazilian national GHG inventory, MCT 2006) and for "hdv" and "mc" a
#' double logistic curve S(t) = 1/(1 + exp(a*(t + b))) + 1/(1 + exp(a*(t - b)))
#' (defaults a = 0.2, b = 17).
#' The fraction of vehicles at each age is NOT S(t) but the decrease of
#' survival between consecutive ages, d(t) = S(t) - S(t + 1)
#' (i.e. \code{-diff(S)}); the last age repeats the value of the previous one.
#' Because sum(d) is usually lower than 1, the remainder (1 - sum(d)) is added
#' in equal parts to every age (or subtracted, when negative), so that the
#' distribution sums 1 and the total number of vehicles x*k is preserved.
#' Hence, when \code{agemax} is small (i.e. the survival curve is truncated
#' well before it reaches zero), most of the fleet is spread uniformly among
#' the ages. With the default parameters, the "ldv" distribution peaks around
#' age 8 and the "hdv"/"mc" distribution around age 16.
#' Ages lower than \code{agemin} are filled with zero. The factor \code{k}
#' is applied once.
#' @export
#' @examples \dontrun{
#' data(net)
#' PC_E25_1400 <- age_veh(x = net$ldv, name = "PC_E25_1400")
#' plot(PC_E25_1400)
#' PC_E25_1400 <- age_veh(x = net$ldv, name = "PC_E25_1400", net = net)
#' plot(PC_E25_1400)
#' LT_B5 <- age_veh(x = net$hdv, type = "hdv", name = "LT_B5")
#' plot(LT_B5)
#' }
age_veh <- function(x,
                    type = "ldv",
                    name = "age",
                    a = if (type == "ldv") 1.698 else 0.2,
                    b = if (type == "ldv") -0.2 else 17,
                    agemin = 1,
                    agemax = 50,
                    k = 1,
                    bystreet = F,
                    net,
                    verbose = FALSE,
                    namerows,
                    time) {
  if (!type %in% c("ldv", "hdv", "mc")) stop("Type can be ldv, hdv or mc")

  # check na
  x[is.na(x)] <- 0

  x <- as.numeric(x)

  # length k
  if (length(k) != 1 && length(k) != length(x)) {
    stop("length of 'k' must be 1 or equal to length of 'x'")
  }
  k <- as.numeric(k)

  # check agemax
  if (agemax < 1) stop("'agemax' must be >= 1")
  if (agemin < 1) stop("'agemin' must be >= 1")
  if (agemin > agemax) stop("'agemin' must be <= 'agemax'")

  # distribution by age from a survival curve
  # -diff(S) and the remainder distributed uniformly so that sum is 1
  dist_age <- function(fx, anos) {
    if (length(anos) == 1) return(1)
    d <- (-1) * diff(fx(anos))
    d[length(d) + 1] <- d[length(d)]
    d + (1 - sum(d)) / length(d)
  }

  anos <- seq(agemin, agemax)

  # bystreet = TRUE
  if (bystreet == T) {
    if (length(x) != length(a)) {
      stop("Lengths of veh and age must be the same")
    }
    if (length(b) == 1) b <- rep(b, length(x))
    if (length(x) != length(b)) {
      stop("length of 'b' must be 1 or equal to length of 'x'")
    }

    d <- lapply(seq_along(x), function(i) {
      if (type == "ldv") {
        fx <- function(t) {
          1 - exp(-exp(a[i] + b[i] * t))
        }
      } else {
        fx <- function(t) {
          1 / (1 + exp(a[i] * (t + b[i]))) + 1 / (1 + exp(a[i] * (t - b[i])))
        }
      }
      dist_age(fx, anos) * x[i]
    })
    df <- as.data.frame(do.call("rbind", d))

    # bystreet = FALSE
  } else {
    # functions
    if (type == "ldv") {
      fx <- function(t) {
        1 - exp(-exp(a + b * t))
      }
    } else {
      fx <- function(t) {
        1 / (1 + exp(a * (t + b))) + 1 / (1 + exp(a * (t - b)))
      }
    }

    d <- dist_age(fx, anos)
    df <- as.data.frame(as.matrix(x) %*% matrix(d, ncol = length(anos), nrow = 1))
  }

  df <- as.data.frame(cbind(as.data.frame(matrix(0, ncol = agemin - 1,
    nrow = length(x))),
  df))

  names(df) <- paste(name, seq(1, agemax), sep = "_")

  # k is applied only once
  df <- df * k

  if (verbose) {
    message(paste("Average age of", name, "is",
      round(sum(seq(1, agemax) * base::colSums(df, na.rm = T) / sum(df, na.rm = T)), 2),
      sep = " "))
    message(paste("Number of", name, "is",
      round(sum(df, na.rm = T) / 1000, 2),
      "* 10^3 veh",
      sep = " ")
    )
    cat("\n")
  }
  if (!missing(namerows)) {
    if (length(namerows) != nrow(df)) stop("length of namerows must be the length of number of rows of veh")
    row.names(df) <- namerows
  }

  if (!missing(time)) {
    veh <- Vehicles(df, time = time)
  } else {
    veh <- Vehicles(df)
  }

  if (!missing(net)) {
    netsf <- sf::st_as_sf(net)
    return(sf::st_sf(veh, geometry = sf::st_geometry(netsf)))
  } else {
    return(veh)
  }

}
