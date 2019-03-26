#' Returns amount of vehicles at each age applying survival functions
#'
#' @description \code{\link{age_ldv}} returns amount of vehicles at each age
#'
#' @param x Numeric; numerical vector of vehicles with length equal to lines features of raod network
#' @param type Character; any of "gompertz", "double_logistic", "weibull" and "weibull2"
#' @param a Numeric; parameter of survival equation
#' @param b Numeric; parameter of survival equation
#' @param name Character; of vehicle assigned to columns of dataframe
#' @param agemin Integer; age of newest vehicles for that category
#' @param agemax Integer; age of oldest vehicles for that category
#' @param k Numeric; multiplication factor. If its length is > 1, it must match the length of x
#' @param net SpatialLinesDataFrame or Spatial Feature of "LINESTRING"
#' @param verbose Logical;  message with average age and total numer of vehicles
#' @param namerows Any vector to be change row.names. For instance, name of
#' regions or streets.
#' @return dataframe of age distrubution of vehicles
#' @importFrom sf st_sf st_as_sf
#' @export
#' @note
#' \strong{gompertz: 1 - exp(-exp(a + b*time))},
#' defaults PC: b = -0.137, a = 1.798, LCV: b = -0.141, a = 1.618
#' MCT (2006). de Gases de Efeito Estufa-Emissoes de Gases de
#' Efeito Estufa por Fontes Moveis, no Setor Energético. Ministerio da Ciencia e Tecnologia.
#' This curve is also used by Guo and Wang (2012, 2015) in the form:
#' V*exp(alpha*exp(beta*E)) where
#' V is the saturation  car ownership level and E GDP per capita
#' Huo, H., & Wang, M. (2012). Modeling future vehicle sales and stock in China.
#' Energy Policy, 43, 17–29. doi:10.1016/j.enpol.2011.09.063
#' Huo, Hong, et al. "Vehicular air pollutant emissions in China: evaluation of past control
#' policies and future perspectives." Mitigation and Adaptation Strategies for Global Change 20.5 (2015): 719-733.
#' \strong{double_logistic: 1/(1 + exp(a*(time + b))) + 1/(1 + exp(a*(time - b)))},
#' defaults PC: b = 21, a = 0.19, LCV: b = 15.3, a = 0.17, HGV: b = 17, a = 0.1, BUS: b = 19.1, a = 0.16
#' MCT (2006). de Gases de Efeito Estufa-Emissoes de Gases de
#' Efeito Estufa por Fontes Moveis, no Setor Energético. Ministerio da Ciencia e Tecnologia.
#' \strong{weibull: exp(-(time/a)^b)},
#' defaults PC: b = 4.79, a = 14.46, Taxi: b = +inf, a = 5,
#' Government and business: b = 5.33, a = 13.11
#' Non-operating vehicles: b = 5.08, a = 11.53
#' Bus: b = +inf, a = 9, non-transit bus: b = +inf, a = 5.5
#' Heavy HGV: b = 5.58, a = 12.8, Medium HGV: b = 5.58, a = 10.09, Light HGV: b = 5.58, a = 8.02
#' Hao, H., Wang, H., Ouyang, M., & Cheng, F. (2011). Vehicle survival patterns in China.
#' Science China Technological Sciences, 54(3), 625-629.
#' \strong{weibull2: exp(-((time + a)/b)^b )},
#' defaults b = 11, a = 26
#' Zachariadis, T., Samaras, Z., Zierock, K. H. (1995). Dynamic modeling of vehicle
#' populations: an engineering approach for emissions calculations. Technological
#' Forecasting and Social Change, 50(2), 135-149. Cited by Huo and Wang (2012)
#' @examples {
#' data(net)
#' PC_E25_1400 <- age(x = net$ldv)
#' plot(PC_E25_1400)
#' PC_E25_1400 <- age(x = net$ldv,  net = net)
#' plot(PC_E25_1400)
#' }
age <- function (x,
                 type = "weibull",
                 a = 14.46,
                 b = 4.79,
                 name = "veh",
                 agemin = 1,
                 agemax = 50,
                 k = 1,
                 net,
                 verbose = FALSE,
                 namerows){
  #check agemax
  if(agemax < 1) stop("Agemax should be bigger than 1")

  if (missing(x) | is.null(x)) {
    stop (print("Missing vehicles"))
  }

  # gompertz ####
  if(type == "gompertz") {
    surv <- function (time, a, b) {1 - exp(-exp(a + b*time))}
    anos <- seq(agemin, agemax)
    if(length(a) > 1 | length(b) > 1) {
      if(length(a) != length(x) | length(b) != length(x)) {
        stop("When length of a or b > 1, length must be length of x")
      }
      d <- do.call("rbind", lapply(1:length(a), function(i){
        surv(time = anos, a = a[i], b = b[i])
      }))
      df <- as.data.frame(as.matrix(x) %*% d)
    } else {
      d <- surv(time = anos, a = a, b = b)
      df <- as.data.frame(as.matrix(x) %*% matrix(d,
                                                  ncol = length(anos),
                                                  nrow = 1))
    }
    # double logistic ####
  } else if(type == "double_logistic"){
    surv <- function (time, a, b) {
      1/(1 + exp(a*(time + b))) + 1/(1 + exp(a*(time - b)))
    }
    anos <- seq(agemin, agemax)
    if(length(a) > 1 | length(b) > 1) {
      if(length(a) != length(x) | length(b) != length(x)) {
        stop("When length of a or b > 1, length must be length of x")
      }
      d <- do.call("rbind", lapply(1:length(a), function(i){
        surv(time = anos, a = a[i], b = b[i])
      }))
      df <- as.data.frame(as.matrix(x) %*% d)
    } else {
      d <- surv(time = anos, a = a, b = b)
      df <- as.data.frame(as.matrix(x) %*% matrix(d,
                                                  ncol = length(anos),
                                                  nrow = 1))
    }
    # weibull ####
  } else if(type == "weibull"){
    surv <- function (time, a, b) {
      exp(-(time/a)^b)
    }
    anos <- seq(agemin, agemax)
    if(length(a) > 1 | length(b) > 1) {
      if(length(a) != length(x) | length(b) != length(x)) {
        stop("When length of a or b > 1, length must be length of x")
      }
      d <- do.call("rbind", lapply(1:length(a), function(i){
        surv(time = anos, a = a[i], b = b[i])
      }))
      df <- as.data.frame(as.matrix(x) %*% d)
    } else {
      d <- surv(time = anos, a = a, b = b)
      df <- as.data.frame(as.matrix(x) %*% matrix(d,
                                                  ncol = length(anos),
                                                  nrow = 1))
    }
  } else if(type == "weibull2"){
    surv <- function (time, a, b) {
      exp(-((time + a)/b)^b )
    }
    anos <- seq(agemin, agemax)
    if(length(a) > 1 | length(b) > 1) {
      if(length(a) != length(x) | length(b) != length(x)) {
        stop("When length of a or b > 1, length must be length of x")
      }
      d <- do.call("rbind", lapply(1:length(a), function(i){
        surv(time = anos, a = a[i], b = b[i])
      }))
      df <- as.data.frame(as.matrix(x) %*% d)
    } else {
      d <- surv(time = anos, a = a, b = b)
      df <- as.data.frame(as.matrix(x) %*% matrix(d,
                                                  ncol = length(anos),
                                                  nrow = 1))
    }
  }
  # data.frame ####
  if(agemin > 1){
    df <- cbind(matrix(0, ncol = agemin - 1, nrow = length(x)), df)
    df <- as.data.frame(df)
  }
  names(df) <- paste(name, seq(1, agemax), sep = "_")

  if(length(k) > 1){
    df <- vein::matvect(df = df, x = k)
  } else {
    df <- df*k
  }

  if(verbose){
    message(paste("Average age of",name, "is",
                  round(sum(seq(1, agemax)*base::colSums(df, na.rm = T)/sum(df, na.rm = T)), 2),
                  sep=" "))
    message(paste("Number of",name, "is", round(sum(df, na.rm = T)/1000, 2),
                  "* 10^3 veh", sep=" ")
    )
    cat("\n")
  }
  if(!missing(namerows)) {
    if(length(namerows) != nrow(x)) stop("length of namerows must be the length of number of rows of veh")
    row.names(df) <- namerows
  }
  if(!missing(net)){
    netsf <- sf::st_as_sf(net)
    dfsf <- sf::st_sf(Vehicles(df), geometry = netsf$geometry)
    return(dfsf)
  } else {
    return(Vehicles(df))
  }

}
