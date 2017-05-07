#' Post emissions
#'
#' Simplify emissions estimated as total per type category of vehicle or
#' by street. It reads array of emissions
#'
#' @param arra Array of emissions (streets x category of vehicles x hours x days)
#' @param veh Type of vehicle
#' @param size Size or weight
#' @param fuel Fuel
#' @param pollutant Pollutant
#' @param by Type of output, "veh" for total vehicular category ,
#' "streets_narrow" or "streets_wide". "streets_wide" returns  a dataframe with
#'  rows as number of streets and columns the hours as days*hours considered, e.g.
#' 168 columns as the hours of a whole week and "streets_wide repeats the
#' row number of streets by hour and day of the week
#' @export
#' @examples \dontrun{
#' # Do not run
#' data(net)
#' data(pc_profile)
#' data(fe2015)
#' data(fkm)
#' PC_G <- c(33491,22340,24818,31808,46458,28574,24856,28972,37818,49050,87923,
#'           133833,138441,142682,171029,151048,115228,98664,126444,101027,
#'           84771,55864,36306,21079,20138,17439, 7854,2215,656,1262,476,512,
#'           1181, 4991, 3711, 5653, 7039, 5839, 4257,3824, 3068)
#' veh <- data.frame(PC_G = PC_G)
#' pc1 <- my_age(x = net$ldv, y = PC_G, name = "PC")
#' pcw <- temp_fact(net$ldv+net$hdv, pc_profile)
#' speed <- netspeed(pcw, net$ps, net$ffs, net$capacity, net$lkm, alpha = 1,
#' isList = T)
#' pckm <- fkm[[1]](1:24); pckma <- cumsum(pckm)
#' cod1 <- emis_det(po = "CO", cc = 1000, eu = "III", km = pckma[1:11])
#' cod2 <- emis_det(po = "CO", cc = 1000, eu = "I", km = pckma[12:24])
#' #vehicles newer than pre-euro
#' co1 <- fe2015[fe2015$Pollutant=="CO", ] #24 obs!!!
#' cod <- c(co1$PC_G[1:24]*c(cod1,cod2),co1$PC_G[25:nrow(co1)])
#' lef <- ef_ldv_scaled(co1, cod, v = "PC", t = "ALL", cc = "ALL",
#'                      f = "G",p = "CO", eu=co1$Euro_LDV)
#' lef <- c(lef,lef[length(lef)],lef[length(lef)],lef[length(lef)],
#'          lef[length(lef)],lef[length(lef)])
#' E_CO <- emis(veh = pc1,lkm = net$lkm, ef = lef, speed = speed, agemax = 41,
#'              profile = pc_profile, hour = 24, day = 7, array = T)
#' # arguments required: arra, pollutant ad by
#' E_CO_STREETS <- emis_post(arra = E_CO, pollutant = "CO", by = "streets_wide")
#' names(E_CO_STREETS)
#' class(E_CO_STREETS)
#' summary(E_CO_STREETS)
#' head(E_CO_STREETS)
#' E_CO_STREETS_n <- emis_post(arra = E_CO, pollutant = "CO",
#' by = "streets_narrow")
#' head(E_CO_STREETS_n)
#' # arguments required arra, veh, size, fuel, pollutant, by
#' E_CO_DF <- emis_post(arra = E_CO, veh = "PC", size = "<1400cc", fuel = "G",
#'                     pollutant = "CO", by = "veh")
#' head(E_CO_DF)
#' }
emis_post <- function(arra, veh, size, fuel, pollutant, by = "veh") {
  if ( class(arra) != "EmissionsArray" && !is.array(arra) ){
    stop("No EmissionsArray")
  } else if (by == "veh" & class(arra)=="EmissionsArray" && is.array(arra) ){
    x <- unlist(lapply(1:dim(arra)[4], function(j) {
      unlist(lapply (1:dim(arra)[3],function(i) {
        colSums(arra[,,i,j], na.rm = T)
        }))
    }))
    df <- cbind(deparse(substitute(arra)),
                as.data.frame(x))
    names(df) <- c(as.character(df[1,1]), "g")
    nombre <- rep(paste(as.character(df[1,1]),
                        seq(1:dim(arra)[2]),
                        sep = "_"),24*7, by=7 )
    df[,1] <- nombre
    df$veh <- rep(veh, nrow(df))
    df$size <- rep(size, nrow(df))
    df$fuel <- rep(fuel, nrow(df))
    df$pollutant <- rep(pollutant, nrow(df))
    df$age <- rep(seq(1:dim(arra)[2]),24*7, by=7 )
    hour <- rep(seq(0:(dim(arra)[3]-1)),dim(arra)[4],
                by = dim(arra)[2],
                each=dim(arra)[2] )
    df$hour <- hour
    day <- rep(c("Monday", "Tuesday", "Wednesday",
                 "Thursday", "Friday", "Saturday",
                 "Sunday"), each=dim(arra)[2]*dim(arra)[3])
    df$day <- day
    df$g <- Emissions(df$g)
    return(df)
    } else if (by == "streets_narrow") {
      x <- unlist(lapply(1:dim(arra)[4], function(j) {# dia
        unlist(lapply (1:dim(arra)[3],function(i) { # hora
          rowSums(arra[,,i,j], na.rm = T)
        }))
      }))
      df <- cbind(deparse(substitute(arra)),as.data.frame(x))
      names(df) <- c(as.character(df[1,1]), pollutant)
      hour <- rep(seq(0: (dim(arra)[3]-1) ),
                  times = dim(arra)[4],
                  each=dim(arra)[1])
      df$hour <- hour
      day <- c(rep("Monday", dim(arra)[1]*dim(arra)[3]),
               rep("Tuesday", dim(arra)[1]*dim(arra)[3]),
               rep("Wednesday", dim(arra)[1]*dim(arra)[3]),
               rep("Thursday", dim(arra)[1]*dim(arra)[3]),
               rep("Friday", dim(arra)[1]*dim(arra)[3]),
               rep("Saturday", dim(arra)[1]*dim(arra)[3]),
               rep("Sunday", dim(arra)[1]*dim(arra)[3]))
      df$day <- day
      df[,1] <- seq(1,dim(arra)[1])
      df[,2] <- df[,2] * units::parse_unit("g h-1")
      return(df)
    } else if (by == "streets_wide") {
      x <- unlist(lapply(1:dim(arra)[4], function(j) {# dia
        unlist(lapply (1:dim(arra)[3],function(i) { # hora
          rowSums(arra[,,i,j], na.rm = T)
        }))
      }))
      m <- matrix(x, nrow=dim(arra)[1], ncol=dim(arra)[3]*dim(arra)[4])
      df <- as.data.frame(m)
      nombres <- lapply(1:dim(m)[2], function(i){paste0("h",i)})
      return(Emissions(df))
    }
}
