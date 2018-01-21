#' Post emissions
#'
#' @description \code{emis_post} simplify emissions estimated as total per type category of
#' vehicle or by street. It reads EmissionsArray. It can return an dataframe
#' with hourly emissions at each street, or a data base with emissions by vehicular
#' category, hour, including size, fuel and other characteristics.
#'
#' @param arra Array of emissions 4d: streets x category of vehicles x hours x days or
#' 3d: streets x category of vehicles x hours
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
#' @note This function depends on EmissionsArray objests which currently has
#' 4 dimensions. However, a future version of VEIN will produce EmissionsArray
#' with 3 dimensiones and his function also will change. This change will be
#' made in order to not produce inconsistencies with previous versions, therefore,
#' if the user count with an EmissionsArry with 4 dimension, it will be able
#' to use this function.
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
#' speed <- netspeed(pcw, net$ps, net$ffs, net$capacity, net$lkm, alpha = 1)
#' pckm <- fkm[[1]](1:24); pckma <- cumsum(pckm)
#' cod1 <- emis_det(po = "CO", cc = 1000, eu = "III", km = pckma[1:11])
#' cod2 <- emis_det(po = "CO", cc = 1000, eu = "I", km = pckma[12:24])
#' #vehicles newer than pre-euro
#' co1 <- fe2015[fe2015$Pollutant=="CO", ] #24 obs!!!
#' cod <- c(co1$PC_G[1:24]*c(cod1,cod2),co1$PC_G[25:nrow(co1)])
#' lef <- ef_ldv_scaled(co1, cod, v = "PC",  cc = "<=1400",
#'                      f = "G",p = "CO", eu=co1$Euro_LDV)
#' lef <- c(lef,lef[length(lef)],lef[length(lef)],lef[length(lef)],
#'          lef[length(lef)],lef[length(lef)])
#' E_CO <- emis(veh = pc1,lkm = net$lkm, ef = lef, speed = speed, agemax = 41,
#'              profile = pc_profile)
#' # arguments required: arra, pollutant ad by
#' E_CO_STREETS <- emis_post(arra = E_CO, pollutant = "CO", by = "streets_wide")
#' summary(E_CO_STREETS)
#' # arguments required: arra, veh, size, fuel, pollutant ad by
#' E_CO_DF <- emis_post(arra = E_CO,  veh = "PC", size = "<1400", fuel = "G",
#' pollutant = "CO", by = "veh")
#' head(E_CO_DF)
#' # recreating 24 profile
#' lpc <-list(pc1*0.2, pc1*0.1, pc1*0.1, pc1*0.2, pc1*0.5, pc1*0.8,
#'            pc1, pc1*1.1, pc1,
#'            pc1*0.8, pc1*0.5, pc1*0.5,
#'            pc1*0.5, pc1*0.5, pc1*0.5, pc1*0.8,
#'            pc1, pc1*1.1, pc1,
#'            pc1*0.8, pc1*0.5, pc1*0.3, pc1*0.2, pc1*0.1)
#' E_COv2 <- emis(veh = lpc,  lkm = net$lkm, ef = lef, speed = speed[, 1:24],
#'             agemax = 41, hour = 24, day = 1)
#' plot(E_COv2)
#' E_CO_DFv2 <- emis_post(arra = E_COv2,  veh = "PC", size = "<1400", fuel = "G",
#' pollutant = "CO", by = "veh")
#' }
emis_post <- function(arra, veh, size, fuel, pollutant, by = "veh") {
  if ( class(arra) != "EmissionsArray" && !is.array(arra) ){
    stop("No EmissionsArray")
  } else if (length(dim(arra)) == 4){
    if (by == "veh" & class(arra)=="EmissionsArray" && is.array(arra) ){
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
      hour <- rep(1:(dim(arra)[3]*dim(arra)[4]), #hours x days
                  each = dim(arra)[2]) #veh cat
      df$hour <- hour
      df$g <- Emissions(df$g)
      return(df)
    } else if (by == "streets_narrow") {
      # soon deprecated this function?
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
      df[,1] <- seq(1,dim(arra)[1])
      df[,2] <- df[,2] * units::as_units("g h-1")
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

  } else {
    if (by == "veh" & class(arra)=="EmissionsArray" && is.array(arra) ){
      x <- as.vector(apply(X = arra, MARGIN = c(2,3), FUN = sum, na.rm = T))
      df <- cbind(deparse(substitute(arra)),
                  as.data.frame(x))
      names(df) <- c(as.character(df[1,1]), "g")
      nombre <- rep(paste(as.character(df[1,1]),
                          seq(1:dim(arra)[2]),
                          sep = "_"),dim(arra)[3], by=7 )
      df[,1] <- nombre
      df$veh <- rep(veh, nrow(df))
      df$size <- rep(size, nrow(df))
      df$fuel <- rep(fuel, nrow(df))
      df$pollutant <- rep(pollutant, nrow(df))
      df$age <- rep(1:dim(arra)[2], dim(arra)[3])

      hour <- rep(1:dim(arra)[3], #hours x days
                  each = dim(arra)[2]) #veh cat
      df$hour <- hour
      df$g <- Emissions(df$g)
      return(df)
    } else if (by == "streets_narrow") {
      df <- as.vector(apply(X = arra, MARGIN = c(2,3), FUN = sum, na.rm = T))
      warning("TODO: Improve")
      return(df)
    } else if (by == "streets_wide") {
      df <- Emissions(apply(X = arra, MARGIN = c(1,3), FUN = sum, na.rm = T))
      names(df) <- lapply(1:dim(arra)[3], function(i){paste0("h",i)})
      return(df)
    }

    }
}
