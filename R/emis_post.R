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
#' df <- netosm(net = net, x = -46.633949, y = -23.550391, sr = '+init=epsg:32723')
#' net@data <- cbind(net@data,df)
#' rm(df)
#' df <- netspeed(net = net)
#' df$S0 <- df$FFS
#' df$S1 <- df$FFS
#' df$S2 <- df$FFS
#' df$S3 <- df$FFS
#' df$S4 <- df$FFS
#' df$S5 <- df$FFS
#' df$S6 <- df$AS
#' df$S7 <- df$PS
#' df$S8 <- df$PS
#' df$S9 <- df$PS
#' df$S10 <- df$AS
#' df$S11 <- df$AS
#' df$S12 <- df$AS
#' df$S13 <- df$AS
#' df$S14 <- df$AS
#' df$S15 <- df$AS
#' df$S16 <- df$AS
#' df$S17 <- df$PS
#' df$S18 <- df$PS
#' df$S19 <- df$PS
#' df$S20 <- df$AS
#' df$S21 <- df$AS
#' df$S22 <- df$FFS
#' df$S23 <- df$FFS
#' net@data <- cbind(net@data,df)
#' names(net)
#' speed <- lapply(16:39, function(i) {net@data[,i]})
#' pc <- interpolate_pc(net)
#' PC_E25 <- age_ldv(x = pc,name = "PC_E25", b = -0.15,
#' agemax = 40)
#' co1 <- fe2015[fe2015$Pollutant=="CO" & fe2015$Age<25, ] #24 obs!!!
#' l1 <- ef_ldv_scaled(co1, co1$PC_G, v = "PC", t = "ALL", cc = "ALL", f = "G",
#' p = "CO")
#' co2 <- fe2015[fe2015$Pollutant=="CO" & fe2015$Age>24,] #22 obs!!!
#' l2 <- ef_ldv_scaled(co2, co2$PC_G, v = "PC", t = "PRE_ECE", cc = "ALL", f = "G",
#' p = "CO")
#' EF <- c(l1,l2,l2[[12]],l2[[12]],l2[[12]],l2[[12]])
#' E_CO_PC_E25 <- emis(veh = PC_E25,lkm = net$lkm,
#'                ef = EF, speed = speed, agemax = 40, profile = pc_profile,
#'                hour = i, day = j, array = T)
#' emi_table <- emis_post(E_CO_PC_E25, "PC", "1400", "E25", "co", by = "veh")
#' emi_table_1 <- emi_table[emi_table$E_CO_PC_E25=="E_CO_PC_E25_1", ]
#' library(ggplot2)
#' ggplot(emi_table_1, aes(x=emi_table_1$hour, y = emi_table_1$co, colour =day)) +
#' geom_line() + geom_point() + labs(x="hours",y="co g/h",
#' title="Emissions of CO by day of the week") +theme_bw()
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
      df$pollutant <- Emissions(df$pollutant)
      return(df)
    } else if (by == "streets_wide") {
      x <- unlist(lapply(1:dim(arra)[4], function(j) {# dia
        unlist(lapply (1:dim(arra)[3],function(i) { # hora
          rowSums(arra[,,i,j], na.rm = T)
        }))
      }))
      m <- matrix(x, nrow=dim(arra)[1], ncol=dim(arra)[3]*dim(arra)[4])
      df <- Emissions(m)
      nombres <- lapply(1:dim(m)[2], function(i){paste0("h",i)})
      names(df) <- nombres
      return(df)
    }
}
