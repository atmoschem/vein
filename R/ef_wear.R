#' Emissions factors from tyre, break and road surface wear
#'
#' @description \code{\link{ef_wear}} estimates wear emissions.
#' The sources are tyres, breaks and road surface.
#'
#' @param wear Character; type of wear: "tyre" (or "tire"),
#' "break" (or "brake") and "road"
#' @param type Character; type of vehicle: "2W", "MC", "Motorcycle", "PC",
#'  "LCV", 'HDV", "BUS", "TRUCKS"
#' @param pol Character; pollutant: "TSP", "PM10", "PM2.5", "PM1" and "PM0.1"
#' @param speed Data.frame of speeds
#' @param load Load of the HDV
#' @param axle Number of axle of the HDV
#' @param road Type of road "urban", "rural", "motorway".
#' Only applies when type is "E6DV" or "BEV"
#' @param verbose Logical to show more information.
#' Only applies when type is "E6DV" or "BEV"
#' @return emission factors grams/km
#' @references Ntziachristos and Boulter 2016. Automobile tyre and break wear
#' and road abrasion. In: EEA, EMEP. EEA air pollutant emission inventory
#' guidebook-2009. European Environment Agency, Copenhagen, 2016
#'
#' When type is "E6DV" or "BEV":
#' Tivey J., Davies H., Levine J., Zietsman J., Bartington S.,
#' Ibarra-Espinosa S., Ropkins K. 2022. Meta Analysis as Early
#' Evidence on the Particulate Emissions Impact of EURO VI to
#' Battery Electric Bus Fleet Transitions. Paper under development.
#' @export
#' @examples {
#' data(net)
#' data(pc_profile)
#' pc_week <- temp_fact(net$ldv+net$hdv, pc_profile)
#' df <- netspeed(pc_week, net$ps, net$ffs, net$capacity, net$lkm, alpha = 1)
#' ef <- ef_wear(wear = "tyre", type = "PC", pol = "PM10", speed = df)
#'
#' ef_wear(wear = "tyre",
#'         type = c("E6DV"),
#'         pol = "PM10",
#'         verbose = TRUE)
#'
#' ef_wear(wear = "tyre",
#'         type = c("E6DV"),
#'         pol = "PM10",
#'         verbose = FALSE)
#'
#' }
ef_wear <- function (wear,
                     type,
                     pol = "TSP",
                     speed,
                     load = 0.5,
                     axle=2,
                     road = "urban",
                     verbose = FALSE) {

  if(length(type) > 1) stop("only 1 type")
  if(!type %in% c("E6DV", "BEV")) {

    if(is.vector(speed)){
      speed <- matrix(as.numeric(speed), ncol = 1)
    }
    for (i  in 1:ncol(speed) ) {
      speed[, i] <- as.numeric(speed[, i])
    }

    if (wear %in% c("tyre", "tire")) {
      ef <- ifelse(
        type %in% c("2W", "MC", "Motorcycle"), 0.0046,
        ifelse(
          type=="PC",0.0107,
          ifelse(
            type=="LCV", 0.0109,
            ifelse(
              type %in% c("HDV", "BUS", "TRUCKS"),
              (axle/2)*(1.41 + 1.38*load)*0.0107))))

      f <- ifelse(
        pol=="TSP", 1,
        ifelse(
          pol=="PM10", 0.6,
          ifelse(
            pol=="PM2.5", 0.42,
            ifelse(
              pol=="PM1",0.06,
              ifelse(
                pol=="PM0.1",0.048,"pol")))))

      sv <- do.call("cbind",lapply(1:ncol(speed), function(i){
        ifelse(
          speed[, i] > 40, 1.39,
          ifelse(
            speed[, i] >= 40 & speed <= 90,
            -0.00974*speed[, i] + 1.78, 0.902))
      }))


    } else if (wear %in% c("break", "brake")){
      ef <- ifelse(
        type%in% c("2W", "MC", "Motorcycle"), 0.0037,
        ifelse(
          type=="PC",0.0075,
          ifelse(
            type=="LCV", 0.0117,
            ifelse(
              type %in% c("HDV", "BUS", "TRUCKS"),
              3.13*(1 + 0.79*load)*0.0037))))

      f <- ifelse(
        pol=="TSP", 1,
        ifelse(
          pol=="PM10",0.98,
          ifelse(
            pol=="PM2.5", 0.39,
            ifelse(
              pol=="PM1",0.1,
              ifelse(
                pol=="PM0.1",0.08,"pol")))))

      sv <- do.call("cbind",lapply(1:ncol(speed), function(i){
        ifelse(
          speed[, i] > 40, 1.67,
          ifelse(
            speed[, i] >= 40 & speed <= 90,
            -0.0270*speed[, i] + 2.75, 0.185))
      }))

    } else if (wear == "road"){
      sv <- 1
      ef <- ifelse(
        type%in% c("2W", "MC", "Motorcycle"), 0.0060,
        ifelse(
          type=="PC",0.0150,
          ifelse(
            type=="LCV", 0.0150,
            ifelse(
              type %in% c("HDV", "BUS", "TRUCKS"),
              0.0760))))

      f <- ifelse(
        pol=="TSP", 1,
        ifelse(
          pol=="PM10",0.5,
          ifelse(
            pol=="PM2.5", 0.27,
            ifelse(
              pol=="PM1", 0.27,
              ifelse(
                pol=="PM0.1", 0.27,"pol")))))
    }
    efw <- EmissionFactors(ef*f*sv)
    return(efw)

  } else {


    data.frame(
      stringsAsFactors = FALSE,
      wear = c("brake","brake",
               "brake","brake","brake","brake","brake","brake",
               "brake","brake","brake","brake","tyre","tyre",
               "tyre","tyre","tyre","tyre","tyre","tyre","tyre",
               "tyre","tyre","tyre","road","road","road","road",
               "road","road","road","road","road","road",
               "road","road","res","res","res","res","res","res",
               "res","res","res","res","res","res"),
      veh = c("E6DV","E6DV","E6DV",
              "E6DV","E6DV","E6DV","BEV","BEV","BEV","BEV",
              "BEV","BEV","E6DV","E6DV","E6DV","E6DV","E6DV",
              "E6DV","BEV","BEV","BEV","BEV","BEV","BEV",
              "E6DV","E6DV","E6DV","E6DV","E6DV","E6DV","BEV",
              "BEV","BEV","BEV","BEV","BEV","E6DV","E6DV",
              "E6DV","E6DV","E6DV","E6DV","BEV","BEV","BEV",
              "BEV","BEV","BEV"),
      road = c("urban","rural",
               "motorway","urban","rural","motorway","urban","rural",
               "motorway","urban","rural","motorway","urban",
               "rural","motorway","urban","rural","motorway",
               "urban","rural","motorway","urban","rural",
               "motorway","urban","rural","motorway","urban","rural",
               "motorway","urban","rural","motorway","urban",
               "rural","motorway","urban","rural","motorway",
               "urban","rural","motorway","urban","rural",
               "motorway","urban","rural","motorway"),
      pol = c("PM2.5","PM2.5",
              "PM2.5","PM10","PM10","PM10","PM2.5","PM2.5","PM2.5",
              "PM10","PM10","PM10","PM2.5","PM2.5","PM2.5",
              "PM10","PM10","PM10","PM2.5","PM2.5","PM2.5",
              "PM10","PM10","PM10","PM2.5","PM2.5","PM2.5",
              "PM10","PM10","PM10","PM2.5","PM2.5","PM2.5","PM10",
              "PM10","PM10","PM2.5","PM2.5","PM2.5","PM10",
              "PM10","PM10","PM2.5","PM2.5","PM2.5","PM10",
              "PM10","PM10"),
      ef = c(18,11,3.4,47,28,
             8.4,19,12,3.7,50,31,9.1,19,15,13,27,21,18,
             20,16,13,29,22,19,18,18,18,32,32,32,19,
             19,19,35,35,35,25,25,25,100,100,100,27,27,
             27,110,110,110),
      efi = c(12,4.2,0,31,9.8,0,
              12,4.4,0,33,10,0,15,12,9.8,21,16,14,15,
              12,10,22,17,15,13,13,13,24,24,24,14,14,
              14,25,25,25,7.6,7.6,7.6,32,32,32,8.2,8.2,
              8.2,34,34,34),
      efs = c(27L,27L,17L,70L,69L,
              43L,29L,30L,20L,74L,76L,49L,27L,21L,18L,
              38L,30L,25L,29L,22L,19L,40L,31L,27L,24L,24L,
              24L,43L,43L,43L,26L,26L,26L,47L,47L,47L,
              150L,150L,150L,590L,590L,590L,170L,170L,170L,
              690L,690L,690L)
    ) -> dx

    #wear
    uwear <- unique(dx$wear)
    if(any(!wear %in% uwear)) {
      stop("Select wear from:\n", paste(uwear, collapse = "\n"))
    }
    dx <- dx[dx$wear == wear, ]

    #veh
    uveh <- unique(dx$veh)
    if(any(!type %in% uveh)) {
      stop("Select type from:\n", paste(uveh, collapse = "\n"))
    }
    dx <- dx[dx$veh == type, ]

    #road
    uroad <- unique(dx$road)
    if(any(!road %in% uroad)) {
      stop("Select road from:\n", paste(uroad, collapse = "\n"))
    }
    dx <- dx[dx$road == road, ]

    #pol
    upol <- unique(dx$pol)
    if(any(!pol %in% upol)) {
      stop("Select pol from:\n", paste(upol, collapse = "\n"))
    }
    dx <- dx[dx$pol %in% pol, ]
    if(verbose) print(dx)
    return(EmissionFactors(dx$ef))
  }
}
