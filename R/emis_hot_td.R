#' Estimation of hot exhaust emissions with top-down approach
#'
#' @description \code{\link{emis_hot_td}} estimates cld start emissions with
#' a top-down appraoch. This is, annual or monthly emissions or region.
#' Especifically, the emissions are esitmated for row of the simple feature (row
#' of the spatial feature).
#'
#' In general was designed so that each simple feature is a region with
#' different average monthly temperature.
#' This funcion, as other in this package, adapts to the class of the input data.
#' providing flexibility to the user.
#'
#' @param veh "Vehicles" data-frame or spatial feature, where columns are the
#'  age distribution of that vehicle. and rows each simple feature or region.
#' @param lkm Numeric; mileage by the age of use of each vehicle.
#' @param ef Numeric or data.frame; emission factors. When it is a data.frame
#' number of rows can be for each region, or also, each region repeated
#' along 12 months. For instance, if you have 10 regions the number
#' of rows of ef can also be 120 (10 * 120).
#' when you have emission factors that varies with month, see \code{\link{ef_china}}.
#' @param pro_month Numeric or data.frame; montly profile to distribuite annual mileage
#' in each month. When it is a data.frame, each region (row) can have a different
#' monthly profile.
#' @param params List of parameters; Add columns with information to returning data.frame
#' @param verbose Logical; To show more information
#' @param fortran Logical; to try the fortran calculation.
#' @param nt Integer; Number of threads wich must be lower than max available. See \code{\link{check_nt}}.
#' Only when fortran = TRUE
#' @return Emissions data.frame
#' @seealso \code{\link{ef_ldv_speed}} \code{\link{ef_china}}
#' @export
#' @details List  to make easier to use this function.
#' \enumerate{
#'  \item{`pro_month` is data.frame AND rows of `ef` and `veh` are equal.}
#'  \item{`pro_month` is numeric AND rows of `ef` and `veh` are equal.}
#'  \item{`pro_month` is data.frame AND rows of `ef` is 12X rows of `veh`.}
#'  \item{`pro_month` is numeric AND rows of `ef` is 12X rows of `veh`.}
#'  \item{`pro_month` is data,frame AND class of `ef` is 'units'.}
#'  \item{`pro_month` is numeric AND class of `ef` is 'units'.}
#'  \item{NO `pro_month` AND class of `ef` is 'units'.}
#'  \item{NO `pro_month` AND `ef` is data.frame.}
#'  \item{`pro_month` is numeric AND rows of `ef` is 12 (monthly `ef`).}
#' }
#' @examples \dontrun{
#' # Do not run
#' euros <- c("V", "V", "IV", "III", "II", "I", "PRE", "PRE")
#' efh <- ef_ldv_speed(v = "PC", t = "4S", cc = "<=1400", f = "G",
#'           eu = euros, p = "CO", speed = Speed(34))
#' lkm <- units::as_units(c(20:13), "km")*1000
#' veh <- age_ldv(1:10, agemax = 8)
#' system.time(
#' a <- emis_hot_td(veh = veh,
#'                 lkm = lkm,
#'                 ef = EmissionFactors(as.numeric(efh[, 1:8])),
#'                 verbose = TRUE))
#' system.time(
#' a2 <- emis_hot_td(veh = veh,
#'                   lkm = lkm,
#'                   ef = EmissionFactors(as.numeric(efh[, 1:8])),
#'                   verbose = TRUE,
#'                   fortran = TRUE)) #emistd7f.f95
#' identical(a, a2)
#'
#' # adding columns
#' emis_hot_td(veh = veh,
#'             lkm = lkm,
#'             ef = EmissionFactors(as.numeric(efh[, 1:8])),
#'             verbose = TRUE,
#'             params = list(paste0("data_", 1:10), "moredata"))
#'
#' # monthly profile (numeric) with numeric ef
#' veh_month <- c(rep(8, 1), rep(10, 5), 9, rep(10, 5))
#' system.time(
#' aa <- emis_hot_td(veh = veh,
#'                   lkm = lkm,
#'                   ef = EmissionFactors(as.numeric(efh[, 1:8])),
#'                   pro_month = veh_month,
#'                   verbose = TRUE))
#' system.time(
#' aa2 <- emis_hot_td(veh = veh,
#'                   lkm = lkm,
#'                   ef = EmissionFactors(as.numeric(efh[, 1:8])),
#'                   pro_month = veh_month,
#'                   verbose = TRUE,
#'                   fortran = TRUE)) #emistd5f.f95
#' aa$emissions <- round(aa$emissions, 8)
#' aa2$emissions <- round(aa2$emissions, 8)
#' identical(aa, aa2)
#'
#' # monthly profile (numeric) with data.frame ef
#' veh_month <- c(rep(8, 1), rep(10, 5), 9, rep(10, 5))
#' def <- matrix(EmissionFactors(as.numeric(efh[, 1:8])),
#' nrow = nrow(veh), ncol = ncol(veh), byrow = TRUE)
#' def <- EmissionFactors(def)
#' system.time(
#' aa <- emis_hot_td(veh = veh,
#'                   lkm = lkm,
#'                   ef = def,
#'                   pro_month = veh_month,
#'                   verbose = TRUE))
#' system.time(
#' aa2 <- emis_hot_td(veh = veh,
#'                   lkm = lkm,
#'                   ef = def,
#'                   pro_month = veh_month,
#'                   verbose = TRUE,
#'                   fortran = TRUE)) #emistd1f.f95
#' aa$emissions <- round(aa$emissions, 8)
#' aa2$emissions <- round(aa2$emissions, 8)
#' identical(aa, aa2)
#'
#' # monthly profile (data.frame)
#' dfm <- matrix(c(rep(8, 1), rep(10, 5), 9, rep(10, 5)), nrow = 10, ncol = 12,
#' byrow = TRUE)
#' system.time(
#' aa <- emis_hot_td(veh = veh,
#'                   lkm = lkm,
#'                   ef = EmissionFactors(as.numeric(efh[, 1:8])),
#'                   pro_month = dfm,
#'                   verbose = TRUE))
#' system.time(
#' aa2 <- emis_hot_td(veh = veh,
#'                   lkm = lkm,
#'                   ef = EmissionFactors(as.numeric(efh[, 1:8])),
#'                   pro_month = dfm,
#'                   verbose = TRUE,
#'                   fortran = TRUE)) # emistd6f.f95
#' aa$emissions <- round(aa$emissions, 2)
#' aa2$emissions <- round(aa2$emissions, 2)
#' identical(aa, aa2)
#'
#' # Suppose that we have a EmissionsFactor data.frame with number of rows for each month
#' # number of rows are 10 regions
#' # number of columns are 12 months
#' tem <- runif(n = 6*10, min = -10, max = 35)
#' temp <- c(rev(tem[order(tem)]), tem[order(tem)])
#' plot(temp)
#' dftemp <- celsius(matrix(temp, ncol = 12))
#' dfef <- ef_evap(ef = c(rep("eshotfi", 8)),
#'               v = "PC",
#'               cc = "<=1400",
#'               dt = dftemp,
#'               show = F,
#'               ca = "small",
#'               ltrip = units::set_units(10, km),
#'               pollutant = "NMHC")
#' dim(dfef) # 120 rows and 9 columns, 8 ef (g/km) and 1 for month
#' system.time(
#' aa <- emis_hot_td(veh = veh,
#'                   lkm = lkm,
#'                   ef = dfef,
#'                   pro_month = veh_month,
#'                   verbose = TRUE))
#' system.time(
#' aa2 <- emis_hot_td(veh = veh,
#'                    lkm = lkm,
#'                    ef = dfef,
#'                    pro_month = veh_month,
#'                    verbose = TRUE,
#'                    fortran = TRUE)) #emistd3f.f95
#' aa$emissions <- round(aa$emissions, 2)
#' aa2$emissions <- round(aa2$emissions, 2)
#' identical(aa, aa2)
#' plot(aggregate(aa$emissions, by = list(aa$month), sum)$x)
#'
#' # Suppose that we have a EmissionsFactor data.frame with number of rows for each month
#' # monthly profile (data.frame)
#' system.time(
#' aa <- emis_hot_td(veh = veh,
#'                   lkm = lkm,
#'                   ef = dfef,
#'                   pro_month = dfm,
#'                   verbose = TRUE))
#' system.time(
#' aa2 <- emis_hot_td(veh = veh,
#'                    lkm = lkm,
#'                    ef = dfef,
#'                    pro_month = dfm,
#'                    verbose = TRUE,
#'                    fortran = TRUE)) #emistd4f.f95
#' aa$emissions <- round(aa$emissions, 8)
#' aa2$emissions <- round(aa2$emissions, 8)
#' identical(aa, aa2)
#' plot(aggregate(aa$emissions, by = list(aa$month), sum)$x)
#' }
emis_hot_td <- function (veh,
                         lkm,
                         ef,
                         pro_month,
                         params,
                         verbose = FALSE,
                         fortran = FALSE,
                         nt = ifelse(check_nt()==1, 1, check_nt()/2)) {
  # Checking sf
  if(any(class(veh) %in% "sf")){
    if(verbose) message("converting sf to data.frame")
    veh <- sf::st_set_geometry(veh, NULL)
  }
  # Checking veh
  for(i in 1:ncol(veh)){
    veh[, i] <- as.numeric(veh[, i])
  }

  # Check units
  if(class(lkm) != "units"){
    stop("lkm neeeds to has class 'units' in 'km'. Please, check package '?units::set_units'")
  }
  if(units(lkm)$numerator == "m" ){
    stop("Units of lkm is 'm' ")
  }
  if(units(lkm)$numerator == "km" ) {
    lkm <- as.numeric(lkm)
  }
  if(length(lkm) != ncol(veh)) stop("Length of 'lkm' must be the as the number of columns of 'veh'")

  # Checking ef
  if(is.matrix(ef) | is.data.frame(ef)){
    ef <- as.data.frame(ef)
    if(class(ef[, 1]) != "units"){
      stop("columns of ef must has class 'units' in 'g/km'. Please, check ?EmissionFactors")
    }
    if(units(ef[, 1])$numerator == "g" | units(ef[, 1])$denominator == "km"){
      for(i in 1:ncol(veh)){
        ef[, i] <- as.numeric(ef[, i])
      }
    }
    # When row = 1, transform to vector.
    if(nrow(ef) == 1){
      if(verbose) message("Transforming 1 row data.frame into numeric")
      ef <- as.numeric(ef)
    }

  } else {
    if(class(ef) != "units"){
      stop("ef must has class 'units' in 'g/km'. Please, check ?EmissionFactors")
    }
    if(units(ef)$numerator == "g" | units(ef)$denominator == "km"){
      ef <- as.numeric(ef)
    }

  }

  # pro_month
  if(!missing(pro_month)){
    if(is.data.frame(pro_month) | is.matrix(pro_month)){
      pro_month <- as.data.frame(pro_month)
      for(i in 1:nrow(pro_month)){
        pro_month[i, ] <- pro_month[i, ]/sum(pro_month[i, ])
      }
    } else if (is.numeric(pro_month)){
      pro_month <- pro_month/sum(pro_month)
    }
  }

  # Checking pro_month
  if(!missing(pro_month)){

    if(verbose) message("Estimation with monthly profile")

    if(length(pro_month) != 12) stop("Length of pro_month must be 12")

    mes <- ifelse(nchar(1:12)<2, paste0(0, 1:12), 1:12)

    if(is.data.frame(ef)){

      if(verbose) message("Assuming you have emission factors for each simple feature and then for each month")

      #when pro_month varies in each simple feature
      if(is.data.frame(pro_month) & nrow(ef) == nrow(veh)){

        if(verbose) message("'pro_month' is data.frame and number of rows of 'ef' and 'veh' are equal")


        if(nrow(pro_month) == 1) {
          message("Replicating one-row matrix to match number of rows of `veh`")
          pro_month <- matrix(as.numeric(pro_month), nrow = nrow(veh), ncol = ncol(pro_month))
        }

        if(nrow(ef) != nrow(veh)) stop("Number of rows of 'veh' and 'ef' must be equal")
        if(ncol(ef) != ncol(veh)) stop("Number of cols of `ef` and `veh` must be equal")
        if(length(lkm) != ncol(veh)) stop("Length of `lkm` must be equal to number of columns of `veh`")
        if(nrow(pro_month) != nrow(veh)) stop("Number of rows of `month` and `veh` must be equal")

        if(fortran){
          nrowv <- as.integer(nrow(veh))
          ncolv <- as.integer(ncol(veh))
          pmonth <- as.integer(ncol(pro_month))
          lkm <- as.numeric(lkm)
          ef <- as.matrix(ef[, 1:ncol(veh)])
          month <- as.matrix(pro_month)
          # emis(i, j, k) = veh(i, j) * lkm(j) * ef(i, j)*month(i, k)

          if(!missing(nt)) {
            if(nt >= check_nt()) stop("Your machine has ", check_nt(),
                                      " threads and nt must be lower")
            if(verbose) message("Calling emistd2fpar.f95")
            a <-   .Fortran("emistd2fpar",
                            nrowv = nrowv,
                            ncolv = ncolv,
                            pmonth = pmonth,
                            veh = as.matrix(veh),
                            lkm = lkm,
                            ef = ef,
                            month = month,
                            emis = numeric(nrowv*ncolv*pmonth),
                            nt = as.integer(nt))$emis

          } else {
            if(verbose) message("Calling emistd2f.f95")
            a <-   .Fortran("emistd2f",
                            nrowv = nrowv,
                            ncolv = ncolv,
                            pmonth = pmonth,
                            veh = as.matrix(veh),
                            lkm = lkm,
                            ef = ef,
                            month = month,
                            emis = numeric(nrowv*ncolv*pmonth))$emis

          }

          e <- data.frame(emissions = a)
          e <- Emissions(e)
          e$rows <- rep(row.names(veh), each = ncolv*pmonth) # i
          e$age <- rep(seq(1, ncolv), nrowv*pmonth)          # j
          e$month <- rep(seq(1, pmonth), ncolv*nrowv)        # k

        } else {
          e <- do.call("rbind",lapply(1:12, function(k){
            dfi <- unlist(lapply(1:ncol(veh), function(j){

              lkm[j] * veh[, j] * pro_month[, k] *ef[, j]
            }))
            dfi <- as.data.frame(dfi)
            names(dfi) <- "emissions"
            dfi <- Emissions(dfi)
            dfi$rows <- row.names(veh)
            dfi$age <- rep(1:ncol(veh), each = nrow(veh))
            dfi$month <- (1:length(pro_month))[k]
            dfi
          }))
        }
        # is.numeric(pro_month) & nrow(ef) == nrow(veh) ####
      } else if(is.numeric(pro_month) & nrow(ef) == nrow(veh)){

        if(verbose) message("'pro_month' is numeric and number of rows of 'ef' and 'veh' are equal")

        if(nrow(ef) != nrow(veh)) stop("Number of rows of `ef` and `veh` must be equal")
        if(ncol(ef) != ncol(veh)) stop("Number of cols of `ef` and `veh` must be equal")
        if(length(lkm) != ncol(veh)) stop("Length of `lkm` must be equal to number of columns of `veh`")

        if(fortran) {
          nrowv <- as.integer(nrow(veh))
          ncolv <- as.integer(ncol(veh))
          pmonth <- as.integer(length(pro_month))
          lkm <- as.numeric(lkm)
          ef <- as.matrix(ef[, 1:ncol(veh)])
          month <- as.numeric(pro_month)

          # emis(i, j, k) = veh(i, j) * lkm(j) * ef(i, j)*month(k)

          if(!missing(nt)) {
            if(nt >= check_nt()) stop("Your machine has ", check_nt(),
                                      " threads and nt must be lower")
            if(verbose) message("Calling emistd1fpar.f95")
            a <-   .Fortran("emistd1fpar",
                            nrowv = nrowv,
                            ncolv = ncolv,
                            pmonth = pmonth,
                            veh = as.matrix(veh),
                            lkm = lkm,
                            ef = ef,
                            month = month,
                            emis = numeric(nrowv*ncolv*pmonth),
                            nt = as.integer(nt))$emis

          } else {
            if(verbose) message("Calling emistd1f.f95")
            a <-   .Fortran("emistd1f",
                            nrowv = nrowv,
                            ncolv = ncolv,
                            pmonth = pmonth,
                            veh = as.matrix(veh),
                            lkm = lkm,
                            ef = ef,
                            month = month,
                            emis = numeric(nrowv*ncolv*pmonth))$emis

          }


          e <- data.frame(emissions = a)
          e <- Emissions(e)
          e$rows <- rep(row.names(veh), ncolv*pmonth)            # i
          e$age <- rep(rep(seq(1, ncolv), each = nrowv), pmonth) # j
          e$month <- rep(seq(1, pmonth), each = ncolv*nrowv)            # k

        } else {
          e <- do.call("rbind",lapply(1:12, function(k){
            dfi <- unlist(lapply(1:ncol(veh), function(j){
              lkm[j] * veh[, j] * pro_month[k] *ef[, j]
            }))
            dfi <- as.data.frame(dfi)
            names(dfi) <- "emissions"
            dfi <- Emissions(dfi)
            dfi$rows <- row.names(veh)
            dfi$age <- rep(1:ncol(veh), each = nrow(veh))
            dfi$month <- (1:length(pro_month))[k]
            dfi
          }))
        }

        # is.numeric(pro_month) & nrow(ef) == 12 ####
      } else if(is.numeric(pro_month) & nrow(ef) == 12){

        if(verbose) message("'pro_month' is numeric and you have 12 montly ef")

        # if(fortran) {
        #   nrowv <- as.integer(nrow(veh))
        #   ncolv <- as.integer(ncol(veh))
        #   pmonth <- as.integer(length(pro_month))
        #   lkm <- as.numeric(lkm)
        #   ef <- as.matrix(ef[, 1:ncol(veh)])
        #   month <- as.numeric(pro_month)
        #
        #   if(verbose) message("Calling emistd1f.f95")
        #
        #   a <-   .Fortran("emistd1f",
        #                   nrowv = nrowv,
        #                   ncolv = ncolv,
        #                   pmonth = pmonth,
        #                   veh = as.matrix(veh),
        #                   lkm = lkm,
        #                   ef = ef,
        #                   month = month,
        #                   emis = numeric(nrowv*ncolv*pmonth))$emis
        #
        #   e <- data.frame(emissions = a)
        #   e <- Emissions(e)
        #   e$rows <- rep(row.names(veh), ncolv*pmonth)            # i
        #   e$age <- rep(rep(seq(1, ncolv), each = nrowv), pmonth) # j
        #   e$month <- rep(seq(1, pmonth), each = ncolv*nrowv)            # k

        # } else {
        e <- do.call("rbind",lapply(1:12, function(k){
          dfi <- unlist(lapply(1:ncol(veh), function(j){
            lkm[j] * veh[, j] * pro_month[k] *ef[k, j]
          }))
          dfi <- as.data.frame(dfi)
          names(dfi) <- "emissions"
          dfi <- Emissions(dfi)
          dfi$rows <- row.names(veh)
          dfi$age <- rep(1:ncol(veh), each = nrow(veh))
          dfi$month <- (1:length(pro_month))[k]
          dfi
        }))
        # }


        # is.data.frame(pro_month) & nrow(ef) == 12*nrow(veh) ####
      } else if(is.data.frame(pro_month) & nrow(ef) == 12*nrow(veh)){

        if(verbose) message("'pro_month' is data.frame and number of rows of 'ef' is 12*number of rows 'veh'")

        if(nrow(pro_month) != nrow(veh)) stop("Number of rows of 'pmonth' and 'veh' must be equal")
        if(length(ef2) != length(unlist(veh))*pmonth) stop("length of `ef` and `veh`*`months` must be equal be equal")
        if(length(lkm) != ncol(veh)) stop("length of `lkm` must be equal to number of columns of `veh`")


        if(fortran){
          nrowv <- as.integer(nrow(veh))
          ncolv <- as.integer(ncol(veh))
          pmonth <- as.integer(ncol(pro_month))
          lkm <- as.numeric(lkm)
          ef$month <- rep(1:12, each = nrow(veh))
          ef2 <- split(ef[, 1:ncol(veh)], ef$month)
          ef2 <- as.numeric(unlist(lapply(ef2, unlist)))
          month <- as.matrix(pro_month)

          # emis(i, j, k) = veh(i, j) * lkm(j) * ef(i, j, k) * month(i, k)

          if(!missing(nt)) {
            if(nt >= check_nt()) stop("Your machine has ", check_nt(),
                                      " threads and nt must be lower")

            if(verbose) message("Calling emistd4fpar.f95")
            a <-   .Fortran("emistd4fpar",
                            nrowv = nrowv,
                            ncolv = ncolv,
                            pmonth = pmonth,
                            veh = as.matrix(veh),
                            lkm = lkm,
                            ef = ef2,
                            month = month,
                            emis = numeric(nrowv*ncolv*pmonth),
                            nt = as.integer(nt))$emis
          } else {

            if(verbose) message("Calling emistd4f.f95")

            a <-   .Fortran("emistd4f",
                            nrowv = nrowv,
                            ncolv = ncolv,
                            pmonth = pmonth,
                            veh = as.matrix(veh),
                            lkm = lkm,
                            ef = ef2,
                            month = month,
                            emis = numeric(nrowv*ncolv*pmonth))$emis
          }


          e <- data.frame(emissions = a)
          e <- Emissions(e)
          e$rows <- rep(row.names(veh), ncolv*pmonth)              # i
          e$age <- rep(rep(seq(1, ncolv), each = nrowv), pmonth) # j
          e$month <- rep(seq(1, pmonth), each = ncolv*nrowv)      # k

        } else {
          ef$month <- rep(1:12, each = nrow(veh))
          ef <- split(ef, ef$month)

          e <- do.call("rbind",lapply(1:12, function(k){
            dfi <- unlist(lapply(1:ncol(veh), function(j){
              lkm[j] * veh[, j] * pro_month[, k] *ef[[k]][, j]
            }))
            dfi <- as.data.frame(dfi)
            names(dfi) <- "emissions"
            dfi <- Emissions(dfi)
            dfi$rows <- row.names(veh)
            dfi$age <- rep(1:ncol(veh), each = nrow(veh))
            dfi$month <- (1:length(pro_month))[k]
            dfi
          }))

        }
        # is.numeric(pro_month) & nrow(ef) == 12*nrow(veh) ####
      } else if(is.numeric(pro_month) & nrow(ef) == 12*nrow(veh)){

        if(verbose) message("'pro_month' is numeric and number of rows of 'ef' is 12*number of rows 'veh'")

        if(fortran){

          nrowv <- as.integer(nrow(veh))
          ncolv <- as.integer(ncol(veh))
          pmonth <- as.integer(length(pro_month))
          ef$month <- rep(1:12, each = nrow(veh))
          ef2 <- split(ef[, 1:ncol(veh)], ef$month)
          ef2 <- as.numeric(unlist(lapply(ef2, unlist)))
          lkm <- as.numeric(lkm)
          month <- as.numeric(pro_month)


          if(length(ef2) != length(unlist(veh))*pmonth) stop("length of `ef` and `veh`*`months` must be equal be equal")
          if(length(lkm) != ncol(veh)) stop("length of `lkm` must be equal to number of columns of `veh`")
          # emis(i, j, k) = veh(i, j) * lkm(j) * ef(i, j, k) * month(k)

          if(!missing(nt)) {
            if(nt >= check_nt()) stop("Your machine has ", check_nt(),
                                      " threads and nt must be lower")


            if(verbose) message("Calling emistd3fpar.f95")

            a <-   .Fortran("emistd3fpar",
                            nrowv = nrowv,
                            ncolv = ncolv,
                            pmonth = pmonth,
                            veh = as.matrix(veh),
                            lkm = lkm,
                            ef = ef2,
                            month = month,
                            emis = numeric(nrowv*ncolv*pmonth),
                            nt = as.integer(nt))$emis
          } else {
            if(verbose) message("Calling emistd3f.f95")

            a <-   .Fortran("emistd3f",
                            nrowv = nrowv,
                            ncolv = ncolv,
                            pmonth = pmonth,
                            veh = as.matrix(veh),
                            lkm = lkm,
                            ef = ef2,
                            month = month,
                            emis = numeric(nrowv*ncolv*pmonth))$emis

          }
          e <- data.frame(emissions = a)
          e <- Emissions(e)
          e$rows <- rep(row.names(veh),  ncolv*pmonth)           # i
          e$age <- rep(rep(seq(1, ncolv), each = nrowv), pmonth) # j
          e$month <- rep(seq(1, pmonth), each = ncolv*nrowv)     # k

        } else {
          ef$month <- rep(1:12, each = nrow(veh))
          ef <- split(ef, ef$month)

          e <- do.call("rbind",lapply(1:12, function(k){
            dfi <- unlist(lapply(1:ncol(veh), function(j){
              lkm[j] * veh[, j] * pro_month[k] *ef[[k]][, j]
            }))
            dfi <- as.data.frame(dfi)
            names(dfi) <- "emissions"
            dfi <- Emissions(dfi)
            dfi$rows <- row.names(veh)
            dfi$age <- rep(1:ncol(veh), each = nrow(veh))
            dfi$month <- (1:length(pro_month))[k]
            dfi
          }))
        }
      } else {
        stop("Condition is not met. Review your input data dn read the documentation, please")
      }
      # else if(nrow(ef) !=  nrow(veh) & nrow(ef) != 12)(
      # stop("The number of rows can be equal to number of rows of veh, or number of rows of veh times 12")
      # )

      if(!missing(params)){
        if(!is.list(params)) stop("'params' must be a list")
        if(is.null(names(params))) {
          if(verbose) message("Adding names to params")
          names(params) <- paste0("P_", 1:length(params))
        }
        for (i in 1:length(params)){
          e[, names(params)[i]] <- params[[i]]
        }
      }

      if(verbose) cat("Sum of emissions:", sum(e$emissions), "\n")

    } else if(!is.data.frame(ef)){

      if(verbose) message("Assuming you have the same emission factors in each simple feature")

      # when pro_month vary each row
      # is.data.frame(pro_month) | is.matrix(pro_month) ####
      if(is.data.frame(pro_month) | is.matrix(pro_month)){

        if(verbose) message("'pro_month' is data.frame and 'ef' is numeric")


        if(nrow(pro_month) == 1) {
          message("Replicating one-row matrix to match number of rows of `veh`")
          pro_month <- matrix(as.numeric(pro_month), nrow = nrow(veh), ncol = ncol(pro_month))
        }

        if(length(ef) != ncol(veh)) stop("Length of `ef` and number of cols of `veh` must be equal")
        if(length(lkm) != ncol(veh)) stop("Length of `lkm` must be equal to number of columns of `veh`")
        if(nrow(pro_month) != nrow(veh)) stop("Number of rows of `month` and `veh` must be equal")

        if(fortran){
          nrowv <- as.integer(nrow(veh))
          ncolv <- as.integer(ncol(veh))
          pmonth <- as.integer(ncol(pro_month))
          lkm <- as.numeric(lkm)
          ef <- as.numeric(ef)
          month <- as.matrix(pro_month)

          # emis(i, j, k) = veh(i,j) * lkm(j) * ef(j) * month(i, k)

          if(!missing(nt)) {
            if(nt >= check_nt()) stop("Your machine has ", check_nt(),
                                      " threads and nt must be lower")

            if(verbose) message("Calling emistd6fpar.f95")

            a <-   .Fortran("emistd6fpar",
                            nrowv = nrowv,
                            ncolv = ncolv,
                            pmonth = pmonth,
                            veh = as.matrix(veh),
                            lkm = lkm,
                            ef = ef,
                            month = month,
                            emis = numeric(nrowv*ncolv*pmonth),
                            nt = as.integer(nt))$emis

          } else {
            if(verbose) message("Calling emistd6f.f95")

            a <-   .Fortran("emistd6f",
                            nrowv = nrowv,
                            ncolv = ncolv,
                            pmonth = pmonth,
                            veh = as.matrix(veh),
                            lkm = lkm,
                            ef = ef,
                            month = month,
                            emis = numeric(nrowv*ncolv*pmonth))$emis

          }

          e <- data.frame(emissions = a)
          e <- Emissions(e)
          e$rows <- rep(row.names(veh), ncolv*pmonth)   # i
          e$age <- rep(rep(seq(1, ncolv), each = nrowv), pmonth)      # j
          e$month <- rep(seq(1, pmonth), each = ncolv*nrowv)          # k

        } else {

          e <- do.call("rbind",lapply(1:12, function(k){
            dfi <- unlist(lapply(1:ncol(veh), function(j){
              lkm[j] * veh[, j] * pro_month[, k] *ef[j]
            }))
            dfi <- as.data.frame(dfi)
            names(dfi) <- "emissions"
            dfi <- Emissions(dfi)
            dfi$rows <- row.names(veh)
            dfi$age <- rep(1:ncol(veh), each = nrow(veh))
            dfi$month <- (1:length(pro_month))[k]
            dfi
          }))

        }
        # is.numeric(pro_month) ####
      } else if(is.numeric(pro_month)){

        if(verbose) message("'pro_month' is numeric and 'ef' is numeric")

        if(length(ef) != ncol(veh)) stop("Number of columns of 'veh' and length of 'ef' must be equal")

        if(fortran){
          nrowv <- as.integer(nrow(veh))
          ncolv <- as.integer(ncol(veh))
          pmonth <- as.integer(length(pro_month))
          lkm <- as.numeric(lkm)
          month <- as.numeric(pro_month)
          ef <- as.numeric(ef)


          if(length(ef) != ncol(veh)) stop("length of `ef` and number of cols of `veh` must be equal")
          if(length(lkm) != ncol(veh)) stop("length of `lkm` must be equal to number of columns of `veh`")
          # emis(i, j, k) = veh(i, j) * lkm(j) * ef(j) * month(k)

          if(!missing(nt)) {
            if(nt >= check_nt()) stop("Your machine has ", check_nt(),
                                      " threads and nt must be lower")

            if(verbose) message("Calling emistd5fpar.f95")

            a <-   .Fortran("emistd5fpar",
                            nrowv = nrowv,
                            ncolv = ncolv,
                            pmonth = pmonth,
                            veh = as.matrix(veh),
                            lkm = lkm,
                            ef = ef,
                            month = month,
                            emis = numeric(nrowv*ncolv*pmonth),
                            nt = as.integer(nt))$emis

          } else {
            if(verbose) message("Calling emistd5f.f95")

            a <-   .Fortran("emistd5f",
                            nrowv = nrowv,
                            ncolv = ncolv,
                            pmonth = pmonth,
                            veh = as.matrix(veh),
                            lkm = lkm,
                            ef = ef,
                            month = month,
                            emis = numeric(nrowv*ncolv*pmonth))$emis

          }

          e <- data.frame(emissions = a)
          e <- Emissions(e)
          e$rows <- rep(row.names(veh), ncolv*pmonth)
          e$age <- rep(seq(1, ncolv), each = nrowv)
          e$month <- rep(seq(1, pmonth), each = ncolv*nrowv)


        } else {
          e <- do.call("rbind",lapply(1:12, function(k){
            dfi <- unlist(lapply(1:ncol(veh), function(j){
              lkm[j] * veh[, j] * pro_month[k] *ef[j]
            }))
            dfi <- as.data.frame(dfi)
            names(dfi) <- "emissions"
            dfi <- Emissions(dfi)
            dfi$rows <- row.names(veh)
            dfi$age <- rep(1:ncol(veh), each = nrow(veh))
            dfi$month <- (1:length(pro_month))[k]
            dfi
          }))

        }

      }
      if(!missing(params)){
        if(!is.list(params)) stop("'params' must be a list")
        if(is.null(names(params))) {
          if(verbose) message("Adding names to params")
          names(params) <- paste0("P_", 1:length(params))
        }
        for (i in 1:length(params)){
          e[, names(params)[i]] <- params[[i]]
        }
      }

      if(verbose) cat("Sum of emissions:", sum(e$emissions), "\n")
    }


  } else {
    if(verbose) message("Estimation without monthly profile")

    # !is.data.frame(ef) ####
    if(!is.data.frame(ef)) {

      if(verbose) message("'ef' is a numeric vector with units")

      if(fortran){
        lkm <- as.numeric(lkm)
        ef <- as.numeric(ef)
        nrowv = as.integer(nrow(veh))
        ncolv = as.integer(ncol(veh))

        if(length(ef) != ncol(veh)) stop("length of `ef` and number of cols of `veh` must be equal")
        if(length(lkm) != ncol(veh)) stop("length of `lkm` must be equal to number of columns of `veh`")
        # emis(i, j) = veh(i,j) * lkm(j) * ef(j)


        if(!missing(nt)) {
          if(nt >= check_nt()) stop("Your machine has ", check_nt(),
                                    " threads and nt must be lower")

          if(verbose) message("Calling emistd7fpar.f95")

          a <-   .Fortran("emistd7fpar",
                          nrowv = nrowv,
                          ncolv = ncolv,
                          veh = as.matrix(veh),
                          lkm = lkm,
                          ef = ef,
                          emis = numeric(nrowv*ncolv),
                          nt = as.integer(nt))$emis

        } else {
          if(verbose) message("Calling emistd7f.f95")

          a <-   .Fortran("emistd7f",
                          nrowv = nrowv,
                          ncolv = ncolv,
                          veh = as.matrix(veh),
                          lkm = lkm,
                          ef = ef,
                          emis = numeric(nrowv*ncolv))$emis

        }
        # fortran
        # do concurrent(i= 1:nrowv, j = 1:ncolv)
        # emis(i, j) = veh(i,j) * lkm(i) * ef(j)
        # end do
        e <- as.data.frame(a)
        names(e) <- "emissions"
        e <- Emissions(e)
        e$rows <- row.names(e)
        e$age <- rep(1:ncol(veh), each = nrow(veh))

      } else {
        e <-  unlist(lapply(1:ncol(veh), function(j){
          lkm[j] * veh[, j] * ef[j]
        }))
        e <- as.data.frame(e)
        names(e) <- "emissions"
        e <- Emissions(e)
        e$rows <- row.names(e)
        e$age <- rep(1:ncol(veh), each = nrow(veh))

      }
      # ef is data.frame
    } else {
      if(verbose) message("'ef' is data.frame")
      if(nrow(ef) != nrow(veh)) stop("Number of rows of 'ef' and 'veh' must be equal")
      e <-  unlist(lapply(1:ncol(veh), function(j){
        lkm[j] * veh[, j] *ef[, j]
      }))
      e <- as.data.frame(e)
      names(e) <- "emissions"
      e <- Emissions(e)
      e$rows <- row.names(e)
      e$age <- rep(1:ncol(veh), each = nrow(veh))

    }

    if(!missing(params)){
      if(!is.list(params)) stop("'params' must be a list")
      if(is.null(names(params))) {
        if(verbose) message("Adding names to params")
        names(params) <- paste0("P_", 1:length(params))
      }
      for (i in 1:length(params)){
        e[, names(params)[i]] <- params[[i]]
      }
    }

    if(verbose) cat("Sum of emissions:", sum(e$emissions), "\n")

  }


  return(e)
}

