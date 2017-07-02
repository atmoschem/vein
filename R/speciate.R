#' Speciation of emissions
#'
#' @description The separation of emissions in different compunds. It includes
#' black carbon and organic matter from particulate matter. Soon it will be
#' added more speciations
#'
#' @param x Emissions estimation
#' @param spec type of speciation, e.g.: "bcom" stands for black carbon and
#' organic matter. The speciations are: "bcom", tyre", "break", "road","iag"
#' and "nox".
#' @param veh Type of vehicle. When spec is "bcom" or "nox" veh can be "PC",
#' "LCV", HDV" or "Motorcycle". When spec is "iag" veh is only "veh".
#' Not required for "tyre", "break" or "road"
#' @param fuel Fuel. When spec is "bcom" fuel can be "G" or "D".
#' When spec is "iag" fuel can be "G", "E" or "D". When spec is "nox" fuel can
#' be "G", "D", "LPG", "E85" or "CNG". Not required for "tyre", "break" or "road"
#' @param eu Euro emission standard: "PRE", "ECE_1501", "ECE_1502", "ECE_1503",
#'  "I", "II", "III", "IV",  "V", "III-CDFP","IV-CDFP","V-CDFP", "III-ADFP",
#'  "IV-ADFP","V-ADFP" and "OPEN_LOOP". When spec is "iag" accept the values
#'  "Exhaust" "Evaporative" and "Liquid". When spec is "nox" eu can be
#'  "PRE", "I", "II", "III", "IV", "V", "VI", "VIc", "III-DPF" or "III+CRT".
#'  Not required for "tyre", "break" or "road"
#' @param show when TRUE shows row of table with respective speciation
#' @param list when TRUE returns a list with number of elements of the list as
#' the number species of pollutants
#' @return dataframe of speciation in grams
#' @references "bcom": Ntziachristos and Zamaras. 2016. Passneger cars, light
#' commercial trucks, heavy-duty vehicles including buses and motor cycles. In:
#' EEA, EMEP. EEA air pollutant emission inventory guidebook-2009. European
#' Environment Agency, Copenhagen, 2016
#' @references "tyre", "break" and "road": Ntziachristos and Boulter 2016.
#' Automobile tyre and break wear and road abrasion. In: EEA, EMEP. EEA air
#' pollutant emission inventory
#' guidebook-2009. European Environment Agency, Copenhagen, 2016
#' @references "iag": RAFEE, S.A.A. Estudo numerico do impacto das emissoes
#' veiculares e fixas da cidade de Manaus nas concentracoes de poluentes
#' atmosfericos da regiao amazonica. 2015. 109 f. Dissertacao (Mestrado).
#' Programa de Pos-Graduacao em Engenharia Ambiental (PPGEA) - Universidade
#' Tecnologica Federal do Parana. Londrina, 2015.
#' http://repositorio.utfpr.edu.br/jspui/bitstream/1/1675/1/LD_PPGEA_M_Rafee%2c%20Sameh%20Adib%20Abou_2015.pdf
#' @references "iag": Vela, A. L. V. Avaliacao do impacto da mudanca dos
#' fatores de emissao veicular na formacao de ozonio troposferico na Regiao
#' Metropolitana de Sao Paulo. 2013. Dissertacao de Mestrado. Instituto de
#' Astronomia, Geofisica e Ciencias Atmosfericas, Universidade de Sao Paulo,
#' Sao Paulo.
#' http://www.iag.usp.br/pos/sites/default/files/d_angel_l_v_vela_corrigida_0.pdf
#' @note when spec = "iag", veh is only "VEH", STANDARD is "Evaporative",
#' "Liquid" or "Exhaust", FUEL is "G" for gasoline (blended with 25\% ethanol),
#'  "E" for Ethanol and "D" for diesel (blended with 5\% of biodiesel).
#'  When spec = "bcom", veh can be "PC", "LCV", "Motorcycle" or "HDV"
#'   VEH", STANDARD is "Evaporative",
#' "Liquid" or "Exhaust", FUEL is "G" for gasoline (blended with 25\% ethanol),
#'  "E" for Ethanol and "D" for diesel (blended with 5\% of biodiesel).
#' @export
#' @examples \dontrun{
#' # Do not run
#' pm <- rnorm(n = 100, mean = 400, sd = 2)
#' df <- speciate(pm, veh="PC", fuel="G", eu="I")
#' }
speciate <- function (x, spec = "bcom", veh, fuel, eu, show = FALSE, list = FALSE) {
 if (spec=="bcom") {
  bcom <- sysdata[[4]]
  df <- bcom[bcom$VEH == veh & bcom$FUEL == fuel & bcom$STANDARD == eu , ]
  dfb <- Emissions(data.frame(BC = x*df$BC/100,
                    OM = (df$OM/100)*(x*df$BC/100)))
  if (show == TRUE) {print(df) } else if (list == TRUE){
    dfb <- as.list(dfb) }

  } else if (spec=="tyre") {
    df <- data.frame(PM10 = 0.6, PM2.5 = 0.42,PM1 = 0.06,
                      PM0.1 = 0.048)
    dfb <- Emissions(data.frame(PM10 = x*0.6, PM2.5 = x*0.42,PM1 = x*0.06,
                      PM0.1 = x*0.048))
    if (show == TRUE) {print(df) } else if (list == TRUE){
      dfb <- as.list(dfb) }
    } else if (spec=="break") {
    df <- data.frame(PM10 = 0.98, PM2.5 = 0.39,PM1 = 0.1,
                     PM0.1 = 0.08)
    dfb <- Emissions(data.frame(PM10 = x*0.98, PM2.5 = x*0.39,PM1 = x*0.1,
                      PM0.1 = x*0.08))
    if (show == TRUE) {print(df) } else if (list == TRUE){
      dfb <- as.list(dfb) }
    } else if (spec=="road") {
    df <- data.frame(PM10 = 0.5, PM2.5 = 0.27)
    dfb <- Emissions(data.frame(PM10 = x*0.5, PM2.5 = x*0.27))
    if (show == TRUE) {print(df) } else if (list == TRUE){
      dfb <- as.list(dfb) }
    } else if (spec=="iag") {
    iag <- sysdata[[6]]
    df <- iag[iag$VEH == veh & iag$FUEL == fuel & iag$STANDARD == eu , ]
    dfb <- data.frame(e_eth = (x/100)*df$e_eth,
                      e_hc3 = (x/100)*df$e_hc3,
                      e_hc5 = (x/100)*df$e_hc5,
                      e_hc8 = (x/100)*df$e_hc8,
                      e_ol2 = (x/100)*df$e_ol2,
                      e_olt = (x/100)*df$e_olt,
                      e_oli = (x/100)*df$e_oli,
                      e_iso = (x/100)*df$e_iso,
                      e_tol = (x/100)*df$e_tol,
                      e_xyl = (x/100)*df$e_xyl,
                      e_ket = (x/100)*df$e_ket,
                      e_ch3oh = (x/100)*df$e_ch3oh,
                      e_c2h5oh = (x/100)*df$e_c2h5oh,
                      e_hcho = (x/100)*df$e_hcho,
                      e_ald = (x/100)*df$e_ald)
    for (i in 1:length(dfb)) {
        dfb[[i]] <- dfb[[i]] * units::parse_unit("mol h-1")
    }


    if (show == TRUE) {
      print(df)
      } else if (list == TRUE){
      dfb <- as.list(dfb)
      for (i in 1:length(dfb)) {
        for (j in 1:ncol(dfb[[1]])) {
          dfb[[i]][,j] <- dfb[[i]][,j] * units::parse_unit("mol h-1")
        }
      }

      }
    } else if (spec=="nox") {
      bcom <- sysdata[[7]]
      df <- bcom[bcom$VEH == veh & bcom$FUEL == fuel & bcom$STANDARD == eu , ]
      dfb <- Emissions(data.frame(NO2 = x*df$NO2,
                        NO =  x*df$NO))
      if (show == TRUE) {print(df) } else if (list == TRUE){
        dfb <- as.list(dfb)
        }
      }
  return(dfb)
}
