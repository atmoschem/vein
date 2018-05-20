#' Helper function to copy and zip projects
#'
#' @description \code{invcop} help to copy and zip projects
#'
#' @param in_name Character; Name of current project.
#' @param out_name Character; Name of outtput project.
#' @param main Logical; copy or not.
#' @param ef Logical; copy or not.
#' @param est Logical; copy or not.
#' @param network Logical; copy or not.
#' @param veh_rds  Logical; copy or not.
#' @param veh_csv  Logical; copy or not.
#' @param zip Logical; zip or not.
#' @return emission estimation  g/h
#' @note  This function was created to copy and zip project without the emis.
#' @export
#' @examples \dontrun{
#' # Do not run
#' }
invcop <- function (in_name = getwd(),
                    out_name,
                    main = TRUE,
                    ef = TRUE,
                    est = TRUE,
                    network = TRUE,
                    veh_rds = FALSE,
                    veh_csv = TRUE,
                    zip = TRUE) {
  # lista carpetas
  dirs <- list.dirs(path = in_name, full.names = T, recursive = T)

  #reemplaza in)_name con out_name
  dirs2 <- gsub(pattern = in_name, replacement = out_name, x = dirs)

  # crea out_name
  for(i in 1:length(dirs2)){
    dir.create(path = dirs2[i], recursive = T)
  }

  # main
  if(main){
    min <- list.files(path = paste0(in_name),
                      pattern = "*",
                      full.names = T)
    mout <- paste0(out_name)
    file.copy(from = min, to = mout, overwrite = T, recursive = T)
  }

  # ef
  if(ef){
    efin <- list.files(path = paste0(in_name, "/ef"),
                       pattern = "*",
                       full.names = T)
    efout <- paste0(out_name, "/ef")
    file.copy(from = efin, to = efout, overwrite = T, recursive = T)
  }

  # est
  if(est){
    estin <- list.files(path = paste0(in_name, "/est"),
                        pattern = "*",
                        full.names = T)
    estout <- paste0(out_name, "/est")
    file.copy(from = estin, to = estout, overwrite = T, recursive = T)
  }

  # network
  if(network){
    nin <- list.files(path = paste0(in_name, "/network"),
                      pattern = "*",
                      full.names = T)
    nout <- paste0(out_name, "/network")
    file.copy(from = nin, to = nout, overwrite = T, recursive = T)
  }

  # veh rds
  if(veh_rds){
    vin <- list.files(path = paste0(in_name, "/veh"),
                      pattern = ".rds",
                      full.names = T)
    vout <- paste0(out_name, "/veh")
    file.copy(from = vin, to = vout, overwrite = T, recursive = T)
  }

  # veh csv
  if(veh_csv){
    vin <- list.files(path = paste0(in_name, "/veh"),
                      pattern = ".csv",
                      full.names = T)
    vout <- paste0(out_name, "/veh")
    file.copy(from = vin, to = vout, overwrite = T, recursive = T)
  }

  # # emis
  # if(emis){
  #   ein <- list.files(path = paste0(in_name, "/emis"),
  #                     pattern = "*",
  #                     full.names = T)
  #   eout <- paste0(out_name, "/veh")
  #   file.copy(from = ein, to = eout, overwrite = T, recursive = T)
  # }
  # zip
  if(zip){
    files2zip <- dir(out_name, full.names = TRUE)
    zip(zipfile = out_name, files = files2zip)
  }
}

