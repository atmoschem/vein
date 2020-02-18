#' Helper function to copy and zip projects
#'
#' @description \code{invcop} help to copy and zip projects
#'
#' @param in_name Character; Name of current project.
#' @param out_name Character; Name of outtput project.
#' @param all Logical; copy ALL (and for once) or not.
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
                    all = FALSE,
                    main = TRUE,
                    ef = TRUE,
                    est = TRUE,
                    network = TRUE,
                    veh_rds = FALSE,
                    veh_csv = TRUE,
                    zip = TRUE) {
  # nocov start
  # lista carpetas
  dirs <- list.dirs(path = in_name, full.names = TRUE, recursive = TRUE)

  #reemplaza in)_name con out_name
  dirs2 <- gsub(pattern = in_name, replacement = out_name, x = dirs)
 # if(test) return(dirs2)

  # crea out_name
  for(i in 1:length(dirs2)){
    dir.create(path = dirs2[i], recursive = TRUE)
  }

  # all
  if(all){
    min <- list.files(path = paste0(in_name),
                      pattern = "*",
                      full.names = TRUE)
    mout <- paste0(out_name)
    file.copy(from = min, to = mout, overwrite = TRUE, recursive = TRUE)
  } else {
  # main
  if(main){
    min <- list.files(path = paste0(in_name),
                      pattern = "*",
                      full.names = TRUE)
    mout <- paste0(out_name)
    file.copy(from = min, to = mout, overwrite = TRUE)
  }

  # ef
  if(ef){
    efin <- list.files(path = paste0(in_name, "/ef"),
                       pattern = "*",
                       full.names = TRUE)
    efout <- paste0(out_name, "/ef")
    file.copy(from = efin, to = efout, overwrite = TRUE, recursive = TRUE)
  }

  # est
  if(est){
    estin <- list.files(path = paste0(in_name, "/est"),
                        pattern = "*",
                        full.names = TRUE)
    estout <- paste0(out_name, "/est")
    file.copy(from = estin, to = estout, overwrite = TRUE, recursive = TRUE)
  }

  # network
  if(network){
    nin <- list.files(path = paste0(in_name, "/network"),
                      pattern = "*",
                      full.names = TRUE)
    nout <- paste0(out_name, "/network")
    file.copy(from = nin, to = nout, overwrite = TRUE, recursive = TRUE)
  }

  # veh rds
  if(veh_rds){
    vin <- list.files(path = paste0(in_name, "/veh"),
                      pattern = ".rds",
                      full.names = TRUE)
    vout <- paste0(out_name, "/veh")
    file.copy(from = vin, to = vout, overwrite = TRUE, recursive = TRUE)
  }

  # veh csv
  if(veh_csv){
    vin <- list.files(path = paste0(in_name, "/veh"),
                      pattern = ".csv",
                      full.names = TRUE)
    vout <- paste0(out_name, "/veh")
    file.copy(from = vin, to = vout, overwrite = TRUE, recursive = TRUE)
  }
  }
  # # emis
  # if(emis){
  #   ein <- list.files(path = paste0(in_name, "/emis"),
  #                     pattern = "*",
  #                     full.names = TRUE)
  #   eout <- paste0(out_name, "/veh")
  #   file.copy(from = ein, to = eout, overwrite = TRUE, recursive = TRUE)
  # }
  # zip
  if(zip){
    files2zip <- dir(out_name, full.names = TRUE)
    zip(zipfile = out_name, files = files2zip)
  }
  # nocov end
}

