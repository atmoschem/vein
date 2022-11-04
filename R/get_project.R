#' Download vein project
#'
#' \code{\link{get_project}} downloads a project for running vein.
#' The projects are available on Github.com/atmoschem/vein/projects
#'
#' @param directory Character; Path to an existing or a new directory to be created.
#' @param case Character; One of  of the following:
#' \tabular{llll}{
#'   \strong{case}       \tab \strong{Description}\tab  \strong{EF} \tab \strong{Outputs}   \cr
#'   brazil or brazil_bu or brasil or brasil_bu\tab Bottom-up \tab CETESB \tab  .rds\cr
#'   emislacovid         \tab Bottom-up March 2020\tab CETESB \tab  .rds\cr
#'   brazil_bu_csvgz     \tab Bottom-up \tab CETESB+tunnel \tab  .csv.gz\cr
#'   brazil_csv         \tab Bottom-up. Faster but heavier\tab CETESB\tab  .csv\cr
#'   brazil_td_chem      \tab Top-down with chemical mechanisms\tab CETESB\tab  .csv and .rds\cr
#'   brazil_bu_chem      \tab Bottom-up  chemical mechanisms\tab CETESB+tunnel\tab  .rds\cr
#'   brazil_bu_chem_streets      \tab Bottom-up  chemical mechanisms for streets and MUNICH\tab CETESB+tunnel\tab  .rds\cr
#'   brazil_td_chem_im      \tab Top-down with chemical mechanisms+IM\tab CETESB\tab  .csv and .rds\cr
#'   brazil_bu_chem_im      \tab Bottom-up  chemical mechanisms+IM\tab CETESB+tunnel\tab  .rds\cr
#'   brazil_bu_chem_month  \tab Bottom-up  chemical mechanisms\tab CETESB+tunnel\tab  .rds\cr
#'   brazil_bu_chem_streets_im      \tab Bottom-up  chemical mechanisms for streets and MUNICH+IM\tab CETESB+tunnel\tab  .rds\cr
#'   sebr_cb05co2      \tab Top-down SP, MG and RJ\tab CETESB+tunnel\tab  .rds\cr
#'   amazon2014      \tab Top-down Amazon\tab CETESB+tunnel\tab  csv and.rds\cr
#'   curitiba      \tab Bottom-down +GTFS\tab CETESB+tunnel\tab  csv and.rds\cr
#'   masp2020      \tab Bottom-down\tab CETESB+tunnel\tab  csv and.rds\cr
#'   ecuador_td      \tab Top-down\tab EEA\tab  csv and.rds\cr
#'   ecuador_td_im      \tab Top-down\tab EEA\tab  csv and.rds\cr
#'   ecuador_td_hot      \tab Top-down\tab EEA\tab  csv and.rds\cr
#'   ecuador_td_hot_month      \tab Top-down\tab EEA\tab  csv and.rds\cr
#'   moves_bu      \tab Bottom-up\tab US/EPA MOVES \tab  csv and.rds (requires MOVES >=3.0 on Windows)\cr
#'   manizales_bu      \tab Bottom-up  chemical mechanisms\tab EEA\tab  csv, csv.gz, .rds\cr
#'   sebr_cb05co2_im  \tab Top-down SP, MG and RJ IM\tab CETESB+tunnel\tab  .rds\cr
#'   eu_bu_chem      \tab Bottom-up  chemical mechanisms\tab EEA 2019\tab  .rds\cr
#'   eu_bu_chem_simple \tab Bottom-up  chemical mechanisms 7 veh\tab EEA 2019\tab  .rds\cr
#'   china_bu_chem      \tab Bottom-up  chemical mechanisms\tab MEE China\tab  .rds\cr
#' }
#' @param url String, with the URL to download VEIN project
#' @note default case can be any of "brasil", "brazil", "brazil_bu", "brasil_bu", they are
#' the same
#' In any case, if you find any error, please, send a pull request in github.
#'
#' In Sao Paulo the IM programs was functioning until 2011.
#'
#' @importFrom utils download.file untar
#' @export
#' @examples \dontrun{
#' #do not run
#' get_project("awesomecity")
#' }
#'
get_project <- function(directory,
                        case = "brazil_bu_chem",
                        url){

  if(missing(directory)) stop("Please, add a path to a directory") #nocov start

  if(missing(url)){
    # brazil ####
  if(case %in% c("brasil",
                 "brazil",
                 "brazil_bu",
                 "brasil_bu",
                 "brazil_bu_chem",
                 "brazil_bu_csvgz",
                 "brazil_bu_csv",
                 "brazil_bu_cb05",
                 "brazil_mech",
                 "brazil_bu_chem_month")) {
    URL <- "https://raw.githubusercontent.com/atmoschem/vein/master/projects/brazil_bu_chem.tar.gz"
    tf <- paste0(tempfile(), ".tar.gz")
    utils::download.file(url = URL,
                         destfile =  tf)
    utils::untar(tarfile = tf, exdir = directory)
    message("Your directory is in ", directory)

    # moves_bu ####
  } else if(case %in% c("moves_bu", "moves")){
    URL <- "https://raw.githubusercontent.com/atmoschem/vein/master/projects/moves.tar.gz"
    tf <- paste0(tempfile(), ".tar.gz")
    utils::download.file(url = URL,
                         destfile =  tf)
    utils::untar(tarfile = tf, exdir = directory)
    message("Your directory is in ", directory)

    # emislacovid ####
  } else if(case == "emislacovid"){
    URL <- "https://raw.githubusercontent.com/atmoschem/vein/master/projects/emislacovid.tar.gz"
    tf <- paste0(tempfile(), ".tar.gz")
    utils::download.file(url = URL,
                         destfile =  tf)
    utils::untar(tarfile = tf, exdir = directory)
    message("Your directory is in ", directory)

    # brazil_bu_chem_im ####
  } else if(case %in% c("brazil_bu_chem_im")){
    URL <- "https://raw.githubusercontent.com/atmoschem/vein/master/projects/brazil_bu_chem.tar.gz"
    tf <- paste0(tempfile(), ".tar.gz")
    utils::download.file(url = URL,
                         destfile =  tf)
    utils::untar(tarfile = tf,
                 exdir = directory)
    x <- readLines(paste0(directory, "/main.R"))
    x <- gsub(pattern = "IM <- FALSE",
              replacement = "IM <- TRUE",
              x = x)
    writeLines(x, paste0(directory, "/main.R"))
    message("Your directory is in ", directory)

    # manizales_bu ####
  } else if(case %in% c("manizales_bu")){
    URL <- "https://raw.githubusercontent.com/atmoschem/vein/master/projects/manizales_bu.tar.gz"
    tf <- paste0(tempfile(), ".tar.gz")
    utils::download.file(url = URL,
                         destfile =  tf)
    utils::untar(tarfile = tf, exdir = directory)
    message("Your directory is in ", directory)

    # brazil_bu_chem_streets ####
  } else if(case %in% c("brazil_bu_chem_streets",
                        "brazil_bu_chem_streets_im")){
    URL <- "https://raw.githubusercontent.com/atmoschem/vein/master/projects/brazil_bu_chem.tar.gz"
    tf <- paste0(tempfile(), ".tar.gz")
    utils::download.file(url = URL,
                         destfile =  tf)
    utils::untar(tarfile = tf,
                 exdir = directory)
    x <- readLines(paste0(directory, "/main.R"))[1:153]
    x <- gsub(pattern = "type <- 'grids'",
              replacement = "type <- 'streets'",
              x = x)
    writeLines(x, paste0(directory, "/main.R"))
    message("Your directory is in ", directory)

    # masp2020 ####
  } else if(case %in% c("masp2020")){
    URL <- "https://raw.githubusercontent.com/atmoschem/vein/master/projects/MASP_2020.tar.gz"
    tf <- paste0(tempfile(), ".tar.gz")
    utils::download.file(url = URL,
                         destfile =  tf)
    utils::untar(tarfile = tf, exdir = directory)
    message("Your directory is in ", directory)

    # brazil_td_chem ####
  } else if(case %in% c("brazil_td_chem")){
    URL <- "https://raw.githubusercontent.com/atmoschem/vein/master/projects/brazil_td_chem.tar.gz"
    tf <- paste0(tempfile(), ".tar.gz")
    utils::download.file(url = URL,
                         destfile =  tf)
    utils::untar(tarfile = tf, exdir = directory)
    message("Your directory is in ", directory)

    # brazil_td_chem_im ####
  } else if(case %in% c("brazil_td_chem_im")){
    URL <- "https://raw.githubusercontent.com/atmoschem/vein/master/projects/brazil_td_chem_im.tar.gz"
    tf <- paste0(tempfile(), ".tar.gz")
    utils::download.file(url = URL,
                         destfile =  tf)
    utils::untar(tarfile = tf, exdir = directory)
    message("Your directory is in ", directory)

    # ecuador_td ####
  } else if(case %in% c("ecuador_td")){
    URL <- "https://raw.githubusercontent.com/atmoschem/vein/master/projects/ecuador_td.tar.gz"
    tf <- paste0(tempfile(), ".tar.gz")
    utils::download.file(url = URL,
                         destfile =  tf)
    utils::untar(tarfile = tf, exdir = directory)
    message("Your directory is in ", directory)

    # ecuador_td_im ####
  } else if(case %in% c("ecuador_td_im")){
    URL <- "https://raw.githubusercontent.com/atmoschem/vein/master/projects/ecuador_td_im.tar.gz"
    tf <- paste0(tempfile(), ".tar.gz")
    utils::download.file(url = URL,
                         destfile =  tf)
    utils::untar(tarfile = tf, exdir = directory)
    message("Your directory is in ", directory)

    # ecuador_td_hot ####
  } else if(case %in% c("ecuador_td_hot")){
    URL <- "https://raw.githubusercontent.com/atmoschem/vein/master/projects/ecuador_td_hot.tar.gz"
    tf <- paste0(tempfile(), ".tar.gz")
    utils::download.file(url = URL,
                         destfile =  tf)
    utils::untar(tarfile = tf, exdir = directory)
    message("Your directory is in ", directory)

    # ecuador_td_hot_month ####
  } else if(case %in% c("ecuador_td_hot_month")){
    URL <- "https://raw.githubusercontent.com/atmoschem/vein/master/projects/ecuador_td_hot_month.tar.gz"
    tf <- paste0(tempfile(), ".tar.gz")
    utils::download.file(url = URL,
                         destfile =  tf)
    utils::untar(tarfile = tf, exdir = directory)
    message("Your directory is in ", directory)

    # amazon2014 ####
  } else if(case %in% c("amazon2014")){
    URL <- "https://gitlab.com/ibarraespinosa/veinextras/-/raw/master/amazonas2014.tar.gz"
    tf <- paste0(tempfile(), ".tar.gz")
    utils::download.file(url = URL,
                         destfile =  tf)
    utils::untar(tarfile = tf, exdir = directory)
    message("Your directory is in ", directory)

    # eu_bu_chem ####
  } else if(case %in% c("eu_bu_chem")){
    URL <- "https://gitlab.com/ibarraespinosa/veinextras/-/raw/master/eu_bu_chem.tar.gz"
    tf <- paste0(tempfile(), ".tar.gz")
    utils::download.file(url = URL,
                         destfile =  tf)
    utils::untar(tarfile = tf, exdir = directory)
    message("Your directory for vehicular emissions is in ", directory)

    # eu_bu_chem_simple ####
  } else if(case %in% c("eu_bu_chem_simple")){
    URL <- "https://gitlab.com/ibarraespinosa/veinextras/-/raw/master/eu_bu_chem_simple.tar.gz"
    tf <- paste0(tempfile(), ".tar.gz")
    utils::download.file(url = URL,
                         destfile =  tf)
    utils::untar(tarfile = tf, exdir = directory)
    message("Your directory for vehicular emissions is in ", directory)

    # china_bu_chem ####
  } else if(case %in% c("china_bu_chem")){
    URL <- "https://gitlab.com/ibarraespinosa/veinextras/-/raw/master/china_bu_chem.tar.gz"
    tf <- paste0(tempfile(), ".tar.gz")
    utils::download.file(url = URL,
                         destfile =  tf)
    utils::untar(tarfile = tf, exdir = directory)
    message("Your directory for vehicular emissions is in ", directory)

    # curitiba ####
  } else if(case %in% c("curitiba")){
    URL <- "https://gitlab.com/ibarraespinosa/veinextras/-/raw/master/curitiba_all/curitiba.tar.gz"
    tf <- paste0(tempfile(), ".tar.gz")
    utils::download.file(url = URL,
                         destfile =  tf)
    utils::untar(tarfile = tf, exdir = directory)
    message("Your directory for vehicular emissions is in ", directory)

    URL <- "https://gitlab.com/ibarraespinosa/veinextras/-/raw/master/curitiba_all/gtfs_cur.zip"
    tf <- paste0(tempfile(), ".tar.gz")
    utils::download.file(url = URL,
                         destfile =  paste0(directory, "/network/gtfs_cur.zip"))

    message("GTFS Curitba is in  ", paste0(directory, "/network"))

    URL <- "https://gitlab.com/ibarraespinosa/veinextras/-/raw/master/curitiba_all/curitiba_industrial.tar.gz"
    tf <- paste0(tempfile(), ".tar.gz")
    utils::download.file(url = URL,
                         destfile =  tf)
    utils::untar(tarfile = tf, exdir = paste0(directory, "/industry"))
    message("Your directory for industrial emissions is in ", directory)

    } else if(case %in% c("sebr_cb05co2")){
    dir.create(directory)
    tf <- paste0(tempfile(), ".tar.gz")

    utils::download.file(url = "https://gitlab.com/ibarraespinosa/veinextras/-/raw/master/sebr_cb05co2/MG.tar.gz",
                         destfile =  tf)
    utils::untar(tarfile = tf, exdir = paste0(directory, "/MG"))

    utils::download.file(url = "https://gitlab.com/ibarraespinosa/veinextras/-/raw/master/sebr_cb05co2/SP.tar.gz",
                         destfile =  tf)
    utils::untar(tarfile = tf, exdir = paste0(directory, "/SP"))

    utils::download.file(url = "https://gitlab.com/ibarraespinosa/veinextras/-/raw/master/sebr_cb05co2/RJ.tar.gz",
                         destfile =  tf)
    utils::untar(tarfile = tf, exdir = paste0(directory, "/RJ"))

   utils::download.file(url = "https://gitlab.com/ibarraespinosa/veinextras/-/raw/master/sebr_cb05co2/merge_wrf.R",
                         destfile =  paste0(directory, "/merge_wrf.R"))

   utils::download.file(url = "https://gitlab.com/ibarraespinosa/veinextras/-/raw/master/sebr_cb05co2/sebr_cb05co2.Rproj",
                        destfile =  paste0(directory, "/sebr_cb05co2.Rproj"))

   utils::download.file(url = "https://gitlab.com/ibarraespinosa/veinextras/-/raw/master/sebr_cb05co2_wrfi.tar.gz",
                        destfile =  paste0(directory, "/sebr_cb05co2_wrfi.tar.gz"))
   utils::untar(tarfile = paste0(directory, "/sebr_cb05co2_wrfi.tar.gz"), exdir = directory)

   message("Your directory is in ", directory) #nocov end

   # sebr_cb05co2_im ####
    } else if(case %in% c("sebr_cb05co2_im")){
      dir.create(directory)
      tf <- paste0(tempfile(), ".tar.gz")

      utils::download.file(url = "https://gitlab.com/ibarraespinosa/veinextras/-/raw/master/sebr_cb05co2_im/MG.tar.gz",
                           destfile =  tf)
      utils::untar(tarfile = tf, exdir = paste0(directory, "/MG"))

      utils::download.file(url = "https://gitlab.com/ibarraespinosa/veinextras/-/raw/master/sebr_cb05co2_im/SP.tar.gz",
                           destfile =  tf)
      utils::untar(tarfile = tf, exdir = paste0(directory, "/SP"))

      utils::download.file(url = "https://gitlab.com/ibarraespinosa/veinextras/-/raw/master/sebr_cb05co2_im/RJ.tar.gz",
                           destfile =  tf)
      utils::untar(tarfile = tf, exdir = paste0(directory, "/RJ"))

      utils::download.file(url = "https://gitlab.com/ibarraespinosa/veinextras/-/raw/master/sebr_cb05co2_im/merge_wrf.R",
                           destfile =  paste0(directory, "/merge_wrf.R"))

      utils::download.file(url = "https://gitlab.com/ibarraespinosa/veinextras/-/raw/master/sebr_cb05co2_im/sebr_cb05co2.Rproj",
                           destfile =  paste0(directory, "/sebr_cb05co2.Rproj"))

      utils::download.file(url = "https://gitlab.com/ibarraespinosa/veinextras/-/raw/master/sebr_cb05co2_wrfi.tar.gz",
                           destfile =  paste0(directory, "/sebr_cb05co2_wrfi.tar.gz"))
      utils::untar(tarfile = paste0(directory, "/sebr_cb05co2_wrfi.tar.gz"), exdir = directory)

      message("Your directory is in ", directory) #nocov end
    } else{
      stop("Other cases not supported yet")
  }
  } else {
    URL <- url
  }
}
