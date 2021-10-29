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
#'   sebr_cb05co2      \tab Top-down SP, MG and RJ\tab CETESB+tunnel\tab  .rds\cr
#'   amazon2014      \tab Top-down Amazon\tab CETESB+tunnel\tab  csv and.rds\cr
#'   curitiba      \tab Bottom-down +GTFS\tab CETESB+tunnel\tab  csv and.rds\cr
#'   masp2020      \tab Bottom-down\tab CETESB+tunnel\tab  csv and.rds\cr
#'   ecuador_td      \tab Top-down\tab EEA\tab  csv and.rds\cr
#'   ecuador_td_im      \tab Top-down\tab EEA\tab  csv and.rds\cr
#'   ecuador_td_hot      \tab Top-down\tab EEA\tab  csv and.rds\cr
#'   ecuador_td_hot_month      \tab Top-down\tab EEA\tab  csv and.rds\cr
#'   moves      \tab Bottom-up\tab US/EPA MOVES \tab  csv and.rds (requires MOVES >=3.0 on Windows)\cr
#' }
#' @param url String, with the URL to download VEIN project
#' @note default case can be any of "brasil", "brazil", "brazil_bu", "brasil_bu", they are
#' the same
#' Projects for Ecuador are in development.
#' In any case, if you find any error, please, send a pull request in github or gitlab.
#' @importFrom utils download.file untar
#' @export
#' @examples \dontrun{
#' #do not run
#' get_project("awesomecity")
#' }
#'
get_project <- function(directory,
                        case = "brasil",
                        url){

  if(missing(directory)) stop("Please, add a path to a directory") #nocov start

  if(missing(url)){
  if(case %in% c("brasil", "brazil", "brazil_bu", "brasil_bu")) {
    URL <- "https://raw.githubusercontent.com/atmoschem/vein/master/projects/brazil_bu.tar.gz"
    tf <- paste0(tempfile(), ".tar.gz")
    utils::download.file(url = URL,
                         destfile =  tf)
    utils::untar(tarfile = tf, exdir = directory)
    message("Your directory is in ", directory)

  } else if(case == "moves_bu"){
    URL <- "https://raw.githubusercontent.com/atmoschem/vein/master/projects/moves.tar.gz"
    tf <- paste0(tempfile(), ".tar.gz")
    utils::download.file(url = URL,
                         destfile =  tf)
    utils::untar(tarfile = tf, exdir = directory)
    message("Your directory is in ", directory)

  } else if(case == "emislacovid"){
    URL <- "https://raw.githubusercontent.com/atmoschem/vein/master/projects/emislacovid.tar.gz"
    tf <- paste0(tempfile(), ".tar.gz")
    utils::download.file(url = URL,
                         destfile =  tf)
    utils::untar(tarfile = tf, exdir = directory)
    message("Your directory is in ", directory)

  } else if(case %in% c("brazil_bu_csvgz")){
    URL <- "https://raw.githubusercontent.com/atmoschem/vein/master/projects/brazil_bu_csvgz.tar.gz"
    tf <- paste0(tempfile(), ".tar.gz")
    utils::download.file(url = URL,
                         destfile =  tf)
    utils::untar(tarfile = tf, exdir = directory)
    message("Your directory is in ", directory)

  } else if(case %in% c("brazil_bu_csv")){
    URL <- "https://raw.githubusercontent.com/atmoschem/vein/master/projects/brazil_bu_csv.tar.gz"
    tf <- paste0(tempfile(), ".tar.gz")
    utils::download.file(url = URL,
                         destfile =  tf)
    utils::untar(tarfile = tf, exdir = directory)
    message("Your directory is in ", directory)

  } else if(case %in% c("brazil_bu_cb05", "brazil_mech", "brazil_bu_chem")){
    URL <- "https://raw.githubusercontent.com/atmoschem/vein/master/projects/brazil_bu_chem.tar.gz"
    tf <- paste0(tempfile(), ".tar.gz")
    utils::download.file(url = URL,
                         destfile =  tf)
    utils::untar(tarfile = tf, exdir = directory)
    message("Your directory is in ", directory)

  } else if(case %in% c("brazil_bu_chem_streets")){
    URL <- "https://raw.githubusercontent.com/atmoschem/vein/master/projects/brazil_bu_chem_streets.tar.gz"
    tf <- paste0(tempfile(), ".tar.gz")
    utils::download.file(url = URL,
                         destfile =  tf)
    utils::untar(tarfile = tf, exdir = directory)
    message("Your directory is in ", directory)

  } else if(case %in% c("masp2020")){
    URL <- "https://raw.githubusercontent.com/atmoschem/vein/master/projects/MASP_2020.tar.gz"
    tf <- paste0(tempfile(), ".tar.gz")
    utils::download.file(url = URL,
                         destfile =  tf)
    utils::untar(tarfile = tf, exdir = directory)
    message("Your directory is in ", directory)

  } else if(case %in% c("brazil_td_chem")){
    URL <- "https://raw.githubusercontent.com/atmoschem/vein/master/projects/brazil_td_chem.tar.gz"
    tf <- paste0(tempfile(), ".tar.gz")
    utils::download.file(url = URL,
                         destfile =  tf)
    utils::untar(tarfile = tf, exdir = directory)
    message("Your directory is in ", directory)

  } else if(case %in% c("ecuador_td")){
    URL <- "https://raw.githubusercontent.com/atmoschem/vein/master/projects/ecuador_td.tar.gz"
    tf <- paste0(tempfile(), ".tar.gz")
    utils::download.file(url = URL,
                         destfile =  tf)
    utils::untar(tarfile = tf, exdir = directory)
    message("Your directory is in ", directory)

  } else if(case %in% c("ecuador_td_im")){
    URL <- "https://raw.githubusercontent.com/atmoschem/vein/master/projects/ecuador_td_im.tar.gz"
    tf <- paste0(tempfile(), ".tar.gz")
    utils::download.file(url = URL,
                         destfile =  tf)
    utils::untar(tarfile = tf, exdir = directory)
    message("Your directory is in ", directory)

  } else if(case %in% c("ecuador_td_hot")){
    URL <- "https://raw.githubusercontent.com/atmoschem/vein/master/projects/ecuador_td_hot.tar.gz"
    tf <- paste0(tempfile(), ".tar.gz")
    utils::download.file(url = URL,
                         destfile =  tf)
    utils::untar(tarfile = tf, exdir = directory)
    message("Your directory is in ", directory)

  } else if(case %in% c("ecuador_td_hot_month")){
    URL <- "https://raw.githubusercontent.com/atmoschem/vein/master/projects/ecuador_td_hot_month.tar.gz"
    tf <- paste0(tempfile(), ".tar.gz")
    utils::download.file(url = URL,
                         destfile =  tf)
    utils::untar(tarfile = tf, exdir = directory)
    message("Your directory is in ", directory)

  } else if(case %in% c("amazon2014")){
    URL <- "https://gitlab.com/ibarraespinosa/veinextras/-/raw/master/amazonas2014.tar.gz"
    tf <- paste0(tempfile(), ".tar.gz")
    utils::download.file(url = URL,
                         destfile =  tf)
    utils::untar(tarfile = tf, exdir = directory)
    message("Your directory is in ", directory)

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
  } else{
    stop("Other cases not supported yet")
  }
  } else {
    URL <- url
  }
}
