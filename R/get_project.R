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
#'   sebr_cb05co2      \tab Top-down SP, MG and RJ, CB05+CO2\tab CETESB+tunnel\tab  .rds\cr
#'   amazon2014      \tab Top-down Amazon, Mozart\tab CETESB+tunnel\tab  csv and.rds\cr
#' }
#' @param url String, with the URL to download VEIN project
#' @note default case can be any of "brasil", "brazil", "brazil_bu", "brasil_bu", they are
#' the same
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

  } else if(case %in% c("brazil_td_chem")){
    URL <- "https://raw.githubusercontent.com/atmoschem/vein/master/projects/brazil_td_chem.tar.gz"
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
