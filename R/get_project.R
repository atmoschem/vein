#' Download vein project
#'
#' \code{\link{get_project}} downloads a project for running vein.
#' The projects are available on Github.com/atmoschem/vein/projects
#'
#' @param directory Character; Path to an existing or a new directory to be created.
#' @param case Character; One of  of the following:
#' \tabular{llll}{
#'   \strong{case}       \tab \strong{Description}\tab  \strong{EF} \tab \strong{Outputs}   \cr
#'   brazil or brazil_bu or brasil or brasil_bu\tab Bottom-up \tab CETESB \tab  .rds       \cr
#'   emislacovid         \tab Bottom-up March 2020\tab CETESB \tab  .rds\cr
#'   brazil_bu_csvgz     \tab Bottom-up \tab CETESB \tab  .csv.gz    \cr
#'   brazil_csv         \tab Bottom-up. Faster but heavier\tab CETESB\tab  .csv  \cr
#'   brazil_td_chem      \tab Top-down with chemical mechanisms\tab CETESB\tab  .rds       \cr
#'   brazil_bu_chem      \tab Bottom-up  chemical mechanisms\tab CETESB\tab  .rds       \cr
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

  } else if(case == "emislacovid"){
    URL <- "https://raw.githubusercontent.com/atmoschem/vein/master/projects/emislacovid.tar.gz"

  } else if(case %in% c("brazil_bu_csvgz")){
    URL <- "https://raw.githubusercontent.com/atmoschem/vein/master/projects/brazil_bu_csvgz.tar.gz"

  } else if(case %in% c("brazil_bu_csv")){
    URL <- "https://raw.githubusercontent.com/atmoschem/vein/master/projects/brazil_bu_csv.tar.gz"

  } else if(case %in% c("brazil_bu_cb05", "brazil_mech", "brazil_bu_chem")){
    URL <- "https://raw.githubusercontent.com/atmoschem/vein/master/projects/brazil_bu_chem.tar.gz"
  } else if(case %in% c("brazil_td_chem")){
    URL <- "https://raw.githubusercontent.com/atmoschem/vein/master/projects/brazil_td_chem.tar.gz"
  } else{
    stop("Other cases not supported yet")
  }
  } else {
    URL <- url
  }
  tf <- paste0(tempfile(), ".tar.gz")
  utils::download.file(url = URL,
                       destfile =  tf)
  utils::untar(tarfile = tf, exdir = directory)
  message("Your directory is in ", directory) #nocov end
}
