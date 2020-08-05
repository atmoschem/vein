#' Download vein project to a specificor new directory
#'
#' \code{\link{get_project}} downloads a project for runnign vein.
#' The projects are available on Github.com/atmoschem/vein/projects
#'
#' @param directory Character; Path to an existing or a new directory to be created.
#' It needs absolute path.
#' @param case Character; One of  "brazil", "emislacovid" or "brazil_csvgz",
#' @importFrom utils download.file untar
#' @export
#' @note The projects are:
#' \tabular{lll}{
#'   Case                \tab Description          \tab   Outputs   \cr
#'   brazil or brazil_bu \tab Bottom-up estimation \tab  .rds       \cr
#'   emislacovid         \tab Bottom-up estimation March 2020 \tab  .rds\cr
#'   brazil_bu_csvgz     \tab Bottom-up estimation \tab  .csv.gz    \cr
#'   brazil_bu_csv       \tab Bottom-up estimation \tab  .csv Soon! \cr
#' }
#' @examples \dontrun{
#' #do not run
#' get_proeject("awesomecity")
#' }
#'
get_project <- function(directory,
                        case = "brasil"){

  if(missing(directory)) stop("Please, add a path to a directory")

  if(case %in% c("brasil", "brazil", "brazil_bu", "brasil_bu")) {
    URL <- "https://raw.githubusercontent.com/atmoschem/vein/master/projects/brazil_bu.tar.gz"

  } else if(case == "emislacovid"){
    URL <- "https://raw.githubusercontent.com/atmoschem/vein/master/projects/emislacovid.tar.gz"

  } else if(case %in% c("brazil_bu_csvgz")){
    URL <- "https://raw.githubusercontent.com/atmoschem/vein/master/projects/brazil_bu_csvgz.tar.gz"

  } else{
    stop("Other cases not supported yet")
  }
  tf <- paste0(tempfile(), ".tar.gz")
  utils::download.file(url = URL,
                       destfile =  tf)
  utils::untar(tarfile = tf, exdir = directory)
  message("Your directory is in ", directory)
}
