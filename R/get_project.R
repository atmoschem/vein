#' Download vein project to a specificor new directory
#'
#' \code{\link{get_project}} downloads a project for runnign vein.
#' The projects are available on Github.com/atmoschem/vein/projects
#'
#' @param directory Character; Path to an existing or a new directory to be created.
#' It needs absolute path.
#' @param case Character; Currently only supports "brasil" (or "brazil").
#' @param approach Character; Currently only supports "bottom-up".
#' @importFrom utils download.file untar
#' @export
#' @examples \dontrun{
#' #do not run
#' }
#'
get_project <- function(directory,
                        case = "brasil",
                        approach = "bottom-up"){

  if(case %in% c("brasil", "brazil")) {
    URL <- "https://raw.githubusercontent.com/atmoschem/vein/master/projects/brazil_bu_v2020-03-01.tar.gz"

  tf <- paste0(tempfile(), ".tar.gz")
  utils::download.file(url = URL,
                       destfile =  tf)
if(missing(directory)) stop("Please, add a path to a directory")
    utils::untar(tarfile = tf, exdir = directory)
  message("Your directory is in ", directory)
  } else{
    stop("Other cases not implemented yet")
  }
  }
