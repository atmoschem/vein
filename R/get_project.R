#' Download vein project
#'
#' \code{\link{get_project}} downloads a project for running vein.
#' The projects are available on Github.com/atmoschem/vein/projects
#'
#' @param directory Character; Path to an existing or a new directory to be created.
#' @param case Character; One of  of the following (aliases in parentheses):
#' \tabular{llll}{
#'   \strong{case}  \tab \strong{Description}\tab  \strong{EF} \tab \strong{Notes}   \cr
#'   argentina  \tab top down\tab COPERT \tab  .rds\cr
#'   emislacovid  \tab Bottom-up March 2020\tab CETESB \tab  .rds\cr
#'   brazil_bu_chem (brazil, brasil, brazil_bu, brasil_bu, brazil_bu_csvgz,
#'   brazil_bu_csv, brazil_bu_cb05, brazil_mech, brazil_bu_chem_month,
#'   brazil_bu_chem_im)    \tab Bottom-up  chemical mechanisms\tab CETESB+tunnel\tab  .rds\cr
#'   brazil_bu_speed (brazil_speed, brazil_bu_scaled)   \tab Bottom-up hourly speed-dependent (EMEP/EEA scaled)\tab CETESB+tunnel\tab  .rds\cr
#'   brazil_bu_chem_streets (brazil_bu_chem_streets_im) \tab Bottom-up  chemical mechanisms for streets and MUNICH\tab CETESB+tunnel\tab  .rds\cr
#'   brazil_td_chem (brazil_td, brazil_country)    \tab Top-down with chemical mechanisms\tab CETESB\tab  .csv and .rds\cr
#'   masp2020       \tab Bottom-down\tab CETESB+tunnel\tab  csv and.rds\cr
#'   amazon2014     \tab Top-down Amazon\tab CETESB+tunnel\tab  csv and.rds\cr
#'   curitiba       \tab Bottom-down +GTFS\tab CETESB+tunnel\tab  csv and.rds\cr
#'   ecuador (ecuador_td)    \tab Top-down\tab EEA\tab  csv and.rds\cr
#'   ecuador_mdpi     \tab Top-down\tab EEA\tab  csv and.rds\cr
#'   quito     \tab Bottom-up \tab EEA\tab  csv and.rds\cr
#'   moves_bu (moves)        \tab Bottom-up\tab US/EPA MOVES \tab  csv and.rds (requires MOVES >=3.0 on Windows)\cr
#'   manizales_bu (manizales)    \tab Bottom-up  chemical mechanisms\tab EEA\tab  csv, csv.gz, .rds\cr
#'   eu_bu_chem       \tab Bottom-up  chemical mechanisms\tab EEA 2019\tab  .rds\cr
#'   eu_bu_chem_simple\tab Bottom-up  chemical mechanisms 7 veh\tab EEA 2019\tab  .rds\cr
#'   china_bu_chem    \tab Bottom-up  chemical mechanisms\tab MEE China\tab  .rds\cr
#'   china_bu_chem_1h    \tab Bottom-up  chemical mechanisms\tab MEE China\tab  .rds\cr
#' }
#' @param url String, with the URL of a project to download. When provided,
#' \code{case} is ignored. The file is downloaded and extracted into
#' \code{directory}. Supported formats are ".tar.gz", ".tgz", ".tar" and ".zip".
#' @note  All projects include option to apply survival functions
#' In Sao Paulo the IM programs was functioning until 2011.
#' @return Invisibly, the path of \code{directory}.
#' @importFrom utils download.file untar
#' @export
#' @examples \dontrun{
#' #do not run
#' get_project("awesomecity", case = "brazil_bu_chem")
#' }
#'
get_project <- function(directory, case, url) {
  if (missing(directory)) {
    stop("Please, add a path to a directory")
  } #nocov start

  if (missing(case) && missing(url)) {
    stop("Please, select a case. run ?vein::get_project")
  }

  gh <- "https://raw.githubusercontent.com/atmoschem/vein/master/projects/"
  gl <- "https://gitlab.com/ibarraespinosa/veinextras/-/raw/master/"

  dl <- function(URL, destfile) {
    utils::download.file(url = URL, destfile = destfile, mode = "wb")
  }

  dl_untar <- function(URL, exdir = directory) {
    tf <- paste0(tempfile(), ".tar.gz")
    on.exit(unlink(tf), add = TRUE)
    dl(URL, tf)
    utils::untar(tarfile = tf, exdir = exdir)
  }

  if (!missing(url)) {
    is_zip <- grepl("\\.zip$", url, ignore.case = TRUE)
    is_tar <- grepl("\\.(tar\\.gz|tgz|tar|tar\\.bz2|tar\\.xz)$", url,
                    ignore.case = TRUE)
    if (!is_zip && !is_tar) {
      stop("'url' must point to a .tar.gz, .tgz, .tar or .zip file")
    }
    tf <- tempfile(fileext = paste0(".", basename(url)))
    on.exit(unlink(tf), add = TRUE)
    dl(url, tf)
    if (is_zip) {
      utils::unzip(zipfile = tf, exdir = directory)
    } else {
      utils::untar(tarfile = tf, exdir = directory)
    }
    message("Your directory is in ", directory)
    return(invisible(directory))
  }

  # brazil ####
  if (
    case %in%
      c(
        "brasil",
        "brazil",
        "brazil_bu",
        "brasil_bu",
        "brazil_bu_chem",
        "brazil_bu_csvgz",
        "brazil_bu_csv",
        "brazil_bu_cb05",
        "brazil_mech",
        "brazil_bu_chem_month",
        "brazil_bu_chem_im"
      )
  ) {
    dl_untar(paste0(gh, "brazil_bu_chem.tar.gz"))
    message("Your directory is in ", directory)

    # brazil_bu_speed ####
  } else if (
    case %in% c("brazil_bu_speed", "brazil_speed", "brazil_bu_scaled")
  ) {
    dl_untar(paste0(gh, "brazil_bu_speed.tar.gz"))
    message("Your directory is in ", directory)

    # moves_bu ####
  } else if (case %in% c("moves_bu", "moves")) {
    dl_untar(paste0(gh, "moves.tar.gz"))
    message("Your directory is in ", directory)

    # emislacovid ####
  } else if (case == "emislacovid") {
    dl_untar(paste0(gh, "emislacovid.tar.gz"))
    message("Your directory is in ", directory)

    # brazil_country ####
  } else if (case %in% c("brazil_country", "brazil_td", "brazil_td_chem")) {
    dl_untar(paste0(gh, "brazil_td.tar.gz"))
    message("Your directory is in ", directory)

    # manizales_bu ####
  } else if (case %in% c("manizales_bu", "manizales")) {
    dl_untar(paste0(gh, "manizales_bu.tar.gz"))
    message("Your directory is in ", directory)

    # quito ####
  } else if (case %in% c("quito")) {
    dl_untar(paste0(gh, "quito.tar.gz"))
    message("Your directory is in ", directory)

    # argentina ####
  } else if (case %in% c("argentina")) {
    dl_untar(paste0(gh, "argentina.tar.gz"))
    dl(paste0(gl, "wrf_argentina/wrfinput_d01"),
       file.path(directory, "wrfinput_d01"))
    message("Your directory is in ", directory)

    # brazil_bu_chem_streets ####
  } else if (
    case %in% c("brazil_bu_chem_streets", "brazil_bu_chem_streets_im")
  ) {
    dl_untar(paste0(gh, "brazil_bu_chem.tar.gz"))

    # switch every "type <- 'grids'" in main scripts and scripts/ to streets
    rf <- c(
      list.files(directory, pattern = "^main.*\\.R$", full.names = TRUE),
      list.files(file.path(directory, "scripts"), pattern = "\\.R$",
                 full.names = TRUE)
    )
    for (f in rf) {
      x <- readLines(f, warn = FALSE)
      x <- gsub(
        pattern = "\\btype\\s*<-\\s*['\"]grids['\"]",
        replacement = "type <- 'streets'",
        x = x,
        perl = TRUE
      )
      writeLines(x, f)
    }
    message("Your directory is in ", directory)

    # masp2020 ####
  } else if (case %in% c("masp2020")) {
    dl_untar(paste0(gh, "MASP_2020.tar.gz"))
    message("Your directory is in ", directory)

    # ecuador ####
  } else if (case %in% c("ecuador", "ecuador_td")) {
    dl_untar(paste0(gh, "ecuador_td.tar.gz"))
    message("Your directory is in ", directory)

    # ecuador ####
  } else if (case %in% "ecuador_mdpi") {
    dl_untar(paste0(gh, "ecuador_mdpi.tar.gz"))
    message("Your directory is in ", directory)

    # amazon2014 ####
  } else if (case %in% c("amazon2014")) {
    dl_untar(paste0(gl, "amazonas2014.tar.gz"))
    message("Your directory is in ", directory)

    # eu_bu_chem ####
  } else if (case %in% c("eu_bu_chem")) {
    dl_untar(paste0(gl, "eu_bu_chem.tar.gz"))
    message("Your directory for vehicular emissions is in ", directory)

    # eu_bu_chem_simple ####
  } else if (case %in% c("eu_bu_chem_simple")) {
    dl_untar(paste0(gl, "eu_bu_chem_simple.tar.gz"))
    message("Your directory for vehicular emissions is in ", directory)

    # china_bu_chem ####
  } else if (case %in% c("china_bu_chem")) {
    dl_untar(paste0(gh, "china_bu_chem.tar.gz"))
    message("Your directory for vehicular emissions is in ", directory)

    # china_bu_chem_realtime ####
  } else if (case %in% c("china_bu_chem_1h")) {
    dl_untar(paste0(gh, "china_bu_chem_realtime.tar.gz"))
    message("Your directory for vehicular emissions is in ", directory)

    # curitiba ####
  } else if (case %in% c("curitiba")) {
    dl_untar(paste0(gl, "curitiba_all/curitiba.tar.gz"))
    message("Your directory for vehicular emissions is in ", directory)

    dir.create(file.path(directory, "network"), showWarnings = FALSE,
               recursive = TRUE)
    dl(paste0(gl, "curitiba_all/gtfs_cur.zip"),
       file.path(directory, "network", "gtfs_cur.zip"))
    message("GTFS Curitba is in  ", file.path(directory, "network"))

    dl_untar(paste0(gl, "curitiba_all/curitiba_industrial.tar.gz"),
             exdir = file.path(directory, "industry"))
    message("Your directory for industrial emissions is in ", directory)
  } else {
    stop("Other cases not supported yet")
  }
  invisible(directory)
}
