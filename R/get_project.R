#' Local Emissions factors
#'
#' \code{\link{ef_local}} process an data.frame delivered by the user, but adding
#' similar funcionality and arguments as \code{\link{ef_cetesb}}, which are classification, filtering
#' and projections
#'
#' returns a vector or data.frame of Brazilian emission factors.
#'
#' @param p Character; pollutant delivered by the user. the name of the column of the data.frame must be
#' \strong{Pollutant}.
#'
#' @param veh Character; Vehicle categories available in the data.frame provided by the user
#' @param year Numeric; Filter the emission factor to start from a specific base year.
#' If project is 'constant' values above 2017 and below 1980 will be repeated
#' @param agemax Integer; age of oldest vehicles for that category
#' @param ef data.frame, for local the emission factors. The names of the ef must
#' be `Age`	`Year`	`Pollutant` and all the vehicle categories...
#' @param full Logical; To return a data.frame instead or a vector adding
#' Age, Year, Brazilian emissions standards and its euro equivalents.
#' @param project Character showing the method for projecting emission factors in
#' future. Currently the only value is "constant"
#' @param verbose Logical; To show more information
#' @return A vector of Emission Factor or a data.frame
#' @keywords  emission factors
#' @note The names of the ef must be `Age`	`Year`	`Pollutant` and all the vehicle categories...
#' @seealso  \code{\link{ef_cetesb}}
#' @export
#' @examples \dontrun{
#' #do not run
#' }
#'

untar(tarfile = "borrar/curso_brazil.tar.gz", exdir = "curso")
get_project <- function(case = "brasil",
                        name){

  utils::download.file(paste0(url = df$links[i]),
                       destfile =  paste0(destpath, "/", df$txzz[i]))
  # Check
  if(length(dataset) > 1) stop("Only one dataset per time")      # nocov start
  if(dataset == "v432_AP") {
    ed <- sysdata$v432_AP
  } else if(dataset == "v432_VOC_spec"){
    ed <- sysdata$v432_VOC
  } else {
    stop("Sorry, we are updating other datasets")
  }

  # links
  df <- ed[ed$dataset == dataset, ]

  if(missing(year)){
    stop("missing year")
  } else if(length(year) > 1){
    stop("One year per time please")
  }

  if(missing(sector)){
    if(missing(pol)) stop("Include pollutants")
    df <- ed[ed$dataset %in% dataset &
               ed$pol %in% pol, ]

  } else if(missing(pol)){
    if(missing(sector))stop("Include sector")
    df <- ed[ed$dataset %in% dataset &
               ed$sector %in% sector, ]

  } else {
    df <- ed[ed$dataset %in% dataset &
               ed$pol %in% pol &
               ed$sector %in% sector, ]

  }
  # more checks
  if(dataset == "htap_v2_2") {
    if(!year %in% c(2008, 2010)) {
      stop("When dataset is htap_v2_2, years can be 2008 or 2010 only")
    }
  } else {
    if(!year %in% 1970:2012){
      stop("For this datasets, years go from 1970 to 2012 only")
    }
  }                                                                      # nocov end

  links = unlist(lapply(1:length(df$URL), function(i) {
    year <- eval(parse(text = df$years[i]))
    eval(parse(text = df$URL[i]))
  }))

  txzz <- paste0(eval(parse(text = df$years)),
                 "_", df$pol, "_",df$sector,
                 ".zip")
  if(txt){
    links <- gsub(pattern = ".0.1x0.1", replacement = "", x = links)
    txzz <- gsub(pattern = ".0.1x0.1", replacement = "", x = txzz)
  }
  df$links <- links
  df$txzz <- txzz
  # paths
  for (i in 1:length(links)){
    utils::download.file(paste0(url = df$links[i]),
                         destfile =  paste0(destpath, "/", df$txzz[i]))
    message(paste0(
      "Files at ", destpath,  "\n"))
  }
  if(return_url) return(links)
}
