#' A function to source vein scripts
#'
#' @description \code{\link{emis_source}} source vein scripts
#'
#' @param path Character; path to source scripts. Default is "est".
#' @param ignore Character; caracter to be excluded. Default is "~". Sometimes,
#' the OS creates automatic back-ups, for instance "run.R~", the ideia is to
#' avoid sourcing these files.
#' @param ask Logical; Check inputs or not. Default is "FALSE". It allows to
#' stop inputs
#' @param pattern Character; extensions of R scripts. Default is ".R".
#' @param recursive Logical; recursive or not. Default is "TRUE"
#' @param full.names Logical; full.names or not. Default is "TRUE".
#' @param first Character; first script.
#' @importFrom utils menu
#' @export
#' @examples \dontrun{
#' # Do not run
#' }
emis_source <- function(path = "est", pattern = ".R",
                        ignore = "~", first, ask = TRUE,
                        recursive = TRUE, full.names = TRUE){
  inputs <- list.files(path = path, pattern = pattern,
                       recursive = recursive, full.names =  full.names)
  inputs <- inputs[!grepl(pattern = ignore, x = inputs)]
  if(!missing(first)){
    inputsa <- c(inputs[grepl(pattern = first, x = inputs)],
                 inputs[!grepl(pattern = first, x = inputs)])
    if(ask){
      print(inputsa)
      choice <- utils::menu(c("Yes", "No"), title="inputs are OK?")
      if(choice == 1){
        for(i in 1:length(inputsa)){
          source(inputsa[i])
          # print(inputsa[i])
        }
      } else {
        stop("Change inputs")
      }
    } else {
      for(i in 1:length(inputsa)){
        source(inputsa[i])
        # print(inputsa[i])
      }
    }
  } else {
    if(ask){
      print(inputs)
      choice <- utils::menu(c("Yes", "No"), title="inputs are OK?")
      if(choice == 1){
        for(i in 1:length(inputs)){
          source(inputs[i])
          # print(inputs[i])
        }
      } else {
        stop("Change inputs")
      }
    } else {
      for(i in 1:length(inputs)){
        source(inputs[i])
        # print(inputs[i])
      }
    }

  }
}
