#' vein_notes for writting technical notes about the inventory
#'
#' @description \code{\link{vein_notes}} creates aa text file '.txt' for
#' writting technical notes about this emissions inventory
#'
#' @param file Character; Name of the file. The function will generate a file
#' with an extension '.txt'.
#' @param title Character; Title of this file. For instance: "Vehicular Emissions
#' Inventory of Region XX, Base year XX"
#' @param yourname Character; Name of the inventor compiler.
#' @param notes Character; vector of notes.
#' @return A text file.
#' @importFrom utils menu
#' @export
#' @examples {
#' a <- tempfile()
#' vein_notes(a)
#' }
vein_notes <- function (file = "README",
                        title,
                        yourname,
                        notes){
  if(!missing(file)){
    file <- paste0(file,".txt")
  }
  if(file.exists(file)){
    warning(paste0(file," already exists"))
    choice <- utils::menu(c("Yes", "No"), title="Do you want Overwrite?")
    x <- c("Yes", "No")[choice]
    if(x == "No"){
      stop(paste0("NO Overwritting ", file))
    }else{
      message(paste0("Overwritting ", file))
    }
  }

  if(missing(title)){
    title <- "Vehicular Emissions Inventory on REGIONXX BASE YEAR XXXX"
  }
  if(missing(yourname)){
    yourname <- Sys.info()[["user"]]
  }
  if(missing(notes)){
    notes <- c("This is a note about this inventory",
               "This is another note about this inventory")
  }
  sink(file)
  cat(paste0(title, "\n"))
  cat(rep("=", 40))
  cat(paste0("\nDirectory: ", getwd(), "\n"))
  cat(paste0("Inventory compiler: ", yourname, "\n"))
  cat(rep("=", 40))
  cat(paste0("\nsysname = ", Sys.info()["sysname"], "\n"))
  cat(paste0("release = ", Sys.info()["release"], "\n"))
  cat(paste0("version = ", Sys.info()["version"], "\n"))
  cat(paste0("nodename = ", Sys.info()["nodename"], "\n"))
  cat(paste0("machine = ", Sys.info()["machine"], "\n"))
  cat(paste0("user = ", Sys.info()["user"], "\n"))
  cat(paste0("R version = ", paste0(version$major, ".", version$minor), "\n"))
  cat(paste0("nickname = ", version$nickname, "\n"))
  cat(paste0("Memory used = ",
             format(sum(sapply(environment(), object.size)),
                    units = " Mb"), "Mb \n"))
  cat(rep("=", 40))
  cat("\n")
  cat(paste0("VEIN version = ", packageVersion("vein"), "\n"))
  for(i in 1:length(notes)){
    cat(paste0(notes[i], "\n"))
  }
  cat("\n\n\nThanks for using VEIN\n")
  sink()
}

