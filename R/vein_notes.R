#' @title  Notes with sysinfo
#' @description \code{\link{vein_notes}} creates aa text file '.txt' for
#' writting technical notes about this emissions inventory
#'
#' @param file Character; Name of the file. The function will generate a file
#' with an extension '.txt'.
#' @param title Character; Title of this file. For instance: "Vehicular Emissions
#' Inventory of Region XX, Base year XX"
#' @param yourname Character; Name of the inventor compiler.
#' @param approach Character; vector of notes.
#' @param traffic Character; vector of notes.
#' @param composition Character; vector of notes.
#' @param ef Character; vector of notes.
#' @param cold_start Character; vector of notes.
#' @param evaporative Character; vector of notes.
#' @param standards Character; vector of notes.
#' @param mileage Character; vector of notes.
#' @param notes Character; vector of notes.
#' @return Writes a text file.
#' @importFrom utils menu object.size packageVersion sessionInfo
#' @export
#' @examples \dontrun{
#' #do not run
#' a <- "delete"
#' f <- vein_notes("notes", file = a)
#' file.edit(f)
#' file.remove("delete")
#' }
vein_notes <- function (notes,
                        file = "README",
                        yourname = Sys.info()["login"],
                        title = "Notes for this VEIN run",
                        approach = "Top Down",
                        traffic = "Your traffic information",
                        composition = "Your traffic information",
                        ef = "Your information about emission factors",
                        cold_start = "Your information about cold starts",
                        evaporative = "Your information about evaporative emission factors",
                        standards = "Your information about standards",
                        mileage = "Your information about mileage"){
  file <- paste0(file, "_", gsub(" ", "_", as.character(Sys.time())),".txt")
  file <- gsub(":", "", file)

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

  sink(file)
  cat("========================================\n") # 40
  cat(paste0(title, "\n"))
  cat("========================================\n") # 40
  cat(paste0("\nDirectory: ", getwd(), "\n"))
  cat(paste0("\nLocal Time: ", Sys.time(), "\n"))
  cat(paste0("Inventory compiler: ", yourname, "\n"))
  cat("========================================\n") # 40
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
  cat("========================================\n") # 40
  cat("\n")
  cat(paste0("VEIN version = ", packageVersion("vein"), "\n"))
  cat("========================================\n") # 40
  cat("\n")
  cat("Traffic:\n")
  for(i in 1:length(traffic)){
    cat(paste0(traffic[i], "\n"))
  }
  cat("\n")

  cat("Approach:\n")
  for(i in 1:length(approach)){
    cat(paste0(approach[i], "\n"))
  }
  cat("\n")


  cat("Vehicular composition:\n")
  for(i in 1:length(composition)){
    cat(paste0(composition[i], "\n"))
  }
  cat("\n")

  cat("Emission Factors:\n")
  for(i in 1:length(ef)){
    cat(paste0(ef[i], "\n"))
  }
  cat("\n")

  cat("Cold starts:\n")
  for(i in 1:length(cold_start)){
    cat(paste0(cold_start[i], "\n"))
  }
  cat("\n")

  cat("Evaporative:\n")
  for(i in 1:length(evaporative)){
    cat(paste0(evaporative[i], "\n"))
  }
  cat("\n")

    cat("Traffic standards:\n")
  for(i in 1:length(standards)){
    cat(paste0(standards[i], "\n"))
  }
  cat("\n")
  cat("Traffic mileage:\n")

  for(i in 1:length(mileage)){
    cat(paste0(mileage[i], "\n"))
  }
  if(!missing(notes)){
    cat("\n")
    cat("Notes:\n")

    for(i in 1:length(notes)){
      cat(paste0(notes[i], "\n"))
    }
  }

  cat("========================================\n") # 40
  cat("\n")
  cat("Session Info:\n")
  print(utils::sessionInfo())
  cat("\n")
  cat("========================================\n") # 40
  cat("\n\n\nThanks for using VEIN\n")
  sink()
  message("File at:", file, "\n")
  # return(file)
}

