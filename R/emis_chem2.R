#' Aggregate emissions by lumped groups in chemical mechanism
#'
#' @description \code{\link{emis_chem2}} aggregates VOC emissions by chemical mechanism
#' and convert grams to mol.
#'
#' @param df data.frame with emissions including columns "id" and "pol".
#' @param mech Character, "CB4", "CB05", soon: "S99", "S7", "S7T", "S11", "MOZT1"
#' @param nx Character, colnames for emissions data, for instance "V1", "V2"...
#' @return data.frame with lumped groups by chemical mechanism.
#' @importFrom data.table setDF as.data.table setDT setorderv melt
#' @seealso \code{\link{speciate}}
#' @export
#' @references Carter, W. P. (2015). Development of a database for
#' chemical mechanism assignments for volatile organic emissions.
#' Journal of the Air & Waste Management Association, 65(10), 1171-1184.
#' @examples \dontrun{
#' # experimental
#' }
emis_chem2 <- function(df, mech, nx) {
  chem <- sysdata$chem
  df$pol <- ifelse(df$pol == "isopentane", "2-methyl-butane",
                   ifelse(df$pol == "ethanol", "ethyl alcohol",
                          ifelse(
                            df$pol == "propene", "propylene",
                            df$pol)))

  names(chem)[2] <- "pol"
  if(missing(nx)) stop("Add colnames of emissions data")
  data.table::setDT(chem)
  pol <- NULL
  if(mech == "CB05") {
    cheml <- data.table::melt(data = chem[pol %in% unique(df[["pol"]])],
                              id.vars = c("ID", "pol", "Mwt"),
                              measure.vars = grep(pattern = "CB05", x = names(chem), value = TRUE),
                              variable.name = "CB05",
                              value.name = "mol",
                              na.rm = TRUE)
  } else if(mech == "CB4"){
    cheml <- data.table::melt(data = chem[pol %in% unique(df[[pol]])],
                              id.vars = c("ID", "pol", "Mwt"),
                              measure.vars = grep(pattern = "CB4", x = names(chem), value = TRUE),
                              variable.name = "CB4",
                              value.name = "mol",
                              na.rm = TRUE)
  } else {
    nx <- c("ID", "pol", "Mwt", mech)
    cheml <- cheml[,mech]
    cheml <- cheml[!is.na(cheml[[3]])] #TODO Check
  }



  data.table::setDF(df)
  data.table::setDF(cheml)

  y <- merge(x = df,
             y = cheml,
             by = "pol",
             all.x = T)

  y[, nx] <- y[, nx]/y$Mwt*y$mol

  data.table::setDT(y)

  y[[mech]] <- gsub(pattern = mech, replacement = "", x = y[[mech]])
  id <- CB05 <- NULL
  dy <- y[,
          lapply(.SD, sum, na.rm = T),
          .SDcols = nx,
          by = list(id, CB05)]
  data.table::setorderv(dy, cols = "id")
  return(dy)
}
