#' Aggregate emissions by lumped groups in chemical mechanism
#'
#' @description \code{\link{emis_chem2}} aggregates VOC emissions by chemical mechanism
#' and convert grams to mol.
#'
#' @param df data.frame with emissions including columns "id" and "pol".
#' @param mech Character, "CB4", "CB05", "S99", "S7","CS7", "S7T", "S11",
#' "S11D","S16C","S18B","RADM2", "RACM2","MOZT1", "CBMZ"
#' @param nx Character, colnames for emissions data, for instance "V1", "V2"...
#' @param na.rm Logical, to remove lines with NA from group
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
emis_chem2 <- function(df, mech, nx, na.rm = FALSE) {
  chem <- sysdata$chem
  if(!any(grepl("id", names(df))))stop("Add 'id' column")

  id <- df$id

  if(missing(nx)) stop("Add colnames of emissions data")
  data.table::setDT(chem)
  pol <- mol<- NULL
  if(mech %in% c("CB05", "CB4", "CBMZ")) {
    cheml <- suppressWarnings(
      data.table::melt(
        data = chem[pol %in% unique(df[["pol"]])],
        id.vars = c("ID", "pol", "Mwt"),
        measure.vars = grep(pattern = mech,
                            x = names(chem),
                            value = TRUE),
        variable.name = "CB05",
        value.name = "mol",
        na.rm = TRUE,
        verbose = FALSE
      ))
    cheml <- cheml[mol > 0]

  } else {
    ..nd <- NULL
    nd <- c("ID", "pol", "Mwt", mech, paste0("F", mech))
    cheml <- chem[pol %in% unique(df[["pol"]]), ..nd]
    names(cheml)[length(cheml)] <- "mol"
    cheml <- cheml[!is.na(cheml[[mech]])] #TODO Check

  }

  # important
  # df$id <- rep(id, length(unique(df$pol)))

  data.table::setDF(df)
  data.table::setDF(cheml)

  y <- merge(x = df,
             y = cheml,
             by = "pol",
             all.x = T)
  # key!
  for(i in seq_along(nx)) {
    y[[nx[i]]] <- y[[nx[i]]]/y$Mwt*y$mol
  }

  data.table::setDT(y)

  y[[mech]] <- gsub(pattern = mech, replacement = "", x = y[[mech]])
  y[[mech]] <- gsub(pattern = "_", replacement = "", x = y[[mech]])

  id <-  NULL
  dy <- y[,
          lapply(.SD, sum, na.rm = T),
          .SDcols = nx,
          by = list(id, group = get(mech))]
  data.table::setorderv(dy, c("group", "id"))

  group <- NULL
  if(na.rm) dy <- dy[!is.na(group)]
  return(dy)
}
