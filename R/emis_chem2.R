#' Aggregate emissions by lumped groups in chemical mechanism
#'
#' @description \code{\link{emis_chem2}} aggregates VOC emissions by chemical mechanism
#' and convert grams to mol.
#'
#' @param df data.frame with emissions including columns "id" and "pol".
#' @param mech Character, "CB4", "CB05", "S99", "S7","CS7", "S7T", "S11",
#' "S11D","S16C","S18B","RADM2", "RACM2","MOZT1", "CBMZ", "CB05opt2"
#' @param nx Character, colnames for emissions data, for instance "V1", "V2"...
#' @param na.rm Logical, to remove lines with NA from group
#' @return data.frame with lumped groups by chemical mechanism.
#' @importFrom data.table setDF as.data.table setDT setorderv melt
#' @seealso \code{\link{speciate}}
#' @export
#' @references Carter, W. P. (2015). Development of a database for
#' chemical mechanism assignments for volatile organic emissions.
#' Journal of the Air & Waste Management Association, 65(10), 1171-1184.
#' @note
#' \itemize{
#' \item \strong{CB05}: "ALD"  "ALDX" "ETH"  "HC3"  "HC5"  "HC8"  "HCHO" "KET"
#' "OL2"  "OLI"  "OLT"  "TOL"  "XYL"
#' \item \strong{CB05opt2}:  "ALD2" "ALDX" "BENZENE" "ETH" "ETHA" "FORM"
#' "IOLE" "OLE" "PAR" "TOL" "XYL"
#' \item \strong{RADM2}:  "ALD"  "ETH"  "HC3"  "HC5"  "HC8"  "HCHO" "KET"
#' "MACR" "OL2"  "OLI"  "OLT"  "TOL" "XYL"
#' \item \strong{RACM2}:  ACD"  "ACE"  "ACT"  "ALD"  "BALD" "BEN"  "DIEN"
#' "ETE"  "ETH"  "HC3"  "HC5"  "HC8"  "HCHO" "MACR" "MEK"  "OLI"  "OLT"
#' "TOL"  "UALD" "XYM"  "XYO"  "XYP"
#' \item \strong{CB4}:  "ALD2" "ETH"  "FORM" "OLE"  "PAR"  "TOL"  "XYL"
#' \item \strong{S99}:  "ACET" "ALK1" "ALK2" "ALK3" "ALK4" "ALK5" "ARO1NBZ"
#' "ARO2" "BALD" "BENZENE" "CCHO" "ETHENE" "HCHO" "IPROD" "MACR" "MEK"
#' "OLE1" "OLE2" "RCHO"
#' \item \strong{CB4}: "ACET" "ACYE" "ALK1" "ALK2" "ALK3" "ALK4" "ALK5"
#' "ARO1" "ARO2" "BALD" "BENZ" "CCHO" "ETHE" "HCHO" "IPRD" "MACR" "MEK"
#' "OLE1" "OLE2" "RCHO"
#' \item \strong{CS7}: "ALK3" "ALK4" "ARO1" "ARO2" "CCHO" "ETHE" "HCHO"
#' "IPRD" "NROG" "OLE1" "OLE2" "PRD2" "RCHO"
#' \item \strong{S7}: "ACET" "ACYE" "ALK1" "ALK2" "ALK3" "ALK4" "ALK5"
#' "ARO1" "ARO2" "BALD" "BENZ" "CCHO" "ETHE" "HCHO" "IPRD" "MACR"
#' "MEK"  "OLE1" "OLE2" "RCHO"
#' \item \strong{S7T}: "13BDE" "ACET"  "ACRO"  "ACYE"  "ALK1"  "ALK2"
#' "ALK3"  "ALK4"  "ALK5"  "ARO1"  "ARO2"  "B124" "BALD"  "BENZ"  "CCHO"
#'  "ETHE"  "HCHO"  "IPRD"  "MACR"  "MEK"   "MXYL"  "OLE1"  "OLE2"  "OXYL"
#'  "PRPE"  "PXYL"  "RCHO"  "TOLU"
#' \item \strong{S11}: "ACET" "ACYL" "ALK1" "ALK2" "ALK3" "ALK4" "ALK5"
#' "ARO1" "ARO2" "BALD" "BENZ" "CCHO" "ETHE" "HCHO" "IPRD" "MACR"
#'  "MEK"  "OLE1" "OLE2" "RCHO"
#' \item \strong{S11D}: "ACET" "ACRO" "ACYL" "ALLENE" "BALD" "BENZ"
#' "BUTDE13" "BUTENE1"  "C2BENZ" "C2BUTE" "C2PENT" "C4RCHO1" "CCHO"
#' "CROTALD" "ETACTYL"  "ETHANE" "ETHE"  "HCHO" "HEXENE1" "ISOBUTEN" "M2C3"
#' "M2C4" "M2C6" "M2C7" "M3C6" "M3C7" "MACR" "MEACTYL" "MEK" "MXYLENE"
#' "NC1" "NC4" NC5" "NC6" "NC7" "NC8" "NC9" "OLE2" "OTH2" "OTH4"
#' "OTH5" "OXYLENE" "PENTEN1" "PROPALD" "PROPANE" "PROPENE" "PXYLENE" "RCHO"
#' "STYRENE"  "TMB123"   "TMB124"   "TMB135"   "TOLUENE"
#' \item \strong{S16C}:"ACET"  "ACETL" "ACRO"  "ACYLS" "ALK3"
#'  "ALK4"  "ALK5"  "BALD"  "BENZ"  "BUT13" "BZ123" "BZ124"
#'  "BZ135" "C2BEN" "ETCHO" "ETHAN" "ETHEN" "HCHO"  "MACR"
#'  "MECHO" "MEK"   "MXYL"  "NC4"   "OLE1"
#'  "OLE2"  "OLE3"  "OLE4"  "OLEA1" "OTH1"  "OTH3"  "OTH4"
#'   "OXYL"  "PROP"  "PROPE" "PXYL"  "RCHO" "STYRS" "TOLU"
#' \item \strong{S18B}:"ACET"  "ACETL" "ACRO"  "ACYLS" "ALK3"
#'  "ALK4"  "ALK5"  "BALD"  "BENZ"  "BUT13" "BZ123" "BZ124"
#'  "BZ135" "C2BEN" "ETCHO" "ETHAN" "ETHEN" "HCHO"
#'  "MACR"  "MECHO" "MEK"   "MXYL"  "NC4"   "OLE1"
#'  "OLE2"  "OLE3"  "OLE4"  "OLEA1" "OTH1"  "OTH3"  "OTH4"  "OXYL"
#'  "PROP"  "PROPE" "PXYL"  "RCHO" "STYRS" "TOLU"
#'}
#' @examples {
#' id <-1:2
#' df <- data.frame(V1 = 1:2, V2 = 1:2)
#' dx <- speciate(
#' x = df,
#' spec = "nmhc",
#' fuel = "E25",
#' veh = "LDV",
#' eu = "Exhaust"
#' )
#' dx$id <- rep(id, length(unique(dx$pol)))
#' names(dx)
#' vocE25EX <- emis_chem2(df = dx,mech = "CB05",nx = c("V1", "V2"))
#' }
emis_chem2 <- function(df, mech, nx, na.rm = FALSE) {
    chem <- sysdata$chem
    if(!any(grepl("id", names(df))))stop("Add 'id' column")

    id <- df$id

    if(missing(nx)) stop("Add colnames of emissions data")
    data.table::setDT(chem)
    pol <- mol<- NULL
    if(mech %in% c("CB05", "CB4", "CBMZ","CB05opt2")) {
      cheml <- suppressWarnings(
        data.table::melt(
          data = chem[pol %in% unique(df[["pol"]])],
          id.vars = c("ID", "pol", "Mwt"),
          measure.vars = grep(pattern = paste0(mech, "_"),
                              x = names(chem),
                              value = TRUE),
          variable.name = "CB05",
          value.name = "mol",
          na.rm = TRUE,
          verbose = FALSE
        ))
      cheml <- cheml[mol > 0]

      names(cheml)[4] <- mech

      # if(verbose) print(head(cheml))
    } else {
      ..nd <- NULL
      nd <- c("ID", "pol", "Mwt", mech, paste0("F", mech))
      cheml <- suppressWarnings(chem[pol %in% unique(df[["pol"]]), ..nd])
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
