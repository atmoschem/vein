#' creates a .tex a table from a data.frame
#'
#' @description \code{\link{to_latex}} reads a data.frme and generates a .tex
#' table, aiming to replicate the method of tablegenerator.com
#'
#' @param df data.frame with three column.
#' @param file Character, name of new .tex file
#' @param caption Character caption of table
#' @param label Character,  label of table
#' @family helpers
#' @return a text file with extension .tex.
#' @importFrom sf st_sf
#' @seealso \code{\link{vein_notes}}
#' @export
#' @examples \dontrun{
#' ef <- ef_cetesb(p = "CO", veh = "PC_FG", full = T)
#' to_latex(ef)
#' }
to_latex <- function (df,
                      file,
                      caption = "My table",
                      label = "tab:df"){
  if(inherits(df, "sf")) df <- sf::st_set_geometry(df, NULL)
  df <- as.data.frame(df)
  # escape LaTeX special characters
  esc <- function(x) {
    x <- gsub("\\", "\001", x, fixed = TRUE)
    x <- gsub("([&%$#_{}])", "\\\\\\1", x)
    x <- gsub("~", "\\textasciitilde{}", x, fixed = TRUE)
    x <- gsub("^", "\\textasciicircum{}", x, fixed = TRUE)
    gsub("\001", "\\textbackslash{}", x, fixed = TRUE)
  }
  lo <- unlist(lapply(df, is.numeric))
  order <- paste0(ifelse(lo == TRUE, "r", "l"))

  body <- as.data.frame(lapply(df, function(x) {
    if(is.numeric(x)) as.character(as.numeric(x)) else esc(as.character(x))
  }), stringsAsFactors = FALSE)
  rows <- apply(body, 1, paste, collapse = " & ")
  rows <- paste0(rows, " \\\\ ")
  rows[length(rows)] <- paste0(rows[length(rows)], " \\bottomrule")

  out <- c(
    paste0("% Generated with vein ", packageVersion("vein")),
    "% Please add the following required packages to your document preamble:",
    "% \\usepackage{booktabs}",
    "% \\usepackage{graphicx}",
    "\\begin{table}[]",
    paste0("\\caption{", caption, "}"),
    paste0("\\label{", label, "}"),
    "\\resizebox{\\textwidth}{!}{%",
    paste0("\\begin{tabular}{@{} ", paste(order, collapse = " "), " @{}}"),
    "\\toprule",
    paste0(paste(esc(names(df)), collapse = " & "), " \\\\ \\midrule"),
    rows,
    "\\end{tabular}%",
    "}",
    "\\end{table}"
  )

  if(missing(file)){
    cat(out, sep = "\n")
  } else {
    cat(file, "\n")
    writeLines(out, file)
  }
  invisible(out)
}
