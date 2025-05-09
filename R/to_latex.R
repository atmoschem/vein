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
  lo <- unlist(lapply(df, is.numeric))
  order <- paste0(ifelse(lo == TRUE, "r", "l"))
  if(missing(file)){
    cat(paste0("% Generated with vein ", packageVersion("vein"), "\n"))
    cat("% Please add the following required packages to your document preamble:\n")
    cat("% \\usepackage{booktabs}\n")
    cat("% \\usepackage{graphicx}\n")
    cat("\\begin{table}[]\n")
    cat(paste0("\\caption{", caption, "}\n"))
    cat(paste0("\\label{", label, "}\n"))
    cat("\\resizebox{\\textwidth}{!}{%\n")
    cat("\\begin{tabular}{@{}", order, "@{}}\n")
    cat("\\toprule\n")
    nn <- names(df)
    # top
    top <- paste0(nn[1:(length(nn) - 1)],
                  rep(" & ", ncol(df) - 1))
    cat(top, nn[length(nn)], "\\\\ \\midrule\n")
    # body
    for(i in 1:(ncol(df) - 1)){
      df[, i] <- paste0(df[, i], " & ")
    }
    df[, ncol(df)] <- paste0(df[, ncol(df)], " \\\\ ")
    df[nrow(df), ncol(df)] <- paste0(df[nrow(df), ncol(df)], " \\bottomrule")
    names(df) <- NULL
    for(i in 1:nrow(df)) cat(unlist(df[i, ]), "\n")
    # bottom
    cat("\\end{tabular}%\n")
    cat("}\n")
    cat("\\end{table}\n")
  } else {
    cat(file, "\n")
    sink(file)
    cat(paste0("% Generated with vein ", packageVersion("vein"), "\n"))
    cat("% Please add the following required packages to your document preamble:\n")
    cat("% \\usepackage{booktabs}\n")
    cat("% \\usepackage{graphicx}\n")
    cat("\\begin{table}[]\n")
    cat(paste0("\\caption{", caption, "}\n"))
    cat(paste0("\\label{", label, "}\n"))
    cat("\\resizebox{\\textwidth}{!}{%\n")
    cat("\\begin{tabular}{@{}", order, "@{}}\n")
    cat("\\toprule\n")
    nn <- names(df)
    # top
    top <- paste0(nn[1:(length(nn) - 1)],
                  rep(" & ", ncol(df) - 1))
    cat(top, nn[length(nn)], "\\\\ \\midrule\n")
    # body
    for(i in 1:(ncol(df) - 1)){
      df[, i] <- paste0(df[, i], " & ")
    }
    df[, ncol(df)] <- paste0(df[, ncol(df)], " \\\\ ")
    df[nrow(df), ncol(df)] <- paste0(df[nrow(df), ncol(df)], " \\bottomrule")
    names(df) <- NULL
    for(i in 1:nrow(df)) cat(unlist(df[i, ]), "\n")
    # bottom
    cat("\\end{tabular}%\n")
    cat("}\n")
    cat("\\end{table}\n")
    sink()
  }

}

