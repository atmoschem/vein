#' creates a .tex a table from a data.frame
#'
#' @description \\code{\\link{to_latex}} reads a data.frme an dgenerates a .tex
#' table, aiming to replicate the method of tablegenerator.com
#'
#' @param df data.frame with three column.
#' @param file Character, name of new .tex file
#' @param caption Character caption of table
#' @param label Character,  label of table
#' @param order Character,  alignment of columns of table, e.g.: "lrr"
#' @return a text file with extension .tex.
#' @importFrom sf st_sf
#' @seealso \\code{\\link{vein_notes}}  \\code{\\link{long_to_wide}}
#' @export
#' @examples {
#' df <- data.frame(pollutant = rep(c("CO", "propadiene", "NO2"), 10),
#' emission = vein::Emissions(1:30),
#' region = rep(letters[1:2], 15))
#' df
#' long_to_wide(df)
#' long_to_wide(df, column_fixed = "region")
#' to_latex(long_to_wide(df, column_fixed = "region"))
#' }
to_latex <- function (df,
                      file = "dataframe.tex",
                      caption = "My table",
                      label = "tab:df",
                      order = paste0("l", rep("r", ncol(df)-1))){
  if(missing(file)){
    cat("% Please add the following required packages to your document preamble:\n")
    writeLines("% \\usepackage{booktabs} \n")
    cat("% \\usepackage{graphicx} \n")
    cat("\\begin{table}[]\n")
    cat("\\begin{table}[]\n")
    cat(paste0("\\caption{", caption, "}\n"))
    cat(paste0("\\label{", label, "}\n"))
    cat("\\resizebox{\\textwidth}{!}{%\n")
    cat(paste0("\\begin{tabular}{@{}", order, "@{}}\n"))
    cat("\\toprule\n")
    nn <- names(df)
    top <- c(paste0(nn[1:(length(nn) - 1)],
                    rep(" & ", ncol(df) - 1)),  nn[length(nn)], "\\\ \\midrule")
    cat(paste0(top, "\n"))
    body <- c(paste0(df[, nn[1:(length(nn) - 1)]],
                     rep(" & ", ncol(df) - 1)),  df[, nn[length(nn)]], "\\\ ")
    body[length(body)] <- paste0(body[length(body)], " \\bottomrule\n")
    cat(paste0(body, "\n"))
    cat("\\end{tabular}%\n")
    cat("}\n")
    cat("\\end{table}\n")
  } else {
    sink(file)
    cat("% Please add the following required packages to your document preamble:\n")
    cat("% \\usepackage{booktabs}\n")
    cat("% \\usepackage{graphicx}\n")
    cat("\\begin{table}[]\n")
    cat("\\begin{table}[]\n")
    cat(paste0("\\caption{", caption, "}\n"))
    cat(paste0("\\label{", label, "}\n"))
    cat("\\resizebox{\\textwidth}{!}{%\n")
    cat(paste0("\\begin{tabular}{@{}", order, "@{}}\n"))
    cat("\\toprule\n")
    nn <- names(df)
    top <- c(paste0(nn[1:(length(nn) - 1)],
                    rep(" & ", ncol(df) - 1)),  nn[length(nn)], "\\\ \\midrule")
    cat(paste0(top, "\n"))
    body <- c(paste0(df[, nn[1:(length(nn) - 1)]],
                     rep(" & ", ncol(df) - 1)),  df[, nn[length(nn)]], "\\\ ")
    body[length(body)] <- paste0(body[length(body)], " \\bottomrule\n")
    cat(paste0(body, "\n"))
    cat("\\end{tabular}%\n")
    cat("}\n")
    cat("\\end{table}\n")
    sink()
  }

}

