#' Function to plot columns of data.frames
#'
#' \code{\link{colplot}} plots columns of data.frame
#' @param df data.frame.
#' @param cols Character, columns of data.frame.
#' @param ylim Numeric, numeric vectors of length 2, giving the x and y coordinates ranges.
#' @param xlim Numeric, numeric vectors of length 2, giving the x and y coordinates ranges.
#' @param main Character, a main title for the plot, see also \code{\link{title}}.
#' @param type 1-character string giving the type of plot desired.
#' The following values are possible, for details, see plot: "p" for
#' points, "l" for lines, "b" for both points and lines, "c"
#' for empty points joined by lines, "o" for overplotted points and
#' lines, "s" and "S" for stair steps and "h" for histogram-like vertical
#' lines. Finally, "n" does not produce any points or lines.
#' @param pch plotting ‘character’, i.e., symbol to use. This can either be a
#' single character or an integer code for one of a set of graphics symbols.
#' The full set of S symbols is available with pch = 0:18, see the
#' examples below. (NB: R uses circles instead of the octagons used in S.).
#' Value pch = "." (equivalently pch = 46) is handled specially.
#' It is a rectangle of side 0.01 inch (scaled by cex). In addition,
#' if cex = 1 (the default), each side is at least one pixel
#' (1/72 inch on the pdf, postscript and xfig devices).
#' For other text symbols, cex = 1 corresponds to the default fontsize
#' of the device, often specified by an argument pointsize.
#' For pch in 0:25 the default size is about 75% of
#' the character height (see par("cin")).
#' @param col The colors for lines and points. Multiple colors can
#' be specified so that each point can be given its own color. If
#' there are fewer colors than points they are recycled in the
#' standard fashion. Lines will all be plotted in the first colour specified.
#' future. Currently the only value is "constant"
#' @param colaxes Character; "white" or "black".
#' @param bg a vector of background colors for open plot symbols, see
#' points. Note: this is not the same setting as par("bg").
#' @param spl numer to control space for legend, default is 8.
#' @return a plot
#' @export
#' @examples \dontrun{
#' a <- ef_cetesb("CO", c("PC_G", "PC_FE"), agemax = 10)
#' colplot(a, cols = names(a))
#' colplot(a, cols = names(a), main = "title")
#' }
colplot <- function(df,
                    cols,
                    xlab = NULL,
                    ylab = NULL,
                    main = NULL,
                    ylim = c(0, max(df)),
                    type = "b",
                    pch = 16,
                    col = cptcity::cpt(pal = cptcity::find_cpt("pastel")[4],
                                       n = length(a)),
                    colaxes = "black",
                    bg = "white",
                    spl = 8) {
  if(!colaxes %in% c("white", "black")) {
    stop("colaxes only black or white")
  }
  par(xpd=T, mar=par()$mar+c(0,0,0, spl))
  plot(df[[1]],
       ylim = ylim,
       type = type,
       pch = pch,
       col = col[1],
       axes = FALSE,
       bg = bg)

  title(main = main,
        xlab = xlab,
        ylab = ylab,
        col.lab = colaxes,
        col.main = colaxes)

  axis(side = 1,
       col = colaxes,
       col.axis = colaxes)

  axis(side = 2,
       col = colaxes,
       col.axis = colaxes)

  for(i in 2:length(cols)) {
    points(df[[i]],
           type = type,
           pch = pch,
           col = col[i])
  }

  legend(x = nrow(df)*1.05,
         y = max(df)*1.04,
         col = col[1:length(cols)],
         legend = cols,
         pch = pch,
         bg = bg)
  par(mar=c(5, 4, 4, 2) + 0.1) # default
}
