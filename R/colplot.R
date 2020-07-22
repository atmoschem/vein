#' Function to plot columns of data.frames
#'
#' \code{\link{colplot}} plots columns of data.frame
#' @param df data.frame.
#' @param cols Character, columns of data.frame.
#' @param xlab a label for the x axis, defaults to a description of x.
#' @param ylab a label for the x axis, defaults to a description of x.
#' @param main Character, a main title for the plot, see also \code{\link{title}}.
#' @param theme Character; "black", "dark", "clean", "ink"
#' @param col The colors for lines and points. Multiple colors can
#' be specified so that each point can be given its own color. If
#' there are fewer colors than points they are recycled in the
#' standard fashion. Default are cptcity colour palette "kst_18_pastels"
#' @param type 1-character string giving the type of plot desired.
#' The following values are possible, for details, see plot: "p" for
#' points, "l" for lines, "b" for both points and lines, "c"
#' for empty points joined by lines, "o" for overplotted points and
#' lines, "s" and "S" for stair steps and "h" for histogram-like vertical
#' lines. Finally, "n" does not produce any points or lines.
#' @param lwd a vector of line widths, see \code{\link{par}}.
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
#' @param familyfont "Character" to specify font, default is"", options "serif",
#'  "sans", "mono" or more according device
#' @param spl numer to control space for legend, default is 5.
#' @param all_values logical, if FALSe shows only positive > 0 values
#' @return a nice plot
#' @note This plot shows values > 0 by default. To plot all values, use all_values = TRUE
#' @importFrom graphics legend par plot points
#' @seealso \code{\link{par}}
#' @export
#' @examples {
#' a <- ef_cetesb("CO", c("PC_G", "PC_FE", "PC_FG"), agemax = 20)
#' colplot(df = a, ylab = "CO [g/km]", theme = "dark", pch = NULL, type = "l")
#' colplot(df = a, cols = "PC_FG", main = "EF", ylab = "CO [g/km]")
#' colplot(df = a, ylab = "CO [g/km]", theme = "clean")
#' }
colplot <- function(df,
                    cols = names(df),
                    xlab = "",
                    ylab = "",
                    main = NULL,
                    theme = "black",
                    col = cptcity::cpt(pal = cptcity::find_cpt("pastel")[4],
                                       n = length(names(df))),
                    type = "b",
                    lwd= 2,
                    pch = 1:ncol(df),
                    familyfont = "",
                    spl = 5,
                    all_values = FALSE) {

  #based on https://github.com/KKPMW/basetheme/blob/master/R/themes.R
  if(theme == "clean") {
    graphics::par(fg              = "black",
        adj             = 0.5,
        ann             = TRUE,
        bg              = "white",
        bty             = "o",
        cex             = 1,
        cex.axis        = 0.7,
        cex.lab         = 0.8,
        cex.main        = 1,
        cex.sub         = 0.8,
        col             = "black",
        col.axis        = "black",
        col.lab         = "black",
        col.main        = "black",
        col.sub         = "black",
        family          = familyfont,
        font            = 1,
        font.axis       = 1,
        font.lab        = 1,
        font.main       = 2,
        font.sub        = 3,
        lab             = c(5, 5, 7),
        las             = 1,
        lend            = "round",
        ljoin           = "round",
        lmitre          = 10,
        lty             = "solid",
        lwd             = 1,
        mgp             = c(2, 0.5, 0),
        pch             = 20,
        tck             = -0.01,
        xaxs            = "r",
        xaxt            = "s",
        xpd             = FALSE,
        yaxs            = "r",
        yaxt            = "s")
  } else if(theme == "ink") {
    graphics::par(fg              = "blue",
        adj             = 0.5,
        ann             = TRUE,
        bg              = "navajowhite",
        bty             = "o",
        cex             = 0.8,
        cex.axis        = 1,
        cex.lab         = 1,
        cex.main        = 1.5,
        cex.sub         = 1,
        col             = "blue",
        col.axis        = "blue",
        col.lab         = "blue",
        col.main        = "blue",
        col.sub         = "blue",
        family          = familyfont,
        font            = 3,
        font.axis       = 3,
        font.lab        = 4,
        font.main       = 2,
        font.sub        = 3,
        lab             = c(5, 5, 7),
        las             = 1,
        lend            = "round",
        ljoin           = "round",
        lmitre          = 10,
        lty             = "dotted",
        lwd             = 2,
        mgp             = c(2, 0.5, 0),
        pch             = 4,
        tck             = -0.01,
        xaxs            = "r",
        xaxt            = "s",
        xpd             = FALSE,
        yaxs            = "r",
        yaxt            = "s")
  } else if(theme == "dark") {
    graphics::par(fg              = "#7E848C",
        adj             = 0.5,
        ann             = TRUE,
        bg              = "#2E3947",
        bty             = "n",
        cex             = 0.8,
        cex.axis        = 1,
        cex.lab         = 1,
        cex.main        = 1.5,
        cex.sub         = 1,
        col             = "#BEBEBE",
        col.axis        = "#7E848C",
        col.lab         = "#BEBEBE",
        col.main        = "#EFF0F2",
        col.sub         = "#737D89",
        family          = familyfont,
        font            = 1,
        font.axis       = 1,
        font.lab        = 2,
        font.main       = 2,
        font.sub        = 3,
        lab             = c(5, 5, 7),
        las             = 1,
        lend            = "round",
        ljoin           = "round",
        lmitre          = 10,
        lty             = 1,
        lwd             = 1,
        mgp             = c(3, 0.7, 0),
        pch             = 19,
        tck             = -0.01,
        xaxs            = "r",
        xaxt            = "s",
        xpd             = FALSE,
        yaxs            = "r",
        yaxt            = "s")
  } else if(theme == "black") {
    graphics::par(fg              = "#7E848C",
        adj             = 0.5,
        ann             = TRUE,
        bg              = "black",
        bty             = "n",
        cex             = 0.8,
        cex.axis        = 1,
        cex.lab         = 1,
        cex.main        = 1.5,
        cex.sub         = 1,
        col             = "white",
        col.axis        = "white",
        col.lab         = "white",
        col.main        = "white",
        col.sub         = "white",
        family          = familyfont,
        font            = 1,
        font.axis       = 1,
        font.lab        = 2,
        font.main       = 2,
        font.sub        = 3,
        lab             = c(5, 5, 7),
        las             = 1,
        lend            = "round",
        ljoin           = "round",
        lmitre          = 10,
        lty             = 1,
        lwd             = 1,
        mgp             = c(3, 0.7, 0),
        pch             = 19,
        tck             = -0.01,
        # xaxs            = "r",
        xaxt            = "s",
        xpd             = FALSE,
        yaxs            = "r",
        yaxt            = "s")
  }

  graphics::par(xpd=T, mar=par()$mar+c(0,0,0,spl))


  if(!missing(cols)) df <- df[cols]

  # removing units to filter positive values
  df <- remove_units(df)
  df[df == 0] <- NA

  ldf <- as.list(df)

# if(!all_values) {
  # for(i in seq_along(df)) ldf[[i]] <- ldf[[i]][ldf[[i]] > 0]
# }

  graphics::plot(ldf[[1]],
       ylim = c(0, max(df, na.rm = TRUE)),
       type = type,
       pch = pch,
       col = col[1],
       ylab = ylab,
       xlab= xlab,
       main = main,
       lwd = lwd)

  if(ncol(df) == 1) {
    graphics::legend(x = length(ldf[[1]])*1.05,
           y = max(ldf[[1]], na.rm = T)*1.04,
           col = col[1],
           legend = names(df),
           pch = pch,
          lwd = lwd)

  } else {
    for(i in 2:ncol(df) ) {
      graphics::points(ldf[[i]],
             type = type,
             pch = pch[i],
             col = col[i],
             lwd = lwd)
    }

    graphics::legend(x = nrow(df)*1.05,
           y = max(unlist(ldf), na.rm = T)*1.04,
           col = col[1:ncol(df)],
           legend = cols,
           pch = pch,
           lwd = lwd)

  }

  # reset par
  graphics::par(
    xlog = FALSE,
    ylog = FALSE,
    adj = 0.5,
    ann = TRUE,
    ask = FALSE,
    bg = "white",
    bty = "o",
    cex = 1,
    cex.axis = 1,
    cex.lab = 1,
    cex.main = 1.2,
    cex.sub = 1,
    col = "black",
    col.axis = "black",
    col.lab = "black",
    col.main = "black",
    col.sub = "black",
    crt = 0,
    err = 0,
    family = "",
    fg = "black",
    fig = c(0, 1, 0, 1),
    fin = c(6.239583, 5.6875),
    font = 1,
    font.axis = 1,
    font.lab = 1,
    font.main = 2,
    font.sub = 1,
    lab = c(5, 5, 7),
    las = 0,
    lend = "round",
    lheight = 1,
    ljoin = "round",
    lmitre = 10,
    lty = "solid",
    lwd = 1,
    mai = c(1.02, 0.82, 0.82, 0.42),
    mar = c(5.1, 4.1, 4.1, 2.1),
    mex = 1,
    mfcol = c(1, 1),
    mfg = rep(1, 4),
    mfrow = c(1, 1),
    mgp = c(3, 1, 0),
    mkh = 0.001,
    new = FALSE,
    oma = c(0, 0, 0, 0),
    omd = c(0, 1, 0, 1),
    omi = rep(0, 4),
    pch = 1,
    pin = c(4.999583, 3.8475),
    plt = c(0.131419, 0.9326878, 0.1793407, 0.8558242),
    ps = 12,
    pty = "m",
    smo = 1,
    srt = 0,
    tck = NA,
    tcl = -0.5,
    usr = c(0, 1, 0, 1),
    xaxp = c(0, 1, 5),
    xaxs = "r",
    xaxt = "s",
    xpd = FALSE,
    yaxp = c(0, 1, 5),
    yaxs = "r",
    yaxt = "s",
    ylbias = 0.2
  )
}
