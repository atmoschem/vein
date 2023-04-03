#' function to add a scale to a image plot
#'
#' @description method to plot a scale in image plot.
#'
#' @param z matrix or vector
#' @param zlim z limit
#' @param col color
#' @param breaks interval for the tickmarks
#' @param horiz TRUE (default) to a horizontal scale
#' @param xlim x limit
#' @param ylim y limitS
#' @param ... other arguments to plot
#' @importFrom graphics par axis polygon
#' @importFrom grDevices heat.colors
#'
#' @examples \dontrun{
#' mat <- matrix(100:1,ncol = 10, byrow = F)
#' cor <- grDevices::heat.colors(100)
#' image(mat,axe = FALSE, main = "numbers from 1 to 100", col = cor)
#' axis(2)
#' addscale(mat, col = cor)
#' }
#' @export

## Function to plot color bar
## from https://menugget.blogspot.com/2011/08/adding-scale-to-image-plot.html#more
addscale <- function(z,
                     zlim   = range(z,na.rm = TRUE),
                     col    = grDevices::heat.colors(12),
                     breaks = pretty(zlim),
                     horiz  = TRUE,
                     ylim   = NULL,
                     xlim   = NULL,
                     ...){

  ## adapted from https://github.com/dnychka/fieldsRPackage/blob/main/fields/R/image.family.R
  imageplot.setup <- function(x, add = FALSE, legend.shrink = 0.9,
                              legend.width = 1, horizontal = FALSE,
                              legend.mar = NULL, smallplot = NULL, ...) {
    old.par <- par(no.readonly = TRUE)

    if (is.null(legend.mar)) {
      legend.mar <- ifelse(horizontal, 3.1, 5.1)
    }
    # compute how big a text character is
    char.size <- ifelse(horizontal, par()$cin[2]/par()$din[2],
                        par()$cin[1]/par()$din[1])
    # This is how much space to work with based on setting the margins in the
    # high level par command to leave between strip and big plot
    offset <- char.size * ifelse(horizontal, par()$mar[1], par()$mar[4])
    # this is the width of the legned strip itself.
    legend.width <- char.size * legend.width
    # this is room for legend axis labels
    legend.mar <- legend.mar * char.size
    # smallplot is the plotting region for the legend.
    if (is.null(smallplot)) {
      smallplot <- old.par$plt
      if (horizontal) {
        smallplot[3] <- legend.mar
        smallplot[4] <- legend.width + smallplot[3]
        pr <- (smallplot[2] - smallplot[1]) * ((1 - legend.shrink)/2)
        smallplot[1] <- smallplot[1] + pr
        smallplot[2] <- smallplot[2] - pr
      }
      else {
        smallplot[2] <- 1 - legend.mar
        smallplot[1] <- smallplot[2] - legend.width
        pr <- (smallplot[4] - smallplot[3]) * ((1 - legend.shrink)/2)
        smallplot[4] <- smallplot[4] - pr
        smallplot[3] <- smallplot[3] + pr
      }
    }
    return(smallplot)
  }

  if(!missing(breaks)){
    if(length(breaks) != (length(col)+1)){stop("must have one more break than colour")}
  }
  if(missing(breaks) & !missing(zlim)){
    breaks <- seq(zlim[1], zlim[2], length.out=(length(col)+1))
  }
  if(missing(breaks) & missing(zlim)){
    zlim    <- range(z, na.rm=TRUE)
    zlim[2] <- zlim[2]+c(zlim[2]-zlim[1])*(1E-3)
    zlim[1] <- zlim[1]-c(zlim[2]-zlim[1])*(1E-3)
    breaks  <- seq(zlim[1], zlim[2], length.out=(length(col)+1))
  }
  poly <- vector(mode="list", length(col))
  for(i in seq(poly)){
    poly[[i]] <- c(breaks[i], breaks[i+1], breaks[i+1], breaks[i])
  }
  xaxt <- ifelse(horiz, "s", "n")
  yaxt <- ifelse(horiz, "n", "s")
  if(horiz){YLIM<-c(0,1); XLIM<-range(breaks)}
  if(!horiz){YLIM<-range(breaks); XLIM<-c(0,1)}
  if(missing(xlim)) xlim=XLIM
  if(missing(ylim)) ylim=YLIM

  old.par <- par(no.readonly = TRUE)
  on.exit(par(old.par))

  smallplot <- imageplot.setup(legend.width = 1.2,horizontal = TRUE)

  par(new = TRUE, pty = "m", plt = smallplot, err = -1)

  plot(NA,t="n",ylim=ylim, xlim=xlim, xaxt=xaxt, yaxt=yaxt,
       xaxs="i", yaxs="i",
       xlab = "",ylab = "",
       ...)
  for(i in seq(poly)){
    if(horiz){
      polygon(poly[[i]], c(0,0,1,1), col=col[i], border=NA)
    }
    if(!horiz){
      polygon(c(0,0,1,1), poly[[i]], col=col[i], border=NA)
    }
  }
}
