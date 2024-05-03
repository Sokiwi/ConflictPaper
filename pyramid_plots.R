## pplot_2() produces a plot of an E-D pyramid in colors and pplot_bw() produces a 
## plot of an E-D pyramid in grayscale
## pplot_2() written by Oliver Nakoinz with contributions by Søren Wichmann
## pplot_bw() written by Oliver Nakoinz

# The most important parameter is b, which is a vector of weights
# No other parameters need to be supplied
# Full descriptions of the parameters follow:

#' @param b num,vec conflict facet vector as produced by `fingerprint()`: c(c1e,c2e,c3e,c4e,c5e,c1d,c2d,c3d,c4d,c5d)
#' @param f num scaling factor
#' @param fx num scaling factor for x-axis (controls ratio of pyramid)
#' @param ax num shift of x axis (for plotting on maps and images)
#' @param ay num shift of x axis (for plotting on maps and images)
#' @param col_e \[1,360\] num hue of color for escalation
#' @param col_d \[1,360\] num hue of color for de-escalation
#' @param l num, light of the color
#' @param add log plot parameter add
#' 
#' ay und ay können als Koordinaten verwendet werden, um den Plot auf einer Karte zu platzieren. Eine Skalierung mit f wird dann sinnvoll sein
#'
#' @return b and plot
#' @export
#' 
#' @author Oliver Nakoinz <oliver.nakoinz@ufg.uni-kiel.de>
#' @md
#' 
#' @examples
#' \dontrun{
#' pplot_2(b = c(0,0,1,0,0,1,0,.5,0,0))
#' }
#' 

pplot_2 <- function(
        b     = c(0,0,0,0,0,0,0,0,0,0), 
        f     = 1, 
        fx    = 0.75, 
        ax    = 0, 
        ay    = 0,
        l     = 30, 
        add   = F)  
{
    if (max(b) > 1){b <- b / max(b)}
    ay <- ay - f*2
    if(add == F){
        par(oma=c(0,0,0,0))  #bltr
        plot(ax+f*fx*5, ay+f*5, 
             col  = hcl(h = 1, c = 1, l = 1, alpha = 0, fixup = TRUE), 
             xlim = c(ax+f*fx*(-5), ax+f*fx*5), 
             ylim = c(ay+f*(0), ay+f*5), 
             xlab = "", 
             ylab = "",
             xaxt = 'n',
             yaxt = 'n',
             bty  = "n") 
    }
    #' color of e1: "#FFC000",
    polygon(  # c1e
        x   = c(ax, ax, ax-f*fx*4, ax-f*fx*5),      # X-Coordinates of polygon 
        y   = c(ay+0, ay+f*1, ay+f*1, ay+0),        # Y-Coordinates of polygon
        col = hcl(h = 57.64601, c = 99.05813, l = 81.27183, alpha = b[1]*1, fixup = TRUE)) 
    #' col_d1 = "#6BA300",
    polygon(  # c1d
        x   = c(ax+0, ax+0, ax+f*fx*4, ax+f*fx*5),  # X-Coordinates of polygon 
        y   = c(ay+0, ay+f*1, ay+f*1, ay+0),        # Y-Coordinates of polygon
        col = hcl(h = 111.7517, c = 76.62559, l= 61.06258, alpha = b[6]*1, fixup = TRUE)) 
    #' color of e2: "#D19C0A",
    polygon(  # c2e
        x   = c(ax, ax, ax-f*fx*3, ax-f*fx*4),      # X-Coordinates of polygon 
        y   = c(ay+f*1, ay+f*2, ay+f*2, ay+f*1),    # Y-Coordinates of polygon
        col = hcl(h = 56.99813, c = 81.99129, l = 67.54417, alpha = b[2]*1, fixup = TRUE)) 
    #' col_d2 = "#1AAA42",
    polygon(  # c2d
        x   = c(ax+0, ax+0, ax+f*fx*3, ax+f*fx*4),  # X-Coordinates of polygon 
        y   = c(ay+f*1, ay+f*2, ay+f*2, ay+f*1),    # Y-Coordinates of polygon
        col = hcl(h = 132.1129, c = 80.51976, l = 61.09842, alpha = b[7]*1, fixup = TRUE)) 
    #' col_e3 = "#ED7D31",
    polygon(  # c3e
        x   = c(ax, ax, ax-f*fx*2, ax-f*fx*3),      # X-Coordinates of polygon 
        y   = c(ay+f*2, ay+f*3, ay+f*3, ay+f*2),    # Y-Coordinates of polygon
        col = hcl(h = 29.90063, c = 105.2027, l = 64.07874, alpha = b[3]*1, fixup = TRUE)) 
    #' col_d3 = "#00B0AE",
    polygon(  # c3d
        x   = c(ax+0, ax+0, ax+f*fx*2, ax+f*fx*3),  # X-Coordinates of polygon 
        y   = c(ay+f*2, ay+f*3, ay+f*3, ay+f*2),    # Y-Coordinates of polygon
        col = hcl(h = 190.2921, c = 51.34858, l = 65.04466, alpha = b[8]*1, fixup = TRUE)) 
    #' col_e4 = "#DE3E0A",
    polygon(  # c4e
        x   = c(ax, ax, ax-f*fx*1, ax-f*fx*2),      # X-Coordinates of polygon 
        y   = c(ay+f*3, ay+f*4, ay+f*4, ay+f*3),    # Y-Coordinates of polygon
        col = hcl(h = 16.96181, c = 135.2722, l = 50.68937, alpha = b[4]*1, fixup = TRUE)) 
    #' col_d4 = "#9DC3E6",
    polygon(  # c4d
        x   = c(ax+0, ax+0, ax+f*fx*1, ax+f*fx*2),  # X-Coordinates of polygon 
        y   = c(ay+f*3, ay+f*4, ay+f*4, ay+f*3),    # Y-Coordinates of polygon
        col = hcl(h = 238.7393, c = 38.873, l = 77.2266, alpha = b[9]*1, fixup = TRUE)) 
    #' col_e5 = "#EA161E",
    polygon(  # c5e
        x   = c(ax, ax, ax-f*fx*0, ax-f*fx*1),      # X-Coordinates of polygon 
        y   = c(ay+f*4, ay+f*5, ay+f*5, ay+f*4),    # Y-Coordinates of polygon
        col = hcl(h = 11.72026, c = 157.5214, l = 49.69672, alpha = b[5]*1, fixup = TRUE)) 
    #' col_d5 = "#2E75B6",
    polygon(  # c5d
        x   = c(ax+0, ax+0, ax+f*fx*0, ax+f*fx*1),  # X-Coordinates of polygon 
        y   = c(ay+f*4, ay+f*5, ay+f*5, ay+f*4),    # Y-Coordinates of polygon
        col = hcl(h = 247.4667, c = 66.40711, l = 47.853, alpha = b[10]*1, fixup = TRUE)) 
    return(b)
}

# As with the previous function the most important parameter is b, 
# which is a vector of weights
# No other parameters need to be supplied

pplot_bw <- function(
    b     = c(0,0,0,0,0,0,0,0,0,0), 
    f     = 1, 
    fx    = 0.75, 
    ax    = 0, 
    ay    = 0,
    col_e = 320,
    col_d = 140,
    col_c = 0,
    l     = 30, 
    add   = F)  
{
  if (max(b) > 1){b <- b / max(b)}
  ay <- ay - f*2
  if(add == F){
    par(oma=c(0,0,0,0))  #bltr
    plot(ax+f*fx*5, ay+f*5, 
         col  = hcl(h = 1, c = 1, l = 1, alpha = 0, fixup = TRUE), 
         xlim = c(ax+f*fx*(-5), ax+f*fx*5), 
         ylim = c(ay+f*(0), ay+f*5), 
         xlab = "", 
         ylab = "",
         xaxt = 'n',
         yaxt = 'n',
         bty  = "n") 
  }
  polygon(  # c1e
    x   = c(ax, ax, ax-f*fx*4, ax-f*fx*5),      # X-Coordinates of polygon 
    y   = c(ay+0, ay+f*1, ay+f*1, ay+0),        # Y-Coordinates of polygon
    col = hcl(h = col_e, c = col_c, l = l, alpha = b[1]*1, fixup = TRUE)) 
  polygon(  # c1d
    x   = c(ax+0, ax+0, ax+f*fx*4, ax+f*fx*5),  # X-Coordinates of polygon 
    y   = c(ay+0, ay+f*1, ay+f*1, ay+0),        # Y-Coordinates of polygon
    col = hcl(h = col_d, c = col_c, l = l, alpha = b[6]*1, fixup = TRUE)) 
  polygon(  # c2e
    x   = c(ax, ax, ax-f*fx*3, ax-f*fx*4),      # X-Coordinates of polygon 
    y   = c(ay+f*1, ay+f*2, ay+f*2, ay+f*1),    # Y-Coordinates of polygon
    col = hcl(h = col_e, c = col_c, l = l, alpha = b[2]*1, fixup = TRUE)) 
  polygon(  # c2d
    x   = c(ax+0, ax+0, ax+f*fx*3, ax+f*fx*4),  # X-Coordinates of polygon 
    y   = c(ay+f*1, ay+f*2, ay+f*2, ay+f*1),    # Y-Coordinates of polygon
    col = hcl(h = col_d, c = col_c, l = l, alpha = b[7]*1, fixup = TRUE)) 
  polygon(  # c3e
    x   = c(ax, ax, ax-f*fx*2, ax-f*fx*3),      # X-Coordinates of polygon 
    y   = c(ay+f*2, ay+f*3, ay+f*3, ay+f*2),    # Y-Coordinates of polygon
    col = hcl(h = col_e, c = col_c, l = l, alpha = b[3]*1, fixup = TRUE)) 
  polygon(  # c3d
    x   = c(ax+0, ax+0, ax+f*fx*2, ax+f*fx*3),  # X-Coordinates of polygon 
    y   = c(ay+f*2, ay+f*3, ay+f*3, ay+f*2),    # Y-Coordinates of polygon
    col = hcl(h = col_d, c = col_c, l = l, alpha = b[8]*1, fixup = TRUE)) 
  polygon(  # c4e
    x   = c(ax, ax, ax-f*fx*1, ax-f*fx*2),      # X-Coordinates of polygon 
    y   = c(ay+f*3, ay+f*4, ay+f*4, ay+f*3),    # Y-Coordinates of polygon
    col = hcl(h = col_e, c = col_c, l = l, alpha = b[4]*1, fixup = TRUE)) 
  polygon(  # c4d
    x   = c(ax+0, ax+0, ax+f*fx*1, ax+f*fx*2),  # X-Coordinates of polygon 
    y   = c(ay+f*3, ay+f*4, ay+f*4, ay+f*3),    # Y-Coordinates of polygon
    col = hcl(h = col_d, c = col_c, l = l, alpha = b[9]*1, fixup = TRUE)) 
  polygon(  # c5e
    x   = c(ax, ax, ax-f*fx*0, ax-f*fx*1),      # X-Coordinates of polygon 
    y   = c(ay+f*4, ay+f*5, ay+f*5, ay+f*4),    # Y-Coordinates of polygon
    col = hcl(h = col_e, c = col_c, l = l, alpha = b[5]*1, fixup = TRUE)) 
  polygon(  # c5d
    x   = c(ax+0, ax+0, ax+f*fx*0, ax+f*fx*1),  # X-Coordinates of polygon 
    y   = c(ay+f*4, ay+f*5, ay+f*5, ay+f*4),    # Y-Coordinates of polygon
    col = hcl(h = col_d, c = col_c, l = l, alpha = b[10]*1, fixup = TRUE)) 
  return(b)
}
