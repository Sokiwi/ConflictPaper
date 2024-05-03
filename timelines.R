## This script produces time line illustrations#
## Written by Søren Wichmann with contributions by Andre Santamaria and Oliver Nakoinz

library(ggplot2)

# filenames:
# "indicatormatrix_Bohemia.csv"
# "indicatormatrix_Crete.csv"
# "indicatormatrix_Hedeby.csv" 
# "indicatormatrix_Poland.csv" 
# "indicatormatrix_Rome.csv"
# "indicatormatrix_Rus.csv" 
# "indicatormatrix_Sambia.csv" 
# "indicatormatrix_SchleswigHolstein.csv" 
# "indicatormatrix_TeutonicOrder.csv"
# "indicatormatrix_VolgaGermans.csv"

# run this script by assigning a file names to filename as shown here,
# and then just execute the remaining code
# filename <- "indicatormatrix_VolgaGermans.csv"

# the function reads csv file and returns a matrix
# you should change the path so that it corressponds to where it is on your computer
# the csv files are at https://github.com/Sokiwi/ConflictPaper and just need to
# be downloaded and unzipped
readM <- function(filename) {
  path_to_matrices <- "C:/Wichmann/Adm/ROOTS/editorial work paper/csv_files/"
  M <- read.csv2(file = paste0(path_to_matrices, filename), header = TRUE, sep = ";", row.names = 1)
  M[] <- as.numeric(as.matrix(M[]))
  M <- as.matrix(M)
  M <- t(M)
  M[M==0]  <- NA
  return(M)
}

# relevant data for plotting is extracted
plotdata <- readM(filename)
Event <- colnames(plotdata)
Year_start <- as.vector(as.numeric(plotdata["from",]))
Year_end <- as.vector(as.numeric(plotdata["to",]))

# the pyramid cell color with the greatest weight will be plotted, but
# in case of ties the one which is highest on pyramid, where the cells
# and their "height" assignment is: 
# c1e=1, c2e=2, c3e=3, c4e=4, c5e=5, c1d=1, c2d=2, c3d=3, c4d=4, c5d=5
ph <- c(1,2,3,4,5,1,2,3,4,5)  # stands for pyramid_hierarchy
# if two cells that rank highest in terms of weight have equal weight
# and belong to either side of the pyramd (one to escalation, 
# one to de-escalation), then a grey color is used whose color spans a 
# spectrum from light to dark gray
grayscale <- c("#E5E4E2", "#D3D3D3", "#C0C0C0", "#A9A9A9", "#808080")
pyramid_colors <- c("#FFC000", "#D19C0A", "#ED7D31", "#DE3E0A", "#EA161E", 
                    "#6BA300", "#1AAA42", "#00B0AE", "#9DC3E6","#2E75B6")
all_colors <- c(pyramid_colors, grayscale)

# function for selecting the pyramid cell color to display in case of ties
# takes the c1e-c5d cells ("cells") as input
tr <- function(cells) {  # stands for tie resolution
  cells <- as.numeric(cells)
  w_max <- which(cells==max(cells, na.rm=TRUE))
  # if there is only one cell with the maximal value this determines the color
  if (length(w_max)==1) {
    Number <- w_max
  }
  # if there is more than one cell with the maximal value but only one belongs 
  # to the highest level in the pyramid, this determines the color
  if (length(w_max) > 1) {
    hier_values <- ph[w_max]
    if (length(which(hier_values==max(hier_values)))==1) {
      Number <- w_max[which(hier_values==max(hier_values))]
    }
    # if there is more than one cell with the maximal value and two belong to
    # the (same) highest level in the pyramid, a grayscale color corresponding to
    # the level in the pyramid is chosen
    if (length(which(hier_values==max(hier_values)))>1) {
      max_hier_values <- max(hier_values)
      Number <- 10 + ph[which(ph==max_hier_values)][1]
    }
  }
  return(Number)
}

# now assign the numbers that correspond to pyramid cell colors to each indicator
Numbers <- c()
for (i in 1:ncol(plotdata)) {
  cells <- plotdata[1:10,i]
  Numbers[i] <- tr(cells)
}

# make a dataframe of the indicator data to be plotted
events <- data.frame(Event, Year_start, Year_end, Numbers)
reorder_guide <- c(1,2,3,4,5,15,14,13,12,11,6,7,8,9,10)
reordered <- reorder_guide[Numbers]
events2 <- data.frame(events, reordered)
events2 <- events2[order(reordered),]
reordered2 <- 1:nrow(events2)
events2 <- data.frame(events2, reordered2)
rm(Numbers)

# Order the events (indicators) such that they go from the lower escalation level
# to the highest escalation level, then through grayscale cases (i.e., cases 
# where the highest value is found both at the escalation and de-escalation
# levels, and finally from the top of de-escalation to the bottom of 
# de-escalation; this corresponds to a pyramid tipped 90° to the right, 
# with grayscales sandwiched in the middle

ggplot(events2, aes(x = Year_start, xmin = min(Year_start), xmax = max(Year_end), y = reorder(Event, -reordered2))) +
  geom_point(size = 4, colour = all_colors[events2$Numbers]) +
  geom_segment(aes(xend = Year_end, yend = Event), size = 5, colour = all_colors[events2$Numbers]) +
  geom_point(aes(x = Year_end), size = 4, colour = all_colors[events2$Numbers]) +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(text=element_text(size=15)) +
  xlab("") + 
  ylab("")
