readM <- function(filename) {
  path_to_matrices <- "C:/Wichmann/Adm/ROOTS/editorial work paper/csv_files/"
  M <- read.csv2(file = paste0(path_to_matrices, filename), header = TRUE, sep = ";", row.names = 1)
  M[] <- as.numeric(as.matrix(M[]))
  M <- as.matrix(M)
  M <- t(M)
  M[is.na(M)]  <- 0
  M <- M[1:10,]
  # class(M) <- c(class(M), "indicator-matrix")
  return(M)
}
source("C:/Wichmann/Adm/ROOTS/editorial work paper/Analyses/pplot_2.R")

Te <- readM("indicatormatrix_TeutonicOrder.csv")
Bo <- readM("indicatormatrix_Bohemia.csv")
Cr <- readM("indicatormatrix_Crete.csv")
He <- readM("indicatormatrix_Hedeby.csv")
Po <- readM("indicatormatrix_Poland.csv")
Ro <- readM("indicatormatrix_Rome.csv")
Ru <- readM("indicatormatrix_Rus.csv")
Sa <- readM("indicatormatrix_Sambia.csv")
Sc <- readM("indicatormatrix_SchleswigHolstein.csv")
Vo <- readM("indicatormatrix_VolgaGermans.csv")

median0 <- function(x){
  w_zero <- which(x==0)
  if (length(w_zero) > 0) {
    x <- x[-w_zero]
  }
  w_na <- which(is.na(x))
  if (length(w_na) > 0) {
    x <- x[-w_na]
  }
  med0 <- median(x)
  if(is.na(med0)) {
    med0 <- 0
  }
  return(med0)
}

# the following two lines produce the elements of Figure 22
meds <- apply(Vo, 1, median0)
pplot_bw(meds)

