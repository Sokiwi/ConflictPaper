## Analyze time spans for different indicators
# routines for getting normalized time spans for different cases and indicators
pyramid_cells <- c("c1e", "c2e", "c3e", "c4e", "c5e", "c1d", "c2d", "c3d", "c4d", "c5d")
cases <- c("Bohemia", "Crete", "Hedeby", "Poland", "Rome", "Rus", "Sambia", "SchleswigHolstein", "TeutonicOrder", "VolgaGermans")
tss <- matrix(NA, nrow=10, ncol=10)  # stands for time spans
colnames(tss) <- pyramid_cells
rownames(tss) <- cases

read_tss <- function(filename) {
  path_to_matrices <- "C:/Wichmann/Adm/ROOTS/editorial work paper/PLOS ONE 2nd submission/"
  M <- read.csv2(file = paste0(path_to_matrices, filename), header = TRUE, sep = ";", row.names = 1)
  M[] <- as.numeric(as.matrix(M[]))
  M <- as.matrix(M)
  M <- M[,c(c(1:12))]
  for (i in 1:nrow(M)) {
    for (j in 1:10) {
      if (!is.na(M[i,j])) {
        if (M[i,j]==0) {
          M[i,j] <- NA
        }
      }
    }
  }
  # remove rows for which time spans are not available
  w_from <- which(is.na(M[,"from"]))
  w_to <- which(is.na(M[,"to"]))
  w_from_to <- unique(c(w_from, w_to))
  if (length(w_from_to) > 0) {
    M <- M[-w_from_to,]
  }
  min_from <- min(M[,"from"])
  max_to <- max(M[,"to"])
  normalization_factor <- max_to - min_from
  cat("normalization factor:", normalization_factor, "\n")
  absolute_span <- M[,"to"] - M[,"from"]
  # punctuational events are assigned a 1-year span
  absolute_span[absolute_span==0] <- 1
  norm_span <- round(100*absolute_span/normalization_factor,2)
  M <- cbind(M,norm_span)
  return(M)
}

Bo <- read_tss("indicatormatrix_Bohemia.csv")
Cr <- read_tss("indicatormatrix_Crete.csv")
He <- read_tss("indicatormatrix_Hedeby.csv")
Po <- read_tss("indicatormatrix_Poland.csv")
Ro <- read_tss("indicatormatrix_Rome.csv")
Ru <- read_tss("indicatormatrix_Rus.csv")
Sa <- read_tss("indicatormatrix_Sambia.csv")
Sc <- read_tss("indicatormatrix_SchleswigHolstein.csv")
Te <- read_tss("indicatormatrix_TeutonicOrder.csv")
Vo <- read_tss("indicatormatrix_VolgaGermans.csv")

ts_all <- rbind(Bo,Cr,He,Po,Ro,Ru,Sa,Sc,Te,Vo)

# make vectors for each pyramid cell with all the time spans across cases
c1e_spans <- c()
c2e_spans <- c()
c3e_spans <- c()
c4e_spans <- c()
c5e_spans <- c()
c1d_spans <- c()
c2d_spans <- c()
c3d_spans <- c()
c4d_spans <- c()
c5d_spans <- c()
spans <- list(c1e_spans,c2e_spans,c3e_spans,c4e_spans,c5e_spans,c1d_spans,c2d_spans,c3d_spans,c4d_spans,c5d_spans)

for (i in 1:10) {
  counter <- 0
  for (j in 1:nrow(ts_all)) {
    if(!is.na(ts_all[j,i])) {
      counter <- counter + 1
      spans[[i]][counter] <- ts_all[j,13]
    }
  }
}

# look at the distribution of the individual spans
plot(density(spans[[1]]),xlim=c(0,100),ylim=c(0,0.04))
for (i in 2:9) {
  lines(density(spans[[i]]),xlim=c(0,100),ylim=c(0,0.04))
}

mean_spans <- as.vector(unlist(lapply(spans,mean)))[1:9]
cor.test(mean_spans[1:5],c(1:5))  # r = -0.891, p = 0.0426
cor.test(mean_spans[6:9],c(1:4))  # r = -0.890, p = 0.1102

cor.test(mean_spans[1:9],c(1,2,3,4,5,1,2,3,4))  # r = -0.887, p = p-value = 0.0014

# rounded numbers for the purpose of an illustration
round(mean_spans,1)

median_spans <- as.vector(unlist(lapply(spans,median)))[1:9]


## take means of confidence scores
## and study their distribution
readM <- function(filename) {
  path_to_matrices <- "C:/Wichmann/Adm/ROOTS/editorial work paper/PLOS ONE 2nd submission/"
  M <- read.csv2(file = paste0(path_to_matrices, filename), header = TRUE, sep = ";", row.names = 1)
  M[] <- as.numeric(as.matrix(M[]))
  M <- as.matrix(M)
  M <- t(M)
  M[is.na(M)]  <- 0
  M <- M[1:10,]
  # class(M) <- c(class(M), "indicator-matrix")
  return(M)
}

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

css <- list(Bo, Cr, He, Po, Ro, Ru, Sa, Sc, Te, Vo)

# function for taking means ignoring zeros
mean0 <- function(b){
  b[b==0]  <- NA
  ave0 <- mean(b, na.rm=TRUE)
  return(ave0)
}

# applying Shapiro-Wilk test to check for normal distribution
# css is a list of the 10 case study indicator matrices
ps <- c()  # vector to hold p-values; greater than 0.05 means normal distribution
ls <- c()  # vector to hold lengths of the data vectors; too short is unreliable
counter <- 0
for (i in 1:length(css)) {
  for (j in 1:nrow(css[[i]])) {
    v <- css[[i]][j,]
    w_zeros <- as.vector(which(v==0))
    if (length(w_zeros) > 0) {
      v <- v[-w_zeros]
    }
    w_nas <- as.vector(which(is.na(v)))
    if (length(w_nas) > 0) {
      v <- v[-w_nas]
    }
    # checking for two conditions of the test:
    # there must be 3 or more elements and they cannot all be identical
    if (length(v) >= 3 & length(table(v)) > 1) {
      counter <- counter + 1
      ps[counter] <- round(shapiro.test(v)$p.value, 5)
      ls[counter] <- length(v)
    }
  }
}
ps_ls <- data.frame(ps, ls)
write.table(ps_ls, file="ps_ls_new.txt", sep="\t", quote=FALSE, row.names = FALSE)

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

## routines leading to the plot in Figure 20
## plotting the amount of escalation against the amount of de-escalation
cases_abb <- c("Bo", "Cr", "He", "Po", "Ro", "Ru", "Sa", "Sc", "Te", "Vo")
pyramid_cells <- c("c1e", "c2e", "c3e", "c4e", "c5e", "c1d", "c2d", "c3d", "c4d", "c5d")
m <- matrix(NA, nrow=10, ncol=10)
rownames(m) <- cases
colnames(m) <- pyramid_cells

for (i in 1:length(css)) {
  m[i,] <- apply(css[[i]], 1, median0)
}
lw <- c(1,2,3,4,5,1,2,3,4,5)  # stands for levels weights
m2 <- t(apply(m, 1, function(x) x*lw))

escal <- apply(m2[,c(1:5)], 1, mean0)
deescal <- apply(m2[,c(6:9)], 1, mean0)
ed_means <- data.frame(escal, deescal)
plot(deescal,escal, ylab="Weighted means escalation cells", xlab="Weighted means de-escalation cells", xlim=c(0.8,2.8), ylim=c(0.8,2.8))
text(deescal,escal + .1, labels = cases, cex=1)
abline(coef = c(0, 1),
       col = "red",
       lwd = 1)

## routines leading to the plot in Figure 21
cases <- c("Bohemia", "Crete", "Hedeby", "Poland", "Rome", "Rus", "Sambia", "Schleswig-Holstein", "Teutonic Order", "Volga Germans")


Escalation <- c()
deEscalation <- c()

for (i in 1:length(css)) {
  med <- apply(css[[i]], 1, median0)
  w_zero <- which(med==0)
  if (length(w_zero) > 0) {
    med[w_zero] <- NA
  }
  spearman_esc <- cor.test(med[1:5], 1:5, method="spearman")
  rho_esc <- spearman_esc$estimate
  Escalation[i] <- rho_esc
  if (is.na(rho_esc)) {
    rho_esc <- 0
  }
  spearman_deesc <- cor.test(med[6:9], 6:9, method="spearman")
  rho_deesc <- spearman_deesc$estimate
  if (is.na(rho_deesc)) {
    rho_deesc <- 0
  }
  deEscalation[i] <- rho_deesc
}

# plot(Escalation, deEscalation, pch=20, ylim=c(-1.1,1.1), xlim=c(-1.1,1.1), xlab="Escalation", ylab="De-escalation")
# text(Escalation, deEscalation + .08, cases)
# abline(h=0, col="red")
# abline(v=0, col="red")

plot(Escalation, deEscalation, pch=20, ylim=c(-1.1,1.1), xlim=c(-1.1,1.1), 
     xlab="Correlation escalation levels ~ confidence scores", 
     ylab="Correlation de-escalation levels ~ confidence scores")
text(Escalation, deEscalation + .08, cases)
abline(h=0, col="red")
abline(v=0, col="red")

