library(MSEtool)
library(dplyr)

################### Data

##### Catch
comm <- readxl::read_excel("Data/Commercial_Catch.xlsx")
rec <- readxl::read_excel("Data/Recreational_Catch.xlsx")
FN <- readxl::read_excel("Data/First_Nation_consumption.xlsx")

catch <- left_join(comm, rec, by = "Year") %>% left_join(FN, by = "Year")
catch <- catch[, c(1, 7, 9, 11)]
names(catch) <- c("Year", "Commerical", "Recreational", "FN")

catch$Commerical[69:88] <- 2 * catch$Commerical[69:88] # Double the commercial catch for 1986-2005
catch$Recreational[92] <- 4.3
catch$FN[91:92] <- 2.8

C_hist <- rowSums(catch[, -1], na.rm = TRUE)
#plot(1918:2009, C_hist, typ = 'o', ylim = c(0, 400))

##### Indices
Index <- readxl::read_excel("Data/Index.xlsx")
I_hist <- as.matrix(Index[, c(2:12)])
I_sd <- as.matrix(Index[, c(13:23)])


# Satisficing with SP_SS
#assess <- lapply(1:10, SP_SS, Data = Hist1@Data, 
#                 start = list(tau = 0.05, sigma = 0.15, dep = 0.9, r_prior = c(0.0465, 0.0173)), use_r_prior = TRUE)
#vapply(assess, slot, numeric(1), "conv")
#vapply(assess, slot, numeric(1), "FMSY")
#hist(vapply(assess, function(x) x@B[length(x@B)]/x@B0, numeric(1)))
#hist(vapply(assess, function(x) x@B[length(x@B)]/x@BMSY, numeric(1)))


################## OM scenarios
M_and_h <- expand.grid(M = seq(0.02, 0.07, 0.01), h = seq(0.6, 0.8, 0.01))


################## Preliminary OM setup
OM <- XL2OM("OM/prelim_YE_OM.xlsx")

OM@nsim <- nrow(M_and_h)
OM@nyears <- 92
OM@proyears <- 100
OM <- Replace(OM, Precise_Unbiased)
OM <- Replace(OM, Perfect_Imp)

CV_mat <- 0.5
sigma_mat <- sqrt(log(CV_mat^2 + 1))
Mat_age <- plnorm(1:OM@maxage, log(17.68), sigma_mat)
OM@cpars$Mat_age <- aperm(array(Mat_age, c(OM@maxage, OM@proyears + OM@nyears, OM@nsim)), c(3, 1, 2))

#V_50 <- 15
#V_95 <- 22
#V <- 1/(1 + exp(-log(19) * (1:OM@maxage - V_50)/(V_95-V_50)))
#OM@cpars$V <- aperm(array(V, c(OM@maxage, OM@proyears + OM@nyears, OM@nsim)), c(3, 1, 2))

OM@Perr <- rep(0.01, 2) # Hist sigmaR
sigma_R_future <- 0.3

### From 2011 assessment
OM@D <- rep(0.872 * 0.123, 2)
#OM@cpars$initD <- rep(0.872, OM@nsim)


OM@cpars$M <- M_and_h$M
OM@cpars$h <- M_and_h$h

M_scenario <- h_scenario <- c("base", "alt")
survey_wt <- c("dw_dogfish", "all_survey")
sel_scenario <- c("high", "low")




#################### Generate reference OMs
subset_cpars <- function(x, ind, nsim) {
  if(length(x) == nsim) return(x[ind])
  if(is.matrix(x)) return(x[ind, ])
  if(is.array(x)) return(x[ind, , ])
  stop("Error")
}


# Sample future rec-devs
set.seed(35)
future_Perr_y <- matrix(rlnorm(OM@nsim * OM@proyears, -0.5 * sigma_R_future^2, sigma_R_future), OM@nsim, OM@proyears)

##### Based on depletion
#I_sd_downweight <- I_sd
#I_sd_downweight[, 1] <- I_sd[, 1] * 1.5

SRA_dep_low <- SRA_scope(OM, Chist = C_hist, Index = I_hist, I_sd = I_sd_downweight, cores = 12, I_type = rep(1, ncol(I_hist)))
#SRA_dep_low <- SRA_scope(OM, Chist = C_hist, Index = I_hist[, -1], I_sd = I_sd[, -1], cores = 12, I_type = rep(1, 10))
SRA_dep_high <- SRA_scope(OM, Chist = C_hist, Index = I_hist, I_sd = I_sd, cores = 12, I_type = rep(1, ncol(I_hist)))

SRA_dep_low@OM@cpars$Perr_y[, 202:301] <- SRA_dep_high@OM@cpars$Perr_y[, 202:301] <- future_Perr_y
SRA_dep_low@OM@cpars$Perr <- SRA_dep_high@OM@cpars$Perr <- rep(sigma_R_future, SRA_dep_low@OM@nsim)

saveRDS(SRA_dep_low, file = "OM/prelim_scoping/SRA_grid_low_dep.rds")
SRA_dep_low <- readRDS("OM/prelim_scoping/SRA_grid_low_dep.rds")
plot(SRA_dep_low, filename = "SRA_grid_low_dep", dir = "OM/prelim_scoping", open_file = FALSE)

saveRDS(SRA_dep_high, file = "OM/prelim_scoping/SRA_grid_high_dep.rds")
SRA_dep_high <- readRDS("OM/prelim_scoping/SRA_grid_high_dep.rds")
plot(SRA_dep_high, filename = "SRA_grid_high_dep", dir = "OM/prelim_scoping", open_file = FALSE)

Hist_low <- runMSE(SRA_dep_low@OM, Hist = TRUE, parallel = TRUE)
Hist_high <- runMSE(SRA_dep_high@OM, Hist = TRUE, parallel = TRUE)

M_and_h$dep_low <- SRA_dep_low@OM@cpars$D / Hist_low@Ref$SSBMSY_SSB0
M_and_h$dep_high <- SRA_dep_high@OM@cpars$D / Hist_high@Ref$SSBMSY_SSB0

ggplot(M_and_h, aes(h, M, fill = dep_low)) + geom_tile()
ggplot(M_and_h, aes(h, M, fill = dep_high)) + geom_tile()

dep_low_matrix <- reshape2::acast(M_and_h, list("M", "h"), value.var = "dep_low")
dep_high_matrix <- reshape2::acast(M_and_h, list("M", "h"), value.var = "dep_high")

write.csv(dep_low_matrix, file = "OM/prelim_scoping/dep_low_matrix.csv")
write.csv(dep_high_matrix, file = "OM/prelim_scoping/dep_high_matrix.csv")

rainbow_M <- rainbow(5, start = 0, end = 1)
rainbow_h <- rainbow(11, start = 0, end = 1)
heatmap(dep_low_matrix, NA, NA, xlab = "h", ylab = "M")


##### Based on selectivity
OM_high <- OM
SRA_sel_high <- SRA_scope(OM_high, Chist = C_hist, Index = I_hist, I_sd = I_sd, cores = 12, I_type = rep(1, ncol(I_hist)))
saveRDS(SRA_sel_high, file = "C:/~/InsideYE/OM/prelim_scoping/SRA_grid_sel_high.rds")
plot(SRA_sel_high, filename = "SRA_grid_sel_high", dir = "C:/~/InsideYE/OM/prelim_scoping", open_file = FALSE)

OM_low <- OM
OM_low@LFS <- c(41, 41)
SRA_sel_low <- SRA_scope(OM_low, Chist = C_hist, Index = I_hist, I_sd = I_sd, cores = 12, I_type = rep(1, ncol(I_hist)))
saveRDS(SRA_sel_low, file = "C:/~/InsideYE/OM/prelim_scoping/SRA_grid_sel_low.rds")
plot(SRA_sel_low, filename = "SRA_grid_sel_low", dir = "C:/~/InsideYE/OM/prelim_scoping", open_file = FALSE)


Hist_low <- runMSE(SRA_sel_low@OM, Hist = TRUE, parallel = TRUE)
Hist_high <- runMSE(SRA_sel_high@OM, Hist = TRUE, parallel = TRUE)

M_and_h$sel_low <- SRA_sel_low@OM@cpars$D / Hist_low@Ref$SSBMSY_SSB0
M_and_h$sel_high <- SRA_sel_high@OM@cpars$D / Hist_high@Ref$SSBMSY_SSB0

M_and_h$sel_low_D <- SRA_sel_low@OM@cpars$D
M_and_h$sel_high_D <- SRA_sel_high@OM@cpars$D

M_and_h$sel_low_refpt <- Hist_low@Ref$SSBMSY_SSB0
M_and_h$sel_high_refpt <- Hist_high@Ref$SSBMSY_SSB0

M_and_h$MSY_low <- Hist_low@Ref$MSY
M_and_h$MSY_high <- Hist_high@Ref$MSY

ggplot(M_and_h, aes(h, M, fill = dep_low)) + geom_tile()
ggplot(M_and_h, aes(h, M, fill = dep_high)) + geom_tile()

dep_low_matrix <- reshape2::acast(M_and_h, list("M", "h"), value.var = "sel_low")
dep_high_matrix <- reshape2::acast(M_and_h, list("M", "h"), value.var = "sel_high")

write.csv(dep_low_matrix, file = "C:/~/InsideYE/OM/prelim_scoping/dep_low_matrix.csv")
write.csv(dep_high_matrix, file = "C:/~/InsideYE/OM/prelim_scoping/dep_high_matrix.csv")


dep_low_matrix <- reshape2::acast(M_and_h, list("M", "h"), value.var = "sel_low_D")
dep_high_matrix <- reshape2::acast(M_and_h, list("M", "h"), value.var = "sel_high_D")

write.csv(dep_low_matrix, file = "C:/~/InsideYE/OM/prelim_scoping/dep_low_matrix_D.csv")
write.csv(dep_high_matrix, file = "C:/~/InsideYE/OM/prelim_scoping/dep_high_matrix_D.csv")


dep_low_matrix <- reshape2::acast(M_and_h, list("M", "h"), value.var = "sel_low_refpt")
dep_high_matrix <- reshape2::acast(M_and_h, list("M", "h"), value.var = "sel_high_refpt")

write.csv(dep_low_matrix, file = "C:/~/InsideYE/OM/prelim_scoping/dep_low_matrix_refpt.csv")
write.csv(dep_high_matrix, file = "C:/~/InsideYE/OM/prelim_scoping/dep_high_matrix_refpt.csv")


dep_low_matrix <- reshape2::acast(M_and_h, list("M", "h"), value.var = "MSY_low")
dep_high_matrix <- reshape2::acast(M_and_h, list("M", "h"), value.var = "MSY_high")

write.csv(dep_low_matrix, file = "C:/~/InsideYE/OM/prelim_scoping/MSY_low.csv")
write.csv(dep_high_matrix, file = "C:/~/InsideYE/OM/prelim_scoping/MSY_high.csv")
