library(MSEtool)
library(dplyr)
library(reshape2)

SRA_data <- readRDS("mse/scoping/SRA_data.rds")


################## OM setup
OM <- XL2OM("mse/OM/YE_OM.xlsx")
OM@CurrentYr <- 2019
OM@nsim <- 2
OM@nyears <- length(SRA_data$Year)
OM@maxage <- SRA_data$maxage
OM@proyears <- 100
OM <- Replace(OM, Precise_Unbiased)
OM <- Replace(OM, Perfect_Imp)
OM@Perr <- c(0.4, 0.4)
#OM@cpars$plusgroup <- rep(1L, OM@nsim)

# Maturity
Mat_age <- ifelse(1:OM@maxage <= 7, 0, 1/(1 + exp(-log(19) * (1:OM@maxage - 14.4)/(27.4-14.4))))
OM@cpars$Mat_age <- array(Mat_age, c(OM@maxage, OM@nyears + OM@proyears, OM@nsim)) %>% aperm(perm = c(3, 1, 2))

OM_condition <- OM



################# Run SRA_scope
# Base
SRA <- SRA_scope(OM_condition, Chist = SRA_data$Chist, Index = SRA_data$Index, I_sd = SRA_data$I_sd, I_type = SRA_data$I_type,
                 condition = "catch2", plusgroup = FALSE,
                 selectivity = rep("logistic", 2), s_selectivity = rep("logistic", 5), length_bin = 0.1 * SRA_data$length_bin, cores = 2, #mean_fit = TRUE,
                 s_CAA = SRA_data$s_CAA, vul_par = SRA_data$vul_par, map_s_vul_par = SRA_data$map_s_vul_par,
                 map_log_rec_dev = SRA_data$map_log_rec_dev)
saveRDS(SRA, file = "mse/scoping/no_plusgroup/SRA_regwt_dogfish.rds")
SRA <- readRDS("mse/scoping/no_plusgroup/SRA_regwt_dogfish.rds")
plot(SRA, file = "mse/scoping/no_plusgroup/SRA_regwt_dogfish", dir = getwd(), open_file = FALSE, f_name = SRA_data$f_name, s_name = SRA_data$s_name,
     MSY_ref = c(0.4, 0.8))

# Upweight dogfish with lambdas
SRA <- SRA_scope(OM_condition, Chist = SRA_data$Chist, Index = SRA_data$Index, I_sd = SRA_data$I_sd, I_type = SRA_data$I_type,
                 condition = "catch2", plusgroup = FALSE,
                 selectivity = rep("logistic", 2), s_selectivity = rep("logistic", 5), length_bin = 0.1 * SRA_data$length_bin, cores = 2, mean_fit = TRUE,
                 s_CAA = SRA_data$s_CAA, LWT = list(Index = c(1, 4, 1, 1, 1)),
                 vul_par = SRA_data$vul_par, map_s_vul_par = SRA_data$map_s_vul_par,
                 map_log_rec_dev = SRA_data$map_log_rec_dev)
saveRDS(SRA, file = "mse/scoping/no_plusgroup/SRA_upweight_dogfish.rds")
SRA <- readRDS("mse/scoping/no_plusgroup/SRA_upweight_dogfish.rds")
plot(SRA, file = "mse/scoping/no_plusgroup/SRA_upweight_dogfish", dir = getwd(), open_file = FALSE, f_name = SRA_data$f_name, s_name = SRA_data$s_name,
     MSY_ref = c(0.4, 0.8))

# Remove commercial CPUE
SRA <- SRA_scope(OM_condition, Chist = SRA_data$Chist, Index = SRA_data$Index, I_sd = SRA_data$I_sd, I_type = SRA_data$I_type,
                 selectivity = rep("logistic", 2), s_selectivity = rep("logistic", 5),  length_bin = 0.1 * SRA_data$length_bin, cores = 2, mean_fit = TRUE,
                 s_CAA = SRA_data$s_CAA, LWT = list(Index = c(1, 1, 0, 0, 0)),
                 vul_par = SRA_data$vul_par, map_s_vul_par = SRA_data$map_s_vul_par,
                 map_log_rec_dev = SRA_data$map_log_rec_dev)
saveRDS(SRA, file = "mse/scoping/SRA_no_CPUE.rds")
SRA <- readRDS("mse/scoping/SRA_no_CPUE.rds")
plot(SRA, file = "mse/scoping/SRA_no_CPUE", dir = getwd(), compare = FALSE, open_file = FALSE, f_name = SRA_data$f_name, s_name = SRA_data$s_name, MSY_ref = c(0.4, 0.8))

# Upweight HBLL
SRA <- SRA_scope(OM_condition, Chist = SRA_data$Chist, Index = SRA_data$Index, I_sd = SRA_data$I_sd, I_type = SRA_data$I_type,
                 selectivity = rep("logistic", 2), s_selectivity = rep("logistic", 5), length_bin = 0.1 * SRA_data$length_bin, cores = 2, mean_fit = TRUE,
                 s_CAA = SRA_data$s_CAA, LWT = list(Index = c(4, 1, 1, 1, 1)),
                 vul_par = SRA_data$vul_par, map_s_vul_par = SRA_data$map_s_vul_par,
                 map_log_rec_dev = SRA_data$map_log_rec_dev)
saveRDS(SRA, file = "mse/scoping/SRA_upweight_HBLL.rds")
SRA <- readRDS("mse/scoping/SRA_upweight_HBLL.rds")
plot(SRA, file = "mse/scoping/SRA_upweight_HBLL", dir = getwd(), compare = FALSE, open_file = FALSE, f_name = SRA_data$f_name, s_name = SRA_data$s_name)

# Fix survey selectivity
s_vul_par <- matrix(c(46.56615, 30.56082, 0.99), 3, 5)
map_s_vul_par <- matrix(NA, 3, 5)

SRA <- SRA_scope(OM_condition, Chist = SRA_data$Chist, Index = SRA_data$Index, I_sd = SRA_data$I_sd, I_type = SRA_data$I_type,
                 selectivity = rep("logistic", 2), s_selectivity = rep("logistic", 5), length_bin = 0.1 * SRA_data$length_bin, cores = 2, mean_fit = TRUE,
                 s_CAA = SRA_data$s_CAA, LWT = list(s_CAA = 0),
                 vul_par = SRA_data$vul_par, s_vul_par = s_vul_par, map_s_vul_par = map_s_vul_par,
                 map_log_rec_dev = rep(NA, OM_condition@nyears))
saveRDS(SRA, file = "mse/scoping/SRA_fix_HBLL_sel.rds")
SRA <- readRDS("mse/scoping/SRA_fix_HBLL_sel.rds")
plot(SRA, file = "mse/scoping/SRA_fix_HBLL_sel", dir = getwd(), open_file = FALSE, f_name = SRA_data$f_name, s_name = SRA_data$s_name,
     MSY_ref = c(0.4, 0.8))


##### Compare all these fits
s1 <- readRDS("mse/scoping/SRA_regwt_dogfish.rds")
s2 <- readRDS("mse/scoping/SRA_upweight_dogfish.rds")
s3 <- readRDS("mse/scoping/SRA_upweight_HBLL.rds")
s4 <- readRDS("mse/scoping/SRA_fix_HBLL_sel.rds")
s5 <- readRDS("mse/scoping/SRA_no_CPUE.rds")
MSEtool:::compare_SRA(s1, s2, s3, s4, s5,
            filename = "mse/scoping/compare_SRA", dir = getwd(), open_file = FALSE, f_name = SRA_data$f_name, s_name = SRA_data$s_name,
            MSY_ref = c(0.4, 0.8), scenario = list(names = c("Base", "Up. dogfish", "Up. HBLL", "Fix HBLL sel", "No CPUE"),
                                                   col = gplots::rich.colors(5)))






# Profile over sigmaR
OM_condition@cpars$Perr <- seq(0.2, 0.6, 0.025)
OM_condition@nsim <- length(OM_condition@cpars$Perr)
Mat_age <- ifelse(1:OM@maxage <= 7, 0, 1/(1 + exp(-log(19) * (1:OM@maxage - 14.4)/(27.4-14.4))))
OM_condition@cpars$Mat_age <- array(Mat_age, c(OM@maxage, OM@nyears + OM@proyears, OM_condition@nsim)) %>%
  aperm(perm = c(3, 1, 2))

SRA <- SRA_scope(OM_condition, Chist = SRA_data$Chist, Index = SRA_data$Index, I_sd = SRA_data$I_sd, I_type = SRA_data$I_type,
                 selectivity = rep("logistic", 2), s_selectivity = rep("logistic", 5), length_bin = 0.1 * SRA_data$length_bin, cores = 12,
                 s_CAA = SRA_data$s_CAA,
                 vul_par = SRA_data$vul_par, map_s_vul_par = SRA_data$map_s_vul_par,
                 map_log_rec_dev = SRA_data$map_log_rec_dev)
saveRDS(SRA, file = "mse/scoping/SRA_profile_sigmaR.rds")
SRA <- readRDS("mse/scoping/SRA_profile_sigmaR.rds")
plot(SRA, file = "mse/scoping/SRA_profile_sigmaR", dir = getwd(), compare = FALSE, open_file = FALSE, f_name = SRA_data$f_name, s_name = SRA_data$s_name,
     MSY_refs = c(0.4, 0.8))
nll <- vapply(SRA@Misc, function(x) x$nll, numeric(1))
plot(OM_condition@cpars$Perr, nll)





###### Grid over M and steepness
LH_grid <- expand.grid(M = seq(0.02, 0.06, 0.01), h = seq(0.65, 0.75, 0.01))

OM_condition <- OM
OM_condition@nsim <- nrow(LH_grid)
OM_condition@cpars$M <- LH_grid$M
OM_condition@cpars$h <- LH_grid$h

# Maturity
Mat_age <- ifelse(1:OM@maxage <= 7, 0, 1/(1 + exp(-log(19) * (1:OM@maxage - 14.4)/(27.4-14.4))))
OM_condition@cpars$Mat_age <- array(Mat_age, c(OM@maxage, OM@nyears + OM@proyears, OM_condition@nsim)) %>% aperm(perm = c(3, 1, 2))

#### Base
SRA <- SRA_scope(OM_condition, Chist = SRA_data$Chist, Index = SRA_data$Index, I_sd = SRA_data$I_sd, I_type = SRA_data$I_type,
                 condition = 'catch2', plusgroup = FALSE,
                 selectivity = rep("logistic", 2), s_selectivity = rep("logistic", 5), length_bin = 0.1 * SRA_data$length_bin, cores = 12,
                 s_CAA = SRA_data$s_CAA,
                 vul_par = SRA_data$vul_par, map_s_vul_par = SRA_data$map_s_vul_par,
                 map_log_rec_dev = SRA_data$map_log_rec_dev)
saveRDS(SRA, file = "mse/scoping/no_plusgroup/SRA_LH_grid.rds")
SRA <- readRDS("mse/scoping/no_plusgroup/SRA_LH_grid.rds")
plot(SRA, file = "mse/scoping/no_plusgroup/SRA_LH_grid_base", dir = getwd(), open_file = FALSE, f_name = SRA_data$f_name, s_name = SRA_data$s_name,
     MSY_ref = c(0.4, 0.8))

# Likelihood profile & stock status
Hist <- runMSE(SRA@OM, Hist = TRUE, parallel = TRUE)
LH_grid$nll <- vapply(SRA@Misc, getElement, numeric(1), "nll")
LH_grid$B_BMSY <- Hist@SampPars$D / Hist@Ref$SSBMSY_SSB0
write.csv(LH_grid, file = "mse/scoping/LH_grid.csv")
LH_grid <- read.csv("mse/scoping/LH_grid.csv")

png("mse/scoping/LH_grid_base.png", width = 10, height = 5, units = "in", res = 400)
par(mfrow = c(1, 2))

plot(NULL, NULL, xlim = range(LH_grid$M) + c(-0.01, 0.01), ylim = range(LH_grid$h) + c(-0.01, 0.01), xlab = "M", ylab = "steepness")
text(LH_grid$M, LH_grid$h, labels = round(LH_grid$nll, 2))
title("Negative log-likelihood")

plot(NULL, NULL, xlim = range(LH_grid$M) + c(-0.01, 0.01), ylim = range(LH_grid$h) + c(-0.01, 0.01), xlab = "M", ylab = "steepness")
text(LH_grid$M, LH_grid$h, labels = round(LH_grid$B_BMSY, 2))
title("Terminal B/BMSY")
dev.off()


#### Upweight dogfish
SRA <- SRA_scope(OM_condition, Chist = SRA_data$Chist, Index = SRA_data$Index, I_sd = SRA_data$I_sd, I_type = SRA_data$I_type,
                 selectivity = rep("logistic", 2), s_selectivity = rep("logistic", 5), length_bin = 0.1 * SRA_data$length_bin, cores = 12,
                 s_CAA = SRA_data$s_CAA, LWT = list(Index = c(1, 4, 1, 1, 1)),
                 vul_par = SRA_data$vul_par, map_s_vul_par = SRA_data$map_s_vul_par,
                 map_log_rec_dev = SRA_data$map_log_rec_dev)
saveRDS(SRA, file = "mse/scoping/SRA_LH_grid_upweight_dogfish.rds")
SRA <- readRDS("mse/scoping/SRA_LH_grid_upweight_dogfish.rds")

plot(SRA, filename = "mse/scoping/SRA_LH_grid_upweight_dogfish", dir = getwd(), open_file = FALSE, f_name = SRA_data$f_name, s_name = SRA_data$s_name,
     MSY_ref = c(0.4, 0.8))

# Likelihood profile & stock status
Hist <- runMSE(SRA@OM, Hist = TRUE, parallel = TRUE)
LH_grid$nll <- vapply(SRA@Misc, getElement, numeric(1), "nll")
LH_grid$B_BMSY <- Hist@SampPars$D / Hist@Ref$SSBMSY_SSB0
write.csv(LH_grid, file = "mse/scoping/LH_grid_upweight_dogfish.csv")
LH_grid <- read.csv("mse/scoping/LH_grid_upweight_dogfish.csv")

png("LH_grid_upweight_dogfish.png", width = 10, height = 5, units = "in", res = 400)
par(mfrow = c(1, 2))

plot(NULL, NULL, xlim = range(LH_grid$M) + c(-0.01, 0.01), ylim = range(LH_grid$h) + c(-0.01, 0.01), xlab = "M", ylab = "steepness")
text(LH_grid$M, LH_grid$h, labels = round(LH_grid$nll, 2))
title("Negative log-likelihood")

plot(NULL, NULL, xlim = range(LH_grid$M) + c(-0.01, 0.01), ylim = range(LH_grid$h) + c(-0.01, 0.01), xlab = "M", ylab = "steepness")
text(LH_grid$M, LH_grid$h, labels = round(LH_grid$B_BMSY, 2))
title("Terminal B/BMSY")
dev.off()






### Fit a surplus production model
SRA <- readRDS("mse/scoping/SRA_regwt_dogfish.rds")
mod <- SP(Data = SRA@OM@cpars$Data, AddInd = 1:5, use_r_prior = TRUE, start = list(r_prior = c(0.068, 0.03), dep = 0.9))
plot(mod, dir = getwd(), filename = "mse/scoping/report_SP", open_file = FALSE)

