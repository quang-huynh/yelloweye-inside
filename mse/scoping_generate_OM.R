library(MSEtool)
library(dplyr)
library(reshape2)

SRA_data <- readRDS("mse/scoping/SRA_data.rds")

################## OM setup
truncate_rnorm_internal <- function(x, mu, sd, l, u) {
  samp <- rnorm(1, mu, sd)
  if(samp < l || samp > u) {
    Recall(x = x, mu, sd, l, u)
  } else {
    return(samp)
  }
}
trnorm <- function(n, mu, sd, l, u) vapply(1:n, truncate_rnorm_internal, numeric(1), mu = mu, sd = sd, l = l, u = u)


OM <- XL2OM("mse/OM/YE_OM.xlsx")
OM@CurrentYr <- 2019
OM@nsim <- 250
OM@nyears <- length(SRA_data$Year)
OM@maxage <- SRA_data$maxage
OM@proyears <- 100
OM <- Replace(OM, Precise_Unbiased)
OM <- Replace(OM, Perfect_Imp)
OM@Perr <- c(0.4, 0.4)
OM@R0 <- 1200
#OM@cpars$plusgroup <- rep(1L, OM@nsim)

# Maturity
Mat_age <- ifelse(1:OM@maxage <= 7, 0, 1/(1 + exp(-log(19) * (1:OM@maxage - 14.4)/(27.4-14.4))))
OM@cpars$Mat_age <- array(Mat_age, c(OM@maxage, OM@nyears + OM@proyears, OM@nsim)) %>% aperm(perm = c(3, 1, 2))

# Natural mortality M ~ lognormal mean 0.045, sd = 0.2
set.seed(91283)
M_samps <- rlnorm(OM@nsim, log(0.045) - 0.5 * 0.2^2, 0.2)
OM@cpars$M <- M_samps

# Sample steepness h ~ transformed beta with mean = 0.71, sd = 0.15. Implies x ~ beta(mean = 0.6375, sd = 0.12)
h_alpha <- alphaconv(0.6375, 0.12)
h_beta <- betaconv(0.6375, 0.12)
set.seed(65423)

h_samps <- rbeta(OM@nsim, h_alpha, h_beta)
h_samps <- 0.8 * h_samps + 0.2
OM@cpars$h <- h_samps
OM_condition <- OM

### Make some plots
#matplot(t(SRA@SSB[SRA@conv, ]), type = 'l')
#
#plot(OM@cpars$h[SRA@conv], OM@cpars$M[SRA@conv], xlim = c(0.4, 0.9))
#points(OM@cpars$h[!SRA@conv], OM@cpars$M[!SRA@conv], col = "red", pch = 16)
#matplot(t(SRA@SSB[SRA@SSB[, 20] > 2000, ]), type = 'l')
#
#SSB <- SRA@SSB[SRA@SSB[, 20] > 2000, ]
#matplot(t(SSB), col = "black", lty = c(3, 1, 3), type = 'l')
#matplot(t(apply(SSB, 2, quantile, probs = c(0.025, 0.5, 0.975))), col = "black", lty = c(3, 1, 3), type = 'l')

setup(12)

# Base
SRA <- SRA_scope(OM_condition, condition = "catch2", Chist = SRA_data$Chist, Index = SRA_data$Index, I_sd = SRA_data$I_sd, I_type = SRA_data$I_type,
                 selectivity = rep("logistic", 2), s_selectivity = rep("logistic", 5), length_bin = 0.1 * SRA_data$length_bin, cores = 12, #mean_fit = TRUE,
                 s_CAA = SRA_data$s_CAA, vul_par = SRA_data$vul_par, map_s_vul_par = SRA_data$map_s_vul_par,
                 map_log_rec_dev = SRA_data$map_log_rec_dev)
saveRDS(SRA, file = "mse/OM/base.rds")
#SRA <- readRDS("mse/OM/base.rds")

plot(SRA, file = "mse/OM/OM_base", dir = getwd(), open_file = FALSE, f_name = SRA_data$f_name, s_name = SRA_data$s_name,
     MSY_ref = c(0.4, 0.8))

# Upweight dogfish with lambdas
SRA <- SRA_scope(OM_condition, condition = "catch2", Chist = SRA_data$Chist, Index = SRA_data$Index, I_sd = SRA_data$I_sd, I_type = SRA_data$I_type,
                 selectivity = rep("logistic", 2), s_selectivity = rep("logistic", 5), length_bin = 0.1 * SRA_data$length_bin, cores = 12, mean_fit = TRUE,
                 s_CAA = SRA_data$s_CAA, LWT = list(Index = c(1, 4, 1, 1, 1)),
                 vul_par = SRA_data$vul_par, map_s_vul_par = SRA_data$map_s_vul_par,
                 map_log_rec_dev = SRA_data$map_log_rec_dev)

saveRDS(SRA, file = "mse/OM/upweight_dogfish.rds")
#SRA <- readRDS("mse/OM/upweight_dogfish.rds")
plot(SRA, file = "mse/OM/OM_upweight_dogfish", dir = getwd(), open_file = FALSE, f_name = SRA_data$f_name, s_name = SRA_data$s_name,
     MSY_ref = c(0.4, 0.8))

# M = 0.02

# h in sim 17 is very low 0.38 and needs to be replaced
OM_condition@cpars$M <- rep(0.02, OM@nsim)
SRA <- SRA_scope(OM_condition, condition = "catch2", Chist = SRA_data$Chist, Index = SRA_data$Index, I_sd = SRA_data$I_sd, I_type = SRA_data$I_type,
                 selectivity = rep("logistic", 2), s_selectivity = rep("logistic", 5), length_bin = 0.1 * SRA_data$length_bin, cores = 12, #mean_fit = TRUE,
                 s_CAA = SRA_data$s_CAA, vul_par = SRA_data$vul_par, map_s_vul_par = SRA_data$map_s_vul_par,
                 map_log_rec_dev = SRA_data$map_log_rec_dev)
saveRDS(SRA, file = "mse/OM/lowM.rds")
SRA <- readRDS("mse/OM/lowM.rds")
plot(SRA, file = "mse/OM/OM_lowM", dir = getwd(), open_file = FALSE, f_name = SRA_data$f_name, s_name = SRA_data$s_name,
     MSY_ref = c(0.4, 0.8))


# Low catch
SRA_data$Chist[match(1986:2005, SRA_data$Year), 1] <- 0.5 * SRA_data$Chist[match(1986:2005, SRA_data$Year), 1]
sfExportAll()
SRA <- SRA_scope(OM_condition, condition = "catch2", Chist = SRA_data$Chist, Index = SRA_data$Index, I_sd = SRA_data$I_sd, I_type = SRA_data$I_type,
                 selectivity = rep("logistic", 2), s_selectivity = rep("logistic", 5), length_bin = 0.1 * SRA_data$length_bin, cores = 12, mean_fit = TRUE,
                 s_CAA = SRA_data$s_CAA, LWT = list(Index = c(1, 4, 1, 1, 1)),
                 vul_par = SRA_data$vul_par, map_s_vul_par = SRA_data$map_s_vul_par,
                 map_log_rec_dev = SRA_data$map_log_rec_dev)

saveRDS(SRA, file = "mse/OM/low_catch.rds")
#SRA <- readRDS("mse/OM/OM_low_catch.rds")
plot(SRA, file = "mse/OM/OM_low_catch", dir = getwd(), open_file = FALSE, f_name = SRA_data$f_name, s_name = SRA_data$s_name,
     MSY_ref = c(0.4, 0.8))



# M pinniped
Mp <- readxl::read_excel("mse/scoping/pinniped_M.xlsx") + 0.02
Mp$mu <- log(Mp$Median)

find_sigma <- function(Mp) {
  uniroot_fn <- function(sd, mu, upper_q) {
    mean_ <- exp(mu + 0.5 * sd *sd)
    mean_log <- log(mean_)
    plnorm(upper_q, mean_log, sd) - 0.975
  }
  out <- mapply(function(x, y) uniroot(uniroot_fn, interval = c(1e-8, 10), mu = x, upper_q = y)$root,
                x = Mp$mu, y = Mp$Upper)
  return(out)
}
Mp$sigma <- find_sigma(Mp)

set.seed(205)
Msamps_p <- runif(OM@nsim)
out_fn <- function(x, y) qlnorm(x, -0.5 * y^2, y)
M_dev <- outer(Msamps_p, Mp$sigma, out_fn) %>% t() %>% "*"(Mp$Median)
matplot(M_dev, type = 'l')
apply(M_dev, 1, mean)

Msamps_extra <- M_dev[nrow(M_dev), ] %>% matrix(OM@proyears + 10, OM@nsim, byrow = TRUE)

OM@cpars$M <- NULL
OM@cpars$M_ageArray <- rbind(M_dev, Msamps_extra) %>% array(c(OM@nyears + OM@proyears, OM@nsim, OM@maxage)) %>%
  aperm(c(2, 3, 1))
OM_condition <- OM

SRA <- SRA_scope(OM_condition, condition = "catch2", Chist = SRA_data$Chist, Index = SRA_data$Index, I_sd = SRA_data$I_sd, I_type = SRA_data$I_type,
                 selectivity = rep("logistic", 2), s_selectivity = rep("logistic", 5), length_bin = 0.1 * SRA_data$length_bin, cores = 12, #mean_fit = TRUE,
                 s_CAA = SRA_data$s_CAA, vul_par = SRA_data$vul_par, map_s_vul_par = SRA_data$map_s_vul_par,
                 map_log_rec_dev = SRA_data$map_log_rec_dev)
saveRDS(SRA, file = "mse/OM/pinniped.rds")
#SRA <- readRDS("mse/OM/pinniped.rds")
plot(SRA, file = "mse/OM/OM_pinniped", dir = getwd(), open_file = FALSE, f_name = SRA_data$f_name, s_name = SRA_data$s_name,
     MSY_ref = c(0.4, 0.8))


#### Episodic recruitment
SRA <- readRDS("mse/OM/upweight_dogfish.rds")
set.seed(324)

sporadic_recruitment2 <- function(x, years = length(x), low_sigmaR = 0.4, high_sigmaR = 0.8) {
  require(dplyr)
  nhigh <- 25

  high_ind <- sample(1:years, nhigh)
  new_samp <- rnorm(nhigh, -0.5 * high_sigmaR^2, high_sigmaR) %>% exp()

  x[high_ind] <- new_samp

  return(x)
}

new_Perr_y <- apply(SRA@OM@cpars$Perr_y[, 182:281], 1, sporadic_recruitment2)
SRA@OM@cpars$Perr_y[, 182:281] <- t(new_Perr_y)
saveRDS(SRA, file = "mse/OM/sporadic_recruitment.rds")



