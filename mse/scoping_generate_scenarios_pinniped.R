library(MSEtool)
library(dplyr)
library(reshape2)

#### Setup SRA data inputs
#SRA_data <- list(Year = 1918:2019)
#SRA_data$Len_bins <- seq(100, 850, 50)
#SRA_data$length_bin <- SRA_data$Len_bins[-length(SRA_data$Len_bins)] + 25
#SRA_data$maxage <- 80
#
## Data list
#MO <- readRDS("Data/2019/yelloweye-rockfish-ins-privacy.rds")
#
####### Indices: HBLL, dogfish, commercial CPUE
#generate_indices <- function() {
#
#  Index <- I_sd <- matrix(NA, length(SRA_data$Year), 5)
#
#  #HBLL
#  HBLL <- readRDS("Data/2019/hbll-joint-index.rds")
#  Index[, 1] <- vapply(SRA_data$Year, function(x) ifelse(!is.na(match(x, HBLL$year)), HBLL$est[match(x, HBLL$year)], NA), numeric(1))
#  I_sd[, 1] <- vapply(SRA_data$Year, function(x) ifelse(!is.na(match(x, HBLL$year)), HBLL$se[match(x, HBLL$year)], NA), numeric(1))
#
#  # Dogfish
#  dogfish <- readRDS("Data/2019/dogfish-index.rds")
#  Index[, 2] <- vapply(SRA_data$Year, function(x) ifelse(!is.na(match(x, dogfish$year)), dogfish$est[match(x, dogfish$year)], NA), numeric(1))
#  I_sd[, 2] <- vapply(SRA_data$Year, function(x) ifelse(!is.na(match(x, dogfish$year)), dogfish$se[match(x, dogfish$year)], NA), numeric(1))
#
#  # Preliminary dogfish from survey (use selectivity from?)
#  Ind_2011 <- readxl::read_excel("Data/Index.xlsx")
#
#  # Commerical CPUE with 2 breakpoints (3 series)
#  Index[, 3] <- vapply(SRA_data$Year, function(x) ifelse(!is.na(match(x, Ind_2011$Year)), Ind_2011$CC1[match(x, Ind_2011$Year)], NA), numeric(1))
#  I_sd[, 3] <- vapply(SRA_data$Year, function(x) ifelse(!is.na(match(x, Ind_2011$Year)), Ind_2011$CC1_SE[match(x, Ind_2011$Year)], NA), numeric(1))
#
#  Index[, 4] <- vapply(SRA_data$Year, function(x) ifelse(!is.na(match(x, Ind_2011$Year)), Ind_2011$CC2[match(x, Ind_2011$Year)], NA), numeric(1))
#  I_sd[, 4] <- vapply(SRA_data$Year, function(x) ifelse(!is.na(match(x, Ind_2011$Year)), Ind_2011$CC2_SE[match(x, Ind_2011$Year)], NA), numeric(1))
#
#  Index[, 5] <- vapply(SRA_data$Year, function(x) ifelse(!is.na(match(x, Ind_2011$Year)), Ind_2011$CC3[match(x, Ind_2011$Year)], NA), numeric(1))
#  I_sd[, 5] <- vapply(SRA_data$Year, function(x) ifelse(!is.na(match(x, Ind_2011$Year)), Ind_2011$CC3_SE[match(x, Ind_2011$Year)], NA), numeric(1))
#
#
#  SRA_data$I_type <<- c("est", "est", 1, 1, 1)
#  SRA_data$Index <<- Index
#  SRA_data$I_sd <<- I_sd
#
#  ##### Map survey selectivity of HBLL and dogfish
#  map_s_vul_par <- matrix(NA, 3, 5)
#  map_s_vul_par[1:2, 1:2] <- c(1, 2, 1, 2)
#
#  SRA_data$map_s_vul_par <<- map_s_vul_par
#}
#generate_indices()
#
#
#################### Data
###### Rec lengths
#generate_CAL <- function() {
#
#  res <- readxl::read_excel("Data/2019/Files/SC Rec YellowEye Biodata.xlsx", sheet = 2) %>% filter(!is.na(`LENGTH(MM)`))
#  res$Len_bin <- cut(res$`LENGTH(MM)`, breaks = SRA_data$Len_bins, labels = SRA_data$Len_bins[-length(SRA_data$Len_bins)])
#  rec_len_matrix <- summarise(group_by(res, YEAR, Len_bin), n = n()) %>% acast(list("YEAR", "Len_bin"), value.var = "n", fill = 0)
#
#  Len_bins_avail <- colnames(rec_len_matrix) %>% as.numeric()
#  Nareas <- summarise(group_by(res, YEAR), n = length(unique(SUBAREA)))
#
#  #plot_composition(as.numeric(rownames(rec_len_matrix)), rec_len_matrix, CAL_bins = Len_bins_avail)
#  #plot(Len_bins_avail, colSums(rec_len_matrix), typ = 'o')
#
#  rec_CAL_fn <- function(x) {
#    ind <- match(x, as.numeric(rownames(rec_len_matrix)))
#    out <- rep(NA, length(SRA_data$Len_bins)-1) %>% as.numeric()
#    if(!is.na(ind)) {
#      L_ind <- match(Len_bins_avail, SRA_data$Len_bins)
#      out[L_ind] <- rec_len_matrix[ind, ] * Nareas$n[ind]/sum(rec_len_matrix[ind, ])
#    }
#    return(out)
#  }
#
#  CAL <- array(dim = c(length(SRA_data$Year), length(SRA_data$Len_bins)-1, 2))
#  CAL[,,2] <- do.call(rbind, lapply(SRA_data$Year, rec_CAL_fn))
#  #plot_composition(1918:2018, SRA_data$CAL[,,2], CAL_bins = SRA_data$Len_bins[-length(SRA_data$Len_bins)])
#
#  SRA_data$CAL <<- CAL
#}
#generate_CAL()
#
####### Commerical data: handline, trawl, comps
##HL <- readRDS("Data/2019/ye-ins-catch-hl.rds")
##TR <- readRDS("Data/2019/ye-ins-catch-tr.rds")
##MO$catch$total <- MO$catch$sum_landed_kg + MO$catch$sum_discarded_kg
##comm <- summarise(group_by(MO$catch, year, gear), total = sum(total)) %>% right_join(data.frame(year = 1969:2019)) %>% acast(list("year", "gear"))
##write.csv(1e-3 * rowSums(cbind(HL, TR), na.rm = TRUE), file = "Data/2019/comm.csv")
#
#
####### Catch data. Fleet 1 = commerical (Hook and line plus trawl). Fleet 2 = Rec (FSC should be included in both fleets)
#generate_catch <- function() {
#  cat <- read.csv("Data/2019/catch.csv")
#
#  Chist <- matrix(NA, length(SRA_data$Year), 2)
#  Chist[, 1] <- vapply(SRA_data$Year, function(x) ifelse(!is.na(match(x, cat$Year)), cat$Comm[match(x, cat$Year)], NA), numeric(1))
#  #Chist[, 2] <- vapply(SRA_data$Year, function(x) ifelse(!is.na(match(x, cat$Year)), cat$Rec[match(x, cat$Year)] + cat$FSC[match(x, cat$Year)], NA), numeric(1))
#  Chist[, 2] <- vapply(SRA_data$Year, function(x) ifelse(!is.na(match(x, cat$Year)), cat$Rec[match(x, cat$Year)], NA), numeric(1))
#
#  SRA_data$Chist <<- Chist
#}
#generate_catch()
#
### Double the commercial catch from 1986-2005
#double_commercial_catch <- function() {
#  ind <- match(1986:2005, SRA_data$Year)
#  SRA_data$Chist[ind, 1] <<- 2 * SRA_data$Chist[ind, 1]
#}
#double_commercial_catch()
#
### Comm age comps - 1989 HL
#generate_CAA <- function() {
#  comm_age <- filter(MO[[1]], !is.na(age) & gear_desc == "LONGLINE") %>% mutate(age2 = ifelse(age >= SRA_data$maxage, SRA_data$maxage, age)) %>%
#    group_by(year, age2) %>% summarise(n = n())
#
#  n_obs <- comm_age %>% group_by(year) %>% summarise(n = sum(n))
#
#  comm_age <- comm_age %>% full_join(data.frame(age2 = 1:SRA_data$maxage)) %>% acast(list("year", "age2"), value.var = "n", fill = 0)
#
#  n_fishing_events <- filter(MO[[1]], !is.na(age) & gear_desc == "LONGLINE")  %>%
#    group_by(year) %>% summarise(ntrips = length(unique(fishing_event_id))) %>% full_join(n_obs)
#
#  CAA <- array(dim = c(length(SRA_data$Year), SRA_data$maxage, 2))
#
#  CAA_fn <- function(x, mat, ntrips) {
#    ind <- match(x, as.numeric(rownames(mat)))
#    out <- rep(NA, SRA_data$maxage) %>% as.numeric()
#    if(!is.na(ind)) {
#      out[as.numeric(colnames(mat))] <- mat[ind, ]
#
#      if(sum(out, na.rm = TRUE) > 0) {
#        Nt <- min(ntrips$ntrips[match(x, ntrips$year)], ntrips$n[match(x, ntrips$year)])
#        out <- out * Nt/sum(out, na.rm = TRUE)
#      }
#    }
#    return(out)
#  }
#  CAA[,,1] <- do.call(rbind, lapply(SRA_data$Year, CAA_fn, mat = comm_age, ntrips = n_fishing_events))
#  SRA_data$CAA <<- CAA
#}
#generate_CAA()
#
### Survey age comps
#generate_survey_CAA <- function() {
#  s_age <- filter(MO[[2]], !is.na(age) & (survey_abbrev == "HBLL INS N" | survey_abbrev == "HBLL INS S")) %>%
#    mutate(age2 = ifelse(age >= SRA_data$maxage, SRA_data$maxage, age)) %>% group_by(year, age2) %>%
#    summarise(n = n())
#
#  n_obs <- s_age %>% group_by(year) %>% summarise(n = sum(n))
#  s_age <- s_age %>% full_join(data.frame(age2 = 1:SRA_data$maxage)) %>% acast(list("year", "age2"), value.var = "n", fill = 0)
#
#  n_fishing_events <- filter(MO[[2]], !is.na(age) & (survey_abbrev == "HBLL INS N" | survey_abbrev == "HBLL INS S"))  %>%
#    group_by(year) %>% summarise(ntrips = length(unique(fishing_event_id))) %>% full_join(n_obs)
#
#  s_CAA <- array(dim = c(length(SRA_data$Year), SRA_data$maxage, 5))
#
#  CAA_fn <- function(x, mat, ntrips) {
#    ind <- match(x, as.numeric(rownames(mat)))
#    out <- rep(NA, SRA_data$maxage) %>% as.numeric()
#    if(!is.na(ind)) {
#      out[as.numeric(colnames(mat))] <- mat[ind, ]
#
#      if(sum(out, na.rm = TRUE) > 0) {
#        Nt <- ntrips$ntrips[match(x, ntrips$year)]
#        out <- out * Nt/sum(out, na.rm = TRUE)
#      }
#    }
#    return(out)
#  }
#
#  s_CAA[,,1] <- do.call(rbind, lapply(SRA_data$Year, CAA_fn, mat = s_age, ntrips = n_fishing_events))
#
#  SRA_data$s_CAA <<- s_CAA
#}
#generate_survey_CAA()
#
## Potential selectivity parameters for fishery
##OM <- XL2OM("OM/YE_OM.xlsx")
##OM@Linf[1] * (1 - exp(-OM@K[1] * (1:50 - OM@t0[1])))
#SRA_data$vul_par <- matrix(c(45, 35, 0.99, 35, 28, 0.99), 3, 2)
#
## Fix rec devs
#SRA_data$map_log_rec_dev <- c(rep(NA, 20), 1:71, rep(NA, 11))
#SRA_data$map_log_rec_dev <- c(rep(NA, 20), 1:82)
#SRA_data$map_log_rec_dev <- c(rep(NA, 30), 1:67, rep(NA, 5))
#
## For growth and maturity, see growth_maturity.R
#SRA_data$f_name <- c("Commercial", "Recreational")
#SRA_data$s_name <- c("HBLL", "Dogfish", "Com_CPUE", "Com_CPUE", "Com_CPUE")
#
#saveRDS(SRA_data, file = "mse/scoping/SRA_data.rds")
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

# M pinniped
Mp <- readxl::read_excel("mse/scoping/pinniped_M.xlsx") + 0.02
Mp$mu <- log(Mp$Median)
Mpp <- c(Mp$Median, rep(Mp$Median[nrow(Mp)], 10))
Mpp

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

#png("mse/scoping/pinniped/pinniped_M.png", res = 300, units = "in", height = 4, width = 5)
#plot(SRA_data$Year, Mpp, ylim = c(0, 0.15), xlab = "Year", ylab = "M", typ = "l", lwd = 2)
#lines(Lower ~ Year, Mp, lty = 3)
#lines(Upper ~ Year, Mp, lty = 3)
#lines(SRA_data$Year, rep(0.045, 102), col = 'red', lwd = 2)
#legend("topleft", c("Pinniped model", "Constant M model"), col = c("black", "red"), lwd = 2)
#dev.off()


OM@cpars$M_ageArray <- c(Mpp, rep(Mpp[length(Mpp)], OM@proyears)) %>% array(c(OM@nyears + OM@proyears, OM@nsim, OM@maxage)) %>%
  aperm(c(2, 3, 1))

OM_condition <- OM



################# Run SRA_scope
# Base
SRA <- SRA_scope(OM_condition, Chist = SRA_data$Chist, Index = SRA_data$Index, I_sd = SRA_data$I_sd, I_type = SRA_data$I_type,
                 condition = "catch2",
                 selectivity = rep("logistic", 2), s_selectivity = rep("logistic", 5), length_bin = 0.1 * SRA_data$length_bin, cores = 2, #mean_fit = TRUE,
                 s_CAA = SRA_data$s_CAA, vul_par = SRA_data$vul_par, map_s_vul_par = SRA_data$map_s_vul_par,
                 map_log_rec_dev = SRA_data$map_log_rec_dev)
saveRDS(SRA, file = "mse/scoping/pinniped/SRA_pinniped.rds")
SRA <- readRDS("mse/scoping/pinniped/SRA_pinniped.rds")
retro <- retrospective(SRA, 11)
saveRDS(retro, file = "mse/scoping/pinniped/ret_pinniped.rds")
retro <- readRDS("mse/scoping/pinniped/ret_pinniped.rds")
plot(SRA, retro = retro, file = "mse/scoping/pinniped/SRA_pinniped", dir = getwd(), open_file = FALSE, f_name = SRA_data$f_name, s_name = SRA_data$s_name,
     MSY_ref = c(0.4, 0.8))

generate_Cpinniped <- function(SRA) {
  Fpinniped <- SRA@mean_fit$report$M - 0.02
  Z <- SRA@mean_fit$report$Z
  N <- SRA@mean_fit$report$N
  Wt <- SRA@mean_fit$obj$env$data$wt
  Cpred <- Fpinniped/Z * (1 - exp(-Z)) * N[-nrow(N), ] * Wt[-nrow(Wt), ]
  Cpred <- rowSums(Cpred)

  matplot(SRA_data$Year, cbind(Cpred, SRA@data$Chist), lty = 1:3, typ = 'l', xlab = "Year", ylab = "Total Catch", lwd = 2)
  legend("topleft", c("Pinniped", "Commercial", "Recreational"), lty = 1:3, col = 1:3, lwd = 2)
  return(invisible(Cpred))
}

#png("mse/scoping/pinniped/pinniped_catch.png", res = 300, units = "in", height = 4, width = 5)
#generate_Cpinniped(SRA)
#dev.off()



############ Selectivity of pinnipeds equal recreational
rec_sel_age <- SRA@mean_fit$report$vul[1,,2]


# M pinniped
#Mp <- readxl::read_excel("mse/scoping/pinniped_M.xlsx")
#Mpp <- c(Mp$Median, rep(Mp$Median[nrow(Mp)], 10)) + 0.02

Mpp2 <- outer(Mpp, rec_sel_age)
Mpp2_proj <- matrix(Mpp2[nrow(Mpp2), ], nrow = OM@proyears, ncol = OM@maxage, byrow = TRUE)

OM@cpars$M_ageArray <- rbind(Mpp2, Mpp2_proj) %>% array(c(OM@nyears + OM@proyears, OM@maxage, OM@nsim)) %>% aperm(c(3, 2, 1))

OM_condition <- OM

SRA <- SRA_scope(OM_condition, Chist = SRA_data$Chist, Index = SRA_data$Index, I_sd = SRA_data$I_sd, I_type = SRA_data$I_type,
                 condition = "catch2",
                 selectivity = rep("logistic", 2), s_selectivity = rep("logistic", 5), length_bin = 0.1 * SRA_data$length_bin, cores = 2, #mean_fit = TRUE,
                 s_CAA = SRA_data$s_CAA, vul_par = SRA_data$vul_par, map_s_vul_par = SRA_data$map_s_vul_par,
                 map_log_rec_dev = SRA_data$map_log_rec_dev)
saveRDS(SRA, file = "mse/scoping/pinniped/SRA_pinniped_recsel.rds")
SRA <- readRDS("mse/scoping/pinniped/SRA_pinniped_recsel.rds")
retro <- retrospective(SRA, 11)
saveRDS(retro, file = "mse/scoping/pinniped/ret_pinniped_recsel.rds")
retro <- readRDS("mse/scoping/pinniped/ret_pinniped_recsel.rds")
plot(SRA, retro = retro, file = "mse/scoping/pinniped/SRA_pinniped_recsel", dir = getwd(), open_file = FALSE,
     f_name = SRA_data$f_name, s_name = SRA_data$s_name, MSY_ref = c(0.4, 0.8))


#png("mse/scoping/pinniped/pinniped_catch_recsel.png", res = 300, units = "in", height = 4, width = 5)
#generate_Cpinniped(SRA)
#dev.off()



s1 <- readRDS("mse/scoping/pinniped/SRA_pinniped.rds")
s2 <- readRDS("mse/scoping/pinniped/SRA_pinniped_recsel.rds")

compare_SRA(s1, s2, filename = "mse/scoping/pinniped/compare_pinniped", dir = getwd(), open_file = FALSE,
     f_name = SRA_data$f_name, s_name = SRA_data$s_name, MSY_ref = c(0.4, 0.8), scenario = list(names = c("All_sel", "Rec_sel")))




