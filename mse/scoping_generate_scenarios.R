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

OM_condition <- OM

################# Add age and length comp from fishery
SRA <- SRA_scope(OM_condition, Chist = SRA_data$Chist, Index = SRA_data$Index, I_sd = SRA_data$I_sd, I_type = SRA_data$I_type,
                 condition = "catch2", CAL = SRA_data$CAL, CAA = 5*SRA_data$CAA, LWT = list(Index = c(1, 1, 1, 1, 1)),
                 selectivity = rep("logistic", 2), s_selectivity = rep("logistic", 5), length_bin = 0.1 * SRA_data$length_bin, cores = 2, #mean_fit = TRUE,
                 s_CAA = SRA_data$s_CAA, vul_par = SRA_data$vul_par, #map_vul_par = matrix(c(NA, NA, NA, 1, 2, NA), 3, 2),
                 map_s_vul_par = SRA_data$map_s_vul_par, map_log_rec_dev = SRA_data$map_log_rec_dev)
saveRDS(SRA, file = "mse/scoping/SRA_hi_fish_sel.rds")
SRA <- readRDS("mse/scoping/SRA_hi_fish_sel.rds")
retro <- retrospective(SRA, 11)
saveRDS(retro, file = "mse/scoping/ret_hi_fish_sel.rds")
retro <- readRDS("mse/scoping/ret_hi_fish_sel.rds")
plot(SRA, retro = retro, file = "mse/scoping/SRA_hi_fish_sel", dir = getwd(), open_file = FALSE, f_name = SRA_data$f_name, s_name = SRA_data$s_name,
     MSY_ref = c(0.4, 0.8))

OM_condition@M = rep(0.02, 2)


################# Run SRA_scope
# Base
SRA <- SRA_scope(OM_condition, Chist = SRA_data$Chist, Index = SRA_data$Index, I_sd = SRA_data$I_sd, I_type = SRA_data$I_type,
                 condition = "catch2",
                 selectivity = rep("logistic", 2), s_selectivity = rep("logistic", 5), length_bin = 0.1 * SRA_data$length_bin, cores = 2, #mean_fit = TRUE,
                 s_CAA = SRA_data$s_CAA, vul_par = SRA_data$vul_par, map_s_vul_par = SRA_data$map_s_vul_par,
                 map_log_rec_dev = SRA_data$map_log_rec_dev)
saveRDS(SRA, file = "mse/scoping/SRA_regwt_dogfish.rds")
SRA <- readRDS("mse/scoping/SRA_regwt_dogfish.rds")
retro <- retrospective(SRA, 11)
saveRDS(retro, file = "mse/scoping/ret_regwt_dogfish.rds")
retro <- readRDS("mse/scoping/ret_regwt_dogfish.rds")
plot(SRA, retro = retro, file = "mse/scoping/SRA_regwt_dogfish", dir = getwd(), open_file = FALSE, f_name = SRA_data$f_name, s_name = SRA_data$s_name,
     MSY_ref = c(0.4, 0.8))

# Upweight dogfish with lambdas
SRA <- SRA_scope(OM_condition, Chist = SRA_data$Chist, Index = SRA_data$Index, I_sd = SRA_data$I_sd, I_type = SRA_data$I_type,
                 condition = "catch2",
                 selectivity = rep("logistic", 2), s_selectivity = rep("logistic", 5), length_bin = 0.1 * SRA_data$length_bin, cores = 2, mean_fit = TRUE,
                 s_CAA = SRA_data$s_CAA, LWT = list(Index = c(1, 4, 1, 1, 1)),
                 vul_par = SRA_data$vul_par, map_s_vul_par = SRA_data$map_s_vul_par,
                 map_log_rec_dev = SRA_data$map_log_rec_dev)
saveRDS(SRA, file = "mse/scoping/SRA_upweight_dogfish.rds")
#SRA <- readRDS("mse/scoping/SRA_upweight_dogfish.rds")
retro <- retrospective(SRA, 11)
saveRDS(retro, file = "mse/scoping/ret_upweight_dogfish.rds")
#retro <- readRDS("mse/scoping/ret_upweight_dogfish.rds")
plot(SRA, retro = retro, file = "mse/scoping/SRA_upweight_dogfish", dir = getwd(), open_file = FALSE, f_name = SRA_data$f_name, s_name = SRA_data$s_name,
     MSY_ref = c(0.4, 0.8))

# Remove commercial CPUE
SRA <- SRA_scope(OM_condition, Chist = SRA_data$Chist, Index = SRA_data$Index, I_sd = SRA_data$I_sd, I_type = SRA_data$I_type,
                 condition = "catch2",
                 selectivity = rep("logistic", 2), s_selectivity = rep("logistic", 5),  length_bin = 0.1 * SRA_data$length_bin, cores = 2, mean_fit = TRUE,
                 s_CAA = SRA_data$s_CAA, LWT = list(Index = c(1, 1, 0, 0, 0)),
                 vul_par = SRA_data$vul_par, map_s_vul_par = SRA_data$map_s_vul_par,
                 map_log_rec_dev = SRA_data$map_log_rec_dev)
saveRDS(SRA, file = "mse/scoping/SRA_no_CPUE.rds")
#SRA <- readRDS("mse/scoping/SRA_no_CPUE.rds")
retro <- retrospective(SRA, 11)
saveRDS(retro, file = "mse/scoping/ret_no_CPUE.rds")
#retro <- readRDS("mse/scoping/ret_no_CPUE.rds")
plot(SRA, retro = retro, file = "mse/scoping/SRA_no_CPUE", dir = getwd(), compare = FALSE, open_file = FALSE, f_name = SRA_data$f_name, s_name = SRA_data$s_name, MSY_ref = c(0.4, 0.8))

# Upweight HBLL
SRA <- SRA_scope(OM_condition, Chist = SRA_data$Chist, Index = SRA_data$Index, I_sd = SRA_data$I_sd, I_type = SRA_data$I_type,
                 condition = "catch2",
                 selectivity = rep("logistic", 2), s_selectivity = rep("logistic", 5), length_bin = 0.1 * SRA_data$length_bin, cores = 2, mean_fit = TRUE,
                 s_CAA = SRA_data$s_CAA, LWT = list(Index = c(4, 1, 1, 1, 1)),
                 vul_par = SRA_data$vul_par, map_s_vul_par = SRA_data$map_s_vul_par,
                 map_log_rec_dev = SRA_data$map_log_rec_dev)
saveRDS(SRA, file = "mse/scoping/SRA_upweight_HBLL.rds")
#SRA <- readRDS("mse/scoping/SRA_upweight_HBLL.rds")
retro <- retrospective(SRA, 11)
saveRDS(retro, file = "mse/scoping/ret_upweight_HBLL.rds")
#retro <- readRDS("mse/scoping/ret_upweight_HBLL.rds")
plot(SRA, retro = retro, file = "mse/scoping/SRA_upweight_HBLL", dir = getwd(), compare = FALSE, open_file = FALSE, f_name = SRA_data$f_name, s_name = SRA_data$s_name)

# Fix survey selectivity
s_vul_par <- matrix(c(46.56615, 30.56082, 0.99), 3, 5)
map_s_vul_par <- matrix(NA, 3, 5)

SRA <- SRA_scope(OM_condition, Chist = SRA_data$Chist, Index = SRA_data$Index, I_sd = SRA_data$I_sd, I_type = SRA_data$I_type,
                 condition = "catch2",
                 selectivity = rep("logistic", 2), s_selectivity = rep("logistic", 5), length_bin = 0.1 * SRA_data$length_bin, cores = 2, mean_fit = TRUE,
                 s_CAA = SRA_data$s_CAA, LWT = list(s_CAA = 0),
                 vul_par = SRA_data$vul_par, s_vul_par = s_vul_par, map_s_vul_par = map_s_vul_par,
                 map_log_rec_dev = rep(NA, OM_condition@nyears))
saveRDS(SRA, file = "mse/scoping/SRA_fix_HBLL_sel.rds")
#SRA <- readRDS("mse/scoping/SRA_fix_HBLL_sel.rds")
retro <- retrospective(SRA, 11)
#retro <- readRDS("SRA_fix_HBLL_sel.rds")
plot(SRA, file = "mse/scoping/SRA_fix_HBLL_sel", dir = getwd(), open_file = FALSE, f_name = SRA_data$f_name, s_name = SRA_data$s_name,
     MSY_ref = c(0.4, 0.8))


##### Compare all these fits
library(MSEtool); library(dplyr)
vec <- c("SRA_regwt_dogfish", "SRA_upweight_dogfish", "SRA_upweight_HBLL", "SRA_fix_HBLL_sel",
         "SRA_no_CPUE", "SRA_hi_fish_sel", "catch/SRA_regwt_dogfish", "catch/SRA_upweight_dogfish")

SRA_list <- lapply(vec, function(x) readRDS(paste0("mse/scoping/", x, ".rds"))) %>% structure(names = vec)

for(i in 1:length(vec)) {
  plot(SRA_list[[i]], file = paste0("mse/scoping/", vec[i]), dir = getwd(), open_file = FALSE,
       f_name = SRA_data$f_name, s_name = SRA_data$s_name, MSY_ref = c(0.4, 0.8),
       render_args = list(output_format = "word_document"))
}

#s1 <- readRDS("mse/scoping/SRA_regwt_dogfish.rds")
#s2 <- readRDS("mse/scoping/SRA_upweight_dogfish.rds")
#s3 <- readRDS("mse/scoping/SRA_upweight_HBLL.rds")
#s4 <- readRDS("mse/scoping/SRA_fix_HBLL_sel.rds")
#s5 <- readRDS("mse/scoping/SRA_no_CPUE.rds")
#s6 <- readRDS("mse/scoping/SRA_hi_fish_sel.rds")
#s7 <- readRDS("mse/scoping/catch/SRA_regwt_dogfish.rds")
#s8 <- readRDS("mse/scoping/catch/SRA_upweight_dogfish.rds")

MSEtool:::compare_SRA(SRA_list[[1]], SRA_list[[2]], SRA_list[[3]], SRA_list[[4]], SRA_list[[5]],
            filename = "mse/scoping/compare_SRA", dir = getwd(), open_file = FALSE, f_name = SRA_data$f_name, s_name = SRA_data$s_name,
            MSY_ref = c(0.4, 0.8), scenario = list(names = c("Base", "Up. dogfish", "Up. HBLL", "Fix HBLL sel", "No CPUE"),
                                                   col = gplots::rich.colors(5)))

MSEtool:::compare_SRA(SRA_list[[1]], SRA_list[[2]], SRA_list[[6]], SRA_list[[7]], SRA_list[[8]],
                      filename = "mse/scoping/compare_SRA2", dir = getwd(), open_file = FALSE, f_name = SRA_data$f_name, s_name = SRA_data$s_name,
                      MSY_ref = c(0.4, 0.8), scenario = list(names = c("Base", "Up. dogfish", "Hi sel", "Low catch", "Low catch up. dogfish"),
                                                             col = gplots::rich.colors(5)))

### Word doc
MSEtool:::compare_SRA(SRA_list[[1]], SRA_list[[2]], SRA_list[[3]], SRA_list[[4]], SRA_list[[5]],
                      filename = "mse/scoping/compare_SRA", dir = getwd(), open_file = FALSE, f_name = SRA_data$f_name, s_name = SRA_data$s_name,
                      MSY_ref = c(0.4, 0.8), scenario = list(names = c("Base", "Up. dogfish", "Up. HBLL", "Fix HBLL sel", "No CPUE"),
                                                             col = gplots::rich.colors(5)),
                      render_args = list(output_format = "word_document"))

MSEtool:::compare_SRA(SRA_list[[1]], SRA_list[[2]], SRA_list[[6]], SRA_list[[7]], SRA_list[[8]],
                      filename = "mse/scoping/compare_SRA2", dir = getwd(), open_file = FALSE, f_name = SRA_data$f_name, s_name = SRA_data$s_name,
                      MSY_ref = c(0.4, 0.8), scenario = list(names = c("Base", "Up. dogfish", "Hi sel", "Low catch", "Low catch up. dogfish"),
                                                             col = gplots::rich.colors(5)),
                      render_args = list(output_format = "word_document"))

##### function to generate dataframe of mean fits (with scenario names)
get_sra_survey <- function(sra, scenario, survey_names = c("HBLL", "Dogfish", "CPUE1", "CPUE2", "CPUE3")) {
  report <- sra@mean_fit$report
  n_surv <- dim(report$Ipred)[2]

  extract_survey_fn <- function(i) {
    observed <- sra@data$Index[, i]
    predicted <- report$Ipred[, i]

    dff <- data.frame(year = (sra@OM@CurrentYr-sra@OM@nyears+1):sra@OM@CurrentYr, observed = observed, predicted = predicted,
                      scenario = scenario, survey = survey_names[i])
    dff
  }

  out <- lapply(1:n_surv, extract_survey_fn)
  do.call(rbind, out) %>% reshape2::melt(id.vars = c("year", "scenario", "survey"),
                                         measure.vars = c("observed", "predicted"), variable.name = "type")
}

#out = get_sra_survey(s1, scenario = "Base")
out = Map(get_sra_survey, sra = list(s1, s2, s3, s4, s5), scenario = c("Base", "Up. dogfish", "Up. HBLL", "Fix HBLL sel", "No CPUE"))
out = do.call(rbind, out)
saveRDS(out, file = "mse/scoping/index_fits.rds")

library(ggplot2)
ggplot(out, aes(year, value, colour = type, linetype = scenario)) + facet_wrap(~ survey, scales = "free_y") + geom_point() + geom_line()


get_sra_comps <- function(sra, scenario) {
  require(dplyr)
  report <- sra@mean_fit$report
  year <- (sra@OM@CurrentYr-sra@OM@nyears+1):sra@OM@CurrentYr
  age <- 1:80
  obs <- structure(sra@data$s_CAA[,,1], dimnames = list(year, age))/rowSums(sra@data$s_CAA[,,1])
  obs <- reshape2::melt(obs, varnames = c("year", "age"), na.rm = TRUE, value.name = "observed")

  pred <- structure(report$s_CAApred[,,1], dimnames = list(year, age))/rowSums(report$s_CAApred[,,1])
  pred <- reshape2::melt(pred, varnames = c("year", "age"), na.rm = TRUE, value.name = "predicted")

  out <- dplyr::left_join(obs, pred, by = c("year", "age")) %>%
    reshape2::melt(out, id.vars = c("year", "age"), measure.vars = c("observed", "predicted"), variable.name = "type")
  out$scenario <- scenario
  return(out)
}
#out = get_sra_comps(s1, scenario = "Base")
out = Map(get_sra_comps, sra = list(s1, s2, s3, s4, s5), scenario = c("Base", "Up. dogfish", "Up. HBLL", "Fix HBLL sel", "No CPUE"))
out = do.call(rbind, out)
saveRDS(out, file = "mse/scoping/age_comp_fits.rds")

library(ggplot2)
ggplot(filter(out, scenario == "Base"), aes(age, value, colour = type)) + facet_wrap(~ year, scales = "free_y") + geom_point() + geom_line()










SRA <- list()

# Loop over M
M_loop <- seq(0.02, 0.06, 0.01)
for(i in 1:length(M_loop)) {
  OM_condition@M <- rep(M_loop[i], 2)
  SRA[[i]] <- SRA_scope(OM_condition, Chist = SRA_data$Chist, Index = SRA_data$Index, I_sd = SRA_data$I_sd, I_type = SRA_data$I_type,
                   condition = "catch2",
                   selectivity = rep("logistic", 2), s_selectivity = rep("logistic", 5), length_bin = 0.1 * SRA_data$length_bin, cores = 2, #mean_fit = TRUE,
                   s_CAA = SRA_data$s_CAA, vul_par = SRA_data$vul_par, map_s_vul_par = SRA_data$map_s_vul_par,
                   map_log_rec_dev = SRA_data$map_log_rec_dev)
  saveRDS(SRA[[i]], file = paste0("mse/scoping/M/regwt_dogfish_M0", M_loop[i] * 100, ".rds"))
}

retro <- sfLapply(SRA, retrospective, nyr = 11)

for(i in 1:length(M_loop)) {
  saveRDS(retro[[i]], paste0("mse/scoping/M/ret_regwt_dogfish_M0", M_loop[i] * 100, ".rds"))
  plot(SRA[[i]], retro = retro[[i]], file = paste0("mse/scoping/M/regwt_dogfish_M0", M_loop[i] * 100), dir = getwd(), open_file = FALSE, f_name = SRA_data$f_name, s_name = SRA_data$s_name,
       MSY_ref = c(0.4, 0.8))
}

SRA <- lapply(M_loop, function(x) readRDS(paste0("mse/scoping/M/regwt_dogfish_M0", x* 100, ".rds")))
SRAp <- c(SRA, list(filename = "mse/scoping/M/compare_M", dir = getwd(), open_file = FALSE, f_name = SRA_data$f_name, s_name = SRA_data$s_name,
                    MSY_ref = c(0.4, 0.8), scenario = list(names = paste0("M = 0.", 2:6))))
do.call(compare_SRA, SRAp)

compare_SRA(SRA[[1]], SRA[[2]], SRA[[3]], SRA[[4]], SRA[[5]],
            filename = "mse/scoping/M/compare_M_regwt", dir = getwd(), open_file = FALSE, f_name = SRA_data$f_name, s_name = SRA_data$s_name,
            scenario = list(names = paste0("M = 0.", 2:6)), MSY_ref = c(0.4, 0.8))






# Loop over M
M_loop <- seq(0.02, 0.06, 0.01)
for(i in 1:length(M_loop)) {
  OM_condition@M <- rep(M_loop[i], 2)
  SRA[[i]] <- SRA_scope(OM_condition, Chist = SRA_data$Chist, Index = SRA_data$Index, I_sd = SRA_data$I_sd, I_type = SRA_data$I_type,
                        condition = "catch2", LWT = c(1, 4, 1, 1, 1),
                        selectivity = rep("logistic", 2), s_selectivity = rep("logistic", 5), length_bin = 0.1 * SRA_data$length_bin, cores = 2, #mean_fit = TRUE,
                        s_CAA = SRA_data$s_CAA, vul_par = SRA_data$vul_par, map_s_vul_par = SRA_data$map_s_vul_par,
                        map_log_rec_dev = SRA_data$map_log_rec_dev)
  saveRDS(SRA[[i]], file = paste0("mse/scoping/M/upweight_dogfish_M0", M_loop[i] * 100, ".rds"))
}

retro <- sfLapply(SRA, retrospective, nyr = 11)

for(i in 1:length(M_loop)) {
  saveRDS(retro[[i]], paste0("mse/scoping/M/ret_upweight_dogfish_M0", M_loop[i] * 100, ".rds"))
  plot(SRA[[i]], retro = retro[[i]], file = paste0("mse/scoping/M/upweight_dogfish_M0", M_loop[i] * 100), dir = getwd(), open_file = FALSE, f_name = SRA_data$f_name, s_name = SRA_data$s_name,
       MSY_ref = c(0.4, 0.8))
}

SRAp <- SRA
SRAp <- c(SRA, list(file = "mse/scoping/M/compare_M", dir = getwd(), open_file = FALSE, f_name = SRA_data$f_name, s_name = SRA_data$s_name,
                    MSY_ref = c(0.4, 0.8)))
do.call(compare_SRA, SRAp)





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





###### Profile over M and steepness
LH_grid <- expand.grid(M = seq(0.02, 0.07, 0.01), h = seq(0.65, 0.75, 0.01))

OM_condition <- OM
OM_condition@nsim <- nrow(LH_grid)
OM_condition@cpars$M <- LH_grid$M
OM_condition@cpars$h <- LH_grid$h

# Maturity
Mat_age <- ifelse(1:OM@maxage <= 7, 0, 1/(1 + exp(-log(19) * (1:OM@maxage - 14.4)/(27.4-14.4))))
OM_condition@cpars$Mat_age <- array(Mat_age, c(OM@maxage, OM@nyears + OM@proyears, OM_condition@nsim)) %>% aperm(perm = c(3, 1, 2))

#### Base
SRA <- SRA_scope(OM_condition, Chist = SRA_data$Chist, Index = SRA_data$Index, I_sd = SRA_data$I_sd, I_type = SRA_data$I_type,
                 condition = "catch2",
                 selectivity = rep("logistic", 2), s_selectivity = rep("logistic", 5), length_bin = 0.1 * SRA_data$length_bin, cores = 12,
                 s_CAA = SRA_data$s_CAA,
                 vul_par = SRA_data$vul_par, map_s_vul_par = SRA_data$map_s_vul_par,
                 map_log_rec_dev = SRA_data$map_log_rec_dev)
saveRDS(SRA, file = "SRA_profile_M_and_h.rds")
SRA <- readRDS("SRA_profile_M_and_h.rds")
plot(SRA, file = "SRA_profile_M_and_h", dir = getwd(), open_file = FALSE, f_name = SRA_data$f_name, s_name = SRA_data$s_name,
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
mod <- SP_SS(Data = SRA@OM@cpars$Data, AddInd = 1:5, use_r_prior = TRUE, start = list(r_prior = c(0.068, 0.03)))
plot(mod, dir = getwd(), filename = "mse/scoping/report_SP", open_file = FALSE)

# Plot SRA data age comps
png("Commerical_age_comp.png", height = 6, width = 6, res = 400, units = 'in')
plot(SRA_data$CAA[72,,1], typ = 'l', xlab = "Age", ylab = "Proportion")
plot_composition(c(1918:2019)[-92], SRA_data$s_CAA[-92,,1], ind = rowSums(SRA_data$s_CAA[-92,,1], na.rm = TRUE) > 0)
dev.off()
