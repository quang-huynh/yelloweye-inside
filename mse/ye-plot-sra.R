library("dplyr")
library("DLMtool")
library("MSEtool")
library("here")
library("purrr")
library("cowplot")
library("ggplot2")

species_name <- "Inside Yelloweye Rockfish"
starting_year <- 1918
ending_year <- 2019
all_years <- seq(starting_year, ending_year)
nyear <- length(all_years)

fig_dir <- "mse/figures"
if (!dir.exists(fig_dir)) dir.create(fig_dir)

# Set up the scenario names ---------------------------------------------------

sc <- tibble::tribble(
  ~scenario,     ~scenarios_human,        ~scenario_type,
  "base",                 "Base",             "Reference",
  "upweight_dogfish",     "Upweight dogfish survey",  "Reference",
  "low_catch",            "Low catch",                "Reference",
  "sporadic_recruitment", "Episodic recruitment",     "Reference",
  "lowM",                 "Low M (M=0.02)",           "Robustness",
  "pinniped",             "Pinniped mortality",               "Robustness"
)
sc <- mutate(sc, order = seq_len(n()))
saveRDS(sc, file = "mse/OM/ye-scenarios.rds")

sra_ye <- lapply(sc$scenario, function(x) readRDS(paste0("mse/OM/", x, ".rds")))
names(sra_ye) <- sc$scenario

mse <- lapply(sc$scenario, function(x) readRDS(paste0("mse/OM/MSE_", x, ".rds")))
names(mse) <- sc$scenario

# Some plots ------------------------------------------------------------------

# FIXME: get this into gfdlm:
get_depletion <- function(x, scenario, rel = TRUE, mse = NULL) {
  if(rel) {
    depletion <- x@SSB / sapply(x@Misc, getElement, "E0_SR")
  } else {
    depletion <- x@SSB
  }
  if(!is.null(mse)) depletion <- depletion/mse@Misc$MSYRefs[[1]]$Refs$SSBMSY

  d1 <- t(apply(depletion[, -nyear], 2,
    FUN = quantile,
    probs = c(0.025, 0.5, 0.975)
  )) %>%
    as.data.frame() %>%
    cbind(all_years) %>%
    mutate(scenario = scenario) %>%
    rename(lwr = 1, med = 2, upr = 3, year = all_years)
  d2 <- t(apply(depletion[, -nyear], 2, FUN = quantile, probs = c(0.25, 0.75))) %>%
    as.data.frame() %>%
    cbind(all_years) %>%
    rename(lwr50 = 1, upr50 = 2, year = all_years)

  left_join(d1, d2, by = "year")
}

g <- purrr::map2_df(sra_ye, sc$scenarios_human, get_depletion) %>%
  mutate(scenario = factor(scenario, levels = sc$scenarios_human)) %>%
  ggplot(aes(year, med, ymin = lwr, ymax = upr)) +
  geom_ribbon(fill = "grey90") +
  geom_ribbon(fill = "grey70", mapping = aes(ymin = lwr50, ymax = upr50)) +
  geom_line() +
  facet_wrap(vars(scenario)) +
  gfplot::theme_pbs() +
  labs(x = "Year", y = "Depletion")
ggsave(file.path(fig_dir, paste0("ye-compare-SRA-depletion-panel.png")), width = 8, height = 6)

g <- purrr::map2_df(sra_ye, sc$scenarios_human, get_depletion, rel = FALSE) %>%
  mutate(scenario = factor(scenario, levels = sc$scenarios_human)) %>%
  ggplot(aes(year, med, ymin = lwr, ymax = upr)) +
  geom_ribbon(fill = "grey90") +
  geom_ribbon(fill = "grey70", mapping = aes(ymin = lwr50, ymax = upr50)) +
  geom_line() +
  facet_wrap(vars(scenario)) +
  gfplot::theme_pbs() +
  labs(x = "Year", y = "Spawning biomass")
ggsave(file.path(fig_dir, paste0("ye-compare-SRA-SSB-panel.png")), width = 8, height = 6)

g <- do.call(rbind, Map(get_depletion, x = sra_ye, scenario = sc$scenarios_human, mse = mse, rel = FALSE)) %>%
  mutate(scenario = factor(scenario, levels = sc$scenarios_human)) %>%
  ggplot(aes(year, med, ymin = lwr, ymax = upr)) +
  geom_ribbon(fill = "grey90") +
  geom_ribbon(fill = "grey70", mapping = aes(ymin = lwr50, ymax = upr50)) +
  geom_line() + geom_hline(yintercept = c(0.4, 0.8), linetype = 3) + coord_cartesian(ylim = c(0, 4)) +
  facet_wrap(vars(scenario)) +
  gfplot::theme_pbs() +
  labs(x = "Year", y = expression(B/B[MSY]))
ggsave(file.path(fig_dir, paste0("ye-compare-SRA-SSB-MSY-panel.png")), width = 8, height = 6)




# FIXME: get this into gfdlm along with Quang's composition version:
surveys
#sra <- readRDS("mse/OM/pinniped.rds")
get_sra_survey <- function(sra, sc_name, survey_names = c("HBLL", "Dogfish", "CPUE_1", "CPUE_2", "CPUE_3")) {
  n_surv <- dim(sra@Misc[[1]]$Ipred)[2]
  out2 <- purrr::map(seq_len(n_surv), function(i) {
    surveys <- do.call(cbind, purrr::map(sra@Misc, ~ .$Ipred[,i,drop=FALSE]))
    out <- reshape2::melt(surveys) %>%
      rename(year = Var1, iter = Var2)
    out$year <- out$year
    out$scenario <- sc_name
    out$survey <- survey_names[i]
    out
  })
  bind_rows(out2)
}
#surv <- get_sra_survey(sra, "pinniped")
#surv <- left_join(surv, sc, by = "scenario")

surv <- purrr::map2_dfr(sra_ye, sc$scenario, get_sra_survey)
surv <- left_join(surv, sc, by = "scenario")
surv$scenarios_human <- factor(surv$scenarios_human, levels = sc$scenarios_human)
surv$year <- surv$year + min(all_years) - 1

#surv_plot <- surv %>%
#  group_by(scenarios_human, survey) %>%
#  mutate(geo_mean = exp(mean(log(value)))) %>%
#  mutate(value = value/geo_mean)
#
#surv_plot_distinct <- surv_plot %>% select(scenarios_human, survey, geo_mean) %>%
#  distinct()

# FIXME: functionalize this:
I_sd <- sra_ye[[1]]@data$I_sd %>% structure(dimnames = list(all_years, c("HBLL", "Dogfish", "CPUE_1", "CPUE_2", "CPUE_3"))) %>%
  as.data.frame() %>% cbind(data.frame(Year = all_years)) %>%
  reshape2::melt(id.vars = c("Year"), variable.name = "survey", value.name = "SD")

Index <- sra_ye[[1]]@data$Index %>% structure(dimnames = list(all_years, c("HBLL", "Dogfish", "CPUE_1", "CPUE_2", "CPUE_3"))) %>%
  as.data.frame() %>% cbind(data.frame(Year = all_years)) %>%
  reshape2::melt(id.vars = c("Year"), variable.name = "survey") %>% left_join(I_sd, by = c("Year", "survey"))

Index$lower <- exp(log(Index$value) - 2 * Index$SD * 1.5)
Index$upper <- exp(log(Index$value) + 2 * Index$SD * 1.5)

g <- ggplot(filter(surv, year >= 1980), aes(year, value, group = paste(iter, survey),
                            colour = as.character(survey))) +
  geom_line(alpha = 0.05) +
  geom_pointrange(data = Index, mapping = aes(x = Year, y = value, ymin = lower, ymax = upper,
                                                 fill = as.character(survey)), inherit.aes = FALSE, pch = 21, colour = "grey40") +
  facet_grid(survey~scenarios_human, scales = "free_y") +
  gfplot::theme_pbs() +
  scale_color_brewer(palette = "Set2", direction = -1) +
  scale_fill_brewer(palette = "Set2", direction = -1) +
  ylab("Index") + xlab("Year") + labs(colour = "Survey", fill = "Survey") + coord_cartesian(xlim = c(1980, 2020))
ggsave("mse/figures/ye-index-fits.png", width = 15, height = 10)

#plot(1:5, col = RColorBrewer::brewer.pal(5, "Set2"), pch = 16)
# HBLL only
g <- ggplot(filter(surv, year >= 1980 & survey == "HBLL"), aes(year, value, group = paste(iter))) +
  geom_line(alpha = 0.05, colour = "#66C2A5") +
  geom_pointrange(data = filter(Index, survey == "HBLL"), mapping = aes(x = Year, y = value, ymin = lower, ymax = upper),
                  inherit.aes = FALSE, pch = 21, colour = "grey40", fill = "#66C2A5") +
  facet_wrap(~scenarios_human) +
  gfplot::theme_pbs() +
  scale_color_brewer(palette = "Set2", direction = -1) +
  scale_fill_brewer(palette = "Set2", direction = -1) +
  ylab("Index") + xlab("Year") + coord_cartesian(xlim = c(1980, 2020))
ggsave("mse/figures/ye-index-HBLL.png", width = 8, height = 5)

g <- ggplot(filter(surv, year >= 2000 & survey == "HBLL"), aes(year, value, group = paste(iter))) +
  geom_line(alpha = 0.05, colour = "#66C2A5") +
  geom_pointrange(data = filter(Index, survey == "HBLL"), mapping = aes(x = Year, y = value, ymin = lower, ymax = upper),
                  inherit.aes = FALSE, pch = 21, colour = "grey40", fill = "#66C2A5") +
  facet_wrap(~scenarios_human) +
  gfplot::theme_pbs() +
  scale_color_brewer(palette = "Set2", direction = -1) +
  scale_fill_brewer(palette = "Set2", direction = -1) +
  ylab("Index") + xlab("Year") + coord_cartesian(xlim = c(2000, 2020))
ggsave("mse/figures/ye-index-HBLL2.png", width = 8, height = 5)

# Dogfish only
g <- ggplot(filter(surv, year >= 1980 & survey == "Dogfish"), aes(year, value, group = paste(iter))) +
  geom_line(alpha = 0.05, colour = "#FC8D62") +
  geom_pointrange(data = filter(Index, survey == "Dogfish"), mapping = aes(x = Year, y = value, ymin = lower, ymax = upper),
                  inherit.aes = FALSE, pch = 21, colour = "grey40", fill = "#FC8D62") +
  facet_wrap(~scenarios_human) +
  gfplot::theme_pbs() +
  scale_color_brewer(palette = "Set2", direction = -1) +
  scale_fill_brewer(palette = "Set2", direction = -1) +
  ylab("Index") + xlab("Year") + coord_cartesian(xlim = c(1980, 2020))
ggsave("mse/figures/ye-index-dogfish.png", width = 8, height = 5)



#indexes <- readRDS(here::here("generated-data/rex-indexes.rds"))
#indexes1 <- bind_rows(data.frame(
#  year = indexes$year,
#  biomass = indexes$trawl_cpue,
#  lwr = exp(log(indexes$trawl_cpue) - 2 * indexes$trawl_sd * 1.5), # FIXME: 1.5 * hardcoded
#  upr = exp(log(indexes$trawl_cpue) + 2 * indexes$trawl_sd * 1.5), # FIXME: 1.5 * hardcoded
#  survey = "CPUE"),
#  data.frame(
#    year = indexes$year,
#    biomass = indexes$biomass,
#    lwr = exp(log(indexes$biomass) - 2 * indexes$re),
#    upr = exp(log(indexes$biomass) + 2 * indexes$re),
#    survey = "SYN WCVI")) %>%
#  left_join(surv_plot_distinct, by = "survey") %>%
#  mutate(biomass = biomass / geo_mean, lwr = lwr / geo_mean, upr = upr / geo_mean)
#
## FIXME: BAD TEMPORARY HACK!!! SA: 2020-01-21
#surv_plot2 <- surv_plot %>%
#  group_by(iter, survey, scenarios_human) %>%
#  group_split() %>%
#  map_dfr(~{if(.$value[1] > 0.5 || .$scenario == "ceq200-cpue") .})
#
#g <- ggplot(surv_plot2, aes(year, value,
#  group = paste(iter, survey), colour = as.character(survey))) +
#  geom_line(alpha = 0.05) +
#  geom_pointrange(data = indexes1, mapping = aes(x = year, y = biomass, ymin = lwr, ymax = upr,
#    fill = as.character(survey)), inherit.aes = FALSE, pch = 21, colour = "grey40") +
#  facet_wrap(vars(scenarios_human)) +
#  gfplot::theme_pbs() +
#  scale_color_brewer(palette = "Set2", direction = -1) +
#  scale_fill_brewer(palette = "Set2", direction = -1) +
#  ylab("Scaled index value") + xlab("Year") + labs(colour = "Survey", fill = "Survey")
#ggsave(here::here("report/figure/rex-index-fits.png"), width = 9, height = 7)
#
## FIXME: get this into gfdlm:
get_sra_selectivity <- function(sc_name) {
  sra <- sra_ye[[sc_name]]
  x <- do.call(cbind, purrr::map(sra@Misc, ~ .$s_vul[101,,1]))
  out <- reshape2::melt(x) %>%
    rename(Length = Var1, iter = Var2)
  out$scenario <- sc_name
  out
}
sel <- map_dfr(sc$scenario, get_sra_selectivity) # pick one
sel <- left_join(sel, sc, by = "scenario")
sel$scenarios_human <- factor(sel$scenarios_human, levels = sc$scenarios_human)
sel %>%
  ggplot(aes(Length, value, group = paste(iter))) +
  geom_line(alpha = 0.15) +
  gfplot::theme_pbs() + facet_wrap(~scenarios_human) +
  ylab("Selectivity") + xlab("Age") +
  coord_cartesian(expand = FALSE, ylim = c(-0.01, 1.01))
ggsave("mse/figures/HBLL-selectivity.png", width = 8, height = 5)

# FIshery selectivity
sel2 <- data.frame(Age = 1:80, Commercial = sra_ye[[1]]@Misc[[1]]$vul[1,,1],
                   Recreational = sra_ye[[1]]@Misc[[1]]$vul[1,,2]) %>%
  reshape2::melt(id.var = "Age", variable.name = "Fleet", value.name = "Selectivity")
ggplot(sel2, aes(Age, Selectivity, linetype = Fleet)) + geom_line() + theme_pbs() +
  coord_cartesian(xlim = c(0, 40))
ggsave("mse/figures/fishery-selectivity.png", width = 7, height = 4)

#### Life history information
#Maturity

#Length-at-age

#histogram of M and h
samps <- data.frame(M = sra_ye[[1]]@OM@cpars$M, h = sra_ye[[1]]@OM@cpars$h)
png("mse/figures/M.png", res = 400, units = "in", height = 4, width = 5)
hist(samps$M, xlab = "M", main = "")
abline(v = 0.045, lty = 3, lwd = 2)
dev.off()

png("mse/figures/h.png", res = 400, units = "in", height = 4, width = 5)
hist(samps$h, xlab = "steepness", main = "")
abline(v = 0.71, lty = 3, lwd = 2)
dev.off()

ggplot(samps, aes(M)) + geom_histogram(bins = 12) + gfplot::theme_pbs()

ggplot(samps, aes(h)) + geom_histogram() + gfplot::theme_pbs()
#### Low/high catch
cat <- data.frame(Year = 1918:2019, Recreational = sra_ye$base@data$Chist[, 2],
                  Commerical_base = sra_ye$base@data$Chist[, 1],
                  Commerical_low = sra_ye$low_catch@data$Chist[, 1]) %>%
  reshape2::melt(id.var = "Year", variable.name = "Fleet", value.name = "Catch")

ggplot(cat, aes(Year, Catch, colour = Fleet)) + geom_line() + theme_pbs()
ggsave("mse/figures/catch.png", width = 7, height = 4)

### Index error and autocorrelation

get_indfit <- function(sra, scenario, quantiles = FALSE) {
  sim.index <- lapply(sra@Misc, function(x) x$Ipred[, 1])
  obs.ind <- sra@data$Index[, 1]
  out <- do.call(rbind, lapply(sim.index, DLMtool:::indfit, obs.ind = obs.ind))
  out$scenario <- scenario
}

future_index <- purrr::map2_df(sra_ye, sc$scenarios_human, get_indfit)

AC_plot <- future_index %>% group_by(scenario) %>%
  summarise(lower = quantile(AC, probs = 0.25), med = median(AC), upper = quantile(AC, probs = 0.75))
AC_plot$scenario <- factor(AC_plot$scenario, levels = sc$scenarios_human)
ggplot(AC_plot, aes(factor(scenario), med, ymin = lower, ymax = upper)) + geom_pointrange() + theme_pbs() +
  ylab("Autocorrelation of projected HBLL") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("mse/figures/HBLL_AC.png", width = 5, height = 4)

SD_plot <- future_index %>% group_by(scenario) %>%
  summarise(lower = quantile(sd, probs = 0.25), med = median(sd), upper = quantile(sd, probs = 0.75))
SD_plot$scenario <- factor(SD_plot$scenario, levels = sc$scenarios_human)
ggplot(SD_plot, aes(scenario, med, ymin = lower, ymax = upper)) + geom_pointrange() + theme_pbs() +
  ylab("SD of projected HBLL") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("mse/figures/HBLL_SD.png", width = 5, height = 4)


future_index <- readRDS("mse/HBLL_indfit.rds")
#for(i in 1:6) future_index[[i]]$scenario <- sc$scenarios_human[i]
#future_index <- do.call(rbind, future_index)


### COSEWIC metrics
COSEWIC_Bdecline_hist <- function(MSEobj, Ref = 0.7, Yr = NULL) {

  # Historic
  stopifnot(!is.null(Yr))

  if(length(Yr) > 1) stop("Length of Yr is one.")

  year_start <- MSEobj@nyears - abs(Yr) + 1
  if(year_start < 1) year_start <- 1
  SSB <- apply(MSEobj@SSB_hist, c(1, 3), sum)
  metric <- 1 - SSB[, MSEobj@nyears]/SSB[, year_start]
  out <- sum(metric >= Ref)/length(metric)
  return(out)
}

P_LRP <- function(MSEobj, LRP = 0.4) {
  SSB <- apply(MSEobj@SSB_hist, c(1, 3), sum)
  metric <- SSB[, MSEobj@nyears]/MSEobj@OM$SSBMSY
  out <- sum(metric >= LRP)/length(metric)
  return(out)
}



COSEWIC_P70 <- COSEWIC_P50 <- COSEWIC_P30 <- COSEWIC_Bdecline_hist
formals(COSEWIC_P50)$Ref <- 0.5
formals(COSEWIC_P30)$Ref <- 0.3

YE_COSEWIC <- rbind(vapply(mse, COSEWIC_P70, numeric(1), Yr = 3 * 38),
                    vapply(mse, COSEWIC_P50, numeric(1), Yr = 3 * 38),
                    vapply(mse, COSEWIC_P30, numeric(1), Yr = 3 * 38),
                    vapply(mse, P_LRP, numeric(1))) %>%
  structure(dimnames = list(c("P70", "P50", "P30", "LRP"),sc$scenarios_human)) %>%
    t() %>% as.data.frame()
YE_COSEWIC$MP <- factor(sc$scenarios_human, levels = sc$scenarios_human)

g <- gfdlm::plot_tigure(YE_COSEWIC, mp_order = rev(sc$scenarios_human))
ggsave("mse/figures/COSEWIC.png", height = 4, width = 3)

# Plot episodic recruitment
png("rec_dev.png", height = 4, width = 6, units = "in", res = 400)
matplot(1918:2119, t(sra_ye$upweight_dogfish@OM@cpars$Perr_y[1:5, -c(1:79)]), typ = 'l', lty = 1, xlab = "Year",
        ylab = "Recruitment deviations (normal space)")
dev.off()

png("rec_dev2.png", height = 4, width = 6, units = "in", res = 400)
matplot(t(sra_ye$sporadic_recruitment@OM@cpars$Perr_y[1:5, -c(1:79)]), typ = 'l', lty = 1, xlab = "Year",
        ylab = "Recruitment deviations (normal space)")
dev.off()
