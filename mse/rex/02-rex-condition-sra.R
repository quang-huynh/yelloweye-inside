library("dplyr")
library("DLMtool")
library("MSEtool")
library("here")
library("purrr")
library("cowplot")
library("ggplot2")

species_name <- "Rex Sole"
starting_year <- 1996
ending_year <- 2018
all_years <- seq(starting_year, ending_year)
nyear <- length(all_years)

fig_dir <- here("report", "figure")
if (!dir.exists(fig_dir)) dir.create(fig_dir)

drex <- readRDS(here("generated-data", "rex-filter-data.rds"))

rex_om <- readRDS(here("generated-data", "rex-om.rds"))
rex_om@M
rex_om@Linf
rex_om@K
rex_om@t0
rex_om@a
rex_om@b
rex_om@L50
rex_om@L50_95
rex_om@LFS
rex_om@L5
rex_om@Cobs
rex_om@Perr
rex_om@Msd <- c(0, 0)
rex_om@Linfsd <- c(0, 0)
rex_om@Ksd <- c(0, 0)
rex_om@nyears
rex_om@maxage
rex_om@M
rex_om@maxage
rex_om@h
rex_om@L5
rex_om@LFS
assertthat::assert_that(identical(rex_om@nyears, length(all_years)))

make_cal <- function(dat, survey, yrs, length_bin = 2) {
  cal <- dat %>%
    dplyr::filter(survey_abbrev == survey, sex == 2) %>%
    gfdlm::tidy_cal(yrs = yrs, interval = length_bin)

  length_bins <- gfdlm:::get_cal_bins(cal, length_bin_interval = length_bin)
  list(cal = cal[1, , ], length_bins = length_bins)
}

cal_wcvi <- make_cal(drex$survey_samples, "SYN WCVI", yrs = all_years)

if ("catch" %in% names(drex)) {
  catch <- drex$catch %>%
    gfplot::tidy_catch() %>%
    group_by(year) %>%
    summarize(value = sum(value)) %>%
    right_join(tibble(year = all_years), by = "year") %>%
    pull(value)
  saveRDS(catch, file = here::here("generated-data", "rex-catch.rds"))
} else {
  catch <- readRDS(here::here("generated-data", "rex-catch.rds"))
}
catch
plot(all_years, catch, type = "o")

# catch per unit effort from the trawl fleet only:
# cpue <- readRDS("../../gfs/report/cpue-cache/rex-sole.rds")
# readr::write_csv(cpue, path = here("generated-data", "rex-cpue.csv"))
cpue <- read.csv(here("generated-data", "rex-cpue.csv")) %>%
  filter(area == "3CD")

indexes <- drex$survey_index %>%
  dplyr::filter(survey_abbrev %in% c("SYN WCVI")) %>%
  select(year, biomass, re) %>%
  right_join(tibble(year = all_years),by  = "year") %>%
  left_join(rename(select(cpue, year, est, se_link), trawl_cpue = est, trawl_sd = se_link), by = "year")

indexes %>% as.data.frame()
plot(all_years, indexes$biomass, type = "p")
plot(all_years, indexes$trawl_cpue, type = "p")

# MSEtool::plot_composition(all_years,
#   obs = cal_wcvi$cal,
#   CAL_bins = cal_wcvi$length_bins
# )

rex_om@nsim <- 210 # 10 extras in case some don't converge
cores <- floor(parallel::detectCores() / 2)

rex_om@Cobs <- c(0, 0)
rex_om@Cbiascv <- c(0, 0)
rex_om@D <- c(0.3, 0.8) # gets replaced

# Set up alternative OMs for reference set and robustness set -----------------

saveRDS(indexes, file = "generated-data/rex-indexes.rds")
fit_sra_rex <- function(om, c_eq = 1, ...) {
  MSEtool::SRA_scope(rex_om,
    Chist = catch, Index = indexes$biomass,
    C_eq = c_eq * catch[1],
    I_sd = indexes$re, I_type = "1", cores = cores,
    drop_nonconv = TRUE, mean_fit = FALSE, ...
  )
}

rex_sra_ceq0 <- fit_sra_rex(rex_om, c_eq = 0)
rex_sra_ceq50 <- fit_sra_rex(rex_om, c_eq = 0.5)
rex_sra_ceq100 <- fit_sra_rex(rex_om, c_eq = 1)
rex_sra_ceq200 <- fit_sra_rex(rex_om, c_eq = 2)

saveRDS(rex_sra_ceq0, file = here("generated-data", "rex-sra-ceq0.rds"))
saveRDS(rex_sra_ceq50, file = here("generated-data", "rex-sra-ceq50.rds"))
saveRDS(rex_sra_ceq100, file = here("generated-data", "rex-sra-ceq100.rds"))
saveRDS(rex_sra_ceq200, file = here("generated-data", "rex-sra-ceq200.rds"))

median(rex_sra_ceq0@OM@cpars$D)
median(rex_sra_ceq50@OM@cpars$D)
median(rex_sra_ceq100@OM@cpars$D)
median(rex_sra_ceq200@OM@cpars$D)

# Add commercial CPUE:
reshape2::melt(indexes[,c(1, 2, 4)], id.vars = "year") %>%
  group_by(variable) %>% mutate(value = value/max(value, na.rm=TRUE)) %>%
  ggplot(aes(year, value, colour = variable)) + geom_point() + geom_path()

#s_CAL <- array(NA, c(rex_om@nyears, length(cal_wcvi$length_bins), 2L))
#s_CAL[,,1] <- cal_wcvi$cal
rex_sra_ceq200_cpue <- MSEtool::SRA_scope(rex_om,
  data = list(
    Chist = catch,
    #s_CAL = s_CAL,
    #ESS = c(50, 50),
    #length_bin = cal_wcvi$length_bins,
    #CAL = array(0, c(rex_om@nyears, length(cal_wcvi$length_bins), 1L)),
    Index = cbind(indexes$biomass, indexes$trawl_cpue),
    I_sd = cbind(indexes$re, indexes$trawl_sd * 1.5),
    I_type = c("1", "1"),
    C_eq = 2 * catch[1]
  ),
  cores = 1,
  drop_nonconv = TRUE
)
#plot(rex_sra_ceq200_cpue)
saveRDS(rex_sra_ceq200_cpue, file = here("generated-data", "rex-sra-ceq200-cpue.rds"))

# Alternative Reference Set OMs: M --------------------------------------------
# Only look at higher M and time-varying M. M in BC unlikely to be lower than GOA.

rex_om@M
rex_om_high_m <- rex_om
rex_om_high_m@M <- c(0.25, 0.25)
rex_sra_high_m <- fit_sra_rex(rex_om_high_m)

saveRDS(rex_sra_high_m, file = here("generated-data", "rex-sra-high-m.rds"))

quantile(rex_sra_ceq50@OM@cpars$D)
quantile(rex_sra_high_m@OM@cpars$D)

# Alternative Reference Set OMs: Steepness (h) --------------------------------

rex_om_low_h <- rex_om
rex_om@h
rex_om_low_h@h <- c(0.5, 0.7)
rex_sra_low_h <- fit_sra_rex(rex_om_low_h)
saveRDS(rex_sra_low_h, file = here("generated-data", "rex-sra-low-h.rds"))

quantile(rex_sra_ceq50@OM@cpars$D)
quantile(rex_sra_low_h@OM@cpars$D)
median(rex_sra_ceq50@OM@cpars$D)
median(rex_sra_low_h@OM@cpars$D)

rex_om_high_h <- rex_om
rex_om@h
rex_om_high_h@h <- c(0.95, 0.95)
rex_sra_high_h <- fit_sra_rex(rex_om_high_h)
saveRDS(rex_sra_high_h, file = here("generated-data", "rex-sra-high-h.rds"))

quantile(rex_sra_ceq50@OM@cpars$D)
quantile(rex_sra_high_h@OM@cpars$D)

# Robustness Set OMs: M increasing/decreasing over time -----------------------

rex_om_inc_m <- rex_om
rex_om_inc_m@nyears
rex_om_inc_m@proyears
M_age <- array(0.2,
  c(rex_om_inc_m@nsim, rex_om_inc_m@maxage, rex_om_inc_m@nyears + rex_om_inc_m@proyears))
m <- seq(0.2, 0.4, length.out = rex_om_inc_m@proyears)
for (i in seq_along(m)) M_age[, , rex_om_inc_m@nyears + i] <- m[i]
plot(M_age[1,1,]) # one sim; one age
rex_om_inc_m@cpars$M_ageArray <- M_age
rex_sra_inc_m <- fit_sra_rex(rex_om_inc_m)
saveRDS(rex_sra_inc_m, file = here("generated-data", "rex-sra-inc-m.rds"))

rex_om_dec_m <- rex_om
M_age <- array(0.20,
  c(rex_om_dec_m@nsim, rex_om_dec_m@maxage, rex_om_dec_m@nyears + rex_om_dec_m@proyears))
m <- seq(0.20, 0.10, length.out = rex_om_dec_m@proyears)
for (i in seq_along(m)) M_age[, , rex_om_dec_m@nyears + i] <- m[i]
plot(M_age[1,1,]) # one sim; one age
rex_om_dec_m@cpars$M_ageArray <- M_age
rex_sra_dec_m <- fit_sra_rex(rex_om_dec_m)
saveRDS(rex_sra_dec_m, file = here("generated-data", "rex-sra-dec-m.rds"))

# Set up the scenario names ---------------------------------------------------

sc <- tibble::tribble(
  ~scenario,     ~scenarios_human,        ~scenario_type,
  "ceq50",       "Catch eq. 50%",         "Reference",
  "ceq100",      "Catch eq. 100%",        "Reference",
  "ceq200",      "Catch eq. 200%",        "Reference",
  "ceq200-cpue", "Catch eq. 200% + CPUE", "Reference",
  "high-m",      "M = 0.25",              "Reference",
  "low-h",       "h = 0.5-0.7",           "Reference",
  "high-h",      "h = 0.95",              "Reference",
  "inc-m",       "M increasing",          "Robustness",
  "dec-m",       "M decreasing",          "Robustness"
)
sc <- mutate(sc, order = seq_len(n()))
saveRDS(sc, file = "generated-data/rex-scenarios.rds")

sra_rex <- purrr::map(sc$scenario, ~ {
  readRDS(here("generated-data", paste0("rex-sra-", .x, ".rds")))
})
names(sra_rex) <- sc$scenario

# Some plots ------------------------------------------------------------------

# FIXME: get this into gfdlm:
get_depletion <- function(x, scenario) {
  depletion <- x@SSB / sapply(x@Misc, getElement, "E0_SR")

  # FIXME: BAD TEMPORARY HACK!!! SA: 2020-01-21
  if (scenario != "Catch eq. 200% + CPUE" & scenario != "Catch eq. 200%")
    depletion <- depletion[depletion[,1] > 0.05,]

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

g <- purrr::map2_df(sra_rex, sc$scenarios_human, get_depletion) %>%
  mutate(scenario = factor(scenario, levels = sc$scenarios_human)) %>%
  ggplot(aes(year, med, ymin = lwr, ymax = upr)) +
  geom_ribbon(fill = "grey90") +
  geom_ribbon(fill = "grey70", mapping = aes(ymin = lwr50, ymax = upr50)) +
  geom_line() +
  facet_wrap(vars(scenario)) +
  gfplot::theme_pbs() +
  labs(x = "Year", y = "Depletion")
ggsave(file.path(fig_dir, paste0("rex-compare-SRA-depletion-panel.png")),
  width = 8, height = 6
)

# FIXME: get this into gfdlm along with Quang's composition version:
get_sra_survey <- function(sra, sc_name, survey_names = c("SYN WCVI", "CPUE")) {
  n_surv <- dim(sra@Misc[[1]]$Ipred)[2]
  out2 <- purrr::map(seq_len(n_surv), function(i) {
    surveys <- do.call(cbind, purrr::map(sra@Misc, ~ .$Ipred[,i,drop=FALSE]))
    out <- reshape2::melt(surveys) %>%
      rename(year = Var1, iter = Var2)
    out$scenario <- sc_name
    out$survey <- survey_names[i]
    out
  })
  bind_rows(out2)
}
surv <- purrr::map2_dfr(sra_rex, sc$scenario, get_sra_survey)
surv <- left_join(surv, sc, by = "scenario")
surv$scenarios_human <- factor(surv$scenarios_human, levels = sc$scenarios_human)
surv$year <- surv$year + min(indexes$year) - 1

surv_plot <- surv %>%
  group_by(scenarios_human, survey) %>%
  mutate(geo_mean = exp(mean(log(value)))) %>%
  mutate(value = value/geo_mean)

surv_plot_distinct <- surv_plot %>% select(scenarios_human, survey, geo_mean) %>%
  distinct()

# FIXME: functionalize this:
indexes <- readRDS(here::here("generated-data/rex-indexes.rds"))
indexes1 <- bind_rows(data.frame(
  year = indexes$year,
  biomass = indexes$trawl_cpue,
  lwr = exp(log(indexes$trawl_cpue) - 2 * indexes$trawl_sd * 1.5), # FIXME: 1.5 * hardcoded
  upr = exp(log(indexes$trawl_cpue) + 2 * indexes$trawl_sd * 1.5), # FIXME: 1.5 * hardcoded
  survey = "CPUE"),
  data.frame(
    year = indexes$year,
    biomass = indexes$biomass,
    lwr = exp(log(indexes$biomass) - 2 * indexes$re),
    upr = exp(log(indexes$biomass) + 2 * indexes$re),
    survey = "SYN WCVI")) %>%
  left_join(surv_plot_distinct, by = "survey") %>%
  mutate(biomass = biomass / geo_mean, lwr = lwr / geo_mean, upr = upr / geo_mean)

# FIXME: BAD TEMPORARY HACK!!! SA: 2020-01-21
surv_plot2 <- surv_plot %>%
  group_by(iter, survey, scenarios_human) %>%
  group_split() %>%
  map_dfr(~{if(.$value[1] > 0.5 || .$scenario == "ceq200-cpue") .})

g <- ggplot(surv_plot2, aes(year, value,
  group = paste(iter, survey), colour = as.character(survey))) +
  geom_line(alpha = 0.05) +
  geom_pointrange(data = indexes1, mapping = aes(x = year, y = biomass, ymin = lwr, ymax = upr,
    fill = as.character(survey)), inherit.aes = FALSE, pch = 21, colour = "grey40") +
  facet_wrap(vars(scenarios_human)) +
  gfplot::theme_pbs() +
  scale_color_brewer(palette = "Set2", direction = -1) +
  scale_fill_brewer(palette = "Set2", direction = -1) +
  ylab("Scaled index value") + xlab("Year") + labs(colour = "Survey", fill = "Survey")
ggsave(here::here("report/figure/rex-index-fits.png"), width = 9, height = 7)

# FIXME: get this into gfdlm:
get_sra_selectivity <- function(sc_name) {
  sra <- sra_rex[[sc_name]]
  x <- do.call(cbind, purrr::map(sra@Misc, ~ .$vul_len))
  out <- reshape2::melt(x) %>%
    rename(Length = Var1, iter = Var2)
  out$scenario <- sc_name
  out
}
sel <- map_dfr(sc$scenario[1], get_sra_selectivity) # pick one
sel %>%
  ggplot(aes(Length, value, group = paste(iter))) +
  geom_line(alpha = 0.15) +
  gfplot::theme_pbs() +
  ylab("Selectivity") + xlab("Length") +
  coord_cartesian(expand = FALSE, ylim = c(-0.01, 1.01))
ggsave(here::here("report/figure/rex-selectivity.png"), width = 5, height = 3)
