library("DLMtool")
library("MSEtool")
library("dplyr")
library("purrr")
library("ggplot2")
library("gfdlm")
library("here")
# source(here("analysis/rex/plot-functions.R"))

# Settings --------------------------------------------------------------------
reference_mp <- c("FMSYref", "NFref")
sp <- "ye"

# Satisficing rules:

# 80% above LRP within 56 years (1.5 MGT), 95% within 1.5 MGT
# 50% above USR
LT_LRP_thresh <- 0.8
LT_USR_thresh <- 0.5
STC_thresh <- 0.7

# Set up PMs ------------------------------------------------------------------

`LT LRP` <- gfdlm::pm_factory("SBMSY", 0.4, c(56, 100))
`LT USR` <- gfdlm::pm_factory("SBMSY", 0.8, c(56, 100))

`LRP 1GT` <- gfdlm::pm_factory("SBMSY", 0.4, c(38, 38))
`LRP 1.5GT` <- gfdlm::pm_factory("SBMSY", 0.4, c(56, 56))
`USR 1.5GT` <- gfdlm::pm_factory("SBMSY", 0.8, c(56, 56))
`FMSY` <- DLMtool::PNOF
`AAVC` <- DLMtool::AAVY
`STC` <- gfdlm::pm_factory("LTY", 0.5, c(1, 10))
`LTC` <- gfdlm::pm_factory("LTY", 0.5, c(38, 38))
PM <- c("LRP 1GT", "LRP 1.5GT", "USR 1.5GT", "FMSY", "STC", "LTC", "AAVC")

# Set up and checks -----------------------------------------------------------
sc <- readRDS("mse/OM/ye-scenarios.rds")
#stopifnot(all(reference_mp %in% mp$mp))
fig_dir <- "mse/figures"
if (!dir.exists(fig_dir)) dir.create(fig_dir)

.ggsave <- function(filename, width, height) {
  ggsave(file.path(fig_dir, paste0(sp, "-", filename, ".png")),
    width = width, height = height
  )
}

get_filtered_scenario <- function(type, column) {
  filter(sc, scenario_type == type) %>%
    pull(!!column) %>%
    set_names()
}
scenarios <- sc$scenario %>% set_names()
scenarios_human <- sc$scenarios_human %>% set_names()
scenarios_ref <- get_filtered_scenario("Reference", "scenario")
scenarios_ref_human <- get_filtered_scenario("Reference", "scenarios_human")
scenarios_rob <- get_filtered_scenario("Robustness", "scenario")
scenarios_rob_human <- get_filtered_scenario("Robustness", "scenarios_human")

# Read OMs --------------------------------------------------------------------
om <- lapply(scenarios, function(x) readRDS(paste0("mse/OM/", x, ".rds"))@OM)
names(om) <- scenarios
#

mse <- lapply(scenarios, function(x) readRDS(paste0("mse/OM/MSE_", x, ".rds")))
names(mse) <- scenarios


# Satisficing -----------------------------------------------------------------

pm_df_list <- map(mse[scenarios_ref], ~ gfdlm::get_probs(.x, PM)) # List with all OMs and PMs
pm_df_list_rob <- map(mse[scenarios_rob], ~ gfdlm::get_probs(.x, PM)) # Robustness only
pm_df <- bind_rows(pm_df_list, .id = "scenario") # All as a data.frame



saveRDS(pm_df, file = "mse/ye-pm-all.rds")
pm_avg <- group_by(pm_df, MP) %>% summarise_if(is.numeric, mean) # Average across OMs
pm_min <- group_by(pm_df, MP) %>% summarise_if(is.numeric, min)

mp_sat <- pm_df_list[[1]]$MP[-19]
mp_fixedTAC <- mp_sat[1:5]
mp_index <- mp_sat[6:14]
mp_sp <- mp_sat[15:18]


mse_fixed <- purrr::map(scenarios, ~ DLMtool::Sub(mse[[.x]], MPs = mp_fixedTAC))
mse_index <- purrr::map(scenarios, ~ DLMtool::Sub(mse[[.x]], MPs = mp_index))
mse_sp <- purrr::map(scenarios, ~ DLMtool::Sub(mse[[.x]], MPs = mp_sp))

pm_df_list_fixed <- map(pm_df_list, ~filter(.x, MP %in% mp_fixedTAC))
pm_df_list_index <- map(pm_df_list, ~filter(.x, MP %in% mp_index))
pm_df_list_sp <- map(pm_df_list, ~filter(.x, MP %in% mp_sp))


pm_df_list_fixed_rob <- map(pm_df_list_rob, ~filter(.x, MP %in% mp_fixedTAC))
pm_df_list_index_rob <- map(pm_df_list_rob, ~filter(.x, MP %in% mp_index))
pm_df_list_sp_rob <- map(pm_df_list_rob, ~filter(.x, MP %in% mp_sp))

# Tigure plots ----------------------------------------------------------------

g <- gfdlm::plot_tigure(pm_df_list_fixed_rob[[3]])
g <- gfdlm::plot_tigure(pm_df_list_index[[1]])

ggsave('mse/figures/pm-table-fixed-low_catch.png', width= 5,height= 6.25)
ggsave('mse/figures/pm-table-fixed-base.png', width= 5,height= 6.25)
.ggsave("pm-table-avg", 4.25, 6.25)
g <- gfdlm::plot_tigure(pm_min,
  satisficed = c("LT LRP" = LT_LRP_thresh, "STC" = STC_thresh)
)
.ggsave("pm-table-min", 4.25, 6.25)

mp_order <- arrange(pm_avg, `LT LRP`, `LT USR`, `STC`, `LTC`, AAVC) %>%
  filter(MP %in% mp_sat_with_ref) %>% pull(MP)

g <- map(pm_df_list, filter, MP %in% mp_sat_with_ref) %>%
  set_names(scenarios_ref_human) %>%
  plot_tigure_facet()
.ggsave("pm-tigures-ref-set", 12, 7.5)

g <- map(pm_df_list_rob, filter, MP %in% mp_sat_with_ref) %>%
  set_names(scenarios_rob_human) %>%
  plot_tigure_facet()
.ggsave("pm-tigures-rob-set", 8.5, 3.3)

# Convergence -----------------------------------------------------------------

walk(names(mse_sat_with_ref), ~ {
  g <- gfdlm::plot_convergence(mse_sat_with_ref[[.x]], PM) +
    scale_color_brewer(palette = "Set2")
  .ggsave(paste0("converge-", .x), 6.5, 6.5)
})

walk(names(mse_sat), ~ {
  g <- gfdlm::plot_convergence(mse_sat[[.x]], PM) +
    scale_color_brewer(palette = "Set2")
  .ggsave(paste0("converge-", .x), 6.5, 6.5)
})

# Projections -----------------------------------------------------------------

walk(names(mse_fixed), ~ {
  g <- plot_main_projections(mse_fixed[[.x]],
    catch_breaks = c(0, 100, 200, 300),
    catch_labels = c("0", "100", "200", "300"))
  .ggsave(paste0("projections-fixedTAC-", .x), 6.5, 6.5)
}
)


walk(names(mse_index), ~ {
  g <- plot_main_projections(mse_index[[.x]],
                             catch_breaks = c(0, 100, 200, 300),
                             catch_labels = c("0", "100", "200", "300"))
  .ggsave(paste0("projections-index-", .x), 6.5, 6.5)
}
)


walk(names(mse_sp), ~ {
  g <- plot_main_projections(mse_sp[[.x]],
                             catch_breaks = c(0, 100, 200, 300),
                             catch_labels = c("0", "100", "200", "300"))
  .ggsave(paste0("projections-sp-", .x), 6.5, 6.5)
}
)

# Plot future HBLL
future_hbll <- function(mse, MPs) {
  MP_ind <- match(MPs, mse[[1]]@MPs)
  hbll <- lapply(mse, function(x) {
    lapply(x@Misc$Data[MP_ind], function(y) {
      apply(y@AddInd[, 1, ], 2, quantile, probs = c(0.025, 0.25, 0.5, 0.75, 0.975), na.rm = TRUE)
      })
  })
  par(mfrow = c(length(mse), length(MPs)), mar = c(0, 0, 0, 0), oma = c(5, 4, 3, 3))
  for(i in 1:length(mse)) {
    for(j in 1:length(MPs)) {
      matplot(c(1917 + 1:ncol(hbll[[i]][[j]]))[-c(1:82)], t(hbll[[i]][[j]][, -c(1:82)]), typ = 'l',
              lty = c(2, 1, 1, 2, 2), col = "black", lwd = c(1, 1, 2, 1, 1),
              yaxt = ifelse(j == 1, 's', 'n'), xaxt = ifelse(i == length(mse), "s", "n"))
      #if(j == 1) legend("topleft", names(mse)[i], bty = 'n')
      if(i == 1) {
        text(2060, 1.2 * max(hbll[[i]][[j]], na.rm = TRUE), MPs[j], xpd = NA, font = 2)
      }
      if(j == length(MPs)) {
        text(x = 2150, y = mean(range(hbll[[i]][[j]], na.rm=TRUE)), names(mse)[i], xpd = NA, srt = -90, font = 2)
      }
    }
  }
  mtext("Year", side = 1, outer = TRUE, line = 3)
  mtext("Index", side = 2, outer = TRUE, line = 3)
}


png("mse/figures/future_hbll.png", height = 8, width = 10, units = "in", res = 500)
future_hbll(mse, mp_index)
dev.off()





names(sc)[2] <- "scenario_human"
mp_sat_with_ref <- mp_sat
custom_pal <- structure(gplots::rich.colors(length(mp_sat)), names = mp_sat)
satisficed_criteria <- structure(rep(0, length(PM)), names = PM)
plots <- gfdlm::make_typical_plots(mse_list = mse, pm = PM, scenario_df = sc, this_year = this_year,
                                   mp_sat = mp_sat, mp_not_sat = mp_sat, mp_not_sat_highlight = mp_sat,
                                   eg_scenario = "base", custom_pal = custom_pal, satisficed_criteria = satisficed_criteria)




