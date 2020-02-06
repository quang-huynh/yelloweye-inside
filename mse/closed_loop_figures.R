library(MSEtool)
library(dplyr)

source('mse/OM/figures_MPs.R')
scenario <- c("base", "upweight_dogfish", "lowM")

fixedTAC <- lapply(scenario, function(x) readRDS(paste0("mse/OM/MSE_", x, "_fixedTAC.rds")))
IDX <- lapply(scenario, function(x) readRDS(paste0("mse/OM/MSE_", x, "_IDX.rds")))

scenario_names <- c("Base", "Up. dogfish", "M = 0.02")

MP_names <- fixedTAC[[1]]@MPs
MP_names2 <- IDX[[1]]@MPs

#png("mse/OM_figures/B_threshold.png", height = 5, width = 4, res = 600, units = "in")
#plot_B_threshold(fixedTAC, scenario_names = scenario_names, MP_names = MP_names)
#dev.off()
#
#png("mse/OM_figures/B_increase.png", height = 5, width = 4, res = 600, units = "in")
#plot_B_increase(fixedTAC, scenario_names = scenario_names, MP_names = MP_names)
#dev.off()

#png("mse/OM_figures/B_BMSY.png", height = 5, width = 4, res = 600, units = "in")
#plot_B_BMSY(fixedTAC, scenario_names = scenario_names, MP_names = MP_names, fun = 'mean')
#dev.off()


rmarkdown::render("mse/OM/summary_01.13.2020.Rmd", "word_document")
