
library(MSEtool)
source("mse/OM/YE_MPs.R")
setup(12)
sfExportAll()
sfLibrary(gfdlm)


scenario <- c("upweight_dogfish", "base", "lowM")

for(i in 1:3) {
  SRA <- readRDS(paste0("mse/OM/", scenario[i], ".rds"))

  # Constant catch scenarios
  myOM <- SRA@OM
  myOM@interval <- 200
  myMSE <- runMSE(SRA@OM, MPs = c("NF", "FMSYref", "CC_5t", "CC_10t", "CC_15t"), parallel = TRUE)
  saveRDS(myMSE, file = paste0("mse/OM/MSE_", scenario[i], "_fixedTAC.rds"))

  # Index scenarios
  myOM <- SRA@OM
  myOM@interval <- 1
  message("Running ", scenario[i], " scenario...")
  myMSE <- runMSE(SRA@OM, MPs = c("IDX_YE", "IDX_smooth_YE"), parallel = TRUE)
  message("Done.\n")
  saveRDS(myMSE, file = paste0("mse/OM/MSE_", scenario[i], "_IDX.rds"))

}

