
library(MSEtool)
source("mse/OM/YE_MPs.R")
setup(12)
sfExportAll()
sfLibrary(gfdlm)


scenario <- c("upweight_dogfish", "base", "lowM")

for(i in 1:3) {
  SRA <- readRDS(paste0("mse/OM/", scenario[i], ".rds"))

  message("Running ", scenario[i], " scenario...")

  # Constant catch scenarios
  myOM <- SRA@OM
  myOM@interval <- 200
  message("Constant catch and ref MPs...")
  myMSE <- runMSE(myOM, MPs = c("NF", "FMSYref", "CC_5t", "CC_10t", "CC_15t"), parallel = TRUE)
  saveRDS(myMSE, file = paste0("mse/OM/MSE_", scenario[i], "_fixedTAC.rds"))

  # IDX and SP_4080
  myOM <- SRA@OM
  myOM@interval <- c(1, 5, 10)
  message("IDX...")
  myMSE <- runMSE(myOM, MPs = c("IDX_YE", "IDX_smooth_YE", "SP_4080", "SP_4080"), parallel = TRUE)
  myMSE@MPs[c(2:3)] <- c("SP_4080_5f", "SP_4080_10f")
  saveRDS(myMSE, file = paste0("mse/OM/MSE_", scenario[i], "_IDX.rds"))

  message("Done.\n")

}

sfStop()
