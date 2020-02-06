
library(MSEtool)
source("mse/OM/YE_MPs.R")
setup(12)
sfExportAll()
sfLibrary(gfdlm)


scenario <- c("upweight_dogfish", "base", "lowM", "low_catch", "pinniped",
              "sporadic_recruitment")
subsets <- c("fixedTAC", "index", "IDX", "SP")

for(i in 6:6) {
  SRA <- readRDS(paste0("mse/OM/", scenario[i], ".rds"))
  myOM <- SRA@OM

  message("Running ", scenario[i], " scenario...")

  if (i > 3) {
    # Constant catch scenarios
    myOM@interval <- c(1, rep(200, 4))
    message("Constant catch and ref MPs...")
    myMSE <- runMSE(myOM, MPs = c("FMSYref", "NF", "CC_5t", "CC_10t", "CC_15t"), parallel = TRUE)
    saveRDS(myMSE, file = paste0("mse/OM/MSE_", scenario[i], "_fixedTAC.rds"))
  }

  ## All other index-based MPs
  myOM@interval <- 1
  message("Other index-based MPs...")
  myMSE <- runMSE(myOM, MPs = c("ICI_YE", "ICI2_YE", "Iratio_YE", "GB_slope_YE", "IT5_YE", "IT10_YE", "Islope_YE"), parallel = TRUE)
  saveRDS(myMSE, file = paste0("mse/OM/MSE_", scenario[i], "_index.rds"))

  # IDX and SP_4080 with 5 and 10 yr intervals
  myOM@interval <- c(1, 1, 5, 10)
  message("IDX and SP4080...")
  myMSE <- runMSE(myOM, MPs = c("IDX_YE", "IDX_smooth_YE", "SP_4080", "SP_4080"), parallel = TRUE)
  myMSE@MPs[3:4] <- c("SP_4080_5f", "SP_4080_10f")
  saveRDS(myMSE, file = paste0("mse/OM/MSE_", scenario[i], "_IDX.rds"))

  # SP_2060 with 5 and 10 yr intervals and interim SP
  myOM@interval <- c(5, 10, 1)
  message("SP_2060 and interim SP_MSY...")
  myMSE <- runMSE(myOM, MPs = c("SP_2060", "SP_2060", "SP_interim"), parallel = TRUE)
  myMSE@MPs[1:2] <- c("SP_2060_5f", "SP_2060_10f")
  saveRDS(myMSE, file = paste0("mse/OM/MSE_", scenario[i], "_SP.rds"))

  message("Done for ", scenario[i], ".\n")
}

sfStop()
