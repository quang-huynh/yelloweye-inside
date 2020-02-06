
library(MSEtool)
source("mse/OM/YE_MPs.R")


scenario <- c("upweight_dogfish", "base", "lowM", "low_catch", "pinniped",
              "sporadic_recruitment")
subsets <- c("fixedTAC", "index", "IDX", "SP")

for(i in c(1:4, 6)) {
  MSE <- lapply(subsets, function(x) readRDS(paste0("mse/OM/MSE/MSE_", scenario[i], "_", x, ".rds")))
  if(length(MSE[[4]]@MPs) == 2) MSE[[4]]@MPs <- c(MSE[[4]]@MPs, "SP_interim")

  out <- merge_MSE(MSE)
  saveRDS(out, file = paste0("mse/OM/MSE_", scenario[i], ".rds"))
  message("Done: ", scenario[i], "\n")
}
