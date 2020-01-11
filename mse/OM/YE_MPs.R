

##### Constant catch MPs of 0, 5, 10, or 15 tonnes
NF <- function(x, Data, reps) {
  Rec <- new("Rec")
  Rec@TAC <- rep(1e-8, reps)
  return(Rec)
}
class(NF) <- "MP"

CC_5t <- function(x, Data, reps) {
  Rec <- new("Rec")
  Rec@TAC <- rep(5, reps)
  return(Rec)
}
class(CC_5t) <- "MP"

CC_10t <- function(x, Data, reps) {
  Rec <- new("Rec")
  Rec@TAC <- rep(10, reps)
  return(Rec)
}
class(CC_10t) <- "MP"

CC_15t <- function(x, Data, reps) {
  Rec <- new("Rec")
  Rec@TAC <- rep(15, reps)
  return(Rec)
}
class(CC_15t) <- "MP"

##### IDX that uses HBLL
IDX_YE <- function(x, Data, reps) {
  Data@Ind <- Data@AddInd[, 1, ]
  Data@CV_Ind <- Data@CV_AddInd[, 1, ]
  gfdlm::IDX(x, Data, reps)
}
class(IDX_YE) <- "MP"

IDX_smooth_YE <- function(x, Data, reps) {
  Data@Ind <- Data@AddInd[, 1, ]
  Data@CV_Ind <- Data@CV_AddInd[, 1, ]
  gfdlm::IDX_smooth(x, Data, reps)
}
class(IDX_smooth_YE) <- "MP"

##### SP model that only uses future HBLL
SP_YE <- function(x, Data, ...) {
  nyears <- ncol(Data@Cat)
  if(nyears > 102) Data@AddInd[, 2:5, 103:nyears] <- Data@CV_AddInd[, 2:5, 103:nyears] <- NA
  SP(x = x, Data = Data, AddInd = 1:5, start = list(r_prior = c(0.068, 0.03)), ...)
}
class(SP_YE) <- "Assess"

##### SP with 40-80 HCR
SP_4080 <- make_MP(SP_YE, HCR_ramp, LRP = 0.4, TRP = 0.8, RP_type = "SSB_SSBMSY", diagnostic = "min")
