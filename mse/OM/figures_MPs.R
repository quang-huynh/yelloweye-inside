

# Merge MSE objects

merge_MSE <- function(...) {
  dots <- list(...)

  slots_identical <- function(slotname, x = dots, is_logical = FALSE) {
    res <- lapply(x, getElement, slotname)
    is_identical <- all(vapply(res[-1], identical, logical(1), res[[1]]))
    if(is_logical) {
      return(is_identical)
    } else return(unique(do.call(c, res)))
  }

  slots_identical("Name")
  slots_identical("nyears")
  slots_identical("proyears")
  slots_identical("nsim")

  stopifnot(slots_identical("OM", is_logical = TRUE))
  stopifnot(slots_identical("Obs", is_logical = TRUE))
  stopifnot(slots_identical("SSB_hist", is_logical = TRUE))
  stopifnot(slots_identical("CB_hist", is_logical = TRUE))
  stopifnot(slots_identical("FM_hist", is_logical = TRUE))

  nMPs <- vapply(dots, getElement, numeric(1), "nMPs")

  slotvec <- c("B_BMSY", "F_FMSY", "B", "SSB", "VB", "FM", "C", "TAC", "Effort")
  res <- list()
  for(i in 1:length(slotvec)) {
    new_mat <- array(NA, dim = c(slots_identical("nsim"), sum(nMPs), slots_identical("proyears")))
    for(j in 1:length(dots)) {
      if(j == 1) new_mat[, 1:nMPs[1], ] <- getElement(dots[[j]], slotvec[i])
      if(j > 1) new_mat[, (sum(nMPs[1:(j-1)]) + 1):(sum(nMPs[1:(j-1)]) + nMPs[j]), ] <- getElement(dots[[j]], slotvec[i])
    }
    res[[i]] <- new_mat
  }

  slotvec2 <- c("PAA", "CAA", "CAL")
  res2 <- list()
  for(i in 1:length(slotvec2)) {
    maxage <- dim(dots[[1]]@PAA)[3]
    if(i < 3) new_mat <- array(NA, dim = c(slots_identical("nsim"), sum(nMPs), maxage))
    if(i == 3) new_mat <- array(NA, dim = c(slots_identical("nsim"), sum(nMPs), length(slots_identical("CALbins"))))
    for(j in 1:length(dots)) {
      if(j == 1) new_mat[, 1:nMPs[1], ] <- getElement(dots[[j]], slotvec2[i])
      if(j > 1) new_mat[, (sum(nMPs[1:(j-1)]) + 1):(sum(nMPs[1:(j-1)]) + nMPs[j]), ] <- getElement(dots[[j]], slotvec2[i])
    }
    res2[[i]] <- new_mat
  }

  ## Create MSE Object ####
  MSEout <- new("MSE", Name = slots_identical("Name"), nyears = slots_identical("nyears"),
                proyears = slots_identical("proyears"), nMPs = length(slots_identical("MPs")),
                MPs = slots_identical("MPs"), nsim = slots_identical("nsim"),
                OM = dots[[1]]@OM, Obs = dots[[1]]@Obs, B_BMSY = res[[1]], F_FMSY = res[[2]], B = res[[3]], SSB = res[[4]],
                VB = res[[5]], FM = res[[6]], res[[7]], TAC = res[[8]], SSB_hist = dots[[1]]@SSB_hist, CB_hist = dots[[1]]@CB_hist,
                FM_hist = dots[[1]]@FM_hist, Effort = res[[9]], PAA = res2[[1]], CAA = res2[[2]], CAL = res2[[2]],
                CALbins = slots_identical("CALbins"), Misc = list(Data = do.call(c, lapply(dots, function(x) x@Misc$Data))))

  # Store MSE info
  attr(MSEout, "version") <- packageVersion("DLMtool")
  attr(MSEout, "date") <- date()
  attr(MSEout, "R.version") <- R.version

  MSEout
}

Map_merge_MSE <- function(x, y) merge_MSE(x, y)

# Annual probability of B > 0.4 BMSY
B_threshold <- function(x, val = 0.4) {
  apply(x@B_BMSY, c(2, 3), function(y) sum(y >= val)/length(y)) %>% structure(dimnames = list(x@MPs, 1:x@proyears)) %>% t()
}

# Incrases in biomass over time interval
B_increase <- function(MSE, interval = 10) {
  ind <- which(1:MSE@proyears %% interval == 1)
  output <- matrix(NA, MSE@nMPs, length(ind) - 1) %>% structure(dimnames = list(MSE@MPs, ind[-1]))
  for(i in 1:(length(ind)-1)) output[, i] <- apply(MSE@B[,,ind[i+1]] >= MSE@B[,,ind[i]], 2, function(x) sum(x)/length(x))
  return(output %>% t())
}


mean_yield <- function(x, type = c("arithmetic", "geometric"), annual = TRUE) {
  type <- match.arg(type)
  if(type == "arithmetic") fun <- mean
  if(type == "geometric") fun <- function(y) log(y) %>% sum() %>% match.fun("/")(length(y)) %>% exp()

  if(annual) {
    apply(x@C, c(2, 3), fun) %>% structure(dimnames = list(x@MPs, 1:x@proyears)) %>% t()
  } else {
    apply(x@C, c(1, 2), fun) %>% structure(dimnames = list(1:x@nsim, x@MPs)) %>% apply(2, quantile, probs = c(0.25, 0.5, 0.75))
  }
}


plot_B_threshold <- function(MSElist, scenario_names, MP_names, val = 0.4, MGT = 56, prob_ref = 0.56) {
  PB40 <- lapply(MSElist, B_threshold, val = val)
  par(mfrow = c(3, 1), oma = c(7, 5, 1, 3), mar = rep(0, 4))
  for(i in 1:length(PB40)) {
    matplot(2020:(2020 + 100 - 1), PB40[[i]], ylim = c(0, 1.1), type = 'n', xaxt = ifelse(i == length(PB40), "s", "n"),
            xlab = "", ylab = "")
    text(x = 2130, y = 0.5 * 1.1, labels = scenario_names[i], srt = -90, xpd = NA, font = 2)
    abline(h = prob_ref, v = 2020 + MGT, lwd = 1.5, col = "grey")
    matlines(2020:(2020 + 100 - 1), PB40[[i]], lwd = 1.5)
  }
  mtext("Year", side = 1, line = 3, outer = TRUE)
  mtext("Probability B > 40% BMSY", side = 2, line = 3, outer = TRUE)

  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
  legend("bottom", MP_names, col = 1:6, lty = 1:5, xpd = TRUE, horiz = TRUE, bty = "n", lwd = 1.5)

  invisible()
}


plot_B_increase <- function(MSElist, scenario_names, MP_names, interval = 10) {
  B_increase <- lapply(MSElist, B_increase, interval = interval)
  par(mfrow = c(3, 1), oma = c(7, 5, 1, 3), mar = rep(0, 4))
  for(i in 1:length(B_increase)) {
    yr_inc <- rownames(B_increase[[i]]) %>% as.numeric()
    matplot(2020 + yr_inc - 1, B_increase[[i]], xlim = c(2020, 2120), ylim = c(0, 1.1), type = "o", xaxt = ifelse(i == length(B_increase), "s", "n"),
            xlab = "", ylab = "", lwd = 1.5)
    text(x = 2130, y = 0.5 * 1.1, labels = scenario_names[i], srt = -90, xpd = NA, font = 2)
  }
  mtext("Year", side = 1, line = 3, outer = TRUE)
  mtext("Probability of increasing biomass", side = 2, line = 3, outer = TRUE)

  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
  legend("bottom", MP_names, col = 1:6, lty = 1:5, xpd = TRUE, horiz = TRUE, bty = "n", lwd = 1.5)

  invisible()
}

plot_B_BMSY <- function(MSElist, scenario_names, MP_names, fun = c("median", "mean"), MGT = 38) {
  fun <- match.arg(fun) %>% match.fun()
  B_BMSY <- lapply(MSElist, function(x) apply(x@B_BMSY, c(2, 3), fun) %>% structure(dimnames = list(x@MPs, 1:x@proyears)) %>% t())

  par(mfrow = c(3, 1), oma = c(7, 5, 1, 3), mar = rep(0, 4))
  for(i in 1:length(B_BMSY)) {
    matplot(2020:(2020 + 100 - 1), B_BMSY[[i]], ylim = c(0, 3), type = 'n', xaxt = ifelse(i == length(B_BMSY), "s", "n"),
            xlab = "", ylab = "")
    text(x = 2130, y = 0.5 * 3, labels = scenario_names[i], srt = -90, xpd = NA, font = 2)
    abline(h = c(0.4, 0.8), v = 2020 + MGT, col = "grey")
    matlines(2020:(2020 + 100 - 1), B_BMSY[[i]], lwd = 1.5)
  }
  mtext("Year", side = 1, line = 3, outer = TRUE)
  mtext(expression(B/B[MSY]), side = 2, line = 3, outer = TRUE)

  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
  legend("bottom", MP_names, col = 1:6, lty = 1:5, xpd = TRUE, horiz = TRUE, bty = "n", lwd = 1.5)

  invisible()
}


plot_mean_yield <- function(MSElist, scenario_names, MP_names, fun = c("median", "mean"), MGT = 38) {
  fun <- match.arg(fun) %>% match.fun()
  yield <- lapply(MSElist, mean_yield, type = "arithmetic")
  #B_BMSY <- lapply(MSElist, function(x) apply(x@B_BMSY, c(2, 3), fun) %>% structure(dimnames = list(x@MPs, 1:x@proyears)) %>% t())

  par(mfrow = c(3, 1), oma = c(7, 5, 1, 3), mar = rep(0, 4))
  for(i in 1:length(yield)) {
    matplot(2020:(2020 + 100 - 1), yield[[i]], ylim = c(0, 1.1 * max(yield[[i]])), type = 'n', xaxt = ifelse(i == length(yield), "s", "n"),
            xlab = "", ylab = "")
    text(x = 2130, y = 0.5 * 1.1 * max(yield[[i]]), labels = scenario_names[i], srt = -90, xpd = NA, font = 2)
    abline(h = 0, v = 2020 + MGT, col = "grey")
    matlines(2020:(2020 + 100 - 1), yield[[i]], lwd = 1.5)
  }
  mtext("Year", side = 1, line = 3, outer = TRUE)
  mtext("Mean yield", side = 2, line = 3, outer = TRUE)

  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
  legend("bottom", MP_names, col = 1:6, lty = 1:5, xpd = TRUE, horiz = TRUE, bty = "n", lwd = 1.5)

  invisible()
}
