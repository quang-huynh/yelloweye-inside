
#### Plot HBLL age of full selectivity against M, h, R0

SRA <- readRDS("mse/OM/upweight_dogfish.rds")
M <- SRA@OM@cpars$M_ageArray[,1,1]
h <- SRA@OM@cpars$h

R0 <- SRA@OM@cpars$R0

ageFS <- vapply(SRA@Misc, function(x) which(x$s_vul[1,,1] == 1)[1], numeric(1))
#ageFS <- lapply(SRA@Misc, function(x) x$s_vul_par[1,1])

plot(M, ageFS)
plot(h, ageFS)
plot(R0, ageFS)

png("AFS.png", height = 4, width = 4, units = "in", res = 400)
par(mfrow = c(2, 1))
plot(M, h, typ = 'n')
text(M[ageFS > 40], h[ageFS > 40], labels = ageFS[ageFS > 40] , col = 'red')
text(M[ageFS <= 40], h[ageFS <= 40], labels = ageFS[ageFS <= 40])

plot(R0, ageFS)

dev.off()


###### Plot future HBLL

MSE <- readRDS("mse/OM/MSE_upweight_dogfish_fixedTAC.rds")
MSE@MPs

SRA <- readRDS("mse/OM/upweight_dogfish.rds")
Perr_y <- SRA@OM@cpars$Perr_y
mean_Perr <- apply(Perr_y, 2, mean)
plot(mean_Perr)

source("mse/OM/YE_MPs.R")
library(DLMtool)
library(MSEtool)
setup(8)
MSE <- runMSE(SRA@OM, parallel = TRUE, MPs = c("CC_5t", "CC_15t"))

### 15 t TAC
hbll <- MSE@Misc$Data[[1]]@AddInd[,1,]
hbll_q <- hbll %>% apply(2, quantile, probs = c(0.025, 0.5, 0.975), na.rm = TRUE)
year <- 1918:2118
matplot(year, t(hbll_q), typ = 'o', pch = 16)

png("hbll_15t.png", height = 12, width = 8, units = 'in', res = 400)
png("hbll_15t_CV.png", height = 12, width = 8, units = 'in', res = 400)
par(mfrow = c(3, 1), mar = c(5,4,1,1))
plot(1918:2019, hbll_q[2, 1:MSE@nyears], xlim = c(2000, 2118), typ = 'o', xlab = "Year", ylab = "HBLL",
     ylim = c(0, 5e4))
points(2020:2118, hbll_q[2, MSE@nyears + 1:(MSE@proyears - 1)], pch = 16)
arrows(2020:2118, y0 = hbll_q[1, MSE@nyears + 1:(MSE@proyears - 1)], y1 = hbll_q[3, MSE@nyears + 1:(MSE@proyears - 1)],
       length = 0)

plot(1918:2019, hbll_q[2, 1:MSE@nyears], xlim = c(2000, 2118), typ = 'o', xlab = "Year", ylab = "HBLL",
     ylim = c(0, 5e4))
matlines(2020:2118, t(hbll[1:5, MSE@nyears + 1:(MSE@proyears - 1)]), lty = 1)

plot(1918:2019, hbll_q[2, 1:MSE@nyears], xlim = c(2000, 2030), typ = 'o', xlab = "Year", ylab = "HBLL",
     ylim = c(0, 5e4))
matlines(2020:2029, t(hbll[1:5, MSE@nyears + 1:10]), lty = 1)
dev.off()

# 5 t
hbll <- MSE@Misc$Data[[3]]@AddInd[,1,]
hbll_q <- hbll %>% apply(2, quantile, probs = c(0.025, 0.5, 0.975), na.rm = TRUE)
year <- 1918:2118
matplot(year, t(hbll_q), typ = 'o', pch = 16)

png("hbll_5t.png", height = 12, width = 8, units = 'in', res = 400)
par(mfrow = c(3, 1), mar = c(5,4,1,1))
plot(1918:2019, hbll_q[2, 1:MSE@nyears], xlim = c(2000, 2118), typ = 'o', xlab = "Year", ylab = "HBLL",
     ylim = c(0, 5e4))
points(2020:2118, hbll_q[2, MSE@nyears + 1:(MSE@proyears - 1)], pch = 16)
arrows(2020:2118, y0 = hbll_q[1, MSE@nyears + 1:(MSE@proyears - 1)], y1 = hbll_q[3, MSE@nyears + 1:(MSE@proyears - 1)],
       length = 0)

plot(1918:2019, hbll_q[2, 1:MSE@nyears], xlim = c(2000, 2118), typ = 'o', xlab = "Year", ylab = "HBLL",
     ylim = c(0, 5e4))
matlines(2020:2118, t(hbll[1:5, MSE@nyears + 1:(MSE@proyears - 1)]), lty = 1)

plot(1918:2019, hbll_q[2, 1:MSE@nyears], xlim = c(2000, 2030), typ = 'o', xlab = "Year", ylab = "HBLL",
     ylim = c(0, 5e4))
matlines(2020:2029, t(hbll[1:5, MSE@nyears + 1:10]), lty = 1)
dev.off()



### SRA_data rec length comps

SRA <- readRDS("mse/scoping/SRA_regwt_dogfish.rds")
SRA <- readRDS("mse/scoping/SRA_hi_fish_sel.rds")
CALpred <- SRA@mean_fit$report$CALpred[,,2]

rec_len <- colSums(SRA_data$CAL[,,2], na.rm = TRUE)
rec_len_prop <- rec_len/sum(rec_len)
#plot(SRA_data$Len_bins[1:15] + 25, rec_len, typ = "o", xlab = "Length (mm)", ylab = "Frequency")


#par(mfrow = c(1, 1))
#plot(SRA_data$Len_bins[1:15] + 25, rec_len, typ = "o", xlab = "Length (mm)", ylab = "Frequency",
#     xlim = c(100, 850))


# All years
CALpred_prop <- apply(CALpred, 1, function(x) x/sum(x))
#matplot(SRA@mean_fit$obj$env$data$length_bin*10, CALpred_prop, type = "l", xlim = c(100, 850), xlab = "Length (cm)",
#        ylab = "Frequency", ylim = c(0, 0.2)) # all years

png("rec_lens_upweight_dogfish.png", height = 4, width = 5, units = 'in', res = 400)

png("rec_lens_upweight_hi_fish_sel.png", height = 4, width = 5, units = 'in', res = 400)
matplot(SRA@mean_fit$obj$env$data$length_bin*10, CALpred_prop,
        type = "l", xlim = c(100, 850), xlab = "Length (mm)",
        ylab = "Frequency", ylim = c(0, 0.2)) # Since 2002
lines(SRA_data$Len_bins[1:15] + 25, rec_len_prop, col = "red", lwd = 2,
      xlim = c(100, 850))

dev.off()



CALpred <- lapply(sra_ye$upweight_dogfish@Misc, function(x) x$CALpred[1918:2009 >= 2002,,2])
CALpred <- do.call(rbind, CALpred)

CALpred_prop <- apply(CALpred, 1, function(x) x/sum(x))
