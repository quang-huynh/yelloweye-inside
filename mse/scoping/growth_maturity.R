dat <- MO

# GROWTH
if (nrow(dat$survey_samples) >= 10) {
  
  check_convergence_tmb <- TRUE
  
  #if (spp == "yelloweye rockfish") # FIXME
    tmb_init <- list(k = 0.9, linf = 55, log_sigma = log(0.1), t0 = -1)
  #else
  #  tmb_init <- list(k = 0.5, linf = 40, log_sigma = log(0.1), t0 = -1)
  
  # Linf = 65.9, K = 0.04, t0 = -8.59, sigma = 0.14
  vb_m <- fit_vb(dat$survey_samples, sex = "male", method = "tmb",
                 too_high_quantile = 1, check_convergence_tmb = check_convergence_tmb,
                 tmb_init = tmb_init)
  # Linf = 66.8, K = 0.03, t0 = -10.3, sigma = 0.13
  vb_f <- fit_vb(dat$survey_samples, sex = "female", method = "tmb",
                 too_high_quantile = 1, check_convergence_tmb = check_convergence_tmb,
                 tmb_init = tmb_init)
  
  # Linf = 65.2, K = 0.04, t0 = -9.04, sigma = 0.14
  vb_a <- fit_vb(dat$survey_samples, sex = "all", method = "tmb",
                 too_high_quantile = 1, check_convergence_tmb = check_convergence_tmb,
                 tmb_init = tmb_init)
  
  # Plot figure and residual
  datpl <- data.frame(age = dat$survey_samples$age, length = dat$survey_samples$length) %>% 
    mutate(pred = 65.2 * (1 - exp(-0.04 * (age + 9.04))) * exp(-0.5 * 0.14^2)) %>% mutate(resid = length - pred) %>%
    mutate(resid_log = log(length/pred))
  plot(length ~ jitter(age), datpl)
  lines(length ~ age, vb_a[[1]], col = "red", lwd = 3)
  
  plot(resid ~ jitter(age), datpl)
  abline(h = 0)
  
  plot(resid_log ~ jitter(age), datpl)
  abline(h = 0)
  
  
  #vb <- list()
  #vb$m <- vb_m
  #vb$f <- vb_f
  #
  ## FIXME: these look way off; omitting them for now
  #if (identical(spp, "shortspine thornyhead")) {
  #  vb$f$predictions$age <- NA
  #  vb$f$predictions$length <- NA
  #  vb$f$pars <- list(k = NA, linf = NA, t0 = NA)
  #}
  #
  #g_vb <- plot_vb(object_female = vb$f, object_male = vb$m, french = french) +
  #  guides(colour = FALSE, fill = FALSE, lty = FALSE) +
  #  ggtitle(en2fr("Growth", french)) +
  #  xlab(en2fr("Age (years)", french)) + ylab(paste0(en2fr("Length",french), " (cm)"))
  
  lw_m <- fit_length_weight(dat$survey_samples, sex = "male", method = "tmb",
                            too_high_quantile = 1)
  lw_f <- fit_length_weight(dat$survey_samples, sex = "female", method = "tmb",
                            too_high_quantile = 1)
  
  # log_a = -11.24, b = 3.09
  lw_a <- fit_length_weight(dat$survey_samples, sex = "all", method = "tmb",
                            too_high_quantile = 1)
  
  #g_length_weight <-
  #  plot_length_weight(object_female = lw_f, object_male = lw_m, french = french) +
  #  ggplot2::theme(legend.position = c(0.9, 0.2),
  #                 legend.key.width = grid::unit(1.8, units = "char")) +
  #  ggplot2::guides(lty =
  #                    guide_legend(override.aes = list(lty = c(1, 2), lwd = c(.7, .7)))) +
  #  xlab(paste0(en2fr("Length",french), " (cm)")) + ylab(paste0(en2fr("Weight", french), " (kg)")) +
  #  ggtitle(en2fr("Length-weight relationship", french))
  
} 



#### MATURITY
if (sum(!is.na(dat$survey_samples$maturity_code)) > 10) {
  mat_age <- dat$survey_samples %>%
    fit_mat_ogive(
      type = "age",
      months = seq(1, 12))
} else {
  mat_age <- NA
}

prop <- mat_age[[3]]$data %>% group_by(female, age_or_length) %>% summarise(prop_mature = sum(mature)/n()) %>% acast(list("female", "age_or_length"))

# A50 = 14.4, A95 = 27.4, Mat = 0 if age <=7
plot(as.numeric(colnames(prop)), prop[1, ], typ = 'n', xlim = c(0, 101))
lines(as.numeric(colnames(prop)), prop[2, ], typ = "o", col = "red")

lines(glmm_fe ~ age_or_length, filter(mat_age[[2]], female == 1), col = "red", lwd = 3)


#type <- "none"
#
#if (length(mat_age) > 1L) {
#  sample_size <- mat_age$data %>% group_by(female, mature) %>%
#    summarise(N = n()) %>%
#    group_by(female) %>%
#    summarise(N_min = min(N, na.rm = TRUE))
#  
#  sample_size <- reshape2::melt(table(mat_age$data$mature, mat_age$data$female))
#  names(sample_size) <- c("mature", "female", "N")
#  sample_size <- sample_size %>%
#    group_by(female) %>%
#    summarise(N_min = min(N, na.rm = TRUE))
#  
#  # if prob. mature looks wrong, fake a low sample size to not plot it:
#  prob_mat <- mat_age$pred_data %>%
#    select(age_or_length, female, glmm_fe) %>%
#    unique() %>%
#    group_by(female) %>%
#    filter(age_or_length < quantile(age_or_length, probs = 0.1)) %>%
#    summarise(mean_mat = mean(glmm_fe, na.rm = TRUE))
#  
#  sample_size <- left_join(sample_size, prob_mat, by = "female") %>%
#    mutate(N_min = ifelse(mean_mat > 0.5, 0, N_min))
#  
#  if (nrow(sample_size) > 1L && length(unique(sample_size$female)) > 1L && nrow(prob_mat) >= 1L) {
#    if (sample_size[sample_size$female == 0, "N_min", drop = TRUE] < mat_min_n &&
#        sample_size[sample_size$female == 1, "N_min", drop = TRUE] < mat_min_n)
#      type <- "none"
#    if (sample_size[sample_size$female == 0, "N_min", drop = TRUE] >= mat_min_n &&
#        sample_size[sample_size$female == 1, "N_min", drop = TRUE] >= mat_min_n)
#      type <- "all"
#    if (sample_size[sample_size$female == 0, "N_min", drop = TRUE] >= mat_min_n &&
#        sample_size[sample_size$female == 1, "N_min", drop = TRUE] < mat_min_n)
#      type <- "male"
#    if (sample_size[sample_size$female == 0, "N_min", drop = TRUE] < mat_min_n &&
#        sample_size[sample_size$female == 1, "N_min", drop = TRUE] >= mat_min_n)
#      type <- "female"
#  }
#}
#
#if (length(mat_age) > 1L && type != "none") {
#  g_mat_age <- plot_mat_ogive(mat_age, prediction_type = type) +
#    guides(colour = FALSE, fill = FALSE, lty = FALSE) +
#    ggplot2::guides(lty = FALSE, colour = FALSE) +
#    ggtitle(en2fr("Age at maturity", french)) +
#    ggplot2::labs(x = en2fr("Age (years)", french), y = en2fr("Probability mature", french), colour =  en2fr("Sex", french), lty = en2fr("Sex", french))
#} else {
#  g_mat_age <- ggplot() + theme_pbs() + ggtitle(en2fr("Age at maturity", french)) +
#    ggplot2::labs(x = en2fr("Age (years)", french), y = en2fr("Probability mature", french)) +
#    ggplot2::guides(lty = FALSE, colour = FALSE)
#}
#