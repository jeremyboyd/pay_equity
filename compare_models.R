# Author: Jeremy Boyd (jeremyboyd@pm.me)
# Description: Comparison of models fit using different likelihoods--e.g,
# lognormal, logt, mixture models...

# Read in data and compute log wage
df <- read_feather("Idaho_2019_ACS_wages.feather")
df <- df %>% mutate(log_wage = log(WAGP))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### lognormal likelihood (or normal likelihood of log wages) ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fit_ln <- brm(
    log_wage ~ gender + (gender | job.cat/job),
    data = df,
    family = gaussian(),
    control = list(adapt_delta = 0.95),
    chains = 4,
    iter = 2000,
    warmup = 1000,
    cores = 4,
    seed = 1,
    backend = "cmdstanr")
saveRDS(fit_ln, "lognormal_model.RDS")

# Check rhats
mcmc_plot(fit_ln, type = "rhat_hist")

# Loo. PSIS is Pareto-smoothed importance sampling. Setting save_psis = TRUE
# saves the estimated shape parameters (Pareto-k) for later use.
loo_ln <- loo(fit_ln, save_psis = TRUE)

# We have a handful of Pareto-k estimates that are problematic. Try refitting
# once for each problematic observation and then combine back with
# non-problematic. Doesn't look like this changes anything--could be that this
# functionality is only available in models fit using rstanarm.
loo_ln3 <- loo(fit_ln, save_psis = TRUE, k_threshold = 0.7)
plot(loo_ln3, label_points = TRUE)

# The Pareto k diagnostic is used to assess the reliability of estimates for
# individual datapoints. Large values indicate datapoints that are difficult to
# predict. Using label_points = TRUE plots the row numbers of troublesome
# datapoints.
plot(loo_ln, label_points = TRUE)

# Check that the number of parameters in p_loo is less that the total number of parameters in the model. Number in the model first, then p_loo. This looks okay.
length(neff_ratio(fit_ln))
loo_ln

# Look at some of the datapoints with high pareto-k. These are either very low
# or very high.
df %>%
    filter(row_number() %in% c(3592, 3538, 941, 2097, 2261, 2679))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### logt likelihood (or student-t likelihood of log wages) ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fit_t <- brm(
    log_wage ~ gender + (gender | job.cat/job),
    data = df,
    family = student(),
    control = list(adapt_delta = 0.95),
    chains = 4,
    iter = 2000,
    warmup = 1000,
    cores = 4,
    seed = 1,
    backend = "cmdstanr")
saveRDS(fit_t, "logt_model.RDS")

# Check rhats
mcmc_plot(fit_t, type = "rhat_hist")

# Loo
loo_t <- loo(fit_t, save_psis = TRUE)

# One additional parameter here is nu, which controls tail thickness in the
# t-distribution.
length(neff_ratio(fit_t))

# Compare ln & t models on expected log predictive density (ELPD). Shows better
# fit when using a t-distributed likelihood.
loo_compare(loo_ln, loo_t)

# Pareto-k plot is more well-behaved using logt
plot(loo_t, label_points = TRUE)

# Shows that nu is between ~ 5.64. I think when you get to about nu = 40 you're
# pretty close to the normal distribution. Here we're learning a reasonable
# value of nu from the data, so this is additional evidence that the data are
# closer to t-distributed versus normally distributed.
posterior_summary(fit_t, probs = c(0.025, 0.975), variable = "nu")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### This isn't legit #####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fit_t2 <- brm(
    WAGP ~ gender + (gender | job.cat/job),
    data = df,
    family = student(link = "log"),
    control = list(adapt_delta = 0.95),
    chains = 4,
    iter = 2000,
    warmup = 1000,
    cores = 4,
    seed = 1,
    backend = "cmdstanr")
saveRDS(fit_t2, "logt2_model.RDS")

# Check rhats
mcmc_plot(fit_t2, type = "rhat_hist")

# Loo
loo_t2 <- loo(fit_t2, save_psis = TRUE)
plot(loo_t2, label_points = TRUE)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Compare lognormal versus t-distributed with log link #####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fit_ln2 <- brm(
    WAGP ~ gender + (gender | job.cat/job),
    data = df,
    family = lognormal(),
    control = list(adapt_delta = 0.95),
    chains = 4,
    iter = 2000,
    warmup = 1000,
    cores = 4,
    seed = 1,
    backend = "cmdstanr")
saveRDS(fit_ln2, "lognormal2_model.RDS")

# Loo
loo_ln2 <- loo(fit_ln2, save_psis = TRUE)
plot(loo_ln2, label_points = TRUE)

# Favors fit_ln2
loo_compare(loo_ln2, loo_t2)



# Do fit_2 and fit_t2 make the same predictions?
# Do fit_ln and fit_ln2 make the same predictions?



