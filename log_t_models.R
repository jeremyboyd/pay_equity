# Author: Jeremy Boyd (jeremyboyd@pm.me)
# Description: ACS wage models with log-t likelihood.

library(broom.mixed)        # Tidy output for mixed effects models

# Read in data
df <- read_feather("Idaho_2019_ACS_wages.feather")

# Compute log wage
df <- df %>% mutate(log_wage = log(WAGP))

# Taking the log of WAGP leads to a more normal distribution. Still an unwanted
# bump in the right tail though.
df %>% ggplot(mapping = aes(x = WAGP)) +
    geom_density()
df %>% ggplot(mapping = aes(x = log_wage)) +
    geom_density()

# Model
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
    
    # Cmdstanr should be faster
    backend = "cmdstanr")

# Want to compare pp checks for log-t versus lognormal models. Also want to make sure this log-t model is converging.
saveRDS(fit_t, "log_t_model.RDS")

# Where can I see the tail parameter for t?
tidy(fit_t, effects = "fixed")
tidy(fit_t, effects = "ran_pars")

# All rhats <= 1.05
mcmc_plot(fit_t, type = "rhat_hist")

# Some neff ratios <= 0.5, smaller number <= 0.1
mcmc_plot(fit_t, type = "neff_hist")

# 55 total parameters have neff ratios <= 0.5
low_neff <- neff_ratio(fit_t)[neff_ratio(fit_t) < 0.5]

# Manually inspect traceplots of parameters with low neff. All are well mixed.
plot(fit_t, variable = names(low_neff))

# Facet plot showing histogram of empirical data with 10 replications. Seems
# like we're getting the rough distribution we want.
pp_check(fit, ndraws = 11, type = "hist")
pp_check(fit_t, ndraws = 11, type = "hist")

# Looks like we're underpredicting wages at our main peak, overpredicting in the
# right shoulder.
pp_check(fit, ndraws = 100, type = "dens_overlay")
pp_check(fit_t, ndraws = 100, type = "dens_overlay")

# Log-t fits the mean better than lognormal
pp_check(fit, ndraws = 100, type = "stat", stat = "mean")
pp_check(fit_t, ndraws = 100, type = "stat", stat = "mean")

pp_check(fit, ndraws = 100, type = "stat", stat = "median")
pp_check(fit_t, ndraws = 100, type = "stat", stat = "median")


# Both models are predicting mins that are too low
pp_check(fit, ndraws = 100, type = "stat", stat = "min")
pp_check(fit_t, ndraws = 100, type = "stat", stat = "min")

# Both models are predicting maxs that are too high
pp_check(fit, ndraws = 100, type = "stat", stat = "max")
pp_check(fit_t, ndraws = 100, type = "stat", stat = "max")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Visualize population-level pay distributions by gender ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Compute population-level predictions
pop_levelt <- msp(fit = fit_t,
                 new_data = expand_grid(gender = c("Male", "Female")) %>%
                     mutate(across(where(is.character), as_factor)))

# Convert log-space predictions to dollars
pop_levelt$pred$.epred <- exp(pop_levelt$pred$.epred)

# Summarize the population-level wage difference
pop_levelt$sum

# Compute female_male difference from dollar preds
d <- pop_levelt$pred %>%
    ungroup() %>%
    select(-.row) %>%
    pivot_wider(names_from = "gender", values_from = ".epred") %>%
    mutate(female_male = Female - Male)



# Figure of wage distribution by gender
fig_gender_dist_popt <- pop_levelt$pred %>%
    gen_plot(
        x_var = ".epred",
        x_label = "Annual wages",
        fill_var = "gender")

# Figure of sex effect
fig_diff_dist_popt <- d %>%
    gen_plot(
        x_var = "female_male",
        x_label = "Difference (female-male) in annual wages")

# Combined plot
(fig_gender_dist_popt | fig_diff_dist_popt)

# Predictions in log space
job_level_t <- msp(fit = fit_t,
                 new_data = expand_grid(
                     gender = unique(df$gender),
                     OCCP_label = unique(df$OCCP_label)) %>%
                     mutate(job.cat = str_extract(OCCP_label, "^[A-Z]+"),
                            job = str_extract(OCCP_label, "-.+$"),
                            job = str_remove(job, "^-"),
                            across(where(is.character), as_factor)) %>%
                     select(-OCCP_label))

# Convert log-space predictions to dollars
job_level_t$pred$.epred <- exp(job_level_t$pred$.epred)

job_diff_t <- job_level_t$pred %>%
    ungroup() %>%
    select(-.row) %>%
    pivot_wider(names_from = "gender", values_from = ".epred") %>%
    mutate(female_male = Female - Male)

sum_t <- job_diff_t %>%
    select(job.cat, job, female_male) %>%
    group_by(job.cat, job) %>%
    median_hdi() %>%
    arrange(desc(abs(female_male)))

# Number of jobs to fix is 187, so lower than normal model. Range of median adjustments is %5-29K, versus $6-57K. So there's a big practical difference between the normal and t-distributed models. In the t model there are fewer credible differences and smaller adjustments being recommended. We also seem to be getting better representation of mean wages based on pp checks.
sum_t %>%
    filter((.lower < 0 & .upper < 0 | .lower > 0 & .upper > 0)) %>%
    arrange(desc(abs(female_male)))



top <- sum_t %>%
    filter((.lower < 0 & .upper < 0 | .lower > 0 & .upper > 0)) %>%
    arrange(desc(abs(female_male))) %>%
    filter(row_number() %in% 1:10)

# Distributions by job and sex
fig_gender_dist_job_t <- job_level_t$pred %>%
    filter(job %in% top$job) %>%
    mutate(job = factor(job, levels = rev(as.character(top$job)))) %>%
    gen_plot(
        x_var = ".epred",
        x_label = "Annual wages",
        y_var = "job",
        y_label = "Job",
        fill_var = "gender")

# Distribution of sex differences by job
fig_diff_dist_job_t <- job_diff_t %>%
    filter(job %in% top$job) %>%
    mutate(job = factor(job, levels = rev(as.character(top$job)))) %>%
    gen_plot(
        x_var = "female_male",
        x_label = "Difference (female-male) in annual wages",
        y_var = "job")

# Put two figures together
# Predicted medians for jobs are lower in t versus normal model.
# Gender-based differences are smaller, closer to 0.
(fig_gender_dist_job_t | fig_diff_dist_job_t) +
    plot_annotation(
        title = "T Gender-based wage differences by job",
        subtitle = "Black dots represent medians. Thin and thick black bars are the 95% and 80% credible intervals, respectively.")


# For t-model, think I need to run a repair-measure loop and look at how it affects empirical wages.






# Save model
# saveRDS(fit, "fit varying effects of gender by job category and job.RDS")

