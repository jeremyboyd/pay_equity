# Author: Jeremy Boyd (jeremyboyd@pm.me)
# Description: Detect and repair gender pay gaps.

# Resources
library(brms)
# library(broom.mixed)        # Tidy output for mixed effects models
library(feather)
library(patchwork)          # Combine ggplot objects

# TODO
# Also need another plotting function to view empirical job means/medians

# For adjust, want at least one option that adjusts lower wages so that they hit the median for the upper group. Maybe everyone below the median gets a raise? Maybe the further away from the median you are, the bigger the raise?

# Need to define priors for models
# Model as logt (robust) instead of lognormal.

# How good are these models at fitting the data? See posterior predictive checks. Also maybe think about computing a metric like RMSE and use that as a benchmark to compare lognormal versus log-t.

# One thing that could be going on with physicians is that the model just isn't as good at predicting high salaries. The empirical medians for both men & women is $366K, while the predicted medians are something like $240K & $180K, respectively. So the model seems to be undershooting high salaries and maybe magnifying the M-F difference.

# MAYBE TRY LOG-T DISTRIBUTION TO GET BETTER AT HIGHER SALARIES? Also compute RMSE for both lognormal and log-t.


# Read in data
df <- read_feather("Idaho_2019_ACS_wages.feather")

# See this blog post: https://www.andrewheiss.com/blog/2021/11/10/ame-bayes-re-guide/

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Initial model ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Correlated varying intercepts and slopes for job.cat and job. Jobs nested
# within job.cat.
fit <- brm(
    WAGP ~ gender + (gender | job.cat/job),
    data = df,
    family = lognormal(),
    control = list(adapt_delta = 0.95),
    chains = 4,
    iter = 4000,
    warmup = 1000,
    cores = 4,
    seed = 1,
    
    # Cmdstanr should be faster
    backend = "cmdstanr")

# Save model
# saveRDS(fit, "fit varying effects of gender by job category and job.RDS")
# fit <- readRDS("fit varying effects of gender by job category and job.RDS")

# Tidy summaries of fixed and random effects. Throws a warning:
# "posterior_samples" is deprecated, use as_draws instead." Probably because
# broom.mixed::tidy() hasn't been updated yet.
# broom.mixed::tidy(fit, effects = "fixed")
# broom.mixed::tidy(fit, effects = "ran_pars")

# NEED TO ADD AUTOMATED CHECKS OF RHAT AND POSSIBLY NEFF. If these aren't right
# the script should issue a warning message to user.

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Visualize population-level pay distributions by gender ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Compute population-level predictions
pop_level <- msp(fit = fit,
                 new_data = expand_grid(gender = c("Male", "Female")) %>%
                     mutate(across(where(is.character), as_factor)))

# Summarize the population-level wage difference
pop_level$sum

# Figure of wage distribution by gender
fig_gender_dist_pop <- pop_level$pred %>%
    gen_plot(
        x_var = ".epred",
        x_label = "Annual wages",
        fill_var = "gender")

# Figure of sex effect
fig_diff_dist_pop <- pop_level$diff %>%
    gen_plot(
        x_var = "female_male",
        x_label = "Difference (female-male) in annual wages")

# Combined plot
(fig_gender_dist_pop | fig_diff_dist_pop) +
    plot_annotation(
        title = "After controlling for job family and job we're 95% sure that the average female wage is $9-15K lower than the average male wage",
        subtitle = "Black dots represent medians; black bars represent the 95% (wider) and 80% (narrower) credible intervals")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Visualize job-level pay distributions by gender ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Can't use expand_grid to fully cross gender, job.cat, & job because job is
# nested in job.cat. Instead, fully cross gender & OCCP_label, then mutate to
# get just the combinations of job.cat and job that actually exist in the data.
job_level <- msp(fit = fit,
                 new_data = expand_grid(
                     gender = unique(df$gender),
                     OCCP_label = unique(df$OCCP_label)) %>%
                     mutate(job.cat = str_extract(OCCP_label, "^[A-Z]+"),
                            job = str_extract(OCCP_label, "-.+$"),
                            job = str_remove(job, "^-"),
                            across(where(is.character), as_factor)) %>%
                     select(-OCCP_label))

# Jobs where the gender difference in wages is 95% credible. Wow--this ends up
# being ~ 70% of jobs in the dataset!
job_level$sum %>%
    filter((.lower < 0 & .upper < 0 | .lower > 0 & .upper > 0)) %>%
    arrange(desc(abs(female_male)))

# Jobs with the 10 largest reliable salary gaps
top_ten <- job_level$sum %>%
    filter((.lower < 0 & .upper < 0 | .lower > 0 & .upper > 0)) %>%
    arrange(desc(abs(female_male))) %>%
    filter(row_number() %in% 1:10)

# Distributions by job and sex
fig_gender_dist_job <- job_level$pred %>%
    filter(job %in% top_ten$job) %>%
    mutate(job = factor(job, levels = rev(as.character(top_ten$job)))) %>%
    gen_plot(
        x_var = ".epred",
        x_label = "Annual wages",
        y_var = "job",
        y_label = "Job",
        fill_var = "gender")

# Distribution of sex differences by job
fig_diff_dist_job <- job_level$diff %>%
    filter(job %in% top_ten$job) %>%
    mutate(job = factor(job, levels = rev(as.character(top_ten$job)))) %>%
    gen_plot(
        x_var = "female_male",
        x_label = "Difference (female-male) in annual wages",
        y_var = "job")

# Put two figures together
(fig_gender_dist_job | fig_diff_dist_job) +
    plot_annotation(
        title = "Gender-based wage differences by job",
        subtitle = "Black dots represent medians. Thin and thick black bars are the 95% and 80% credible intervals, respectively.")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Visualize job.cat pay distributions by gender ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#  THIS DOESN'T WORK. have to edit msp() to do job.cat level preditions with correct random effects.
# job.cat_level <- msp(fit = fit,
#                  new_data = expand_grid(
#                      gender = unique(df$gender),
#                      job.cat = unique(df$job.cat)))
# 
# # Jobs where the gender difference in wages is 95% credible. Wow--this ends up
# # being ~ 70% of jobs in the dataset!
# job.cat_level$sum %>%
#     filter((.lower < 0 & .upper < 0 | .lower > 0 & .upper > 0)) %>%
#     arrange(desc(abs(female_male)))
# 
# # Jobs with the 10 largest reliable salary gaps
# top_ten <- job_level$sum %>%
#     filter((.lower < 0 & .upper < 0 | .lower > 0 & .upper > 0)) %>%
#     arrange(desc(abs(female_male))) %>%
#     filter(row_number() %in% 1:10)
# 
# # Distributions by job and sex
# fig_gender_dist_job <- job_level$pred %>%
#     filter(job %in% top_ten$job) %>%
#     mutate(job = factor(job, levels = rev(as.character(top_ten$job)))) %>%
#     gen_plot(
#         x_var = ".epred",
#         x_label = "Annual wages",
#         y_var = "job",
#         y_label = "Job",
#         fill_var = "gender")
# 
# # Distribution of sex differences by job
# fig_diff_dist_job <- job_level$diff %>%
#     filter(job %in% top_ten$job) %>%
#     mutate(job = factor(job, levels = rev(as.character(top_ten$job)))) %>%
#     gen_plot(
#         x_var = "female_male",
#         x_label = "Difference (female-male) in annual wages",
#         y_var = "job")
# 
# # Put two figures together
# (fig_gender_dist_job | fig_diff_dist_job) +
#     plot_annotation(
#         title = "Gender-based wage differences by job",
#         subtitle = "Black dots represent medians. Thin and thick black bars are the 95% and 80% credible intervals, respectively.")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Adjust & measure loop ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Compute table of jobs with gender wage imbalances
jobs_to_fix <- compute_imbalance(job_level_sum = job_level$sum,
                                 data = df)

# Loop while there's evidence of gender wage imbalance
while(nrow(jobs_to_fix) > 0) {
    
    # User message
    message(paste("Making wage adjustments for", nrow(jobs_to_fix),
                  "job(s)..."))
    
    # Adjust wages for jobs with wage imbalance
    adjustments <- map_dfr(1:nrow(jobs_to_fix), function(row) {
        job <- as.character(jobs_to_fix[row,]$job)
        raise <- abs(jobs_to_fix[row,]$female_male)
        df %>%
            filter(job == !!job,
                   gender == jobs_to_fix[row,]$lower_gender) %>%
            mutate(new_wage = WAGP + raise)
    })
    
    # No adjustments to these wages
    no_adjustments <- df %>%
        anti_join(adjustments, by = c("job", "gender")) %>%
        mutate(new_wage = WAGP)
    
    # New dataset including wage adjustments
    df2 <- bind_rows(adjustments, no_adjustments)
    
    # Model of adjusted wages
    fit_adjusted <- brm(
        new_wage ~ gender + (gender | job.cat/job),
        data = df2,
        family = lognormal(),
        control = list(adapt_delta = 0.95),
        chains = 4,
        iter = 4000,
        warmup = 1000,
        cores = 4,
        seed = 1,
        
        # Cmdstanr should be faster
        backend = "cmdstanr")
    
    # DID WHILE-LOOP EXIT right here after fitting model?
    
    # Save model
    saveRDS(fit_adjusted, "fit varying effects of gender by job category and job adjusted wages.RDS")
    
    job_level2 <- msp(fit = fit_adjusted,
                     new_data = expand_grid(
                         gender = unique(df$gender),
                         OCCP_label = unique(df$OCCP_label)) %>%
                         mutate(job.cat = str_extract(OCCP_label, "^[A-Z]+"),
                                job = str_extract(OCCP_label, "-.+$"),
                                job = str_remove(job, "^-"),
                                across(where(is.character), as_factor)) %>%
                         select(-OCCP_label))
    
    # Recompute table of jobs with gender wage imbalances
    jobs_to_fix <- compute_imbalance(job_level_sum = job_level2$sum,
                                     data = df)
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Review adjustments ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 20 jobs with biggest model-based gaps
job_subset <- job_level$sum %>%
    filter((.lower < 0 & .upper < 0 | .lower > 0 & .upper > 0)) %>%
    arrange(desc(abs(female_male))) %>%
    filter(row_number() %in% 1:20)

# All jobs with job.cat == MED
job_subset <- job_level$sum %>%
    filter((.lower < 0 & .upper < 0 | .lower > 0 & .upper > 0),
           job.cat == "MED") %>%
    mutate(job = fct_reorder(job, -abs(female_male)),
           job = fct_drop(job))
levels(job_subset$job)


df %>%
    filter(job == "Physicians") %>%
    group_by(gender) %>%
    summarize(n = sum(!is.na(WAGP)),
              mean = mean(WAGP, na.rm = TRUE), .groups = "drop")



bind_rows(
    df2 %>%
        filter(job %in% job_subset$job) %>%
        mutate(wage = WAGP,
               condition = "original"),
    df2 %>%
        filter(job %in% job_subset$job) %>%
        mutate(wage = new_wage,
               condition = "adjusted")) %>%
    group_by(job.cat, job, gender, condition) %>%
    summarize(n = sum(!is.na(wage)),
              median = median(wage, na.rm = TRUE), .groups = "drop") %>%
    mutate(condition = factor(condition,
                              levels = c("original", "adjusted")),
           job_label = str_wrap(paste0(job.cat, "-", job), width = 18),
           job = fct_relevel(job, levels(job_subset$job))) %>%
    filter(!job %in% c("Physicians", "Surgeons", "Dentists", "Nurse Anesthetists")) %>%
    ggplot(mapping = aes(x = condition, y = median, group = gender,
                         color = gender)) +
    geom_line() +
    scale_y_continuous(labels = label_dollar(prefix = "$",
                                             suffix = "K",
                                             scale = 1e-3)) +
    facet_wrap(~ job)

# No adjustment made to Physicians. It has the largest predicted difference, but the male & female medians are identical, so no adjustment is made. So what's the basis for the model thinking there's such a big gap?
# Why is an adjustment made to Pharmacists 








# Visualize original versus adjusted, summarized by job.cat only. These look
# much more reasonable. I think when you summarize by job you get results that
# look crazier because you're often dealing with low N, outliers, etc.
bind_rows(
    df4 %>%
        mutate(wage = WAGP,
               condition = "original"),
    df4 %>%
        mutate(wage = new_wage,
               condition = "adjusted")) %>%
    group_by(job.cat, sex, condition) %>%
    summarize(n = sum(!is.na(wage)),
              median = median(wage, na.rm = TRUE), .groups = "drop") %>%
    mutate(condition = factor(condition,
                              levels = c("original", "adjusted"))) %>%
    ggplot(mapping = aes(x = condition, y = median, group = sex, color = sex)) +
    geom_line() +
    scale_y_continuous(labels = label_dollar(prefix = "$",
                                             suffix = "K",
                                             scale = 1e-3)) +
    facet_wrap(~ job.cat)

# Weighted gender pay ratios for original versus adjusted wages. Shows that we
# go from 0.84 to 1.00. Perfect!
# Original
orig_fm <- df4 %>%
    group_by(job, sex) %>%
    summarize(n = sum(!is.na(WAGP)),
              mean = mean(WAGP, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = "sex", values_from = c("n", "mean")) %>%
    mutate(n = n_Male + n_Female) %>%
    filter(!is.na(n)) %>%
    mutate(fm_ratio = mean_Female / mean_Male)
n_orig <- sum(orig_fm$n)
orig_fm %>%
    mutate(weighted_fm_ratio = fm_ratio * n / n_orig) %>%
    pull(weighted_fm_ratio) %>%
    sum()

# Adjusted
adj_fm <- df4 %>%
    group_by(job, sex) %>%
    summarize(n = sum(!is.na(new_wage)),
              mean = mean(new_wage, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = "sex", values_from = c("n", "mean")) %>%
    mutate(n = n_Male + n_Female) %>%
    filter(!is.na(n)) %>%
    mutate(fm_ratio = mean_Female / mean_Male)
n_adj <- sum(adj_fm$n)
adj_fm %>%
    mutate(weighted_fm_ratio = fm_ratio * n / n_adj) %>%
    pull(weighted_fm_ratio) %>%
    sum()

# WANT TO AUTOMATE ENTIRE PROCESS. Think it's iterative (measurement-adjustment)
# until the measurement comes back with no statsig job differences and no
# statsig population-level difference. Also might be a good idea to allow the
# user to explore cost-equity tradeoffs: for example, maybe they just want to
# improve in the current cycle and don't have the money to get to complete
# equity. Want to be able to show how you'll get to X level by spending Y
# dollars. Should also summarize cost to get to different levels of equity. Also don't want to waste money, so how to dial back adjustments that overshoot the male mean/median?





