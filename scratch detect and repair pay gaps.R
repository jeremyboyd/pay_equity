# Author: Jeremy Boyd (jeremyboyd@pm.me)
# Description: Get individual-level pay data as American Community Survey (ACS)
# microdata.

# Resources
library(tidyverse)
library(brms)
library(scales)
library(tidybayes)
library(broom.mixed)
library(emmeans)
library(patchwork)
library(ggokabeito)

vars <- pums_variables %>%
    filter(year == 2019, survey == "acs1")

# Shows 531 different occupation labels
vars %>%
    filter(var_code == "OCCP") %>%
    count(val_label)

# Looks like the same labels that are used in OCCP...
vars %>%
    filter(var_code == "SOCP") %>%
    count(val_label)


# census_api_key("a845900cb625396f9b0d36c5d7e94c209c332521", install = TRUE,
#                overwrite = TRUE)

# For SEX, 1 = male, 2 = female
# WAGP is person-level wages or salary for the past 12 months
# AGEP is person-level age
# ESR is employment status recode; levels tell you whether someone is a civilian, military, unemployed, etc.
# COW is class of worker; stuff like state/federal employee, private company employee, etc.
df <- get_pums(
    variables = c("PUMA", "SEX", "AGEP", "OCCP", "NAICSP", "SOCP", "COW", "ESR",
                  "WKHP", "WAGP"),
    state = "ID",
    survey = "acs1",
    year = 2019,
    recode = TRUE,
    rep_weights = NULL,
    show_call = TRUE)

# Table of OCCP codes & labels
occp <- df %>%
    select(OCCP, OCCP_label) %>%
    unique()

# Table of NAICSP codes & labels
naics <- df %>%
    select(NAICSP, NAICSP_label) %>%
    unique()

# Table of SOCP codes & labels. Has same levels as occp, so probably the same.
socp <- df %>%
    select(SOCP, SOCP_label) %>%
    unique()

# Table of ESR codes & labels
esr <- df %>%
    select(ESR, ESR_label) %>%
    unique()

# Table of COW codes & labels
cow <- df %>%
    select(COW, COW_label) %>%
    unique()

# Table of SEX codes & labels
sex <- df %>%
    select(SEX, SEX_label) %>%
    unique()

# Table of ST & ST_label
state <- df %>%
    select(ST, ST_label) %>%
    unique()

# Get non-recode version of data
# Why is this downloading the data twice? Possible that setting rep_weights to NULL or setting show_call to FALSE get rid of this.
# Other possible occupation variables: NAICSP (North american industry classification system), SOCP (standard occupational classification system)
df2 <- get_pums(
    variables = c("PUMA", "SEX", "AGEP", "OCCP", "NAICSP", "SOCP", "COW", "ESR",
                  "WKHP", "WAGP"),
    state = "ID",
    survey = "acs1",
    year = 2019,
    recode = FALSE,
    rep_weights = NULL)

# Dataset with codes converted to labels
df3 <- df2 %>%
    left_join(occp, by = "OCCP") %>%
    left_join(esr, by = "ESR") %>%
    left_join(cow, by = "COW") %>%
    left_join(sex, by = "SEX") %>%
    left_join(state, by = "ST") %>%
    left_join(naics, by = "NAICSP") %>%
    left_join(socp, by = "SOCP") %>%
    select(SERIALNO, SPORDER, AGEP:PUMA, OCCP_label:SOCP_label) %>%
    
    # None of the factors should be ordered
    mutate(across(where(is.factor), ~ factor(.x, ordered = FALSE)),
           
           # Create job category and job from OCCP_label
           job.cat = str_extract(OCCP_label, "^[A-Z]+"),
           job = str_extract(OCCP_label, "-.+$"),
           job = str_remove(job, "^-"),
           sex = SEX_label,
           across(matches("job"), factor)) %>%
    
    # OCCP code 0009 doesn't seem to have a label. What is 0009?
    filter(!is.na(OCCP_label)) %>%
    
    # Drop rows with these values
    filter(!ESR_label %in% c("N/A (less than 16 years old)", "Unemployed",
                             "Not in labor force"),
           !COW_label %in% c(
        "Working without pay in family business or farm",
        "Unemployed and last worked 5 years ago or earlier or never worked"),
           
           # Only full-time
           !WKHP < 40,
           
           # Only 18+
           AGEP >= 18,
           
           # Only wages of 10K/year or more. Trying to exclude really low wages
           # that don't seem possible if you're working full time for a year.
           WAGP >= 10000) %>%
    
    # Drop any unused levels
    mutate(across(where(is.factor), fct_drop))

# Can use WKHP to define full-time. Is WKHP = 99 a special code? Doesn't look
# like it based on this histogram. If it were a special code then you'd probably
# have more of them.
ggplot(data = df3, mapping = aes(x = WKHP)) +
    geom_histogram() +
    scale_x_continuous(breaks = seq(0, 100, 10))

# Shows how OCCP labels are hierarchical--lots of specific jobs under SCI.
df3 %>%
    filter(str_detect(OCCP_label, "SCI")) %>%
    count(OCCP_label) %>%
    arrange(desc(n))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Model ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Starting with default priors, crossed random effects of occupation and PUMA
# (public use microdata area), which is a census geography that contains at
# least 100K people and is within a single state. This will allow us to estimate
# gender pay differences for each combination of occupation and geography.

# Hierarchical with jobs nested within job categories
fit2 <- brm(WAGP ~ 1 + SEX_label +
               (SEX_label|job_cat/job),
           family = lognormal(),
           data = df3,
           
           # Increase iter from 2K to 4K to get rid of low ESS warning
           iter = 4000,
           warmup = 1000,
           
           # Increasing from 0.8 to 0.9 gets rid of divergent transitions
           control = list(adapt_delta = 0.95))

# Save model for later use
# saveRDS(fit, "fit varying effects of gender by occupation and puma.RDS")
# saveRDS(fit, "fit varying effects of gender by job category and job.RDS")
# saveRDS(fit2, "job nested within job category.RDS")

# All rhats <= 1.05
mcmc_plot(fit, type = "rhat_hist")

# Some neff ratios <= 0.5, smaller number <= 0.1
mcmc_plot(fit, type = "neff_hist")

# 55 total parameters have neff ratios <= 0.5
low_neff <- neff_ratio(fit)[neff_ratio(fit) < 0.5]

# Manually inspect traceplots of parameters with low neff. All are well mixed.
plot(fit, variable = names(low_neff))

# Facet plot showing histogram of empirical data with 10 replications. Seems
# like we're getting the rough distribution we want.
pp_check(fit, ndraws = 11, type = "hist")

# Looks like we're underpredicting wages at our main peak, overpredicting in the
# right shoulder.
pp_check(fit, ndraws = 100, type = "dens_overlay")

# Looks like the mean of our predicted distributions is ~ 1K higher than
# empirical.
pp_check(fit, ndraws = 100, type = "stat", stat = "mean")

# Empirical is ~ 1K (which is very suspect for full-time annual wages); mean of
# distribution of min is ~ 2.8K.
pp_check(fit, ndraws = 100, type = "stat", stat = "min")

# Mean of distribution of max is too high
pp_check(fit, ndraws = 100, type = "stat", stat = "max") +
    scale_x_continuous(labels = label_number(suffix = "M", scale = 1e-6)) 

# THINK WE WANT TO DEFINE GENDER EFFECT FOR EACH COMBINATION OF PUMA & OCCUPATION, WITH 95% CrI. Want to highlight job-geo combinations where there are credible differences. See how I did this in the cutting model.

new_data <- expand.grid(
    SEX_label = unique(df3$SEX_label),
    OCCP_label = unique(df3$OCCP_label)) %>%
    as_tibble() %>%
    mutate(
        job_cat = str_extract(OCCP_label, "^[A-Z]+"),
        job = str_extract(OCCP_label, "-.+$"),
        job = str_remove(job, "^-"))
new_data$cond_num <- 1:nrow(new_data)
new_data$group_num <- rep(1:(nrow(new_data) / 2), each = 2)

# Each col represents a condition. Each col is a vector made up of 12K samples.
pred <- posterior_predict(
    fit2,
    newdata = new_data) %>%
    as.data.frame()

# Iterate over columns two at a time to compute gender effect distributions for
# each combination of occupation and puma.
male_idx <- seq(1, ncol(pred) - 1, 2)
female_idx <- seq(2, ncol(pred), 2)
gender_dists <- map2_dfc(male_idx, female_idx, function(x, y) {
    pred[, y] - pred[, x]}) %>%
    as.data.frame()

# Compute means and CIs for each condition
gender_effects <- map_dfr(1:ncol(gender_dists), function(col){
    current_col <- gender_dists[, col]
    tibble(group_num = col,
           mean = mean(current_col),
           lower = quantile(current_col, probs = 0.025),
           upper = quantile(current_col, probs = 0.975),
           fem_lower = sum(current_col < 0) / length(current_col),
           male_lower = sum(current_col > 0) / length(current_col))}) %>%
    left_join(new_data %>%
                  select(-SEX_label, -cond_num) %>%
                  unique(),
              by = "group_num")

# Summarize prediction distributions
pred_sum <- map_dfr(1:ncol(pred), function(col){
    current_col <- pred[, col]
    tibble(cond_num = col,
           mean = mean(current_col),
           lower = quantile(current_col, probs = 0.025),
           upper = quantile(current_col, probs = 0.975))}) %>%
    left_join(new_data, by = "cond_num")

# Ten occupations with largest gender effects (females disadvantaged)
top_ten <- gender_effects %>%
    arrange(mean) %>%
    filter(row_number() %in% 1:10) %>%
    mutate(OCCP_label2 = paste(row_number(), OCCP_label))

# Faceted occupations. Here I'm ordering by mean effect size. Could also order by credibility.
pred_sum %>%
    filter(OCCP_label %in% top_ten$OCCP_label) %>%
    left_join(top_ten %>%
                  select(OCCP_label, OCCP_label2), by = "OCCP_label") %>%
    ggplot(mapping = aes(x = SEX_label, y = mean, group = OCCP_label2)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1) +
    scale_x_discrete(name = "Gender") +
    scale_y_continuous(name = "Annual wages",
                       labels = label_number(suffix = "K", scale = 1e-3)) +
    facet_wrap(~ OCCP_label2) +
    ggtitle("Gender effects on annual wages ordered from largest to smallest")

# Occupations by color
pred_sum %>%
    filter(OCCP_label %in% top_ten$OCCP_label) %>%
    ggplot(mapping = aes(x = SEX_label, y = mean, group = OCCP_label,
                         color = OCCP_label)) +
    geom_line() +
    scale_y_continuous(name = "Annual wages",
                       labels = label_number(suffix = "K", scale = 1e-3)) +
    scale_color_brewer(type = "qual", palette = "Paired")

# Empirical summary of the number of males & females per job, wages
emp_sum <- df3 %>%
    group_by(OCCP_label, SEX_label) %>%
    summarize(n = sum(!is.na(WAGP)),
              mean_wage = mean(WAGP, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = "SEX_label", values_from = c("n", "mean_wage"))

# Join empirical summary to gender effects
gender_effects <- gender_effects %>%
    left_join(emp_sum, by = "OCCP_label")

# Sort by confidence that women are paid less than men
gender_effects %>%
    arrange(desc(fem_lower)) %>%
    filter(n_Male > 0, n_Female > 0)

# Sort by effect size. This tends to highlight higher-paying jobs where the gaps
# are largest. But oftentimes these aren't the ones with the strongest evidence
# for a gap because there's less data. Also, a problem with highlighting based
# on the effect size is that we're ignoring the CrIs, and hence the likelihood
# the the effect is credible. If we sort by confidence in the effect (above
# table) we should do better.
gender_effects %>%
    arrange(mean) %>%
    filter(n_Male > 0, n_Female > 0)

# So, using the confidence sorting rather than the effect size sorting, we get jobs like retail salesperson, where we have salaries for 46 and 32 men and women respectively, and mean wages of $68K versus $30K. Seems like a big gap. How do we fix it?
df3 %>%
    filter(OCCP_label == "SAL-Retail Salespersons") %>%
    select(OCCP_label, SEX_label, WAGP) %>%
    arrange(WAGP) %>%
    
    # The thing that stands out the most are the large number of low-wage
    # females. But you also have fewer females represented in the high wages as
    # well. Means that you're not going to be able to repair this distribuion
    # just by moving all of the low-wage women up.
    ggplot(mapping = aes(x = WAGP, color = SEX_label)) +
    geom_density() +
    scale_x_continuous(name = "Annual wages",
                       labels = label_number(suffix = "K", scale = 1e-3))

# Maybe move female salaries that are less than the predicted male mean up to the male mean?
male_pred_mean <- pred_sum %>%
    filter(OCCP_label == "SAL-Retail Salespersons",
           SEX_label == "Male") %>%
    pull(mean)

# 28 female salaries would be affected by this rule
df3 %>%
    filter(OCCP_label == "SAL-Retail Salespersons",
           SEX_label == "Female",
           WAGP < male_pred_mean) %>%
    select(OCCP_label, SEX_label, WAGP) %>%
    arrange(WAGP) 

# Female retail salespersons with wages less than the predicted male mean get
# new wages that are the predicted male mean. NOTE: This isn't a subtle wage intervention. Not equitable either: how come no above the predicted male mean got a raise? What about an intervention where all women in the group get an X% raise? What's the best way to make the female distribution look like the male distribution?
df3 <- df3 %>%
    mutate(new_wage = case_when(
        OCCP_label == "SAL-Retail Salespersons" & SEX_label == "Female" &
            WAGP < male_pred_mean ~ male_pred_mean,
        TRUE ~ WAGP))


# Think I need to write some functions to automatically summarize model predictions to see whether I've put a dent in gender effects.

# Is there an automated way to scale one distribution to look more like another?
# Quantile method?
m <- df3 %>%
    filter(OCCP_label == "SAL-Retail Salespersons",
           SEX_label == "Male") %>%
    pull(WAGP)
f <- df3 %>%
    filter(OCCP_label == "SAL-Retail Salespersons",
           SEX_label == "Female") %>%
    pull(WAGP)


quantile(m, probs = seq(0, 1, .1))
quantile(f, probs = seq(0, 1, .1))

# Gets the rank (in probability space) of each female salary
f_ecdf <- ecdf(f)
pr <- f_ecdf(f)

# Function that takes two wage vectors as input--one where the wages are lower,
# and one where they're higher--and returns the lower vector transformed to have
# the same distribution as the higher vector.
transform_wages <- function(lower, higher) {
    ecdf_lower <- ecdf(lower)
    probs_lower <- ecdf_lower(lower)
    return(unname(quantile(higher, probs = probs_lower)))
}

# So for each job where I want to adjust wages I run this function.





# Then feed the rank back into quantile(m...) to get the equivalently ranked salary in the distribution of male salaries.
x <- tibble(
    SEX_label = "Female adjusted",
    WAGP = transform_wages(lower = f, higher = m)) %>%
    bind_rows(df3 %>%
                  filter(OCCP_label == "SAL-Retail Salespersons") %>%
                  select(SEX_label, WAGP))
x %>%
    ggplot(mapping = aes(x = WAGP, color = SEX_label)) +
    geom_density()

# Check means & sds. Female now much closer.
x %>%
    group_by(SEX_label) %>%
    summarize(mean = mean(WAGP, na.rm = TRUE),
              sd = sd(WAGP, na.rm = TRUE), .groups = "drop")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Example intervention ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# This is assuming you've already fit an initial model of the non-adjusted data.
# You need to have done that in order to make the assessment of which jobs to
# fix. Should return a list of brms models based on datasets with different
# levels of adjustments controlled by conf.
model_list <- map(c(0.65, 0.63, 0.61), function(conf) {
    
    # User message
    message(paste("Starting simulation for conf =", conf))
    
    # List of jobs to make adjustments to, along with indicator of which gender
    # in the job should get raises.
    jobs_to_fix <- gender_effects %>%
        filter(
            n_Male > 0,
            n_Female > 0,
            (fem_lower >= !!conf | male_lower >= !!conf)) %>%
        mutate(
            raise = case_when(
                fem_lower > male_lower ~ "f",
                male_lower > fem_lower ~ "m",
                fem_lower == male_lower ~ "equal"),
            OCCP_label = as.character(OCCP_label)) %>%
        arrange(desc(fem_lower)) %>%
        select(OCCP_label, raise) %>%
        split(., seq(nrow(.))) %>%
        map(unlist)
    
    # Adjust wages
    adjusted <- map_dfr(jobs_to_fix, function(job) {
        
        # Get data for current job & split into male & female
        current_job <- df3 %>%
            filter(OCCP_label == !!unname(job["OCCP_label"])) %>%
            mutate(!!paste0("wage_", as.character(conf)) := NA_real_)
        male <- current_job %>% filter(SEX_label == "Male")
        female <- current_job %>% filter(SEX_label == "Female")
        
        # Give raises to whichever gender has lower wages, otherwise keep them
        # the same.
        if (job["raise"] == "f") {
            female <- female %>%
                mutate(!!paste0("wage_", as.character(conf)) := transform_wages(
                    lower = female$WAGP,
                    higher = male$WAGP))
            male <- male %>%
                mutate(!!paste0("wage_", as.character(conf)) := WAGP)
        } else if (job["raise"] == "m") {
            female <- female %>%
                mutate(!!paste0("wage_", as.character(conf)) := WAGP)
            male <- male %>%
                mutate(!!paste0("wage_", as.character(conf)) := transform_wages(
                    lower = male$WAGP,
                    higher = female$WAGP))
        } else {
            female <- female %>%
                mutate(!!paste0("wage_", as.character(conf)) := WAGP)
            male <- male %>%
                mutate(!!paste0("wage_", as.character(conf)) := WAGP)
        }
        bind_rows(female, male)
    })
    
    # Combine adjusted and unadjusted wages
    df3 <- bind_rows(
        df3 %>%
            filter(!OCCP_label %in%
                       map_chr(jobs_to_fix, pluck("OCCP_label")) %>%
                       unname()) %>%
            mutate(!!paste0("wage_", as.character(conf)) := WAGP),
        adjusted) %>%
        
        # Don't allow any wages to be adjusted down
        mutate(!!paste0("wage_", as.character(conf)) := if_else(
            !!paste0("wage_", as.character(conf)) < WAGP,
            WAGP,
            !!sym(paste0("wage_", as.character(conf)))))
    
    # Fit model using this formula
    form <- as.formula(paste0(
        "wage_",
        as.character(conf),
        " ~ 1 + SEX_label + (SEX_label | job_cat / job)"))
    brm(
        formula = form,
        family = lognormal(),
        data = df3,
        iter = 4000,
        warmup = 1000,
        control = list(adapt_delta = 0.95))
})

# Summarize population-level gender effect. Suggests that model 63 might be
# okay--no population-level effect detectable.
results_65_61 <- map_dfr(model_list, function(model) {
    model_name = model$formula$resp
    effect <- fixef(model, pars = c("SEX_labelFemale"))
    tibble(model = model_name,
           Estimate = effect[1],
           Est.Error = effect[2],
           Q2.5 = effect[3],
           Q97.5 = effect[4])
})

# Adjustments for 0.63 look reasonable at the global level. Note that this isn't
# controlling for job or job category. When we do that in the model we should
# get male/female wage distributions that look much closer to one another--the
# model is saying that the population-level gender effect isn't distinguishable
# from zero.
bind_rows(
    df3 %>%
        filter(SEX_label == "Male") %>%
        select(wage = WAGP) %>%
        mutate(Condition = "Male"),
    df3 %>%
        filter(SEX_label == "Female") %>%
        select(wage = WAGP) %>%
        mutate(Condition = "Female original"),
    df3 %>%
        filter(SEX_label == "Female") %>%
        select(wage = wage_0.63) %>%
        mutate(Condition = "Female adjusted")) %>%
    ggplot(mapping = aes(x = wage, fill = Condition, color = Condition)) +
    geom_density(alpha = .15) +
    scale_x_continuous(name = "Annual wages",
                       labels = label_number(suffix = "K", scale = 1e-3)) +
    ggtitle("Global effect of 0.63 adjustments on wage distributions")


# Want to examine job-level confidences in an effect. They should all be down relative to the initial fit.
# Want to examine the types of changes made in specific jobs
# Might want to look at this page more closely: https://solomonkurz.netlify.app/post/2019-02-02-robust-linear-regression-with-student-s-t-distribution/. Talks about methods to identify outliers and using robust regression (t-distributions) to model outliers without excluding.
# Is it possible to combine log & t-distribution? Like lognormal, but logt? Or is it the case that logging pulls outliers in enough that they're  no longer outliers?
# Good visualization for wage changes might be to have origional vs. adjusted on the x-axis, wage on the y-axis, with lines connecting wage observations. Should see a bunch of flat lines for men (no adjustment), and upward adjustments for women. For the original datapoints we want to see separable clouds with men > women. For the adjusted data we want to see clouds that are largely overlapping, showing that the adjustment was effective.


# Do 3 x 3 facet plot of original versus adjusted for the jobs with the highest confidence of a gap in jobs_to_fix
top_nine <- gender_effects %>%
    filter(
        n_Male > 0,
        n_Female > 0,
        (fem_lower >= 0.65 | male_lower >= 0.65)) %>%
    mutate(
        raise = case_when(
            fem_lower > male_lower ~ "f",
            male_lower > fem_lower ~ "m",
            fem_lower == male_lower ~ "equal"),
        OCCP_label = as.character(OCCP_label)) %>%
    arrange(desc(fem_lower)) %>%
    filter(row_number() %in% 1:9) %>%
    pull(OCCP_label)

# Shows that the effect of around 6 in 10 wage interventions is to end up with a female mean that's higher than the male mean. Ideally we'd want them to be the same. This seems to indicate that we're over-adjusting for the jobs in jobs_to_fix, which probably means that there are additional jobs we're not adjusting for at all but should be. The adjustments seem to be too high in situations where (1) there are very few women, and (2) where there's are some male wages that are quite a bit higher than the usual male wage. In these cases the adjustment seems to favor increasing female salaries to the level of the unusually high male wage.
df4 <- df3 %>%
    filter(OCCP_label %in% top_nine) %>%
    mutate(person = paste(SERIALNO, "_", SPORDER))
bind_rows(
    df4 %>%
        filter(SEX_label == "Male") %>%
        select(person, wage = WAGP, SEX_label, OCCP_label) %>%
        mutate(Condition = "original"),
    df4 %>%
        filter(SEX_label == "Male") %>%
        select(person, wage = WAGP, SEX_label, OCCP_label) %>%
        mutate(Condition = "adjusted"),
    df4 %>%
        filter(SEX_label == "Female") %>%
        select(person, wage = WAGP, SEX_label, OCCP_label) %>%
        mutate(Condition = "original"),
    df4 %>%
        filter(SEX_label == "Female") %>%
        select(person, wage = wage_0.65, SEX_label, OCCP_label) %>%
        mutate(Condition = "adjusted")) %>%
    mutate(Condition = fct_relevel(Condition, "original")) %>%
    ggplot(mapping = aes(x = Condition, y = wage
                         ,group = person
                         ,color = SEX_label)) +
    # geom_boxplot() +
    geom_line(alpha = .25) +
    stat_summary(mapping = aes(x = Condition, y = wage, color = SEX_label),
                 fun = "mean", geom = "point", inherit.aes = FALSE) +
    scale_y_continuous(name = "Annual wages",
                       labels = label_number(suffix = "K", scale = 1e-3)) +
    facet_wrap(~ OCCP_label)

# Summary so far: at the global level we seem to be doing okay--we're getting female wage distributions that are much closer to the male distributions. But there are problems at the local level. It seems like we may be getting better at the global level by doing overly aggressive wage increases for females in the jobs_to_fix category, and not adjusting other jobs as much as necessary.

# Let's see what happens for the bottom nine jobs adjusted in the 0.65 model. Seems like at least one female wage (in MED-Physicians) is erroneously being adjusted down. Still prone to making overly large adjustments to female salaries in cases where there are few females and high outlier male salaries. In these cases the adjustment is always a low female salary all the way up to the highest male salary, which leads to some situations where the female mean actually outstrips the male mean.
bottom_nine <- gender_effects %>%
    filter(
        n_Male > 0,
        n_Female > 0,
        (fem_lower >= 0.65 | male_lower >= 0.65)) %>%
    mutate(
        raise = case_when(
            fem_lower > male_lower ~ "f",
            male_lower > fem_lower ~ "m",
            fem_lower == male_lower ~ "equal"),
        OCCP_label = as.character(OCCP_label)) %>%
    arrange(fem_lower) %>%
    filter(row_number() %in% 1:9) %>%
    pull(OCCP_label)
df5 <- df3 %>%
    filter(OCCP_label %in% bottom_nine) %>%
    mutate(person = paste(SERIALNO, "_", SPORDER))
bind_rows(
    df5 %>%
        filter(SEX_label == "Male") %>%
        select(person, wage = WAGP, SEX_label, OCCP_label) %>%
        mutate(Condition = "original"),
    df5 %>%
        filter(SEX_label == "Male") %>%
        select(person, wage = WAGP, SEX_label, OCCP_label) %>%
        mutate(Condition = "adjusted"),
    df5 %>%
        filter(SEX_label == "Female") %>%
        select(person, wage = WAGP, SEX_label, OCCP_label) %>%
        mutate(Condition = "original"),
    df5 %>%
        filter(SEX_label == "Female") %>%
        select(person, wage = wage_0.65, SEX_label, OCCP_label) %>%
        mutate(Condition = "adjusted")) %>%
    mutate(Condition = fct_relevel(Condition, "original")) %>%
    ggplot(mapping = aes(x = Condition, y = wage, group = person,
                         color = SEX_label)) +
    geom_line(alpha = .25) +
    stat_summary(mapping = aes(x = Condition, y = wage, color = SEX_label),
                 fun = "mean", geom = "point", inherit.aes = FALSE) +
    scale_y_continuous(name = "Annual wages",
                       labels = label_number(suffix = "K", scale = 1e-3)) +
    facet_wrap(~ OCCP_label)

# Summary so far: need an adjustment scheme that doesn't make the errors outlined above. If there are small numbers of female wages they should be adjusted towards the male mean rather than the male max, which is currently what's happening. If there's a single female salary it always gets adjusted to the maximum male salary in the distribution. Probably in the model-based gender effects it's taking outliers into account and generating a more reasonable estimate. How can I take advantage of this in the adjustment process? The idea is that I've already fit a model that's giving me reasonable information, so how do I use it? Maybe use the estimate to set a limit on the size of the adjustment? So we're going to use the distribution-mapping technique, but it it returns a wage with too big of an increase we dial it back to the average adjustment size? Dunno if that makes sense. Maybe we need to scale all adjustments by the average adjustment size? Or maybe take it literally: just give every woman in a job the average adjustment size? This would retain the ordering of female wages with respect to each other, and move the female distribution higher. Also, if the model is correct, this would erase the gender difference in a specific job.
# The job-level differences are the sum of the population-level gender effect, plus the job category adjustment, plus the job adjustment. Then this sum needs to get transformed from log space to wage space in order to apply the adjustments. So for every job there should be a distinct adjustment representing the average difference between male and female wages.

# Need to translate job_cat and job into strings used for brms parameter names.
job_table <- gender_effects %>%
    select(OCCP_label, job_cat, job) %>%
    mutate(job_param = paste0(
        "r_job_cat:job[",
        job_cat, "_",
        str_replace_all(job, " ", "\\."))) %>%
    select(job_cat, job, job_param)
    
    
# Group-based gender adjustments
gen_job_cat <- (ranef(fit2,
                      pars = "SEX_labelFemale",
                      groups = "job_cat"))[[1]][,,] %>%
    as_tibble(rownames = "job_cat")
gen_job_cat.job <- (ranef(fit2,
                         pars = "SEX_labelFemale",
                         groups = "job_cat:job"))[[1]][,,] %>%
    as_tibble(rownames = "job_cat.job")



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Model-based adjustments ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Remove any wage cols from data
df3 <- df3 %>% select(-matches("wage_"))

model_list2 <- map(c(0.65, 0.63), function(conf) {
    
    # User message
    message(paste("Starting simulation for conf =", conf))
    
    # List of jobs to make adjustments to, along with indicator of which gender
    # in the job should get raises.
    jobs_to_fix <- gender_effects %>%
        filter(
            n_Male > 0,
            n_Female > 0,
            (fem_lower >= !!conf | male_lower >= !!conf)) %>%
        mutate(
            raise = case_when(
                fem_lower > male_lower ~ "f",
                male_lower > fem_lower ~ "m",
                fem_lower == male_lower ~ "equal"),
            OCCP_label = as.character(OCCP_label)) %>%
        arrange(desc(fem_lower)) %>%
        select(OCCP_label, mean, raise)
    
    # Adjust wages
    adjusted <- map_dfr(1:nrow(jobs_to_fix), function(row) {
        current_job <- df3 %>%
            filter(OCCP_label == jobs_to_fix[row, ]$OCCP_label)
        male <- current_job %>% filter(SEX_label == "Male")
        female <- current_job %>% filter(SEX_label == "Female")
        
        # If women need raises then the gender coefficient ("mean") will be
        # negative. Subtracting a negative will increase their wages.
        if(jobs_to_fix[row, ]$raise == "f") {
            female <- female %>%
                mutate(!!paste0("wage_", as.character(conf)) := WAGP -
                           jobs_to_fix[row, ]$mean)
            male <- male %>%
                mutate(!!paste0("wage_", as.character(conf)) := WAGP)
            
            # If men need raises then the gender coefficient ("mean") will be
            # positive. Adding a positive will increase their wages.
        } else if(jobs_to_fix[row, ]$raise == "m") {
            female <- female %>%
                mutate(!!paste0("wage_", as.character(conf)) := WAGP)
            male <- male %>%
                mutate(!!paste0("wage_", as.character(conf)) := WAGP +
                           jobs_to_fix[row, ]$mean)
        } else {
            female <- female %>%
                mutate(!!paste0("wage_", as.character(conf)) := WAGP)
            male <- male %>%
                mutate(!!paste0("wage_", as.character(conf)) := WAGP)
        }
        
        # No adjustments can be down
        bind_rows(female, male) %>%
            mutate(!!paste0("wage_", as.character(conf)) := if_else(
                !!paste0("wage_", as.character(conf)) < WAGP,
                WAGP,
                !!sym(paste0("wage_", as.character(conf)))))
    })
                       
    # Combine adjusted and unadjusted wages
    df3 <- bind_rows(
        df3 %>%
            filter(!OCCP_label %in% jobs_to_fix$OCCP_label) %>%
            mutate(!!paste0("wage_", as.character(conf)) := WAGP),
        adjusted)
    
    # Fit model using this formula
    form <- as.formula(paste0(
        "wage_",
        as.character(conf),
        " ~ 1 + SEX_label + (SEX_label | job_cat / job)"))
    brm(
        formula = form,
        family = lognormal(),
        data = df3,
        iter = 4000,
        warmup = 1000,
        control = list(adapt_delta = 0.95),
        backend = "cmdstanr")
})

# Doing model-based adjustments leads to more subtle increases within jobs. When looking at the top 9 we can see that mean wages go up, but only in about one case (electrical and electronics engineers) is the resulting female mean close to the male mean. They're still mostly a good bit below. If we instead plot medians then we get better results, especially in cases with little data and high outlier male wages. Median is more robust against these outliers. Mean is a problem for lots of jobs where there are a small number of men who make way more than everyone else in the job.
# For bottom 9 there are 5/9 where the female mean is larger to begin with, 2/9 where we flip to larger female means, and 2/9 where we shrink the gap. If we start out with a female > male mean then we shouldn't make any adjustments.

# Another algorithm for adjustments could be to take the male mean/median and compute the female adjustment that's necessary to match the mean/median.

# Or maybe compute a mean/sd for male distributions minus outliers and map female salaries over to THAT distribution?

# Distribution of male MGR-Sales Managers. Pretty clear outlier.
df3 %>%
    filter(OCCP_label == "MGR-Sales Managers", SEX_label == "Male") %>%
    ggplot(mapping = aes(x = WAGP)) +
    geom_density() +
    scale_x_continuous(name = "Annual wages",
                       labels = label_number(suffix = "K", scale = 1e-3))

s <- df3 %>%
    filter(OCCP_label == "MGR-Sales Managers", SEX_label == "Male") %>%
    pull(WAGP)

# First and third quantiles
quartile1 <- unname(quantile(s, probs = c(.25)))
quartile3 <- unname(quantile(s, probs = c(.75)))

# IQR
iqr <- quartile3 - quartile1
lower_iqr <- quartile1 - 1.5 * iqr
upper_iqr <- quartile3 + 1.5 * iqr

# Outliers removed
mgrs <- df3 %>%
    filter(OCCP_label == "MGR-Sales Managers",
           SEX_label == "Male",
           !(WAGP < lower_iqr | WAGP > upper_iqr))

# Mean & sd
mgr_mean <- mean(mgrs$WAGP)
mgr_sd <- sd(mgrs$WAGP)

# Plot
mgrs %>%
    ggplot(mapping = aes(x = WAGP)) +
    geom_density()

# Function that maps lower wage distribution onto higher wage values after
# removing outliers from the higher wage distribution.
transform_wages2 <- function(lower, higher) {
    
    # First and third quantiles
    quartile1 <- unname(quantile(higher$WAGP, probs = c(.25)))
    quartile3 <- unname(quantile(higher$WAGP, probs = c(.75)))
    
    # Inter-quartile range (IQR)
    iqr <- quartile3 - quartile1
    lower_iqr <- quartile1 - 1.3 * iqr
    upper_iqr <- quartile3 + 1.3 * iqr
    
    # Outliers removed
    higher <- higher %>%
        filter(!(WAGP < lower_iqr | WAGP > upper_iqr)) %>%
        pull(WAGP)

    # Map lower wages to higher distribution
    ecdf_lower <- ecdf(lower$WAGP)
    probs_lower <- ecdf_lower(lower$WAGP)
    return(unname(quantile(higher, probs = probs_lower)))
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Distribution mapping with outlier removal ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Remove any wage cols from data
df3 <- df3 %>% select(-matches("wage_"))

model_list2 <- map(c(0.65, 0.63), function(conf) {
    
    # User message
    message(paste("Starting simulation for conf =", conf))
    
    # List of jobs to make adjustments to, along with indicator of which gender
    # in the job should get raises.
    jobs_to_fix <- gender_effects %>%
        filter(
            n_Male > 0,
            n_Female > 0,
            (fem_lower >= !!conf | male_lower >= !!conf)) %>%
        mutate(
            raise = case_when(
                fem_lower > male_lower ~ "f",
                male_lower > fem_lower ~ "m",
                fem_lower == male_lower ~ "equal"),
            OCCP_label = as.character(OCCP_label)) %>%
        arrange(desc(fem_lower)) %>%
        select(OCCP_label, mean, raise)
    
        # Adjust wages
        adjusted <- map_dfr(1:nrow(jobs_to_fix), function(row) {
            current_job <- df3 %>%
                filter(OCCP_label == jobs_to_fix[row, ]$OCCP_label)
            male <- current_job %>% filter(SEX_label == "Male")
            female <- current_job %>% filter(SEX_label == "Female")
            
            # If women need raises then the gender coefficient ("mean") will be
            # negative. Subtracting a negative will increase their wages.
            if(jobs_to_fix[row, ]$raise == "f") {
                female <- female %>%
                    mutate(!!paste0("wage_", as.character(conf)) :=
                               transform_wages2(lower = female,
                                                higher = male))
                male <- male %>%
                    mutate(!!paste0("wage_", as.character(conf)) := WAGP)
                
                # If men need raises then the gender coefficient ("mean") will be
                # positive. Adding a positive will increase their wages.
            } else if(jobs_to_fix[row, ]$raise == "m") {
                female <- female %>%
                    mutate(!!paste0("wage_", as.character(conf)) := WAGP)
                male <- male %>%
                    mutate(!!paste0("wage_", as.character(conf)) :=
                               transform_wages2(lower = male,
                                                higher = female))
            } else {
                female <- female %>%
                    mutate(!!paste0("wage_", as.character(conf)) := WAGP)
                male <- male %>%
                    mutate(!!paste0("wage_", as.character(conf)) := WAGP)
            }
            
            # No adjustments can be down
            bind_rows(female, male) %>%
                mutate(!!paste0("wage_", as.character(conf)) := if_else(
                    !!paste0("wage_", as.character(conf)) < WAGP,
                    WAGP,
                    !!sym(paste0("wage_", as.character(conf)))))
        })
        
        # Combine adjusted and unadjusted wages
        df3 <- bind_rows(
            df3 %>%
                filter(!OCCP_label %in% jobs_to_fix$OCCP_label) %>%
                mutate(!!paste0("wage_", as.character(conf)) := WAGP),
            adjusted)
        
        # Fit model using this formula
        form <- as.formula(paste0(
            "wage_",
            as.character(conf),
            " ~ 1 + SEX_label + (SEX_label | job_cat / job)"))
        brm(
            formula = form,
            family = lognormal(),
            data = df3,
            iter = 4000,
            warmup = 1000,
            control = list(adapt_delta = 0.95))
    })
    

# For some reason the lines above that are supposed to guarantee no downward adjustments aren't working...    
df3 <- df3 %>%
    mutate(wage_0.65 = if_else(wage_0.65 < WAGP, WAGP, wage_0.65))
    
# Probelm with ENG-Electrical And Electronics Engineers: there's only a single woman, and even after excluding the big outlier it maps her salary onto the second-highest salary, which puts the female mean above male. New logic: if there's only one person in the lower group, map their salary to the mean of the upper.
    
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### tidybayes & expected values from posterior distribution ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# See this blog post: https://www.andrewheiss.com/blog/2021/11/10/ame-bayes-re-guide/

fit <- brm(
    WAGP ~ sex + (sex | job.cat/job),
    data = df3,
    family = lognormal(),
    control = list(adapt_delta = 0.95),
    chains = 4,
    iter = 4000,
    warmup = 1000,
    cores = 4,
    seed = 1,
    
    # Cmdstanr should be faster
    backend = "cmdstanr")    

# Tidy summaries of fixed and random effects
tidy(fit, effects = "fixed")
tidy(fit, effects = "ran_pars")

# Distribution of wages by sex
grand_mean_sex_dist <- fit %>%
    epred_draws(newdata = expand_grid(sex = c("Male", "Female")),
                re_formula = NA)

# Figure of wage distribution by sex
plot_grand_mean_sex <- ggplot(data = grand_mean_sex_dist,
                              mapping = aes(x = .epred,
                                            fill = sex)) +
    stat_halfeye(point_interval = mean_hdi, .width = c(0.8, 0.95),
                 alpha = .6) +
    scale_x_continuous(labels = label_dollar(prefix = "$",
                                             suffix = "K",
                                             scale = 1e-3)) +
    scale_fill_okabe_ito() +
    labs(x = "Annual wages",
         y = "Density",
         fill = "Sex") +
    theme(legend.position = "bottom")

# Distribution of sex effect.
# NOTE: emmeans seems to be okay for predicting stuff at the population-level,
# but fails when you're trying to do job-specific predictions. Doesn't seem like
# it's able to infer the correct structure of the model. I think it's trying to
# fully cross job.cat and job--doesn't seem to be able to figure out that job is
# nested in job.cat. So it throws an error saying that it's having to do
# computations for too many categories. I can get around this (see below) if I
# don't use emmeans.
grand_mean_sex_effect <- fit %>%
    emmeans(~ sex,
            epred = TRUE,
            re_formula = NA) %>%
    contrast(method = "revpairwise") %>%
    gather_emmeans_draws()
grand_mean_sex_effect %>% mean_hdi()

# Figure of sex effect
plot_grand_mean_sex_effect <- ggplot(data = grand_mean_sex_effect,
                                     mapping = aes(x = .value)) +
    stat_halfeye(point_interval = mean_hdi, .width = c(0.8, 0.95),
                 alpha = .5,
                 fill = palette_okabe_ito(order = 3)) +
    scale_x_continuous(labels = label_dollar(prefix = "$",
                                             suffix = "K",
                                             scale = 1e-3)) +
    labs(x = "Difference (female - male) in annual wages",
         y = "Density")

# Combined plot
(plot_grand_mean_sex | plot_grand_mean_sex_effect) +
    plot_annotation(title = "After controlling for job family and job we're 95% sure that average female wage is $9K-15K lower than the average male wage",
                    subtitle = "Black dots represent means; bars extending from the dots represent the 95% (wider) and 80% (narrower) credible intervals")

# Make predictions for these conditions in new_data. Can't use expand_grid() to
# fully cross sex, job.cat, and job because jobs are nested in job.cat. So
# create just the represented combinations of job.cat and job by re-creating
# them from OCCP_label.
new_data = expand_grid(
    sex = unique(df3$sex),
    OCCP_label = unique(df3$OCCP_label)) %>%
    mutate(job.cat = str_extract(OCCP_label, "^[A-Z]+"),
           job = str_extract(OCCP_label, "-.+$"),
           job = str_remove(job, "^-"),
           across(where(is.character), as_factor)) %>%
    select(-OCCP_label)

# Generate expected salary distributions for each job & sex
job_dists <- fit %>%
    epred_draws(newdata = new_data,
                re_formula = NULL)

# Generate sex differences in salary distributions for each job
sex_dists <- job_dists %>%
    ungroup() %>%
    select(-.row) %>%
    pivot_wider(names_from = "sex", values_from = ".epred") %>%
    mutate(female_male = Female - Male)

# Compute median & CIs for each sex-based difference for each job.
sex_job_sum <- sex_dists %>%
    group_by(job.cat, job) %>%
    select(-(Male:Female)) %>%
    median_hdi()

# Jobs where the sex difference in wages is 95% credible. Wow--this ends up
# being ~ 70% of jobs in the dataset!
sex_job_sum %>%
    filter(.lower < 0, .upper < 0) %>%
    arrange(female_male)

# Visualize jobs with the 10 largest salary gaps
top_ten <- sex_job_sum %>%
    filter(.lower < 0, .upper < 0) %>%
    arrange(female_male) %>%
    filter(row_number() %in% 1:10)

# Distributions by job and sex
job_fig <- job_dists %>%
    filter(job %in% top_ten$job) %>%
    mutate(job = factor(job, levels = rev(as.character(top_ten$job)))) %>%
    ggplot(mapping = aes(x = .epred, y = job, fill = sex)) +
    stat_halfeye(alpha = 0.6) +
    scale_fill_okabe_ito(name = "Gender") +
    scale_x_continuous(labels = label_dollar(prefix = "$",
                                             suffix = "K",
                                             scale = 1e-3)) +
    labs(x = "Annual wages", y = "Job") +
    theme(legend.position = "bottom",
          axis.title.y = element_text(angle = 0, vjust = 0.5))

# Distribution of sex differences by job
sex_fig <- sex_dists %>%
    filter(job %in% top_ten$job) %>%
    mutate(job = factor(job, levels = rev(as.character(top_ten$job)))) %>%
    ggplot(mapping = aes(x = female_male, y = job)) +
    stat_halfeye(alpha = 0.6, fill = palette_okabe_ito(order = 7)) +
    scale_fill_okabe_ito() +
    scale_x_continuous(labels = label_dollar(prefix = "$",
                                             suffix = "K",
                                             scale = 1e-3)) +
    labs(x = "Difference (female - male) in annual wages") +
    theme(axis.title.y = element_blank())

(job_fig | sex_fig) +
    plot_annotation(title = "Gender-based wage differences by job",
                    subtitle = "Black dots represent medians. Thin and thick black bars are the 95% and 66% credible intervals, respectively.")

# Jobs where we're 95% sure women are making less than men
fem_lower <- sex_job_sum %>%
    filter(.lower < 0, .upper < 0) %>%
    arrange(female_male)

# Jobs where we're 95% sure men are making less than women
male_lower <- sex_job_sum %>%
    filter(.lower > 0, .upper > 0) %>%
    arrange(desc(female_male))

# Need to add empirical N and male & female averages to fem_lower
empirical <- df3 %>%
    group_by(job, sex) %>%
    summarize(n = sum(!is.na(WAGP)),
              median = median(WAGP, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = "sex", values_from = c("n", "median"))

# See filter() for rows that get dropped. This leaves us with 132 out of 290
# jobs to adjust, so a big difference.
fem_lower <- fem_lower %>%
    left_join(empirical, by = "job") %>%
    filter(n_Male > 0,
           n_Female > 0,
           median_Female < median_Male)

# Since there are no jobs where we're sure men are making less, for now
# concentrate on adjustments in jobs where women make less. fem_lower gives the
# average adjustment we need to make per job.
adjustments <- map_dfr(1:nrow(fem_lower), function(row) {
    job <- as.character(fem_lower[row,]$job)
    raise <- -fem_lower[row,]$female_male
    df3 %>%
        filter(job == !!job,
               sex == "Female") %>%
        mutate(new_wage = WAGP + raise)
})

# Add adjustments to df3
df4 <- bind_rows(
    
    # Rows with no adjustments
    df3 %>%
        filter(!(job %in% as.character(fem_lower$job) & sex == "Female")) %>%
        mutate(new_wage = WAGP),
    
    # Rows with adjustments
    adjustments
)

# Fit model with adjustments
fit_adjusted <- brm(
    new_wage ~ sex + (sex | job.cat/job),
    data = df4,
    family = lognormal(),
    control = list(adapt_delta = 0.95),
    chains = 4,
    iter = 4000,
    warmup = 1000,
    cores = 4,
    seed = 1,
    
    # Cmdstanr should be faster
    backend = "cmdstanr")
fit_adjusted

# Generate predicted distributions by job and sex after adjustment
job_dists_adjusted <- fit_adjusted %>%
    epred_draws(newdata = new_data,
                re_formula = NULL)

# Generate predicted female - male difference distributions by job after
# adjustment.
sex_dists_adjusted <- job_dists_adjusted %>%
    ungroup() %>%
    select(-.row) %>%
    pivot_wider(names_from = "sex", values_from = ".epred") %>%
    mutate(female_male = Female - Male)

# Summarize difference distributions
sex_job_sum_adjusted <- sex_dists_adjusted %>%
    group_by(job.cat, job) %>%
    select(-(Male:Female)) %>%
    median_hdi() %>%
    arrange(female_male)

# After adjustment we're getting null sex effects for every job. Combined with
# the null population-level sex effect this is pretty good evidence that we've
# resolved all detectable gender pay gaps.
sex_job_sum_adjusted %>%
    filter((.lower < 0 & .upper < 0) | (.lower > 0 & .upper > 0))

# Figure of top 10 after adjustments
# Distributions by job and sex
job_fig_adj <- job_dists_adjusted %>%
    filter(job %in% top_ten$job) %>%
    mutate(job = factor(job, levels = rev(as.character(top_ten$job)))) %>%
    ggplot(mapping = aes(x = .epred, y = job, fill = sex)) +
    stat_halfeye(alpha = 0.5) +
    scale_fill_okabe_ito(name = "Gender") +
    scale_x_continuous(labels = label_dollar(prefix = "$",
                                             suffix = "K",
                                             scale = 1e-3)) +
    labs(x = "Annual wages", y = "Job") +
    theme(legend.position = "bottom",
          axis.title.y = element_text(angle = 0, vjust = 0.5))

# Distribution of sex differences by job
sex_fig_adj <- sex_dists_adjusted %>%
    filter(job %in% top_ten$job) %>%
    mutate(job = factor(job, levels = rev(as.character(top_ten$job)))) %>%
    ggplot(mapping = aes(x = female_male, y = job)) +
    stat_halfeye(alpha = 0.6, fill = palette_okabe_ito(order = 7)) +
    scale_fill_okabe_ito() +
    scale_x_continuous(labels = label_dollar(prefix = "$",
                                             suffix = "K",
                                             scale = 1e-3)) +
    labs(x = "Difference (female - male) in annual wages") +
    theme(axis.title.y = element_blank())

# Shows that all 95% interals for the difference include 0.
(job_fig_adj | sex_fig_adj) +
    plot_annotation(
        title = "Gender-based wage differences by job after adjustment",
        subtitle = "Black dots represent medians. Thin and thick black bars are the 95% and 66% credible intervals, respectively.")


# When looking at the medians, these adjustments look pretty good!!
top_ten <- sex_job_sum %>%
    filter(.lower < 0, .upper < 0) %>%
    arrange(female_male) %>%
    filter(row_number() %in% 281:300)
bind_rows(
    df4 %>%
        filter(job %in% top_ten$job) %>%
        mutate(wage = WAGP,
               condition = "original"),
    df4 %>%
        filter(job %in% top_ten$job) %>%
        mutate(wage = new_wage,
               condition = "adjusted")
) %>%
    group_by(job.cat, job, sex, condition) %>%
    summarize(n = sum(!is.na(wage)),
              median = median(wage, na.rm = TRUE), .groups = "drop") %>%
    mutate(condition = factor(condition,
                              levels = c("original", "adjusted")),
           job_label = str_wrap(paste0(job.cat, "-", job), width = 30)) %>%
    ggplot(mapping = aes(x = condition, y = median, group = sex, color = sex)) +
    geom_line() +
    scale_y_continuous(labels = label_dollar(prefix = "$",
                                             suffix = "K",
                                             scale = 1e-3)) +
    facet_wrap(~ job_label)

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




    
    
    
    mutate(f_m_ratio = Female / Male) %>%
    pull(f_m_ratio) %>%
    mean(., na.rm = TRUE)



















# Shows that we've halved the population-level gender effect
fixef(fit3)
fixef(fit2)
    
# Overall views of old versus new wage distributions by gender. Appears that the
# adjustments have subtly shifted some of the female mass towards higher wages.
df3 %>%
    ggplot(mapping = aes(x = WAGP, color = SEX_label)) +
    geom_density() +
    scale_x_continuous(name = "Annual wages",
                       labels = label_number(suffix = "K", scale = 1e-3)) +
    ggtitle("Old wage distributions by gender")
df3 %>%
    ggplot(mapping = aes(x = new_wage, color = SEX_label)) +
    geom_density() +
    scale_x_continuous(name = "Annual wages",
                       labels = label_number(suffix = "K", scale = 1e-3)) +
    ggtitle("New wage distributions by gender")

# Show shift from old to new in the female distribution. This makes it clear how
# the probability mass is being changed.
bind_rows(
    df3 %>%
        filter(SEX_label == "Female") %>%
        select(wage = WAGP) %>%
        mutate(cond = "original"),
    df3 %>%
        filter(SEX_label == "Female") %>%
        select(wage = new_wage) %>%
        mutate(cond = "adjusted")) %>%
    ggplot(mapping = aes(x = wage, fill = cond, color = cond)) +
    geom_density(alpha = .15) +
    scale_x_continuous(name = "Annual wages",
                       labels = label_number(suffix = "K", scale = 1e-3)) +
    ggtitle("Shift from original to adjusted wages affects female wage distributions")



# NEXT. Think I want to do some simulations where I manipulate confidence that women have lower wages in order to get smaller & larger numbers of adjustments. Once I get this dialed in, want to look at the size of individual adjustments to see how many crazy things I'm doing.
# Different parts to the process: Measurement, which uses bayesian hierarchical models to identify places to make adjustments; and Adjustment, which consists of different methods for making the adjustments. We can iterate between the two until we get an outcome we're comfortable with.










# Want automated ways to identify the most problematic jobs. Find highest confidence of a gender effect, must have at least one datapoint per gender in order to highlight.
# Think we want to be able to generate a table showing all salaries in a selected job.
# Think we want to have some recommendations to increase low salaries to mean. Need to create an edited dataset with salary increases represented. Want to be able to re-fit a model and show that (1) statsig job-level differences have gone away, (2) confidence of a gender difference has decreased.


    
    
    
    
    
    
    
    
    
    
    
    


pj = position_jitter(width = .1)
make_preds %>%
    ggplot(mapping = aes(x = SEX_label, y = fitted_wages)) +
    geom_point(position = pj, alpha = .25) +
    geom_line(data = x,
              mapping = aes(x = sex, y = mean, group = group_num)) +
    geom_ribbon(data = x,
                mapping = aes(ymin = lower, ymax = upper, x = sex,
                              y = mean, group = group_num),
                alpha = .05) +
    scale_x_discrete(name = "Gender") +
    scale_y_continuous(name = "Annual wages",
                       labels = label_number(suffix = "K", scale = 1e-3)) +
    facet_wrap(~ label)


















# NOT FINDING ANY JOB-PUMA where there's a reliable pay gap, even when I go to 70% CrI. Look for job-puma where we have lots of data!!!
# Credible negative (women paid less) gaps
pred_effect %>%
    filter(lower < 0 & upper < 0)

# Compute empirical job-puma gender effects and plot the ones where we have the most data
empirical_gaps <- df3 %>%
    group_by(SEX_label, OCCP_label, PUMA) %>%
    summarize(n = sum(!is.na(WAGP)),
              mean = mean(WAGP, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = "SEX_label", values_from = c("mean", "n")) %>%
    mutate(female_gap = mean_Female - mean_Male,
           data_flag = if_else(!is.na(n_Male) & !is.na(n_Female), 1L, 0L)) %>%
    rowwise() %>%
    mutate(tot_n = sum(n_Male, n_Female, na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(desc(tot_n))

# This has data for both men & women:
empirical_gender %>% filter(OCCP_label == "TRN-Driver/Sales Workers And Truck Drivers")

# Conditions with the most women:
empirical_gender %>% filter(SEX_label == "Female") %>% arrange(desc(n))


# Not obviously different
df3 %>%
    filter(OCCP_label == "EDU-Secondary School Teachers",
           PUMA == "00900") %>%
    ggplot(mapping = aes(x = WAGP, color = SEX_label)) +
    geom_density()

# Looks different
df3 %>%
    filter(OCCP_label == "MGR-Other Managers",
           PUMA == "00900") %>%
    ggplot(mapping = aes(x = WAGP, color = SEX_label)) +
    geom_density()

# When looking at empirical data with the biggest gaps it's not altogether clear whether there are differences. Hard to see because we're dealing with low N. So as a sanity check try a model with varying intercepts/slopes for just job. That way we'll be generating estimates across pumas, which should increase power. Way to generate predictions just for jobs, ignoring pumas?

# Make predictions for each combination of sex and job
new_data2 <- expand.grid(
    SEX_label = unique(df3$SEX_label),
    OCCP_label = unique(df3$OCCP_label)) %>%
    as_tibble()
new_data2$cond_num <- rep(1:(nrow(new_data2)/2), each = 2)
pred2 <- posterior_predict(
    fit,
    re_formula = ~ (SEX_label|OCCP_label),
    newdata = new_data2) %>%
    as.data.frame()

# Compute distributions of female effects
male_idx <- seq(2, ncol(pred2), 2)
female_idx <- seq(1, ncol(pred2) - 1, 2)
fem_dist <- map2_dfc(male_idx, female_idx, function(male, female) {
    pred2[, female] - pred2[, male]}) %>%
    as.data.frame()

# Compute means & CIs for female effects
fem_eff <- map_dfr(1:ncol(fem_dist), function(col){
    tibble(cond_num = col,
           mean = mean(fem_dist[, col]),
           lower = quantile(fem_dist[, col], probs = 0.025),
           upper = quantile(fem_dist[, col], probs = 0.975))}) %>%
    left_join(new_data2 %>%
                  select(-SEX_label) %>%
                  unique(),
              by = "cond_num")

# This one is also suspect because (1) all effects are negative and (2) no
# effects are credible. Both of these seem unlikely.
summary(fem_eff)

# NEXT: What if I extract gender effects for all occupations? Need to use regexp to get their names. This would give me an idea of where the model thinks the biggest gaps are.
x <- coef.brmsfit(fit)


fixef(fit)
ran_eff <- ranef(fit)


# Table of gender slope adjustments for pumas
fem_puma <- ran_eff[[2]][, , 2] %>%
    as_tibble(rownames = "puma")

# Table of gender slope adjustments for jobs
fem_job <- ran_eff[[1]][, , 2] %>%
    as_tibble(rownames = "job")

# No credible adjustments to female for jobs
fem_job %>%
    filter((Q2.5 < 0 & Q97.5 < 0) | (Q2.5 > 0 & Q97.5 > 0))

# No credible adjustments to female for pumas
fem_puma %>%
    filter((Q2.5 < 0 & Q97.5 < 0) | (Q2.5 > 0 & Q97.5 > 0))

# No credible adjustments to puma intercepts
ran_eff[[2]][, , 1] %>%
    as_tibble(rownames = "puma") %>%
    filter((Q2.5 < 0 & Q97.5 < 0) | (Q2.5 > 0 & Q97.5 > 0))

# 93 credible adjustments to job intercept
job_intercepts <- ran_eff[[1]][, , 1] %>%
    as_tibble(rownames = "job") %>%
    filter((Q2.5 < 0 & Q97.5 < 0) | (Q2.5 > 0 & Q97.5 > 0))

# Biggest reliable upwards adjustments (jobs with highest wages). These look
# reasonable--all high paying jobs, e.g. doctors, engineers, CEOs
job_intercepts %>%
    arrange(desc(Estimate))

# Biggest reliable downwards adjustments. These are reasonable: tend to be low
# paying jobs like fast food workers, waitstaff, groundskeeping.
job_intercepts %>%
    arrange(Estimate)

# Could be that we don't have enough data to detect reliable gender differences at the job-puma level. What if we look at job category-puma?

# Alternatively, since we're not getting statsig gender ranefs are the data just saying the women are generally paid less in the idaho data, regardless of job and puma? I think that's the conclusion. So it's not giving us a way to target individual jobs or pumas for remediation.

# Create new job category variable
df4 <- df3 %>%
    separate(OCCP_label, into = c("job_cat", "job"), sep = "-")

# Model with crossed random effects for job category and puma
fit2 <- brm(WAGP ~ 1 + SEX_label +
               (SEX_label|job_cat) +
               (SEX_label|PUMA),
           family = lognormal(),
           data = df4,
           
           # Increase iter from 2K to 4K to get rid of low ESS warning
           iter = 4000,
           warmup = 1000,
           
           # Increasing from 0.8 to 0.9 gets rid of divergent transitions
           control = list(adapt_delta = 0.95))

# Save model for later use
# saveRDS(fit2, "fit varying effects of gender by job category and puma.RDS")

# These job cats have reliable up/down adjustments to intercept
(ranef(fit2))[["job_cat"]][, , "Intercept"] %>%
    as_tibble(rownames = "job_cat") %>%
    filter((Q2.5 < 0 & Q97.5 < 0) | (Q2.5 > 0 & Q97.5 > 0)) %>%
    arrange(Estimate)

# No job cats have reliable adjustments to gender effect
(ranef(fit2))[["job_cat"]][, , "SEX_labelFemale"] %>%
    as_tibble(rownames = "job_cat") %>%
    summary()

# No pumas have nonzero adjustments to intercept
(ranef(fit2))[["PUMA"]][, , "Intercept"] %>%
    as_tibble(rownames = "job_cat") %>%
    summary()

# No pumas have nonzero adjustments to gender effect
(ranef(fit2))[["PUMA"]][, , "SEX_labelFemale"] %>%
    as_tibble(rownames = "job_cat") %>%
    summary()

# BASED ON these results--a population-level negative effect of female gender
# and no adjustments for job categories or pumas--we should be able to find lots
# of job cateogry-puma combos where women are paid reliably less. But when I do
# the predictions below I find (1) all negative female effects, and (2) no
# effects that don't include zero in the 95% CrI.
new_data3 <- expand.grid(
    SEX_label = unique(df4$SEX_label),
    job_cat = unique(df4$job_cat),
    PUMA = unique(df4$PUMA)) %>%
    as_tibble()
new_data3$cond_num <- 1:nrow(new_data3)
pred3 <- posterior_predict(
    fit2,
    newdata = new_data3) %>%
    as_tibble(.name_repair = "universal")

# Columns in pred3 represent predicted wage distributions for each combination of sex, job category, and PUMA. Every two columns have the same job category & PUMA, but differ by sex. Compare means of the first two by hand shows that they're different. Looks like in my mf_means table I'm mis-assigning the means.
mean(pred3 %>% pull(1))
mean(pred3 %>% pull(2))
mean(pred3 %>% pull(3))
mean(pred3 %>% pull(4))

# Compute distributions of female effects
male_idx <- seq(2, ncol(pred3), 2)
female_idx <- seq(1, ncol(pred3) - 1, 2)
fem_dist <- map2_dfc(male_idx, female_idx, function(male, female) {
    (pred3 %>% pull(female)) - (pred3 %>% pull(male))}) %>%
    as_tibble(.name_repair = "universal")

# Compute means & CIs for female effects
fem_eff <- map_dfr(1:ncol(fem_dist), function(col){
    tibble(cond_num = col,
           mean = mean(fem_dist %>% pull(col)),
           lower = quantile(fem_dist %>% pull(col), probs = 0.025),
           upper = quantile(fem_dist %>% pull(col), probs = 0.975))}) %>%
    left_join(new_data3 %>%
                  select(-SEX_label) %>%
                  unique(),
              by = "cond_num")
summary(fem_eff)

# Plot wages by gender with facets for each job cat/puma combo
mf_means <- map_dfr(1:ncol(pred3), function(col){
    tibble(cond_num = col,
           mean = mean(pred3 %>% pull(col)),
           lower = quantile(pred3 %>% pull(col), probs = 0.025),
           upper = quantile(pred3 %>% pull(col), probs = 0.975))}) %>%
    left_join(new_data3 %>%
                  unique(),
              by = "cond_num")
mf_means$group_num <- rep(1:350, each = 2)

# Better. Shows lower mean wages for women across all job categories and PUMAS, shows higher wages for categories like medical & engineering. Shows small amounts of variability in intercept and slope according to PUMA.
mf_means %>%
    ggplot(mapping = aes(x = SEX_label, y = mean, color = PUMA,
                         group = group_num)) +
    geom_line() +
    scale_y_continuous(labels = label_number(suffix = "K", scale = 1e-3)) +
    facet_wrap(~ job_cat)

# Just plotting gender effects by puma for the MED job category. Makes it clear
# that we're getting slightly different intercepts and slopes for the different
# pumas. In particular, there are a couple that seem to be generally associated
# with higher wages. Wonder if those are metro areas?
mf_means %>%
    filter(job_cat == "MED") %>%
    ggplot(mapping = aes(x = SEX_label, y = mean, color = PUMA,
                         group = group_num)) +
    geom_line() +
    scale_y_continuous(labels = label_number(suffix = "K", scale = 1e-3))

# Now plot job categories for puma 00100. This makes it clear that we're getting adjustments to the intercept and slope based on job cat. Seeing that jobs in the medical and engineering categories have higher wages; contractor and restaurant 
mf_means %>%
    filter(PUMA == "00100") %>%
    ggplot(mapping = aes(x = SEX_label, y = mean, color = job_cat,
                         group = group_num)) +
    geom_line() +
    scale_y_continuous(labels = label_number(suffix = "K", scale = 1e-3))

# 62% confidence that women are paid less than men in this particular job
# category and puma; 38% confidence that men are paid less than women.
sum(fem_dist %>% pull(1) < 0) / length(fem_dist %>% pull(1))
sum(fem_dist %>% pull(1) > 0) / length(fem_dist %>% pull(1))

# So if no gender effects are credible at the usual levels (e.g., 95%, 80%), then I guess the next step is to show which job-puma combos are the ones where we're most confident there's an effect. For each gender effect, compute confidence that it's negative & positive. These could end up being columns in a sortable table, along with n_male, n_female, n_total. Allow users to sort by confidence and n so they can highlight largest opportunities for improvement. Allow users to view data & gender effect for selected jobs, pumas.

# Figure with gender effect, CIs, datapoints
pneg <- sum(fem_dist %>% pull(9) < 0) / length(fem_dist %>% pull(9)) * 100
ppos <- sum(fem_dist %>% pull(9) > 0) / length(fem_dist %>% pull(9)) * 100
pj = position_jitter(width = .1)
    ggplot(data = df4 %>%
               filter(PUMA == "00100",
                      job_cat == "MED"),
           mapping = aes(x = SEX_label, y = WAGP)) +
    geom_point(position = pj, alpha = .25) +
    geom_line(data = mf_means %>%
                  filter(PUMA == "00100",
                         job_cat == "MED"),
              mapping = aes(x = SEX_label, y = mean, group = group_num)) +
    geom_ribbon(data = mf_means %>%
                    filter(PUMA == "00100",
                           job_cat == "MED"),
                mapping = aes(ymin = lower, ymax = upper, x = SEX_label,
                              y = mean, group = group_num),
                alpha = .05) +
    scale_x_discrete(name = "Gender") +
    scale_y_continuous(name = "Annual wages",
                       labels = label_number(suffix = "K", scale = 1e-3)) +
    ggtitle(paste0(format(pneg, digits = 0), "% confidence that women are paid less than men"))
    
# NEXT: Bring in data from other states and see if I can successfully fit a model to it.
# Work on defining reasonable priors rather than using defaults.
# Work on methods to summarize into useful figures & tables.
# Is there a way to drill down from larger to smaller groups. For example, if you find a population-level gender effect, can you then try to identify the job/puma/job-puma that's contributing to it the most?



    

# Shows population-level effect of gender on wages: median wage for men is ~
# $60K, for women ~ $45K. This is after controlling for differences due to job
# category and geography (PUMA). But still not getting to what I want, which is
# the ability to isolate effects to specific job categories and geographies.
conditional_effects(fit2)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### California ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

df <- get_pums(
    variables = c("PUMA", "SEX", "AGEP", "OCCP", "NAICSP", "SOCP", "COW", "ESR",
                  "WKHP", "WAGP"),
    state = "CA",
    survey = "acs1",
    year = 2019,
    recode = TRUE,
    rep_weights = NULL,
    show_call = TRUE)

# California has way more data: ~ 380K
nrow(df)

# Table of OCCP codes & labels
occp <- df %>%
    select(OCCP, OCCP_label) %>%
    unique() %>%
    mutate(OCCP_label = as.character(OCCP_label))

# Table of NAICSP codes & labels
naics <- df %>%
    select(NAICSP, NAICSP_label) %>%
    unique() %>%
    mutate(NAICSP_label = as.character(NAICSP_label))

# Table of SOCP codes & labels. Has same levels as occp, so probably the same.
socp <- df %>%
    select(SOCP, SOCP_label) %>%
    unique() %>%
    mutate(SOCP_label = as.character(SOCP_label))

# Table of ESR codes & labels
esr <- df %>%
    select(ESR, ESR_label) %>%
    unique() %>%
    mutate(ESR_label = as.character(ESR_label))

# Table of COW codes & labels
cow <- df %>%
    select(COW, COW_label) %>%
    unique() %>%
    mutate(COW_label = as.character(COW_label))

# Table of SEX codes & labels
sex <- df %>%
    select(SEX, SEX_label) %>%
    unique() %>%
    mutate(SEX_label = as.character(SEX_label))

# Table of ST & ST_label
state <- df %>%
    select(ST, ST_label) %>%
    unique() %>%
    mutate(ST_label = as.character(ST_label))

# Get non-recode version of data
# Why is this downloading the data twice? Possible that setting rep_weights to NULL or setting show_call to FALSE get rid of this.
# Other possible occupation variables: NAICSP (North american industry classification system), SOCP (standard occupational classification system)
df2 <- get_pums(
    variables = c("PUMA", "SEX", "AGEP", "OCCP", "NAICSP", "SOCP", "COW", "ESR",
                  "WKHP", "WAGP"),
    state = "CA",
    survey = "acs1",
    year = 2019,
    recode = FALSE,
    rep_weights = NULL)

# Dataset with codes converted to labels
df3 <- df2 %>%
    left_join(occp, by = "OCCP") %>%
    left_join(esr, by = "ESR") %>%
    left_join(cow, by = "COW") %>%
    left_join(sex, by = "SEX") %>%
    left_join(state, by = "ST") %>%
    left_join(naics, by = "NAICSP") %>%
    left_join(socp, by = "SOCP") %>%
    select(SERIALNO, SPORDER, AGEP:PUMA, OCCP_label:SOCP_label) %>%

    # None of the factors should be ordered
    mutate(across(where(is.factor), ~ factor(.x, ordered = FALSE)),
           
           # Create job category and job from OCCP_label
           job_cat = str_extract(OCCP_label, "^[A-Z]+"),
           job = str_extract(OCCP_label, "-.+$"),
           job = str_remove(job, "^-")) %>%
    
    # OCCP code 0009 doesn't seem to have a label. What is 0009?
    filter(!is.na(OCCP_label)) %>%
    
    # Drop rows with these values
    filter(!ESR_label %in% c("N/A (less than 16 years old)", "Unemployed",
                             "Not in labor force"),
           !COW_label %in% c(
        "Working without pay in family business or farm",
        "Unemployed and last worked 5 years ago or earlier or never worked"),
           
           # Only full-time
           !WKHP < 40,
           
           # Only 18+
           AGEP >= 18,
        
           # Only wages of 10K/year or more. Trying to exclude really low wages
           # that don't seem possible if you're working full time for a year.
           WAGP >= 10000)

# ABOVE, set baseline level of sex to male so that estimates reflect what's happening for females.

# Model of CA wages with varying intercepts and slopes for job categories and
# pumas. Takes ~ 11.5 hours to fit with 120K datapoints, which is 4 times as
# large as Micron datasets.
fit.ca1 <- brm(WAGP ~ 1 + SEX_label +
                (SEX_label|job_cat) +
                (SEX_label|PUMA),
            family = lognormal(),
            data = df3,
            
            # Increase iter from 2K to 4K to get rid of low ESS warning
            iter = 4000,
            warmup = 1000,
            
            # Increasing adapt_delta from 0.8 to 0.9 gets rid of divergent
            # transitions. Increase max_treedepth from 10 to 15 to get rid of
            # transitions that exceed max_treedepth. Treedepth warning are not a
            # validity concern, more an indication of inefficient sampling.
            control = list(adapt_delta = 0.95,
                           max_treedepth = 15))

# Save model for later use
# saveRDS(fit.ca1, "CA wages with varying intercepts and slopes for job category and PUMA.RDS")

# All rhats <= 1.05
mcmc_plot(fit.ca1, type = "rhat")
mcmc_plot(fit.ca1, type = "rhat_hist")

# Some neff ratios <= 0.5, smaller number <= 0.1
mcmc_plot(fit.ca1, type = "neff")
mcmc_plot(fit.ca1, type = "neff_hist")

# 129 total parameters have neff ratios <= 0.5
low_neff <- neff_ratio(fit.ca1)[neff_ratio(fit.ca1) < 0.5]

# Manually inspect traceplots of parameters with low neff. All are well mixed.
plot(fit.ca1, variable = names(low_neff))

# Conditions to make predictions for
new_data4 <- expand.grid(
    SEX_label = unique(df3$SEX_label),
    job_cat = unique(df3$job_cat),
    PUMA = unique(df3$PUMA)) %>%
    as_tibble() %>%
    mutate(across(where(is.factor), ~ as.character(.x)))
new_data4$cond_num <- 1:nrow(new_data4)
new_data4$cat_puma_num <- rep(1:(nrow(new_data4) / 2), each = 2)

# Prediction distributions
pred3 <- posterior_predict(
    fit.ca1,
    newdata = new_data4) %>%
    as_tibble(.name_repair = "universal")

# Compute gender effect distributions
male_idx <- seq(1, ncol(pred) - 1, 2)
female_idx <- seq(2, ncol(pred), 2)
gender_dists <- map2_dfc(male_idx, female_idx, function(x, y) {
    pred3[, y] - pred3[, x]}) %>%
    as.data.frame()

# Compute means & CrIs for gender effects
gen_eff <- map_dfr(1:ncol(gender_dists), function(col){
    tibble(cond_num = col,
           mean = mean(gender_dists %>% pull(col)),
           lower = quantile(gender_dists %>% pull(col), probs = 0.025),
           upper = quantile(gender_dists %>% pull(col), probs = 0.975))}) %>%
    left_join(new_data4 %>%
                  select(-SEX_label) %>%
                  unique(),
              by = "cond_num") 

# Still all negative with all CrIs including zero
summary(fem_eff)

# Use this technique to highlight areas were we have highest confidence of a
# gender pay disparity.
sum(gender_dists %>% pull(1) < 0) / nrow(gender_dists)
sum(gender_dists %>% pull(2) < 0) / nrow(gender_dists)
sum(gender_dists %>% pull(3) < 0) / nrow(gender_dists)

# Compute confidence of a gender pay disparity for all job cat-puma combos
conf <- map_dfr(1:ncol(gender_dists), function(col){
    tibble(
        cond_num = col,
        female_lower = sum(gender_dists %>% pull(col) < 0) / nrow(gender_dists),
        male_lower  = sum(gender_dists %>% pull(col) > 0) / nrow(gender_dists))
})

# Some conditions show up to 73% confidence that women are paid less
conf %>% arrange(desc(female_lower))

# In contrast, confidence that men are paid less is max 55%
conf %>% arrange(desc(male_lower))

# Visualize top 10 most cat-puma combos where evidence of women being paid less
# is strongest.
top_10 <- (conf %>%
    arrange(desc(female_lower)))[1:10,] %>%
    pull(cond_num)

# Need to iterate over these rows to get means & CrIs
rows <- new_data4 %>%
    filter(cat_puma_num %in% top_10)
x <- map_dfr(1:nrow(rows), function(row){
    current_row <- rows[row,]
    tibble(cond_num = current_row$cond_num,
           group_num = current_row$cat_puma_num,
           sex = current_row$SEX_label,
           job_cat = current_row$job_cat,
           puma = current_row$PUMA,
           mean = mean(pred3 %>% pull(cond_num)),
           lower = quantile(pred3 %>% pull(cond_num), probs = 0.025),
           upper = quantile(pred3 %>% pull(cond_num), probs = 0.975))
}) %>%
    mutate(label = paste(job_cat, puma))

# puma 08507 shows up here 4 times. Where is that puma?
x %>%
    mutate(label = paste(job_cat, puma)) %>%
    ggplot(mapping = aes(x = sex, y = mean, group = cat_puma_num)) +
    geom_line() +
    scale_y_continuous(labels = label_number(suffix = "K", scale = 1e-3)) +
    facet_wrap(~ label)




# Shows that some of the job cat-pumas where we have highest confidence of a pay disparity don't actually have any data (e.g., FFF 08507 & LGL 01309). Looks like there are lots of pay gaps for job cat LGL and PUMA 08507. Probably the model is learning from those to make inferences for cases where there's no data. But obviously for a pay equity app you don't want to show users those cases. Also don't want to show cases where there's no data from one of the two genders. Instead, highlight cases where you have data from each gender and confidence is relatively high that there's a gender effect.

# Summarize predicted means for specific wages
make_preds <- df3 %>%
    mutate(label = paste(job_cat, PUMA)) %>%
    filter(label %in% x$label)
fitted <- posterior_predict(
    fit.ca1,
    newdata = make_preds) %>%
    as_tibble(.name_repair = "universal")
fitted2 <- map_dfr(1:ncol(fitted), function(x){
        tibble(mean = mean(fitted %>% pull(x)))
    })
make_preds$fitted_wages <- fitted2$mean

# Plot specific predicted wages along with gender effect lines. Shows that, as expected, we're predicting the same value for each combination of job cat, puma, and gender.
pj = position_jitter(width = .1)
make_preds %>%
    ggplot(mapping = aes(x = SEX_label, y = fitted_wages)) +
        geom_point(position = pj, alpha = .25) +
        geom_line(data = x,
                  mapping = aes(x = sex, y = mean, group = group_num)) +
        geom_ribbon(data = x,
                    mapping = aes(ymin = lower, ymax = upper, x = sex,
                                  y = mean, group = group_num),
                    alpha = .05) +
        scale_x_discrete(name = "Gender") +
        scale_y_continuous(name = "Annual wages",
                           labels = label_number(suffix = "K", scale = 1e-3)) +
        facet_wrap(~ label)


# Need to work on methods to automatically generate tables & figures. Maybe given a set of job_cat/puma labels, genrate figures?



