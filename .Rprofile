# Author: Jeremy Boyd (jeremyboyd@pm.me)
# Description: .Rprofile for pay_equity project.

library(tidyverse)
library(scales)             # Dollar scales for figure axes
library(ggokabeito)         # Colorblind-friendly Okabe-Ito palette
library(rstan)
library(tidybayes)          # stat_halfeye & epred_draws

# Stan options
rstan_options(auto_write = FALSE)
options(mc.cores = parallel::detectCores())

# Resolve conflict for filter
filter <- dplyr::filter

# Custom ggplot theme
my_theme <- function(base_size = 14,
                     base_family = ""){
    
    # Replace elements we want to change
    theme_classic(base_size = base_size,
                  base_family = base_family) %+replace%
        
        theme(
            # Rotate y-axis title to be more legible; increase margin so it's a
            # bit further from axis.
            axis.title.y = element_text(angle = 0, vjust = 0.5,
                                        margin = margin(r = 5)),
            
            # Thin, light gridlines
            panel.grid.major = element_line(color = "gray90", size = 0.2),
            
            # Increase size of facet text
            strip.text = element_text(size = 10),
            strip.background = element_blank(),
            
            # Not sure what this does
            complete = TRUE
        )
}
theme_set(my_theme())

# Plot gender distributions
gen_plot <- function(.data,
                     x_var,
                     x_label,
                     y_var = NULL,
                     y_label,
                     fill_var = NULL) {
    
    # Job-level gender distributions
    if (!is.null(fill_var) & !is.null(y_var)) {
        .data %>%
            ggplot(mapping = aes(
                x = !!sym(x_var),
                y = !!sym(y_var),
                fill = !!sym(fill_var))) +
            stat_halfeye(
                point_interval = median_hdi,
                .width = c(0.8, 0.95),
                alpha = .5) +
            scale_x_continuous(labels = label_dollar(
                prefix = "$",
                suffix = "K",
                scale = 1e-3)) +
            scale_fill_okabe_ito() +
            guides(fill = guide_legend(reverse = TRUE)) +
            labs(x = x_label,
                 y = y_label,
                 fill = "Gender") +
            theme(legend.position = "bottom")
        
    # Job-level difference distribution
    } else if (is.null(fill_var) & !is.null(y_var)) {
        .data %>%
            ggplot(mapping = aes(x = !!sym(x_var), y = !!sym(y_var))) +
            stat_halfeye(
                point_interval = median_hdi,
                .width = c(0.8, 0.95),
                alpha = .5,
                fill = palette_okabe_ito(order = 3)) +
            scale_x_continuous(labels = label_dollar(
                prefix = "$",
                suffix = "K",
                scale = 1e-3)) +
            labs(x = x_label) +
            theme(axis.title.y = element_blank(),
                  axis.text.y = element_blank())
        
    # Population-level gender distribution
    } else if (!is.null(fill_var) & is.null(y_var)) {
        .data %>%
            ggplot(mapping = aes(
                x = !!sym(x_var),
                fill = !!sym(fill_var))) +
            stat_halfeye(
                point_interval = median_hdi,
                .width = c(0.8, 0.95),
                alpha = .5) +
            scale_x_continuous(labels = label_dollar(
                prefix = "$",
                suffix = "K",
                scale = 1e-3)) +
            scale_fill_okabe_ito() +
            guides(fill = guide_legend(reverse = TRUE)) +
            labs(x = x_label,
                 fill = "Gender") +
            theme(axis.title.y = element_blank(),
                  legend.position = "bottom")
        
    # Population-level difference distribution
    } else if (is.null(fill_var) & is.null(y_var)) {
        .data %>%
            ggplot(mapping = aes(x = !!sym(x_var))) +
            stat_halfeye(
                point_interval = median_hdi,
                .width = c(0.8, 0.95),
                alpha = .5,
                fill = palette_okabe_ito(order = 3)) +
            scale_x_continuous(labels = label_dollar(
                prefix = "$",
                suffix = "K",
                scale = 1e-3)) +
            labs(x = x_label) +
            theme(axis.title.y = element_blank())
        
    # Error
    } else {
        message("Input doesn't match any of the four conditions for plotting.")
    }
}

# Make and summarize predictions (msp). Assumes that you're pivoting .epred
# values from a variable called "gender" with values "Female" and "Male".
msp <- function(fit, new_data) {
    
    # Job-level predictions. re_formula = NULL means we're predicting using ALL
    # random effects.
    if("job" %in% names(new_data)) {
        pred <- epred_draws(
            object = fit,
            newdata = new_data,
            re_formula = NULL)
        diff <- pred %>%
            ungroup() %>%
            select(-.row) %>%
            pivot_wider(names_from = "gender", values_from = ".epred") %>%
            mutate(female_male = Female - Male)
        sum <- diff %>%
            select(job.cat, job, female_male) %>%
            group_by(job.cat, job) %>%
            median_hdi() %>%
            arrange(desc(abs(female_male)))
    
        # Population-level predictions. re_formula = NA means we're predicting
        # using NO random effects.
        } else {
            pred <- epred_draws(
                object = fit,
                newdata = new_data,
                re_formula = NA)
            diff <- pred %>%
                ungroup() %>%
                select(-.row) %>%
                pivot_wider(names_from = "gender", values_from = ".epred") %>%
                mutate(female_male = Female - Male)
            sum <- diff %>%
                select(female_male) %>%
                median_hdi()
    }
    
    # Return a list
    return(list(pred = pred,
                diff = diff,
                sum = sum))
}

# Function that computes a table of jobs that need wage adjustments
compute_imbalance <- function(job_level_sum, data) {
    
    # Initial table of jobs with wage imbalance
    imbalanced1 <- job_level_sum %>%
        filter((.lower < 0 & .upper < 0 | .lower > 0 & .upper > 0) &
                   .lower != .upper) %>%
        arrange(desc(abs(female_male)))
    
    # Get empirical N and male & female medians
    empirical <- data %>%
        group_by(job, gender) %>%
        summarize(n = sum(!is.na(WAGP)),
                  median = median(WAGP, na.rm = TRUE), .groups = "drop") %>%
        pivot_wider(names_from = "gender", values_from = c("n", "median"))
    
    # Revised table of jobs with wage imbalance
    imbalanced2 <- imbalanced1 %>%
        left_join(empirical, by = "job") %>%
        
        # Only consider a job to have a pay imbalance if that shows up in both
        # the model-based point estimate and the empirical data.
        mutate(lower_gender = case_when(
            female_male < 0 & median_Female < median_Male ~ "Female",
            female_male > 0 & median_Male < median_Female ~ "Male",
            TRUE ~ NA_character_)) %>%
        
        # Only consider a job to have an imbalance if it meets these conditions
        filter(n_Male > 0,
               n_Female > 0,
               lower_gender %in% c("Female", "Male"))
}
