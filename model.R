# Author: Jeremy Boyd (kenyonboyd@gmail.com)
# Description: Script that simulates and models pay data.

# Packages
library(tidyverse)
library(rstan)
library(mvtnorm)
library(scales)
library(foreach)

# rstan options
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#------------------------------------------------------------------------------
#### SIMULATE POPULATION OF JOBS ####
#------------------------------------------------------------------------------

a <- 90000          # Average male pay
b <- 0              # Average difference between male and female pay
sigma_a <- 30000    # Standard deviation in average male pay
sigma_b <- 10000    # Standard deviation in average male-female difference
rho <- -0.5         # Correlation between male pay and male-female difference

# Mu
Mu <- c(a, b)

# Build covariance matrix, Sigma
cov_ab <- sigma_a * sigma_b * rho
Sigma <- matrix(c(sigma_a^2, cov_ab, cov_ab, sigma_b^2), ncol = 2)

# Picking a number for nu that will give pretty thick tails. Ideally this would
# vary--probably thicker tails (more outliers) for jobs with higher pay and
# fewer people. For very common jobs it's more likely that things are
# standardized enough that the distribution is closer to normal.
Nu <- 3

# Number of jobs to simulate
n_jobs <- 3000

# Generate mean alphas and betas for different jobs
set.seed(1)
jobs <- as_tibble(
     
     # Generate a and b for n different jobs
     rmvt(n = n_jobs, sigma = Sigma, df = Nu, delta = Mu)) %>%
     
     # Rename columns
     rename(a_job = V1, b_job = V2) %>%
     
     # Only keep jobs with salaries of at least 24K
     filter(a_job >= 24000) %>%
     
     # Give each job an index
     mutate(job_index = row_number()) %>%
     
     # For each job, generate number of people in the job, and standard
     # deviation of salary in the job
     group_by(job_index) %>%
     mutate(n = round(rnorm(1, mean = 1, sd = 8)),
            sigma = rexp(1, rate = 1) * 30000) %>%
     ungroup() %>%
     
     # Exclude n less than 1
     filter(n >= 1)

# Kind of expecting b_job to be symmetrical around zero since I have to overall
# b set to zero above.
(ggplot(jobs, aes(x = a_job, y = b_job))
     + geom_point(alpha = 0.15)
     
     # These lines uses the scales package to format the xy axis labels
     + scale_x_continuous(labels = comma)
     + scale_y_continuous(labels = comma))

# Gives ~ 1-26 employees per job, ~ 10K employees total.
summary(jobs)
sum(jobs$n)

#------------------------------------------------------------------------------
#### SIMULATE INDIVIDUAL SALARIES ####
#------------------------------------------------------------------------------

# Iterate over jobs, generating n t-distributed salaries per job, with
# mean determined from a_job + b_job, and sigma = sigma. If t-distributed, why don't I get different nu for each job, with nu normally distributed around 30 to get mostly normal distributions, but allow for some low nu to get more outliers? Is there also a way to get nu to be correlated with the number and amount of salaries? So that for jobs with higher salaries and fewer salaries you get lower nu values (more outliers)?
salaries <- foreach(job = 1:nrow(jobs), .combine = "rbind"){
     
     # Code here to generate salaries
     
}