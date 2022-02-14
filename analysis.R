library(tidyverse)
library(carData)
library(rstanarm)
options(mc.cores = parallel::detectCores())


df <- read_csv("Salaries.csv")


# This is actualla a dataset of professor salaries from package carData. Discipline is A (theoretical department) or B (applied department). yrs.since.phd could be a proxy for age. yrs.service is tenure. Salary is 9-month salary in dollars.
df <-  Salaries %>%
    as_tibble()







# Some visual evidence that men are paid more...
df %>%
    ggplot(mapping = aes(x = salary, color = sex)) +
    geom_density() +
    facet_wrap(~ rank)

# Logged salary looks more normal
df %>%
    ggplot(mapping = aes(x = salary)) +
    geom_density()
df %>%
    ggplot(mapping = aes(x = log(salary))) +
    geom_density()


df <- df %>%
    mutate(log_salary = log(salary))


m1 <- stan_glmer(log_salary ~ 1 + yrs.since.phd + discipline + (1|sex),
                 data = df,
                 family = gaussian(link = "identity"))


# Number of divergent transitions. This fails because it applies to Stan models, not rstanarm models. These are different classes.
library(rstan)
divergent <- get_sampler_params(m1, inc_warmup=FALSE)[[1]][,'divergent__']
sum(divergent)








# from Survey of Labour and Income Dynamics. wages is houry wage rate, education is number of years of schooling, age is years.
df2 <- SLID %>%
    as_tibble()