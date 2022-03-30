# Author: Jeremy Boyd (jeremyboyd@pm.me)
# Description: Gets and saves Idaho 2019 pay data from the American Community
# Survey (ACS).

# Resources
library(tidycensus)
library(tidyverse)
library(feather)

vars <- pums_variables %>%
    filter(year == 2019, survey == "acs1")

vars %>% filter(level == "person") %>%
    filter(str_detect(var_label, "[rR]ace")) %>%
    View()

# For SEX, 1 = male, 2 = female
# WAGP is person-level wages or salary for the past 12 months
# AGEP is person-level age
# ESR is employment status recode; levels tell you whether someone is a civilian, military, unemployed, etc.
# COW is class of worker; stuff like state/federal employee, private company employee, etc.
# Person's weight: PWTGP
# CIT is citizenship status. values 1-4 are all american citizens; 5 isn't.
# There are yes/no variables for self-care, hearing, vision difficulty, independent living, ambulatory, cognitive
# ability to speak english (ENG) on a 1-4 scale with 1 = very well
# marital status (MAR)
# educational attainment: SCHL
# For WAGP, wages or salary income in past 12 months, something about using ADJINC to adjust SSIP to constant dollars
# WKHP is usual hours worked in past twelve months
# WKWN is weeks worked in past twelve months
# WKL is worked last, looking for option 1, which is worked within past twelve months
# INDP is industry worked in. How many levels does this have?
# PAOC: presence and age of own children. Not getting how this is coded, what it's getting at
# Seem to be a series of yes/no race variables. Like RACWHT



# Get dataset with recode values. This makes it easy to translate numeric
# category labels into nominal labels.
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
df2 <- get_pums(
    variables = c("PUMA", "SEX", "AGEP", "OCCP", "NAICSP", "SOCP", "COW", "ESR",
                  "WKHP", "WAGP"),
    state = "ID",
    survey = "acs1",
    year = 2019,
    recode = FALSE,
    rep_weights = NULL)

# Final dataset
df3 <- df2 %>%
    
    # Join in nominal labels
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
           gender = SEX_label,
           across(matches("job"), factor)) %>%
    select(-SEX_label) %>%
    
    # OCCP code 0009 doesn't seem to have a label. What is 0009?
    filter(!is.na(OCCP_label)) %>%
    
    # Drop rows with these values
    filter(!ESR_label %in% c("N/A (less than 16 years old)", "Unemployed",
                             "Not in labor force"),
           !COW_label %in% c(
        "Working without pay in family business or farm",
        "Unemployed and last worked 5 years ago or earlier or never worked"),
           
           # Only full-time (40 hours per week or more)
           !WKHP < 40,
           
           # Only 18+
           AGEP >= 18,
           
           # Only wages of 10K/year or more. Trying to exclude really low wages
           # that don't seem possible if you're working full time for a year.
           WAGP >= 10000) %>%
    
    # Drop any unused factor levels
    mutate(across(where(is.factor), fct_drop))

# Write to disk
df3 %>%
    write_feather("Idaho_2019_ACS_wages.feather")


