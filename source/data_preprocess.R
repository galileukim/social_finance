# ==============================================================================
# data preprocessing
# ==============================================================================
# set-up
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(here)
library(fs)

set.seed(1789)

# ------------------------------------------------------------------------------
# 1.0. load-in data
# ------------------------------------------------------------------------------
release_files <- dir_ls(
    here("data", "input"),
    regexp = ".*releases.*.csv$"
)

releases_raw <- read_csv(
    release_files
)

risk_score_raw <- read_csv(
    here("data", "input", "risk_score.csv")
)

# ------------------------------------------------------------------------------
# 2.0. data processing: releases
# ------------------------------------------------------------------------------
# fix dates
releases <- releases_raw %>%
    # transform into date
    mutate(
        across(
            ends_with("date"),
            ~ parse_date_time(., orders = "d-b-Y")
        )
    ) %>%
    arrange(
        pers_id, sentence_date
    ) %>%
    # generate case_id if sentence_date and release_date are identical
    group_by(pers_id, sentence_date, release_date) %>%
    mutate(
        case_id = cur_group_id()
    ) %>%
    ungroup() %>%
    # sort by pers_id and case_id
    arrange(
        pers_id, case_id
    ) %>%
    select(
        case_id, pers_id, everything()
    )

# eliminate cases where release date is less than sentence date
# 32 obs.
releases <- releases %>% 
    filter(
        release_date > sentence_date
    )

# generate date cutpoints for earliest release in 2010
first_release_date <- releases %>%
    group_by(pers_id) %>%
    summarise(
        original_sentence_date = sentence_date[release_date == min(release_date)],
        original_release_date = min(release_date),
        original_release_date_plus_1 = original_release_date + years(1),
        original_release_date_plus_2 = original_release_date + years(2),
        original_release_date_plus_3 = original_release_date + years(3)
    ) %>%
    ungroup()

# identify 2010 cohort
cohort_2010 <- first_release_date %>%
    filter(
        year(original_release_date) == 2010
    )

# releases of cohort of 2010
releases_cohort_2010 <- releases %>% 
    inner_join(
        cohort_2010,
        by = "pers_id"
    )

# ------------------------------------------------------------------------------
# 2.1. data-processing: risk-score
# ------------------------------------------------------------------------------
# clean risk score data
# note: there are people with multiple risk scores
# maintain them if these are identical, otherwise drop (19 obs.)
risk_score <- risk_score_raw %>% 
    group_by(pers_id) %>%
    mutate(
        distinct_risk = n_distinct(risk)
    ) %>%
    ungroup() %>%
    filter(distinct_risk == 1) %>%
    distinct(pers_id, .keep_all = TRUE) %>% 
    select(-distinct_risk)

# ------------------------------------------------------------------------------
# 3.0. write-out
# ------------------------------------------------------------------------------
releases_cohort_2010 %>% 
    write_csv(
        here("data", "output", "release_cohort_2010.csv")
    )

risk_score %>%
    write_csv(
        here("data", "output", "risk_score.csv")
    )
