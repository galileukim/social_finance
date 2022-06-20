# ==============================================================================
# data analysis
# ==============================================================================
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(here)
library(tidyr)

theme_set(
    theme_minimal()
)

generate_dummy_recidivism <- function(data){
    # function to generate dummies for recidivism
    data %>% 
        summarise(
            recidivism_year_1 = if_else(
                any(years_since_original_release == 1, na.rm = TRUE), 
                1, 0
            ),
            recidivism_year_2 = if_else(
                any(years_since_original_release <= 2, na.rm = TRUE),
                1, 0
            ),
            recidivism_year_3 = if_else(
                any(years_since_original_release <= 3, na.rm = TRUE),
                1, 0
            ),
            .groups = "drop"
        )
}

summarise_prop_recidivism <- function(data){
    data %>%
        summarise(
            across(
                recidivism_year_1:recidivism_year_3,
                mean,
                .names = "prop_{col}"
            ),
            .groups = "drop"
        )
}

plot_recidivism <- function(data){
    data %>%
        ggplot(
            aes(
                years_since_original_release, prop_recidivism
            )
        ) +
        geom_col(
            fill = "steelblue3",
            width = 0.5
        ) +
        geom_text(
            aes(
                label = scales::percent(prop_recidivism)
            ),
            position = position_dodge(0.8),
            vjust = -1,
            size = 4
        ) +
        scale_y_continuous( 
            labels = scales::percent_format()
        ) +
        labs(
            x = "Years Since Original Release Date",
            y = ""
        ) +
        theme(
            text = element_text(size = 16)
        )
}

# ------------------------------------------------------------------------------
# 1.0. load-in data
# ------------------------------------------------------------------------------
release_cohort_2010_raw <- read_csv(
    here("data", "output", "release_cohort_2010.csv")
)

risk_score <- read_csv(
    here("data", "output", "risk_score.csv")
)

# ------------------------------------------------------------------------------
# 2.0. data re-shaping
# ------------------------------------------------------------------------------
release_cohort_2010 <- release_cohort_2010_raw %>%
    # incorporate risk scores
    left_join(
        risk_score,
        by = c("pers_id")
    ) %>%
    mutate(
        years_since_original_release = case_when(
            sentence_date <= original_release_date ~ NA_integer_,
            sentence_date %within% interval(original_release_date, original_release_date_plus_1) ~ 1L,
            sentence_date %within% interval(original_release_date_plus_1 + 1, original_release_date_plus_2) ~ 2L,
            sentence_date %within% interval(original_release_date_plus_2 + 1, original_release_date_plus_3) ~ 3L,
            T ~ 4L
        )
    ) 

prop_2010_recidivism <- release_cohort_2010 %>%
    group_by(pers_id) %>%
    generate_dummy_recidivism() %>%
    summarise_prop_recidivism()

prop_edu_2010_recidivism <- release_cohort_2010 %>%
    group_by(pers_id, ed_level) %>%
    generate_dummy_recidivism() %>%
    group_by(ed_level) %>%
    summarise_prop_recidivism()

prop_risk_2010_recidivism <- release_cohort_2010 %>%
    group_by(pers_id, risk) %>%
    generate_dummy_recidivism() %>%
    group_by(risk) %>%
    summarise_prop_recidivism()

# baseline in 2010
cohort_2010_baseline <- release_cohort_2010 %>%
    filter(release_date == original_release_date) %>%
    # add duration of incarceration
    mutate(
        time_incarcerated = time_length(
            release_date - sentence_date, unit = "months"
        )
    )

# ------------------------------------------------------------------------------
# 3.0. data analysis
# ------------------------------------------------------------------------------
# What percentage of individuals released in 2010 were incarcerated again 
# within 1 year, within 2 years, and within 3 years?
prop_2010_recidivism %>%
    pivot_longer(
        cols = everything(),
        names_to = "years_since_original_release",
        names_prefix = "prop_recidivism_year_",
        values_to = "prop_recidivism"
    ) %>%
    plot_recidivism()

# breakdown by education level
release_cohort_2010 %>%
    group_by(pers_id, ed_level) %>%
    summarise_recidivism() %>%
    pivot_longer(
        cols = c(),
        names_to = "years_since_original_release",
        names_prefix = "prop_recidivism_year_",
        values_to = "prop_recidivism"
    ) %>% glimpse()

# ------------------------------------------------------------------------------
# 3.1. heterogeneity analysis
# ------------------------------------------------------------------------------
# breakdown by education
