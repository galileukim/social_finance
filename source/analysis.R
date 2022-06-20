# ==============================================================================
# data analysis
# ==============================================================================
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(RColorBrewer)
library(here)
library(tidyr)

theme_set(
    theme_minimal()
)

source(
    here("source", "utils.R")
)
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
    classify_years_since_release()

prop_2010_recidivism <- release_cohort_2010 %>%
    group_by(pers_id) %>%
    generate_dummy_recidivism() %>%
    summarise_prop_recidivism()

# by education level
prop_edu_2010_recidivism <- release_cohort_2010 %>%
    group_by(pers_id, ed_level) %>%
    generate_dummy_recidivism() %>%
    group_by(ed_level) %>%
    summarise_prop_recidivism()

# by crime type
prop_crime_2010_recidivism <- release_cohort_2010 %>%
    group_by(pers_id, crime_type) %>%
    generate_dummy_recidivism() %>%
    group_by(crime_type) %>%
    summarise_prop_recidivism()

prop_risk_2010_recidivism <- release_cohort_2010 %>%
    group_by(pers_id, risk) %>%
    generate_dummy_recidivism() %>%
    group_by(risk) %>%
    summarise_prop_recidivism()

# time incarcerated
prop_time_2010_recidivism <- release_cohort_2010 %>%
    group_by(pers_id, original_sentence_date, original_release_date) %>%
    generate_dummy_recidivism() %>%
    # add duration of incarceration
    mutate(
        time_incarcerated = time_length(
            original_release_date - original_sentence_date,
            unit = "months"
        ),
        bin_time_incarcerated = cut(
            time_incarcerated,
            breaks = c(0, 12, 24, 36),
            labels = c("Less than a year", "1-2 years", "2-3 years"),
            include.lowest = TRUE
        )
    ) %>%
    group_by(bin_time_incarcerated) %>%
    summarise_prop_recidivism()

# ------------------------------------------------------------------------------
# 3.0. data analysis
# ------------------------------------------------------------------------------
# What percentage of individuals released in 2010 were incarcerated again
# within 1 year, within 2 years, and within 3 years?
prop_2010_recidivism %>%
    plot_recidivism()

ggsave(
    here("figs", "plot_recidivism.png")
)

# breakdown by education level
prop_edu_2010_recidivism %>%
    mutate(
        ed_level = factor(
            ed_level,
            levels = c(
                "Less than high school",
                "High school degree",
                "Some college",
                "Associate degree",
                "Bachelor's degree or higher'"
            )
        )
    ) %>%
    plot_recidivism(grouping = ed_level) +
    theme(
        legend.position = "bottom"
    ) +
    scale_fill_brewer(
        name = "Education Level",
        palette = "Set2"
    )

ggsave(
    here("figs", "plot_edu_recidivism.png")
)

# breakdown by type of crime
prop_crime_2010_recidivism %>%
    plot_recidivism(grouping = crime_type) +
    theme(
        legend.position = "bottom"
    ) +
    scale_fill_brewer(
        name = "Type of Crime",
        palette = "Set2"
    )

ggsave(
    here("figs", "plot_crime_recidivism.png")
)

# risk
prop_risk_2010_recidivism %>%
    filter(!is.na(risk)) %>%
    plot_recidivism(grouping = risk) +
    theme(
        legend.position = "bottom"
    ) +
    scale_fill_brewer(
        name = "Risk Score",
        palette = "Set2"
    )

ggsave(
    here("figs", "plot_risk_recidivism.png")
)

prop_time_2010_recidivism %>%
    plot_recidivism(grouping = bin_time_incarcerated) +
     scale_fill_brewer(
        name = "Time Incarcerated",
        palette = "Set2"
    )

ggsave(
    here("figs", "plot_time_recidivism.png")
)

# risk score
release_cohort_2010 %>%
    filter(
        !is.na(risk) &
        release_date == original_release_date
    ) %>%
    count(risk, crime_type) %>%
    group_by(risk) %>%
    mutate(
        prop = n / sum(n)
    ) %>%
    ungroup() %>%
    ggplot(
        aes(risk, prop, fill = crime_type)
    ) +
    geom_col(
        width = 0.5,
        position = position_dodge(width = 0.7)
    ) +
    geom_text(
        aes(
            label = scales::percent(prop)
        ),
        position = position_dodge(0.7),
        vjust = -1,
        size = 4
    ) +
    scale_y_continuous(
        labels = scales::percent_format()
    ) +
    labs(
        x = "Risk Level",
        y = ""
    ) +
    theme(
        text = element_text(size = 16),
        legend.position = "bottom"
    ) +
    scale_fill_brewer(
        name = "Type of Crime",
        palette = "Set2"
    )

ggsave(
    here("figs", "plot_risk_vs_crime.png")
)
