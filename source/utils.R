
classify_years_since_release <- function(data) {
    data %>%
        mutate(
            years_since_original_release = case_when(
                sentence_date <= original_release_date ~ NA_integer_,
                sentence_date %within% interval(original_release_date, original_release_date_plus_1) ~ 1L,
                sentence_date %within% interval(original_release_date_plus_1 + 1, original_release_date_plus_2) ~ 2L,
                sentence_date %within% interval(original_release_date_plus_2 + 1, original_release_date_plus_3) ~ 3L,
                T ~ 4L
            )
        )
}

generate_dummy_recidivism <- function(data) {
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

summarise_prop_recidivism <- function(data) {
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

pivot_recidivism_longer <- function(data) {
    data %>%
        pivot_longer(
            cols = c(
                starts_with("prop_recidivism_year")
            ),
            names_to = "years_since_original_release",
            names_prefix = "prop_recidivism_year_",
            values_to = "prop_recidivism"
        )
}

plot_recidivism <- function(data, grouping = NULL) {
    data %>%
        pivot_recidivism_longer() %>%
        ggplot(
            aes(
                years_since_original_release,
                prop_recidivism,
                fill = {{ grouping }}
            )
        ) +
        geom_col(
            width = 0.5,
            position = position_dodge(width = 0.7)
        ) +
        geom_text(
            aes(
                label = scales::percent(prop_recidivism, accuracy = 0.1)
            ),
            position = position_dodge(0.7),
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
            text = element_text(size = 16),
            legend.position = "bottom"
        ) 
}
