#--- This script is used for creating summary stats
library(dplyr)
library(tidyr)
library(car)

summary_stats <- function(stocks) {
    results <- list()
    for (symbol in names(stocks)) {
        df <- stocks[[symbol]]

        numeric_cols <- setdiff(names(df)[sapply(df, is.numeric)], "Date")
        stats_df <- df %>%
            select(all_of(numeric_cols)) %>%
            summarise(across(everything(),
                list(
                    Mean = ~ mean(.x, na.rm = TRUE),
                    Median = ~ median(.x, na.rm = TRUE),
                    SD = ~ sd(.x, na.rm = TRUE),
                    Range = ~ range(.x, na.rm = TRUE)
                ),
                .names = "{.col}_{.fn}"
            ))
        results[[symbol]] <- stats_df
    }
    return(results)
}
