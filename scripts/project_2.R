library(dplyr)
# --- Data Loading ---
# tesla_stock <- read.csv("data/TSLA.csv")
# google_stock <- read.csv("data/GoogleStockPrices.csv")
netflix_stock <- read.csv("data/Netflix_Data.csv")
# apple_stock <- read.csv("data/apple_5yr_one.csv")

# --- Data Structure ---
stocks <- list(
    # TSLA = tesla_stock,
    # GOOGL = google_stock,
    NFLX = netflix_stock
    # AAPL = apple_stock
)
# --- Processing Function ---
process_dfs <- function(stocks) {
    drop_cols <- c("X", "Adj.Close", "Adj Close", "Adj_Close")
    for (name in names(stocks)) {
        df <- stocks[[name]]
        df <- df %>% select(-any_of(drop_cols))
        df[["Date"]] <- as.Date(df[["Date"]])

        stocks[[name]] <- df
    }
    return(stocks)
}


# --- Run ---
source("scripts/plotting.R")
source("scripts/statistical_eval.R")


stocks <- process_dfs(stocks)
plot_histograms(stocks, display = TRUE, save_path = "plots/histograms")
plot_box_plts(stocks, display = TRUE, save_path = "plots/boxplots")
plot_time_series(stocks, display = TRUE, save_path = "plots/time_series")
plot_spread(stocks, target_col = "Close", display = TRUE, save_path = "plots/spread")
plot_corr_matrix(stocks, save_path = "plots/correlation_matrix")

summary_stats <- summary_stats(stocks)
