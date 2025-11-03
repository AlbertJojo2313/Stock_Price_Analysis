#--- This script is responsible for plotting
#

# --- Libraries
library(ggplot2)
library(corrplot)
library(dplyr)

# --- Histogram Plot Function ---
plot_histograms <- function(stocks, save_path = NULL, display = FALSE) {
    for (symbol in names(stocks)) {
        df <- stocks[[symbol]]
        numeric_cols <- names(df)[sapply(df, is.numeric)]

        for (col_name in numeric_cols) {
            p <- ggplot(df, aes(x = .data[[col_name]])) +
                geom_histogram(fill = "blue", color = "black", alpha = 0.7, bins = 30) +
                geom_vline(aes(xintercept = mean(.data[[col_name]], na.rm = TRUE)),
                    color = "green", linetype = "dashed", linewidth = 1
                ) +
                geom_vline(aes(xintercept = median(.data[[col_name]], na.rm = TRUE)),
                    color = "red", linetype = "dashed", linewidth = 1
                ) +
                labs(
                    title = paste("Histogram of", col_name, "in", symbol),
                    x = col_name, y = "Frequency"
                ) +
                theme_minimal()

            if (display) print(p)

            if (!is.null(save_path)) {
                subfolder <- file.path(save_path, symbol)
                if (!dir.exists(subfolder)) dir.create(subfolder, recursive = TRUE)
                file_name <- file.path(subfolder, paste0(symbol, "_", col_name, "_hist.png"))
                ggsave(file_name, plot = p, width = 6, height = 4)
            }
        }
    }
    invisible(NULL)
}

#--- Box Plots ---
plot_box_plts <- function(stocks, save_path = NULL, display = FALSE) {
    for (symbol in names(stocks)) {
        df <- stocks[[symbol]]
        numeric_cols <- names(df)[sapply(df, is.numeric)]

        for (col_name in numeric_cols) {
            col_data <- df[[col_name]]
            q1 <- quantile(col_data, 0.25, na.rm = TRUE)
            q2 <- median(col_data, na.rm = TRUE)
            q3 <- quantile(col_data, 0.75, na.rm = TRUE)
            iqr <- IQR(col_data, na.rm = TRUE)
            lower_whisker <- q1 - 1.5 * iqr
            upper_whisker <- q3 + 1.5 * iqr

            p <- ggplot(df, aes(x = "", y = .data[[col_name]])) +
                geom_boxplot(
                    fill = "skyblue", color = "black",
                    outlier.color = "red", outlier.shape = 16, outlier.size = 3
                ) +
                annotate("text",
                    x = 1.4, y = q1, label = paste("Q1 =", round(q1, 2)),
                    hjust = 0, color = "blue", size = 3.5, fontface = "bold"
                ) +
                annotate("text",
                    x = 1.4, y = q2, label = paste("Median =", round(q2, 2)),
                    hjust = 0, color = "darkgreen", size = 4, fontface = "bold"
                ) +
                annotate("text",
                    x = 1.4, y = q3, label = paste("Q3 =", round(q3, 2)),
                    hjust = 0, color = "blue", size = 3.5, fontface = "bold"
                ) +
                labs(
                    title = paste("Box Plot of", col_name, "in", symbol),
                    x = "", y = col_name
                ) +
                theme_minimal() +
                theme(
                    axis.text.x = element_blank(),
                    axis.ticks.x = element_blank()
                )

            if (display) print(p)

            if (!is.null(save_path)) {
                subfolder <- file.path(save_path, symbol)
                if (!dir.exists(subfolder)) dir.create(subfolder, recursive = TRUE)
                file_name <- file.path(subfolder, paste0(symbol, "_", col_name, "_boxplt.png"))
                ggsave(file_name, plot = p, width = 7, height = 6)
            }
        }
    }
    invisible(NULL)
}

#--- Trends / Line Plots ---
plot_time_series <- function(stocks, save_path = NULL, display = FALSE) {
    for (symbol in names(stocks)) {
        df <- stocks[[symbol]]
        numeric_cols <- names(df)[sapply(df, is.numeric)]

        for (col_name in numeric_cols) {
            p <- ggplot(df, aes(x = Date, y = .data[[col_name]])) +
                geom_line(linewidth = 2, alpha = 0.7, linetype = 2) +
                labs(
                    title = paste("Line Plot of", col_name, "in", symbol),
                    x = "Date", y = col_name
                ) +
                theme_minimal()

            if (display) print(p)

            if (!is.null(save_path)) {
                subfolder <- file.path(save_path, symbol)
                if (!dir.exists(subfolder)) dir.create(subfolder, recursive = TRUE)
                file_name <- file.path(subfolder, paste0(symbol, "_", col_name, "_lineplot.png"))
                ggsave(file_name, plot = p, width = 8, height = 7)
            }
        }
    }
    invisible(NULL)
}

#--- Scatter Plots ---
plot_spread <- function(stocks, target_col, save_path = NULL, display = FALSE) {
    for (symbol in names(stocks)) {
        df <- stocks[[symbol]]
        numeric_cols <- df %>% select(where(is.numeric), -Date, -all_of(target_col))

        for (col in names(numeric_cols)) {
            p <- ggplot(df, aes(x = .data[[col]], y = .data[[target_col]])) +
                geom_point() +
                geom_hline(yintercept = 0, color = "red", linetype = "dotted") +
                labs(
                    title = paste("Scatter Plot of", col, "vs", target_col, "in", symbol),
                    x = col, y = target_col
                ) +
                theme_minimal()

            if (display) print(p)

            if (!is.null(save_path)) {
                subfolder <- file.path(save_path, symbol)
                if (!dir.exists(subfolder)) dir.create(subfolder, recursive = TRUE)
                file_name <- file.path(subfolder, paste0(col, "_vs_", target_col, ".png"))
                ggsave(file_name, plot = p, width = 8, height = 7)
            }
        }
    }
    invisible(NULL)
}

#--- Correlation Matrix ---
plot_corr_matrix <- function(stocks, save_path = NULL) {
    for (symbol in names(stocks)) {
        df <- stocks[[symbol]]
        numeric_df <- df %>% select(where(is.numeric), -Date)
        corr_matrix <- cor(numeric_df, use = "complete.obs")

        if (!is.null(save_path)) {
            subfolder <- file.path(save_path, symbol)
            if (!dir.exists(subfolder)) dir.create(subfolder, recursive = TRUE)
            file_name <- file.path(subfolder, paste0(symbol, "_correlation.png"))

            png(file_name, width = 800, height = 800)
            corrplot(corr_matrix,
                method = "color",
                type = "upper",
                order = "hclust",
                addCoef.col = "black",
                tl.col = "black",
                tl.srt = 45,
                title = paste("Correlation Matrix -", symbol),
                mar = c(0, 0, 2, 0)
            )
            dev.off()
        } else {
            corrplot(corr_matrix,
                method = "color",
                type = "upper",
                order = "hclust",
                addCoef.col = "black",
                tl.col = "black",
                tl.srt = 45,
                title = paste("Correlation Matrix -", symbol),
                mar = c(0, 0, 2, 0)
            )
        }
    }
    invisible(NULL)
}
