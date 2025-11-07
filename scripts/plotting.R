library(ggplot2)
library(corrplot)
library(gridExtra)

# =============================================================================
# CORRELATION MATRIX
# =============================================================================
plot_correlation <- function(df, output_file = "plots/corrplot.png",
                             width = 1600, height = 1200, res = 200) {
    dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
    numeric_cols <- names(df)[sapply(df, is.numeric)] # Default to numeric cols
    df_numeric <- df[, numeric_cols, drop = FALSE]
    corr_matrix <- cor(df_numeric, use = "complete.obs")
    png(output_file, width = width, height = height, res = res)
    corrplot(
        corr_matrix,
        method = "color",
        order = "hclust",
        addCoef.col = "black",
        number.cex = 0.7,
        tl.col = "black",
        tl.cex = 0.85,
        tl.srt = 45,
        col = colorRampPalette(c("#3B9AB2", "white", "#E63946"))(200),
        title = "Correlation Matrix",
        mar = c(0, 0, 2, 0)
    )
    dev.off()
    message(paste("Correlation plot saved to:", output_file))
}

# =============================================================================
# DISTRIBUTION PLOTS
# =============================================================================
plot_histograms <- function(df, output_file = "plots/histograms.png",
                            ncol = 3, width = 1600, height = 1200, res = 150) {
    dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
    numeric_cols <- names(df)[sapply(df, is.numeric)]
    plots <- lapply(numeric_cols, function(col) {
        ggplot(df, aes(x = .data[[col]])) +
            geom_histogram(bins = 30, fill = "#3B9AB2", color = "white", alpha = 0.8) +
            labs(title = paste("Distribution of", col), x = col, y = "Frequency") +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 10))
    })
    png(output_file, width = width, height = height, res = res)
    grid.arrange(grobs = plots, ncol = ncol)
    dev.off()
    message(paste("Histograms saved to:", output_file))
}

# =============================================================================
# SCATTER PLOTS
# =============================================================================
plot_scatterplots <- function(df, y_var, x_vars = NULL,
                              output_file = "plots/scatterplots.png",
                              ncol = 3, width = 1600, height = 1200, res = 150) {
    dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
    if (is.null(x_vars)) {
        x_vars <- names(df)[sapply(df, is.numeric) & names(df) != y_var]
    }
    plots <- lapply(x_vars, function(x_var) {
        ggplot(df, aes(x = .data[[x_var]], y = .data[[y_var]])) +
            geom_point(alpha = 0.5, color = "#3B9AB2", size = 2) +
            labs(title = paste(y_var, "vs", x_var), x = x_var, y = y_var) +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 10))
    })
    png(output_file, width = width, height = height, res = res)
    grid.arrange(grobs = plots, ncol = ncol)
    dev.off()
    message(paste("Scatterplots saved to:", output_file))
}
