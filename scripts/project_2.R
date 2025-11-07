library(dplyr)
library(recipes)
library(caret)


# =======
# DATA LOADING & PROCESSING
# =======

load_data <- function(filepath = "data/insurance.csv") {
    df <- read.csv(filepath)
    return(df)
}

process_data <- function(df) {
    # Create recipe for dummy encoding
    rec <- recipe(~., data = df) %>%
        step_dummy(all_nominal(), one_hot = FALSE)

    # Prepare and apply the recipe
    prep_rec <- prep(rec, training = df)
    df_encoded <- bake(prep_rec, new_data = df)

    # Clean column names
    colnames(df_encoded) <- gsub("region_", "R_", colnames(df_encoded))
    colnames(df_encoded) <- gsub("smoker_", "S_", colnames(df_encoded))
    colnames(df_encoded) <- gsub("sex_", "G_", colnames(df_encoded))

    return(as.data.frame(df_encoded))
}


# Source files
source("scripts/plotting.R")
source("scripts/models.R")

# Load and process data
df <- load_data("data/insurance.csv")
df_encoded <- process_data(df)

# Train and evaluate
split <- train_test_split(df_encoded)
# =====
# FULL Model (with CV = 5)
# =====
cat("\nBEST FULL MODEL SUMMARY(CV = 5)\n")
best_base_model <- cross_validate(split$train, charges ~ .)
cat("\nEVALUATION RESULTS:\n")
evaluate_model(best_base_model, split$test)
cat("\nVIF CHECK")
print(check_vif(best_base_model))

# =====
# REDUCED MODEL
# =====
cat("\nBEST REDUCED MODEL SUMMARY(CV=5)\n")
best_reduced_model <- cross_validate(split$train, charges ~ age + bmi + children + S_yes)
cat("\nEVALUATION RESULTS:\n")
evaluate_model(best_reduced_model, split$test)
cat("\nVIF CHECK\n")
print(check_vif(best_reduced_model))

# =====
# MODEL(WITHOUT CHILDREN)
# =====
cat("\nFINAL MODEL WITHOUT(CHILDREN) SUMMARY(CV=5)\n")
best_final_model <- cross_validate(split$train, charges ~ age + bmi + S_yes)
evaluate_model(best_final_model, split$test)
cat("\nVIF CHECK\n")
print(check_vif(best_final_model))
