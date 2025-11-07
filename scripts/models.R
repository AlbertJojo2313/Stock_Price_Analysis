library(recipes)
library(caret) # for Cross -Validations
library(car) # for VIF
### ======
# MODEL TRAINING AND EVALUATING SCRIPTS
### ======
train_test_split <- function(df, seed = 45) {
    set.seed(seed) # For reproducibility
    split_index <- createDataPartition(df$charges, p = 0.8, list = FALSE)
    train_data <- df[split_index, ]
    test_data <- df[-split_index, ]

    return(list(train = train_data, test = test_data))
}
### =====
# CROSS VALIDATION
### =====
cross_validate <- function(train_data, formula) {
    control <- trainControl(method = "cv", number = 5)

    model_cv <- train(
        formula,
        data = train_data,
        method = "lm",
        trControl = control,
        metric = "MAE"
    )
    print(summary(model_cv))
    return(model_cv$finalModel)
}
### ======
# PREDICTION & EVALUATION
### ======
evaluate_model <- function(model, test_data) {
    predictions <- predict(model, newdata = test_data)

    mae <- MAE(predictions, test_data$charges)
    r2 <- R2(predictions, test_data$charges)

    cat(sprintf("Test MAE: %.2f\n", mae))
    cat(sprintf("Test R(squared): %.2f\n", r2))

    return(list(predictions = predictions, MAE = mae, R2 = r2))
}

### =====
# VIF CHECK
### =====
check_vif <- function(model) {
    vif_values <- vif(model)
    return(vif_values)
}
