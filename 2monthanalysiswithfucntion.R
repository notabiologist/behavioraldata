#

rm(list = ls())
getwd()

setwd("C:/Users/depha/OneDrive/Mahseer_behaviour_paper")
dir()
#function tranfrom data @copilot
data <-  read.csv("population1_vinitha_2months.csv")
head(data)
names(data)
str(data)
summary(data)



# Function to apply square root transformation to specified columns in a dataframe

sqrt_transform2 <- function(data, columns) {
        # Check if the specified columns exist in the dataframe
        missing_cols <- setdiff(columns, names(data))
        if(length(missing_cols) > 0) {
                stop(paste("Columns not found in the dataframe:", paste(missing_cols, collapse = ", ")))
        }
        
        # Apply the square root transformation to each specified column and replace the original columns
        for (col in columns) {
                data[[col]] <- sqrt(data[[col]])
        }
        
        return(data)
}

# Function to perform stepwise regression to null model and saves the data to a list use this for analysis
stepwise_to_null_models <- function(formula, family = gaussian, data) {
        # Fit the full model
        full_model <- glm(formula, family = family, data = data)
        
        # Initialize the current model as the full model
        current_model <- full_model
        
        # List to store all models
        models <- list(full_model = full_model)
        
        # Show summary of the full model
        cat("Full Model:\n")
        print(summary(current_model))
        cat("\n")
        
        # Loop to remove variables one by one
        while (length(coef(current_model)) > 1) {  # Continue until only intercept is left
                if (length(coef(current_model)) == 2) {
                        # If only one predictor left, remove it
                        predictor_to_remove <- names(coef(current_model))[2]
                } else {
                        p_values <- summary(current_model)$coefficients[-1, 4]  # Get p-values for all predictors except intercept
                        predictor_to_remove <- names(which.max(p_values))  # Get the name of the predictor with highest p-value
                }
                
                # Update the formula to remove the selected predictor
                current_model <- update(current_model, as.formula(paste(". ~ . -", predictor_to_remove)))
                
                # Store the updated model
                models[[paste("model_after_removing", predictor_to_remove, sep = "_")]] <- current_model
                
                # Show the updated model
                cat(paste("Removed:", predictor_to_remove, "\n"))
                cat("Updated Model:\n")
                print(summary(current_model))
                cat("\n")
        }
        
        # Fit the null model
        null_model <- glm(reformulate("1", response = all.vars(formula)[1]), family = family, data = data)
        cat("Final Null Model:\n")
        print(summary(null_model))
        
        # Store the null model
        models$null_model <- null_model
        
        return(models)
}



# function for calculating delta AIC

calculate_delta_aicwithnull <- function(models, null_model) {
        # Extract the null model AIC
        null_aic <- AIC(null_model)
        
        # Initialize a list to store delta AIC values and formulas
        delta_aic_values <- list()
        
        # Loop through each model in the models list
        for (i in seq_along(models)) {
                # Extract model and its formula
                model <- models[[i]]
                model_formula <- deparse(formula(model))
                
                # Calculate AIC for the model
                model_aic <- AIC(model)
                
                # Calculate delta AIC
                delta_aic <- model_aic - null_aic
                
                # Store delta AIC and formula
                delta_aic_values[[paste("Model", i)]] <- list(
                        formula = model_formula,
                        delta_aic = delta_aic
                )
        }
        
        # Print delta AIC values and formulas for each model
        cat("Delta AIC values for each model:\n")
        for (name in names(delta_aic_values)) {
                cat(paste(name, ": Delta AIC =", round(delta_aic_values[[name]]$delta_aic, 2), "\n"))
                cat("Formula:\n")
                cat(delta_aic_values[[name]]$formula, "\n\n")
        }
        
        # Find the model with the lowest delta AIC
        min_delta_aic <- min(sapply(delta_aic_values, function(x) x$delta_aic))
        best_model_name <- names(which.min(sapply(delta_aic_values, function(x) x$delta_aic)))
        
        cat("\nModel with the lowest delta AIC:\n")
        cat(paste(best_model_name, ": Delta AIC =", round(min_delta_aic, 2), "\n"))
        cat("Formula:\n")
        cat(delta_aic_values[[best_model_name]]$formula, "\n")
}

#null_model <- glm(formula = Emergence.time ~ 1, family = gaussian, data = data)


calculate_delta_aic <- function(models) {
        # Extract AIC values for all models
        aic_values <- sapply(models, AIC)
        
        # Find the model with the lowest AIC
        min_aic <- min(aic_values)
        best_model_index <- which.min(aic_values)
        
        # Initialize a list to store delta AIC values and formulas
        delta_aic_values <- list()
        
        # Loop through each model in the models list
        for (i in seq_along(models)) {
                # Extract model and its formula
                model <- models[[i]]
                model_formula <- deparse(formula(model))
                
                # Calculate delta AIC relative to the best model
                delta_aic <- aic_values[i] - min_aic
                
                # Store delta AIC and formula
                delta_aic_values[[paste("Model", i)]] <- list(
                        formula = model_formula,
                        delta_aic = delta_aic
                )
        }
        
        # Print delta AIC values and formulas for each model
        cat("Delta AIC values for each model:\n")
        for (name in names(delta_aic_values)) {
                cat(paste(name, ": Delta AIC =", round(delta_aic_values[[name]]$delta_aic, 2), "\n"))
                cat("Formula:\n")
                cat(delta_aic_values[[name]]$formula, "\n\n")
        }
        
        # Print the best model based on AIC
        cat("\nModel with the lowest AIC:\n")
        cat(paste("Model", best_model_index, ": AIC =", round(min_aic, 2), "\n"))
        cat("Formula:\n")
        cat(delta_aic_values[[paste("Model", best_model_index)]]$formula, "\n")
}




# Function to extract intercept estimate, p-value, significance, AIC, and formula
extract_model_info <- function(model, model_name, delta_aic) {
        summary_model <- summary(model)
        intercept_estimate <- coef(summary_model)["(Intercept)", "Estimate"]
        p_value <- coef(summary_model)["(Intercept)", "Pr(>|t|)"]
        significance <- ifelse(p_value < 0.001, "***",
                               ifelse(p_value < 0.01, "**",
                                      ifelse(p_value < 0.05, "*", 
                                             ifelse(p_value < 0.1, ".", " "))))
        aic <- AIC(model)
        formula <- paste(deparse(formula(model)), collapse = "")
        list(
                Model = model_name,
                Formula = formula,
                Intercept_Estimate = intercept_estimate,
                P_Value = p_value,
                Significance = significance,
                AIC = aic,
                Delta_AIC = delta_aic[model_name]
        )
}


extract_model_info <- function(model, model_name, delta_aic) {
        summary_model <- summary(model)
        
        # For GLMs, the coefficient matrix is in summary_model$coefficients
        coef_matrix <- summary_model$coefficients
        
        if ("(Intercept)" %in% rownames(coef_matrix)) {
                intercept_estimate <- coef_matrix["(Intercept)", "Estimate"]
                p_value <- coef_matrix["(Intercept)", "Pr(>|z|)"]  # Use Pr(>|z|) for GLMs
                significance <- ifelse(p_value < 0.001, "***",
                                       ifelse(p_value < 0.01, "**",
                                              ifelse(p_value < 0.05, "*", 
                                                     ifelse(p_value < 0.1, ".", " "))))
        } else {
                intercept_estimate <- NA
                p_value <- NA
                significance <- NA
        }
        
        aic <- AIC(model)
        formula <- paste(deparse(formula(model)), collapse = "")
        
        list(
                Model = model_name,
                Formula = formula,
                Intercept_Estimate = intercept_estimate,
                P_Value = p_value,
                Significance = significance,
                AIC = aic,
                Delta_AIC = delta_aic[model_name]
        )
}






columns_to_transform <- c("Emergence.time", "Time.spent.in.outer.area..walls", "Time.spent.in.chamber.A", "Time.spent.in.front.of.the.gate")

data <- sqrt_transform2(data, columns_to_transform)


# Apply Shapiro-Wilk test to all columns in the dataframe
for (col in names(data)) {
        # Check if the column contains numeric data (Shapiro-Wilk test requires numeric input)
        if (is.numeric(data[[col]])) {
                # Perform the Shapiro-Wilk test
                test_result <- shapiro.test(data[[col]])
                
                # Print the results
                cat("Shapiro-Wilk normality test for", col, ":\n")
                print(test_result)
                cat("\n")
        } else {
                cat("Column", col, "is not numeric and cannot be tested.\n")
        }
}




# Define your formula
formula1 <- Emergence.time ~ Time.spent.in.outer.area..walls + 
        Time.spent.in.front.of.the.gate + 
        Crossing.the.center + 
        Vertical.movement + 
        Surfacing + 
        Chamber.switching

# Call the function
models <- stepwise_to_null_models(formula = formula1, family = gaussian, data = data)



# Calculate delta AIC for the models


firstmodel <- delta_aic(models)






# Function to extract intercept estimate, p-value, significance, AIC, and delta AIC




##these lines work

# List of models with consistent names


# Extract AIC for each model
aic_values <- sapply(models, AIC)

# Calculate delta AIC, using the AIC of the null model as the baseline
delta_aic <- aic_values - min(aic_values)



# Extract information for each model including delta AIC
model_info <- lapply(names(models), function(model_name) {
        extract_model_info(models[[model_name]], model_name, delta_aic)
})

# Convert the list to a data frame
model_info_df <- do.call(rbind, model_info)
model_info_df <- data.frame(model_info_df, row.names = NULL)




# Convert list columns to character
model_info_df <- data.frame(lapply(model_info_df, function(col) {
        if (is.list(col)) {
                sapply(col, function(x) paste(x, collapse = ", "))
        } else {
                col
        }
}), stringsAsFactors = FALSE)

# Define the file path
file_path <- "Response_emergence_time.csv"

# Write the data frame to a CSV file
write.csv(model_info_df, file = file_path, row.names = FALSE)

# Print a message to confirm the file has been saved
cat("The model information has been saved to", file_path, "\n")




#### second model

# Define your formula
formula2 <- Time.spent.in.outer.area..walls ~ Emergence.time + 
        Time.spent.in.front.of.the.gate + 
        Crossing.the.center + 
        Vertical.movement + 
        Surfacing + 
        Chamber.switching

# Call the function
models <- stepwise_to_null_models(formula = formula2, family = gaussian, data = data)



# Calculate delta AIC 

secondmodel <- delta_aic(models)






# Function to extract intercept estimate, p-value, significance, AIC, and delta AIC




##these lines work



# Extract AIC for each model
aic_values <- sapply(models, AIC)

# Calculate delta AIC, using the AIC of the null model as the baseline
delta_aic <- aic_values - min(aic_values)



# Extract information for each model including delta AIC
model_info <- lapply(names(models), function(model_name) {
        extract_model_info(models[[model_name]], model_name, delta_aic)
})

# Convert the list to a data frame
model_info_df <- do.call(rbind, model_info)
model_info_df <- data.frame(model_info_df, row.names = NULL)




# Convert list columns to character
model_info_df <- data.frame(lapply(model_info_df, function(col) {
        if (is.list(col)) {
                sapply(col, function(x) paste(x, collapse = ", "))
        } else {
                col
        }
}), stringsAsFactors = FALSE)

# Define the file path
file_path <- "Response_Time.spent.in.outer.area..walls.csv"

# Write the data frame to a CSV file
write.csv(model_info_df, file = file_path, row.names = FALSE)

# Print a message to confirm the file has been saved
cat("The model information has been saved to", file_path, "\n")

# Read the CSV files
time_spent_data <- read.csv("Time.spent.in.outer.area..walls.csv")
emergence_time_data <- read.csv("emergence_time_as_response.csv")
# Combine the data frames
combined_data <- rbind(time_spent_data, emergence_time_data)
# Write the combined data to a new CSV file
write.csv(combined_data, "combined_data.csv", row.names = FALSE)





#third model


# Define your formula
formula3 <- Time.spent.in.front.of.the.gate  ~ Emergence.time +
        Time.spent.in.outer.area..walls+
        Crossing.the.center + 
        Vertical.movement + 
        Surfacing + 
        Chamber.switching

# Call the function
models <- stepwise_to_null_models(formula = formula3, family = gaussian, data = data)



# Calculate delta AIC for the models


thirdmodel <- calculate_delta_aic(models)






# Function to extract intercept estimate, p-value, significance, AIC, and delta AIC




##these lines work



# Extract AIC for each model
aic_values <- sapply(models, AIC)

# Calculate delta AIC, using the AIC of the null model as the baseline
delta_aic <- aic_values - min(aic_values)



# Extract information for each model including delta AIC
model_info <- lapply(names(models), function(model_name) {
        extract_model_info(models[[model_name]], model_name, delta_aic)
})

# Convert the list to a data frame
model_info_df <- do.call(rbind, model_info)
model_info_df <- data.frame(model_info_df, row.names = NULL)




# Convert list columns to character
model_info_df <- data.frame(lapply(model_info_df, function(col) {
        if (is.list(col)) {
                sapply(col, function(x) paste(x, collapse = ", "))
        } else {
                col
        }
}), stringsAsFactors = FALSE)

# Define the file path
file_path <- "Response_Time.spent.in.fornt.off.gate.csv"

# Write the data frame to a CSV file
write.csv(model_info_df, file = file_path, row.names = FALSE)

# Print a message to confirm the file has been saved
cat("The model information has been saved to", file_path, "\n")



# fourth model
# Define your formula with Crossing.the.center and Chamber.switching
formula4 <- Crossing.the.center ~ Emergence.time +
        Time.spent.in.outer.area..walls +
        Time.spent.in.front.of.the.gate + 
        Vertical.movement + 
        Surfacing + 
        Chamber.switching

# Call the function to generate stepwise models from the full to null model
models <- stepwise_to_null_models(formula = formula4, family = poisson, data = data)

# Calculate delta AIC for the models
fourthmodel <- calculate_delta_aic(models)

# Extract AIC for each model
aic_values <- sapply(models, AIC)

# Calculate delta AIC, using the AIC of the null model as the baseline
delta_aic <- aic_values - min(aic_values)

# Extract information for each model including delta AIC
model_info <- lapply(names(models), function(model_name) {
        extract_model_info(models[[model_name]], model_name, delta_aic)
})

# Convert the list to a data frame
model_info_df <- do.call(rbind, model_info)
model_info_df <- data.frame(model_info_df, row.names = NULL)

# Convert list columns to character

# Convert list columns to character
model_info_df <- data.frame(lapply(model_info_df, function(col) {
        if (is.list(col)) {
                sapply(col, function(x) paste(x, collapse = ", "))
        } else {
                col
        }
}), stringsAsFactors = FALSE)

# Define the file path
file_path <- "Response_crossinng.the.center.csv"

# Write the data frame to a CSV file
write.csv(model_info_df, file = file_path, row.names = FALSE)

# Print a message to confirm the file has been saved
cat("The model information has been saved to", file_path, "\n")








#fifth_model

formula5 <- Vertical.movement ~ Emergence.time +
        Time.spent.in.front.of.the.gate +
        Time.spent.in.outer.area..walls+
        Crossing.the.center + 
        Surfacing + 
        Chamber.switching
#ΔAIC
models <- stepwise_to_null_models(formula = formula5, family = poisson, data = data)


# Calculate delta AIC for the models
fifthmodel <- calculate_delta_aic(models)

# Extract AIC for each model
aic_values <- sapply(models, AIC)

# Calculate delta AIC, using the AIC of the null model as the baseline
delta_aic <- aic_values - min(aic_values)



# Extract information for each model including delta AIC
model_info <- lapply(names(models), function(model_name) {
        extract_model_info(models[[model_name]], model_name, delta_aic)
})

# Convert the list to a data frame
model_info_df <- do.call(rbind, model_info)
model_info_df <- data.frame(model_info_df, row.names = NULL)




# Convert list columns to character
model_info_df <- data.frame(lapply(model_info_df, function(col) {
        if (is.list(col)) {
                sapply(col, function(x) paste(x, collapse = ", "))
        } else {
                col
        }
}), stringsAsFactors = FALSE)

# Define the file path
file_path <- "Response_verticalmovement.csv"

# Write the data frame to a CSV file
write.csv(model_info_df, file = file_path, row.names = FALSE)

# Print a message to confirm the file has been saved
cat("The model information has been saved to", file_path, "\n")

#sixth model

formula6 <- Surfacing ~ Emergence.time +
        Time.spent.in.front.of.the.gate +
        Time.spent.in.outer.area..walls+
        Crossing.the.center + Vertical.movement+
        Chamber.switching

models <- stepwise_to_null_models(formula = formula, family = poisson, data = data)

# Calculate delta AIC for the models

sixthmodel <- calculate_delta_aic(models)

# Extract AIC for each model
aic_values <- sapply(models, AIC)

# Calculate delta AIC, using the AIC of the null model as the baseline
delta_aic <- aic_values - min(aic_values)



# Extract information for each model including delta AIC
model_info <- lapply(names(models), function(model_name) {
        extract_model_info(models[[model_name]], model_name, delta_aic)
})

# Convert the list to a data frame
model_info_df <- do.call(rbind, model_info)
model_info_df <- data.frame(model_info_df, row.names = NULL)




# Convert list columns to character
model_info_df <- data.frame(lapply(model_info_df, function(col) {
        if (is.list(col)) {
                sapply(col, function(x) paste(x, collapse = ", "))
        } else {
                col
        }
}), stringsAsFactors = FALSE)

# Define the file path
file_path <- "Response_Surfacing.csv"

# Write the data frame to a CSV file
write.csv(model_info_df, file = file_path, row.names = FALSE)

# Print a message to confirm the file has been saved
cat("The model information has been saved to", file_path, "\n")



#seventh model

formula7 <- Chamber.switching ~ Emergence.time +
        Time.spent.in.front.of.the.gate +
        Time.spent.in.outer.area..walls+
        Crossing.the.center + Vertical.movement + Surfacing
        

models <- stepwise_to_null_models(formula = formula7, family = poisson, data = data)

# Calculate delta AIC for the models
seventhmodel <- calculate_delta_aic(models)

# Extract AIC for each model
aic_values <- sapply(models, AIC)

# Calculate delta AIC, using the AIC of the null model as the baseline
delta_aic <- aic_values - min(aic_values)



# Extract information for each model including delta AIC
model_info <- lapply(names(models), function(model_name) {
        extract_model_info(models[[model_name]], model_name, delta_aic)
})

# Convert the list to a data frame
model_info_df <- do.call(rbind, model_info)
model_info_df <- data.frame(model_info_df, row.names = NULL)




# Convert list columns to character
model_info_df <- data.frame(lapply(model_info_df, function(col) {
        if (is.list(col)) {
                sapply(col, function(x) paste(x, collapse = ", "))
        } else {
                col
        }
}), stringsAsFactors = FALSE)

# Define the file path
file_path <- "Response_chamberswitching.csv"

# Write the data frame to a CSV file
write.csv(model_info_df, file = file_path, row.names = FALSE)

# Print a message to confirm the file has been saved
cat("The model information has been saved to", file_path, "\n")


# combining all the csv files together  example# Read the CSV files

emergence_time_data <- read.csv("Response_emergence_time.csv")
time_spent_outer_area <- read.csv("Response_Time.spent.in.outer.area..walls.csv")
time_spent_infront_of_gate <- read.csv("Response_Time.spent.in.fornt.off.gate.csv")
vertical_movement  <- read.csv("Response_verticalmovement.csv")
crossing_center <- read.csv("Response_crossinng.the.center.csv" )
chamberswitching <- read.csv("Response_chamberswitching.csv" )
surfacing  <- read.csv("Response_Surfacing.csv" )


# Combine the data frames
combined_data <- rbind(
        emergence_time_data,
        time_spent_outer_area,
        time_spent_infront_of_gate,
        vertical_movement,
        crossing_center,
        chamberswitching,
        surfacing)

# Write the combined data to a new CSV file
write.csv(combined_data, "2month_fishmodels.csv", row.names = FALSE)
c <- read.csv("2month_fishmodels.csv")
View(c)
  








