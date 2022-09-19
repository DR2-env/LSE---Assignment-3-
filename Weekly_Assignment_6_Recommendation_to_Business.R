 # --- Weekly Assignment 6: Making recommentdations to the business ------- #

# Set working directory.
setwd('/Users/diegoreineck/Library/Mobile Documents/com~apple~CloudDocs/University/LSE/Course 3 - Advanced analytics for organisational impact/Course 3 - Assignment')


# Step 1: Import all necessary packages. ---------------------------
library(tidyverse) 
library(ggplot2)
library(plotly)
library(corrplot)

# Step 2: import Turltle games sales data ---------------------------
raw_sales <- read.csv(file.choose(), header = T)

# Create new data frame by getting rid of specific columns
sales_clean <- select(raw_sales, -Ranking, -Year, -Genre, -Publisher)

# Sub dataframe for slaes only
sales_only <- select(sales_clean, -Product, -Platform)


# Step 3: Preview and summarise the data ---------------------------
View(sales_only)
summary(sales_only)
head(sales_only)


# Step 4: Determine the correlation between the different sales columns ----
# 4.1 Calculate correlation

# Save correlation as an object:
sales_corr =  cor(sales_only)

# 4.2.a Visualise the correlation (as a matrix)
# Plot a correlation matrix:
corrplot(sales_corr, method = 'number')

# 4.2.b (visualise the correlation, as scatter plot + line of best fit)

# North America - Europe sales
NA_EU_cor <- lm(EU_Sales ~ NA_Sales, data = sales_only)
summary(NA_EU_cor)
# View the output.
plot(sales_only$NA_Sales, sales_only$EU_Sales)
abline(coefficients(NA_EU_cor))

# North America - Global Sales
NA_Global_cor <- lm(Global_Sales ~ NA_Sales, data = sales_only)
summary(NA_Global_cor)
# View the output
plot(sales_only$NA_Sales, sales_only$Global_Sales)
abline(coefficients(NA_Global_cor))


# EU - Global sales
EU_Global_cor <- lm(Global_Sales ~ EU_Sales, data = sales_only)
summary(EU_Global_cor)
# View the output
plot(sales_only$EU_Sales, sales_only$Global_Sales)
abline(coefficients(EU_Global_cor))


# Step 5: Determine global sales based on multilinear regression:----------
# Create model
model1_sales <- lm(Global_Sales ~EU_Sales + NA_Sales, data = sales_only)
summary(model1_sales)

# Observation: 
# Adjusted R squared = 0.9685
# Significance of t-test for both EU & NA sales is <2e-16. Indicating that both,
# variables play a significant role in predicting global sales value. 

# Step 6: Predict future global sales based on built model.
# Load the test data
sales_test <- read.csv(file.choose())
predict_Globa_Sales = predict(model1_sales, newdata=sales_test,
                      interval='confidence')
predict_Globa_Sales

# In comparison to the actual data, the predict global sales is a bit over-estimated.

# E.g., Actual 67.85, predicted: 71.47
# E.g., Actual 6.04, predicted: 6.85
# E.g., Actual 4.32, predicted: 4.25
# E.g., Actual 3.53, predicted 4.13
# E.g., Actual 23.21 predicted 26.43

# Global sales can nearly be fully explained by North America and EU sales. However, 
# There could be other external factors that could influence global sales as well.
# This could explain the overestimation in the model. 

# 3D plot
plot_ly(sales_only,
        x = ~NA_Sales,
        y = ~EU_Sales,
        z= ~Global_Sales)











