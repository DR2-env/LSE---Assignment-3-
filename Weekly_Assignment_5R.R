# ---------Weekly assignment 5: Clean manipulate and visualise the data----#

# Step 1: import necessary libraries ---------------
library(tidyverse)
library(skimr)
library(DataExplorer)
library(BSDA)
library(ggplot2)
library(moments)

setwd('/Users/diegoreineck/Library/Mobile Documents/com~apple~CloudDocs/University/LSE/Course 3 - Advanced analytics for organisational impact/Course 3 - Assignment/LSE_DA301_assignment_files')

# Step 2: import Turtle games sales data ---------------------------

raw_sales <- read.csv(file.choose(), header = T)

# Create new data frame by getting rid of specific columns
sales_clean <- select(raw_sales, -Ranking, -Year, -Genre, -Publisher)

# Preview data:
View(sales_clean)
str(sales_clean)
as_tibble(sales_clean)

# Create a report and summary of the data
DataExplorer::create_report(sales_clean)
head(sales_clean)


# Descriptive statistics: ------
summary(sales_clean)

# NA
NA_max_sales <- max(sales_clean$NA_Sales)
NA_min_sales <- min(sales_clean$NA_Sales)
NA_avg_sales <- mean(sales_clean$NA_Sales)

# EU
EU_max_sales <- max(sales_clean$EU_Sales)
EU_min_sales <- min(sales_clean$EU_Sales)
EU_avg_sales <- mean(sales_clean$EU_Sales)

# Global
GLB_max_sales <- max(sales_clean$Global_Sales)
GLB_min_sales <- min(sales_clean$Global_Sales)
GLB_avg_sales <- mean(sales_clean$Global_Sales)


# Step 3: Determining the impact of sales on product_ID: --------------------

# Aggregate method.
NA_tot_sales <-aggregate(NA_Sales~Platform, sales_clean, sum)
EU_tot_sales <- aggregate(EU_Sales~Platform, sales_clean, sum)
Glob_tot_sales <- aggregate(Global_Sales~Platform, sales_clean, sum)

# Group by method: 
sales_impact <- sales_clean %>% group_by(Platform) %>%
  summarise(NA_sum_sales=sum(NA_Sales),
            EU_sum_sales = sum(EU_Sales),
            GLB_sum_sales = sum(Global_Sales),
            .groups='drop')
sales_impact
# Save sum of sales as a csv file
write.csv(sales_impact, 'summary_sales.csv')


# Step 4: Create plots to determine insights into the data --------------------
head(sales_clean)

# Histograms: 
#NA
ggplot(sales_clean, aes(x = NA_Sales)) + 
  geom_histogram(color = 'black', fill = 'blue', bins = 20) + 
  labs(x = "Sales", y = 'Frequency', title = 'Histogram of North America Sales') +
  theme_minimal()

#EU
ggplot(sales_clean, aes(x=EU_Sales)) + 
  geom_histogram(color = 'black', fill = 'red', bins = 20) + 
  labs(x = 'Sales', y = 'Frequency', title = 'Histogram of Europ Sales') +
  theme_minimal()

# Global
ggplot(sales_clean, aes(x = Global_Sales)) + 
  geom_histogram(color = 'black', fill = 'green', bins = 20) + 
  labs(x = 'Sales', y = 'Frequency', title = 'Histogram of Global Sales') +
  theme_minimal()

# Boxplots: 
# NA
ggplot(sales_clean, aes(x = Platform, y = NA_Sales)) + 
  geom_boxplot(color = 'black', fill = 'blue', outlier.colour = 'red', outlier.shape = 'square') + 
  labs(x = "Consol type", y = 'Sales', title = 'Boxplot of North America Sales') +
   coord_flip() + theme_minimal()

# EU 
ggplot(sales_clean, aes(x = Platform, y = EU_Sales)) + 
  geom_boxplot(color = 'black', fill = 'blue', outlier.colour = 'red', outlier.shape = 'square') + 
  labs(x = "Consol type", y = 'Sales', title = 'Boxplot of Europe Sales') +
  coord_flip() + theme_minimal()

# Global
ggplot(sales_clean, aes(x = Platform, y = Global_Sales)) + 
  geom_boxplot(color = 'black', fill = 'blue', outlier.colour = 'red', outlier.shape = 'square') + 
  labs(x = "Consol type", y = 'Sales', title = 'Boxplot of Global Sales') +
  coord_flip() + theme_minimal()

# Scatter plots:
# NA -  Global
ggplot(sales_clean, aes(x = NA_Sales, y = Global_Sales, col  = Platform)) + 
  geom_smooth(se=F)

# EU  - Global
ggplot(sales_clean, aes(x = EU_Sales, y = Global_Sales, col  = Platform)) + 
  geom_smooth(se=F)

# EU - NA
ggplot(sales_clean, aes(x = EU_Sales, y = NA_Sales, col  = Platform)) + 
  geom_smooth(se=F)

# Note: in each case scenario there is a linear linear relationship between EU/US 
# WII sales to Global WII sales

# Step 5: Test for the normality of the data set ------------------------- 

# QQplots: ---
# NA
qqline(sales_clean$NA_Sales)
# EU
qqline(sales_clean$EU_Sales)
# Global
qqline(sales_clean$Global_Sales)

# Shapiro-Wilks test: ------
# NA
shapiro.test(sales_clean$NA_Sales) 
# P is smaller than .05 thus not normally distributed.

# EU 
shapiro.test(sales_clean$EU_Sales)
# P is smaller than .05 thus not normally distributed.

# Global
shapiro.test(sales_clean$Global_Sales)
# P is smaller than .05 thus not normally distributed.

# Kurtosis & Skewness ---------------
#NA
kurtosis(sales_clean$NA_Sales)
skewness(sales_clean$NA_Sales)

# EU
kurtosis(sales_clean$EU_Sales)
skewness(sales_clean$EU_Sales)

# Global
kurtosis(sales_clean$Global_Sales)
skewness(sales_clean$Global_Sales)

# Note for all three sales data the kurtosis is significantly greater than
# 3, thus the data have a heavier tail than a normal distribution.
# And all three data sets are rightly skewed with values between 4-5.

# Sales correlation: ---------

# NA - EU
cor(sales_clean$NA_Sales, sales_clean$EU_Sales) # 0.70 strong positive

# NA - Global
cor(sales_clean$NA_Sales, sales_clean$Global_Sales) # 0.93 very strong positive

# EU - Global
cor(sales_clean$EU_Sales, sales_clean$Global_Sales) # 0.87 very strong positive.




