 # ----------- Weekly Assignment 4: Data visualisation with R ------------------------
 #--------------------------- Gather insights --------------------------------------
 
 # Step 1: Import all necessary packages and change working directory
 library(tidyverse)
 library(ggplot2)
 setwd(dir = '/Users/diegoreineck/Library/Mobile Documents/com~apple~CloudDocs/University/LSE/Course 3 - Advanced analytics for organisational impact/Course 3 - Assignment')
 
 # Step 2: Import the Turtle Games data.
 data <- read.csv(file.choose())
 
 # Visualise a summary of the data
 summary(data)
 View(data)
 
 # Step 3: Create a sub-data set, remove ranking, year, publisher, genre column
 sales_clean = select(data, -Ranking, -Year, -Genre, -Publisher)
 summary(sales_clean)
 
 # Step 4: Visualize the data - Explore sales data
 
 # Install gridExtra package to allow sublots for ggplots
 install.packages('gridExtra')
 library(gridExtra)
 
 ## plot 1. Histogram plot of NorthAmerica, Europe and Global sales
 
 NA_hist <- qplot(NA_Sales, geom = 'histogram', data = sales_clean)
 EU_hist <- qplot(EU_Sales, geom = 'histogram', data = sales_clean)
 Glob_hist <- qplot(Global_Sales, geom = 'histogram', data = sales_clean)
 grid.arrange(NA_hist,EU_hist,Glob_hist,nrow=3)


 # plot 2. Boxplot of NorthAmerica, Europe and Global sales
    
 NA_boxplot <-qplot(x=NA_Sales, y=Platform, geom = 'boxplot', data=sales_clean)
 EU_boxplot <-qplot(x=EU_Sales, y=Platform, geom = 'boxplot', data=sales_clean)
 Glob_boxplot <-qplot(x=Global_Sales, y=Platform, geom = 'boxplot', data=sales_clean)

 
 # plot 3. scatter plot of NorthAmerica, Europe and Global sales
 NA_EU_scat <-qplot(NA_Sales, EU_Sales, data = sales_clean)
 NA_Glo_scat <- qplot(NA_Sales, Global_Sales, data = sales_clean)
 EU_Glo_scat <- qplot(EU_Sales, Global_Sales, data = sales_clean)
 grid.arrange(NA_EU_scat,NA_Glo_scat,EU_Glo_scat,ncol=3)
 
 # Step 5: Visualize the data  - Explore the impact on sales per product_id
 # I will summarise data according to the Platform.
 
 Sales_group <- group_by(sales_clean, Platform)

View(Sales_group)
 
 
