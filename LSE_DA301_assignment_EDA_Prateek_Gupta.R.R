## LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

# Assignment template

## Scenario
## You are a data analyst working for Turtle Games, a game manufacturer and 
## retailer. They manufacture and sell their own products, along with sourcing
## and selling products manufactured by other companies. Their product range 
## includes books, board games, video games and toys. They have a global 
## customer base and have a business objective of improving overall sales 
##performance by utilising customer trends. 

## In particular, Turtle Games wants to understand:
## - how customers accumulate loyalty points (Week 1)
## - how useful are remuneration and spending scores data (Week 2)
## - can social data (e.g. customer reviews) be used in marketing 
##     campaigns (Week 3)
## - what is the impact on sales per product (Week 4)
## - the reliability of the data (e.g. normal distribution, Skewness, Kurtosis)
##     (Week 5)
## - if there is any possible relationship(s) in sales between North America,
##     Europe, and global sales (Week 6).

################################################################################

# Week 4 assignment: EDA using R

## The sales department of Turtle games prefers R to Python. As you can perform
## data analysis in R, you will explore and prepare the data set for analysis by
## utilising basic statistics and plots. Note that you will use this data set 
## in future modules as well and it is, therefore, strongly encouraged to first
## clean the data as per provided guidelines and then save a copy of the clean 
## data for future use.

# Instructions
# 1. Load and explore the data.
##  - Remove redundant columns (Ranking, Year, Genre, Publisher) by creating 
##      a subset of the data frame.
##  - Create a summary of the new data frame.
# 2. Create plots to review and determine insights into data set.
##  - Create scatterplots, histograms and boxplots to gain insights into
##      the Sales data.
##  - Note your observations and diagrams that could be used to provide
##      insights to the business.
# 3. Determine the impact on sales per product_id.
##  - Use the group_by and aggregate functions to sum the values grouped by
##      product.
##  - Create a summary of the new data frame.
# 4. Create plots to review and determine insights into the data set.
##  - Create scatterplots, histograms, and boxplots to gain insights into 
##     the Sales data.
##  - Note your observations and diagrams that could be used to provide 
##     insights to the business.
# 5. Include your insights and observations.

###############################################################################

# 1. Load and explore the data

# Install and import Tidyverse.
install.packages('tidyverse')
library(tidyverse)

# Import the sales data set.
sales <- read.csv(file.choose(), header=T)

# Print the data frame.
head(sales)
str(sales)
summary(sales)

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns. 
sales_new <- subset(sales, select=-c(Ranking, Year, Genre, Publisher))

# View the data frame.
View(sales_new)
head(sales_new)
dim(sales_new)
as_tibble(sales_new)
sales_new[is.na(sales_new)]

# View the descriptive statistics.
summary(sales_new)
DataExplorer::create_report(sales_new)


################################################################################

# 2. Review plots to determine insights into the data set.

## 2a) Scatterplots
# Create scatterplots.
qplot(Platform, Global_Sales, data=sales_new, colour=I('red')) +
  labs(title='Global Sales by Platform')

qplot(Platform, EU_Sales, data=sales_new, colour=I('blue')) +
  labs(title='EU Sales by Platform')

qplot(Platform, NA_Sales, data=sales_new, colour=I('dark green')) +
  labs(title='NA Sales by Platform')

## 2b) Histograms
# Create histograms.
qplot(Platform, data=sales_new, colour=I('purple')) + 
  labs(title='No of Products per Platform')


## 2c) Boxplots
# Create boxplots.
qplot(Platform, Global_Sales, data=sales_new, geom='boxplot', colour=I('red')) +
  labs(title='Global Sales by Platform')

qplot(Platform, NA_Sales, data=sales_new, geom='boxplot', colour=I('blue')) +
  labs(title='NA Sales by Platform')

qplot(Platform, EU_Sales, data=sales_new, geom='boxplot', 
      colour=I('dark green')) + labs(title='EU Sales by Platform')

###############################################################################

# 3. Determine the impact on sales per product_id.

## 3a) Explore the Data set.
# Duplicate the dataframe
sales_group <- sales_new

# Convert column Product to factor
sales_group$Product <- as.factor(sales_group$Product)

# View the data frame.
View(sales_group)
summary(sales_group)

# Identify Top Sales Turnover products
arrange(sales_group, desc(Global_Sales))
arrange(sales_group, desc(NA_Sales))
arrange(sales_group, desc(EU_Sales))

# Identify Lowest sales turnover products
arrange(sales_group, Global_Sales)
arrange(sales_group, NA_Sales)
arrange(sales_group, EU_Sales)

# Count number of products per Platform
table(sales_group$Platform)

## 3b) Determine which plot is the best to compare game sales.
# Create scatterplots.
qplot(Product, Global_Sales, color=Platform, data=sales_group) + 
  labs(title='Sales by Product')
qplot(Platform, Global_Sales, data=sales_group) + labs(title='Sales by Platform')

# Create histograms.
qplot(Global_Sales, bins= 50, data=sales_group)

# Create boxplots.
qplot(Product, Global_Sales, color=Platform, data=sales_group, geom='boxplot') +
  labs(title='Total Sales by Product')


###############################################################################

# 4. Observations and insights

## Your observations and insights here ......
# Out of top 10 sales turnover products considering by platform, 
# 5 belong to wii platform.
# Highest sales product belongs to wii platform.
# 7 out of lowest sale turnover 10 products, platform wise belong to PC platform. 
# Maximum number of games are sold on x360 platform.
# 17 products in NA registered no sale on at least one platform 
# compared to only 3 in EU. 
# Out of 17 in NA 10 belong to PC. Only 2 products belong to PC in bottom 10 
# in EU.
# Only 1 product each sold for Platform Gen and 2600.
# Scatterplot is most appropriate to compare Game sales. 


###############################################################################
###############################################################################


# Week 5 assignment: Cleaning and manipulating data using R

## Utilising R, you will explore, prepare and explain the normality of the data
## set based on plots, Skewness, Kurtosis, and a Shapiro-Wilk test. Note that
## you will use this data set in future modules as well and it is, therefore, 
## strongly encouraged to first clean the data as per provided guidelines and 
## then save a copy of the clean data for future use.

## Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 4 assignment. 
##  - View the data frame to sense-check the data set.
##  - Determine the `min`, `max` and `mean` values of all the sales data.
##  - Create a summary of the data frame.
# 2. Determine the normality of the data set.
##  - Create and explore Q-Q plots for all sales data.
##  - Perform a Shapiro-Wilk test on all the sales data.
##  - Determine the Skewness and Kurtosis of all the sales data.
##  - Determine if there is any correlation between the sales data columns.
# 3. Create plots to gain insights into the sales data.
##  - Compare all the sales data (columns) for any correlation(s).
##  - Add a trend line to the plots for ease of interpretation.
# 4. Include your insights and observations.

################################################################################

# 1. Load and explore the data

# View data frame created in Week 4.
View(sales_group)


# Check output: Determine the min, max, and mean values.
# Find Min values
min(sales_group$NA_Sales)
min(sales_group$EU_Sales)
min(sales_group$Global_Sales)

# Find max values
max(sales_group$NA_Sales)
max(sales_group$EU_Sales)
max(sales_group$Global_Sales)

# Find Mean values
mean(sales_group$NA_Sales)
mean(sales_group$EU_Sales)
mean(sales_group$Global_Sales)

# View the descriptive statistics.
summary(sales_group)
DataExplorer::create_report(sales_group)

# Use group_by

group_sales <- sales_group %>% group_by(Platform, Product) %>%
  summarise(Total_sales=sum(Global_Sales),
            .groups='drop')
            

group_sales1 <- sales_group %>% group_by(Product, Platform) %>%
  summarise(Total_sales=sum(Global_Sales),
            .groups='drop')

group_sales1.1 <- sales_group %>% group_by(Product, Platform) %>%
  summarise(NA_sales=sum(NA_Sales),
            .groups='drop')

group_sales1.2 <- sales_group %>% group_by(Product, Platform) %>%
  summarise(EU_sales=sum(EU_Sales),
            .groups='drop')

# View the df
View(group_sales)
View(group_sales1)
View(group_sales1.1)
View(group_sales1.2)

# use aggregate function to determine mean and total sales per platform
aggregate(Total_sales~Platform, group_sales, mean)
aggregate(Total_sales~Platform, group_sales, sum)
group_sales2 <- aggregate(Total_sales~Product, group_sales1, sum)
group_sales3 <- aggregate(NA_sales~Product, group_sales1.1, sum)
group_sales4 <- aggregate(EU_sales~Product, group_sales1.2, sum)

# view and arrange new df
View(group_sales2)
arrange(group_sales2, desc(Total_sales))
arrange(group_sales2, Total_sales)

View(group_sales3)
arrange(group_sales3, desc(NA_sales))
arrange(group_sales3, NA_sales)

View(group_sales4)
arrange(group_sales4, desc(EU_sales))
arrange(group_sales4, EU_sales)

# View the descriptive statistics
summary(group_sales2)
summary(group_sales3)
summary(group_sales4)
###############################################################################

# 2. Determine the normality of the data set.

## 2a) Create Q-Q Plots
# Create Q-Q Plots.
qqnorm(group_sales2$Total_sales)
qqline(group_sales2$Total_sales, col='blue')

qqnorm(group_sales3$NA_sales)
qqline(group_sales3$NA_sales, col='blue')

qqnorm(group_sales4$EU_sales)
qqline(group_sales4$EU_sales, col='blue')

## 2b) Perform Shapiro-Wilk test
# Install and import Moments.
library(moments)

# Remove outliers
group_sales2 <- filter(group_sales2, Total_sales<40)
str(group_sales2)
group_sales3 <- filter(group_sales3, NA_sales<25, NA_sales>0.99)
str(group_sales3)
group_sales4 <- filter(group_sales4, EU_sales<11, EU_sales>0.50)
str(group_sales4)


# Perform Shapiro-Wilk test.
shapiro.test(group_sales2$Total_sales)
shapiro.test(group_sales3$NA_sales)
shapiro.test(group_sales4$EU_sales)


## 2c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.
skewness(group_sales2$Total_sales)
skewness(group_sales4$EU_sales)
skewness(group_sales3$NA_sales)

kurtosis(group_sales2$Total_sales)
kurtosis(group_sales3$NA_sales)
kurtosis(group_sales4$EU_sales)

## 2d) Determine correlation
# Determine correlation.
round (cor(sales_group$NA_Sales, sales_group$Global_Sales), digits=2)
round (cor(sales_group$EU_Sales, sales_group$Global_Sales), digits=2)
round (cor(sales_group$NA_Sales, sales_group$EU_Sales), digits=2)


###############################################################################

# 3. Plot the data
# Create plots to gain insights into data.
# Remove Outliers
sales_group0 <- filter(sales_group, Global_Sales<40)
ggplot(data=sales_group0, mapping=aes(NA_Sales, Global_Sales)) +
  geom_point(color='blue', size=2) +
  geom_smooth(method='lm', se=FALSE, size=1.5) +
  labs(title='Relationship between Global Sales and NA sales')


ggplot(data=sales_group0, mapping=aes(EU_Sales, Global_Sales)) +
  geom_point(color='red', size=2) +
  geom_smooth(method='lm', se=FALSE, size=1.5) +
  labs(title='Relationship between Global Sales and EU sales')



ggplot(data=sales_group0, mapping=aes(NA_Sales, EU_Sales)) +
  geom_point(color='blue', size=2) +
  geom_smooth(method='lm', se=FALSE, size=1.5) +
  labs(title='Relationship between EU Sales and NA sales')


ggplot(data=sales_group0, mapping=aes(EU_Sales, NA_Sales, color=Platform)) +
  geom_point(size=2) +
  labs(title='NA and EU Sales by Platform')

ggplot(data=sales_group0, mapping=aes(EU_Sales, Global_Sales, color=Platform)) +
  geom_point(size=2) +
  labs(title='Global and EU Sales by Platform')

ggplot(data=sales_group0, mapping=aes(Global_Sales, NA_Sales, color=Platform)) +
  geom_point(size=2) +
  labs(title='NA and Global Sales by Platform')

###############################################################################

# 4. Observations and insights
# Your observations and insights here...
# NES platform has highest mean sales per product, followed by wii. 
# PSV has the lowest mean. Highest turnover platform is wii.
# Highest turnover products are 107, 123, 195, 231, 249, 254, 263, 283, 291, 326
# Lowest turnover products are 8923, 8235, 7533, 7673, 7532, 7381, 7384, 7042, 
# 6815, 6770
# Lowest turnover game registered 4.2 million sales while highest registered
# 67.8 million. Average sales were 10.7 million per product.
# NA: highest sales - 107: 34.02, lowest - 4491: 0.06
# EU: Highest sales - 107: 23.80, Lowest - 5510: 0.0
# p value for all 3 data sets were lower than 0.05, so can conclude that data 
# is not normally distributed
# All 3 sales data sets are positively skewed.
# Kurtosis is higher than 3, indicating Leptokurtic distributions.
# Strong correlation exist between 3 sales figures, suggesting similar choice
# of games across the globe.
# (Outliers were removed for better presentation of data)

###############################################################################
###############################################################################

# Week 6 assignment: Making recommendations to the business using R

## The sales department wants to better understand if there is any relationship
## between North America, Europe, and global sales. Therefore, you need to
## investigate any possible relationship(s) in the sales data by creating a 
## simple and multiple linear regression model. Based on the models and your
## previous analysis (Weeks 1-5), you will then provide recommendations to 
## Turtle Games based on:
##   - Do you have confidence in the models based on goodness of fit and
##        accuracy of predictions?
##   - What would your suggestions and recommendations be to the business?
##   - If needed, how would you improve the model(s)?
##   - Explain your answers.

# Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 5 assignment. 
# 2. Create a simple linear regression model.
##  - Determine the correlation between the sales columns.
##  - View the output.
##  - Create plots to view the linear regression.
# 3. Create a multiple linear regression model
##  - Select only the numeric columns.
##  - Determine the correlation between the sales columns.
##  - View the output.
# 4. Predict global sales based on provided values. Compare your prediction to
#      the observed value(s).
##  - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
##  - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
##  - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
##  - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
##  - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.
# 5. Include your insights and observations.

###############################################################################

# 1. Load and explor the data
# View data frame created in Week 5.
View(sales_group)

# Determine a summary of the data frame.
summary(sales_group)

###############################################################################

# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.

### NA_Sales vs Global_Sales

# Create a model with only one x variable.
model1 <- lm(Global_Sales~NA_Sales, data=sales_group)

# View full Regression Table
summary(model1)


### EU_Sales vs Global_Sales

# Create a model with only one x variable.
model2 <- lm(Global_Sales~EU_Sales, data=sales_group)

# View full Regression Table
summary(model2)


### NA_Sales vs EU_Sales 

# Create a model with only one x variable.
model3 <- lm(EU_Sales~NA_Sales, data=sales_group)

# View full Regression Table
summary(model3)

# NA_Sales explains 49.78% of variability in EU_Sales

## 2b) Create a plot (simple linear regression)
# Basic visualisation.

#View residuals on a plot.

### model1
plot(model1$residuals)

# Plot the relationship with base R graphics.
plot(sales_group$NA_Sales, sales_group$Global_Sales)
# Add line-of-best-fit.
abline(coefficients(model1), col='blue')

### model2
plot(model2$residuals)

# Plot the relationship with base R graphics.
plot(sales_group$EU_Sales, sales_group$Global_Sales)
# Add line-of-best-fit.
abline(coefficients(model2), col='red')

### model3
plot(model3$residuals)

# Plot the relationship with base R graphics.
plot(sales_group$NA_Sales, sales_group$EU_Sales)
# Add line-of-best-fit.
abline(coefficients(model3), col='green')

###############################################################################

# 3. Create a multiple linear regression model
# Select only numeric columns from the original data frame.

# Multiple linear regression model.
model4 <- lm(Global_Sales~NA_Sales+EU_Sales, data=sales_group)

# View the complete Regression Table
summary(model4)

# Create a visualisation to determine normality of data set.
qqnorm(residuals(model4))
qqline(residuals(model4), col='blue')

###############################################################################

# 4. Predictions based on given values
# Compare with observed values for a number of records.

# Create a new data frame for the forecast values.
NA_Sales <- c(34.02, 3.93, 2.73, 2.26, 22.08)
EU_Sales <- c(23.80, 1.56, 0.65, 0.97, 0.52)
salesForecast <- data.frame(NA_Sales, EU_Sales)

# Predict for the values.
predict(model4, newdata=salesForecast)

# Add the values to the cpiForecast data frame.
salesForecast$Global_Sales <- predict(model4, newdata=salesForecast)

# View the dataframe
salesForecast

# arrange and view the original dataframe to compare the predicted values
arrange(sales_group, Global_Sales)
arrange(sales_group, desc(Global_Sales))

###############################################################################

# 5. Observations and insights
# Your observations and insights here...
# For model1:
# NA_Sales is a highly significant value, explaining over 87.41% of variability
# For model2: 
# EU_Sales is a significant value, but not as much as NA_Sales.
# EU_Sales explains 77.01% of variability in Global_Sales
# For model3:
# NA_Sales explains 49.78% of variability in EU_Sales
# NA_Sales are not good predictors for EU_Sales and vice-versa.
# Model4:
# Both NA_Sales and EU_Sales are significant, and explain 96.85% variability 
# in Global_Sales
# P value in all models was less than 0.05
# Predictions are quite close to the observed values using model4. 
# Model can be used to predict global sales.
###############################################################################
###############################################################################




