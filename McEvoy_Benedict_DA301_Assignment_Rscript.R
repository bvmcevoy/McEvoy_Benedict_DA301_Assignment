###########################################################################

## Course 3 Assignment

### TURTLE GAMES: IMPROVING OVERALL SALES PERFORMANCE

###########################################################################

# Turtle Games is a retailer and manufacturer of games. They would
# like to improve their overall sales performance. By getting a
# better understanding of their customers, they can formulate a
# more effective marketing strategy to increase sales. This
# R Script looks at sales data to help the marketing team at
# Turtle Games answer the following:
#
# - Are there certain products we should be promoting?
#
# - Which regions impact global sales the most?
  
###########################################################################

## Importing Necessary Packages

# Installing and loading the tidyverse package
# install.packages('tidyverse')
library(tidyverse)

# Installing and loading the moments package
# install.packages('moments')
library(moments)

###########################################################################

## Loading and Inspecting the Data

# Setting the working directory
# setwd("~/LSE Data Analytics Career Accelerator/Course 3 - Advanced Analytics for Organisational Impact/Assignment")

# Loading the turtle_sales CSV file
turtle_sales <- read.csv('turtle_sales.csv', header=T)

# Printing the data frame
head(turtle_sales)

# Removing unnecessary columns from the data frame (Ranking, Year, Genre,
# and Publisher)
sales_df <- select(turtle_sales, -Ranking, -Year, -Genre, -Publisher)

# Viewing this new data frame
View(sales_df)

# Inspecting the descriptive statistics
summary(sales_df)

# Changing Product to a factor
sales_df$Product <- as.character(sales_df$Product)

# Checking this has worked
str(sales_df)

# Checking for missing values 
sum(is.na(sales_df))
# There are no missing values

# Checking for duplicate rows 
sum(duplicated(sales_df))
# There are no duplicate rows

# Checking for outliers in North American Sales
# Plotting a boxplot of NA_Sales
ggplot(sales_df, aes(x = NA_Sales)) +
  geom_boxplot(fill = 'darkseagreen4',
               outlier.color='red') +
  labs(title = "North American Sales",
       x = "Sales (£million)") +  
  theme_minimal() +
  theme(axis.text.y = element_blank())
# There are many outliers in NA_Sales but I am reluctant to remove
# them at this stage as it makes sense that there would be a few
# games that sell much more and are more popular than the majority.

# Checking for outliers in European Sales
# Plotting a boxplot of EU_Sales
ggplot(sales_df, aes(x = EU_Sales)) +
  geom_boxplot(fill = 'darkseagreen4',
               outlier.color='red') +
  labs(title = "European Sales",
       x = "Sales (£million)") +  
  theme_minimal() +
  theme(axis.text.y = element_blank())
# There are many outliers in EU_Sales but I am reluctant to remove
# them at this stage as it makes sense that there would be a few
# games that sell much more and are more popular than the majority.

# Checking for outliers in Global Sales
# Plotting a boxplot of Global_Sales
ggplot(sales_df, aes(x = Global_Sales)) +
  geom_boxplot(fill = 'darkseagreen4',
               outlier.color='red') +
  labs(title = "Global Sales",
       x = "Sales (£million)") + 
  theme_minimal() +
  theme(axis.text.y = element_blank())
# There are many outliers in Global_Sales but I am reluctant to 
# remove them at this stage as it makes sense that there would be
# a few games that sell much more and are more popular than the
# majority.


#########################################################################

## Are There Certain Products We Should Be Promoting?

# For this question we will try to identify the products that
# generate the most sales in the regions that we have information
# on.


# Checking how many products there are in the data frame
length(unique(sales_df$Product))
# There are 175 products in the data frame

# Grouping the data frame by product code 
# Then summing the sales for each product
product_df <- sales_df %>% group_by(Product) %>%
  summarise(NA_Sales = sum(NA_Sales),
            EU_Sales = sum(EU_Sales),
            Global_Sales = sum(Global_Sales))

# Looking at this data frame
head(product_df)

#####

# Top Selling Products

# Here we have highlighted the products that are in the top 6 for
# each of North America, Europe, and global sales. These are
# products with product codes 107 and 515.

# Plotting the top 6 selling products in North America
# Highlighting the bars with product code 107 and 515
head(product_df[order(product_df$NA_Sales,decreasing=TRUE),]) %>%
  ggplot(., aes(x = Product, y = NA_Sales)) +
  geom_bar(stat = "Identity",
           fill = c("darkseagreen4",
                    "grey",
                    "grey",
                    "grey",
                    "darkseagreen4",
                    "grey")) +
  labs(title = "Top 6 Selling Products in North America",
       y = "North American Sales (£million)",
       x = "Product Code") + 
  theme_minimal()

# Plotting the top 6 selling products in Europe
# Highlighting the bars with product codes 107 and 515
head(product_df[order(product_df$EU_Sales,decreasing=TRUE),]) %>%
  ggplot(., aes(x = Product, y = EU_Sales)) +
  geom_bar(stat = "Identity",
           fill = c("darkseagreen4",
                    "darkseagreen4",
                    "grey",
                    "grey",
                    "grey",
                    "grey")) +
  labs(title = "Top 6 Selling Products in Europe",
       y = "European Sales (£million)",
       x = "Product Code") + 
  theme_minimal()

# Plotting the top 6 selling products globally
# Highlighting the bars with product code 107 and 515
head(product_df[order(product_df$Global_Sales,decreasing=TRUE),]) %>%
  ggplot(., aes(x = Product, y = Global_Sales)) +
  geom_bar(stat = "Identity", 
           fill = c("darkseagreen4",
                    "darkseagreen4",
                    "grey",
                    "grey",
                    "grey",
                    "grey")) +
  labs(title = "Top 6 Selling Products Globally",
       y = "Global Sales (£million)",
       x = "Product Code") + 
  theme_minimal()

#####

# Bottom Selling Products

# Here, there are no products that feature in the bottom 6 for all
# three regions. We have highlighted the product that features in
# both the top 6 for North American and global sales. This is
# the product with product code 7573.

# Plotting the bottom 6 selling products in North America
# Highlighting the bar with product code 7573
head(product_df[order(product_df$NA_Sales),]) %>%
  ggplot(., aes(x = Product, y = NA_Sales)) +
  geom_bar(stat = "Identity",
           fill = c("grey",
                    "grey",
                    "grey",
                    "grey",
                    "red",
                    "grey")) +
  labs(title = "Bottom 6 Selling Products in North America",
       y = "North American Sales (£million)",
       x = "Product Code") + 
  theme_minimal()

# Plotting the bottom 6 selling products in Europe
head(product_df[order(product_df$EU_Sales),]) %>%
  ggplot(., aes(x = Product, y = EU_Sales)) +
  geom_bar(stat = "Identity", fill = "grey") +
  labs(title = "Bottom 6 Selling Products in Europe",
       y = "European Sales (£million)",
       x = "Product Code") + 
  theme_minimal()

# Plotting the bottom 6 selling products globally
# Highlighting the bar with product code 7573
head(product_df[order(product_df$Global_Sales),]) %>%
  ggplot(., aes(x = Product, y = Global_Sales)) +
  geom_bar(stat = "Identity",
           fill = c("grey",
                    "grey",
                    "grey",
                    "red",
                    "grey",
                    "grey")) +
  labs(title = "Bottom 6 Selling Products Globally",
       y = "Global Sales (£million)",
       x = "Product Code") + 
  theme_minimal()

####################################################################

## Which regions impact global sales the most?

# Here, we will fit various linear regression models to understand
# the relationships between North American And European Sales On 
# Global Sales.


# One assumption of linear regression models is that there are no
# significant outliers or missing values. We already checked for
# missing values and outliers when we imported the data. We did not
# remove outliers to answer the previous question, but we should
# now.

# Removing outliers:

# Start by checking the dimensions of the data set
dim(sales_df)
# This has 352 rows.

# Removing outliers from NA_Sales
quartiles <- quantile(sales_df$NA_Sales, 
                      probs = c(.25, .75),
                      na.rm = FALSE)
IQR <- IQR(sales_df$NA_Sales)
Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 
no_outlier_df <- subset(sales_df, 
                        sales_df$NA_Sales > Lower & 
                          sales_df$NA_Sales < Upper)

# Removing outliers from EU_Sales
quartiles <- quantile(no_outlier_df$EU_Sales, 
                      probs = c(.25, .75),
                      na.rm = FALSE)
IQR <- IQR(no_outlier_df$EU_Sales)
Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 
no_outlier_df <- subset(no_outlier_df, 
                        no_outlier_df$EU_Sales > Lower & 
                          no_outlier_df$EU_Sales < Upper)

# Removing outliers from Global_Sales
quartiles <- quantile(no_outlier_df$Global_Sales, 
                      probs = c(.25, .75),
                      na.rm = FALSE)
IQR <- IQR(no_outlier_df$Global_Sales)
Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 
no_outlier_df <- subset(no_outlier_df, 
                        no_outlier_df$Global_Sales > Lower & 
                          no_outlier_df$Global_Sales < Upper)

# Checking the dimensions of this new data frame
dim(no_outlier_df)
# It appears that 37 rows were removed.

# Now we can start fitting the linear regression models.

#####

# North American Sales and Global Sales

# Checking the correlation between NA_Sales and Global_Sales
cor(no_outlier_df$NA_Sales, no_outlier_df$Global_Sales)
# This tells us that they seem to be highly correlated.

# Plotting this relationship
ggplot(no_outlier_df, aes(x = NA_Sales, y = Global_Sales)) + 
  geom_point() +
  theme_minimal()
# This meets another assumption of the linear regression model
# in that there appears to be a linear relationship between the
# variables.

# Creating a linear regression model
model1 <- lm(Global_Sales ~ NA_Sales, data = no_outlier_df)

# Viewing a summary of this model
summary(model1)
# Here, we can see from the p-values that NA_Sales is highly
# significant in predicting Global_Sales. We can also see, from
# the R-squared value, that North American sales explains 79.4%
# of the variability in global sales. This model suggests that for
# every unit increase (£million) in North American Sales, global 
# sales increases by 1.67 units (£million).

# Plotting the linear model
ggplot(no_outlier_df, aes(x = NA_Sales, y = Global_Sales)) + 
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(title = "North American Sales And Global Sales",
       y = "Global Sales (£million)",
       x = "North American Sales (£million)") +
  theme_minimal()

# Now let's check if the other assumptions of linear regression
# are met for this particular model.

# Checking the residuals follow a normal distribution:

# Plotting a histogram of the residuals
hist(model1$residuals)
# They appear to have a fairly normal distribution.

# Checking the skewness of the distribution of the residuals
skewness(model1$residuals)
# This tells us that the distribution is slightly positively
# skewed.

# Checking the kurtosis of the distribution of the residuals
kurtosis(model1$residuals)
# This tells us that it has heavier tails than the normal
# distribution.

# Plotting a qqplot to check for normality
qqnorm(model1$residuals)
qqline(model1$residuals)
# The tails are away from the line suggesting the residuals might
# not follow a normal distribution. 

# Carrying out the Shapiro-Wilk test for normality 
shapiro.test(model1$residuals)
# As the p-value is less than 0.05, this suggests that the
# residuals are not normally distributed. Therefore, this
# assumption cannot be met, which is a limitation of this model.

# Checking the residuals do not display a pattern:

# Plotting the residuals of this model
plot(model1$residuals)
# There appears to be a pattern going on as the graph displays a
# cone-like shape.

# Taking the log of the dependent variable to counteract this 
no_outlier_df <- mutate(no_outlier_df,
                        logGlobal_Sales = log(Global_Sales))

# Creating a new linear regression model with the log of 
# Global_Sales
model2 <- lm(logGlobal_Sales ~ NA_Sales, data = no_outlier_df)

# Plotting the residuals of this model
plot(model2$residuals)
# This too appears to show a pattern in the residuals. This has
# not made our model any better. While there are limitations 
# to model1, we will use that one to understand the relationship
# between NA_Sales and Global_Sales.

#####

# European Sales and Global Sales

# Checking the correlation between EU_Sales and Global_Sales
cor(no_outlier_df$EU_Sales, no_outlier_df$Global_Sales)
# This tells us that they seem to be highly correlated.

# Plotting this relationship
ggplot(no_outlier_df, aes(x = EU_Sales, y = Global_Sales)) + 
  geom_point() +
  theme_minimal()
# This meets another assumption of the linear regression model
# in that there appears to be a linear relationship between the
# variables.

# Creating a linear regression model
model3 <- lm(Global_Sales ~ EU_Sales, data = no_outlier_df)

# Viewing a summary of this model
summary(model3)
# Here, we can see from the p-values that EU_Sales is highly
# significant in predicting Global_Sales. We can also see, from
# the R-squared value, that European sales explains 69% of the 
# variability in global sales. This model suggests that for every
# unit increase (£million) in European Sales, global sales
# increases by 2.56 units (£million).

# Plotting the linear model
ggplot(no_outlier_df, aes(x = EU_Sales, y = Global_Sales)) + 
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(title = "European Sales And Global Sales",
       y = "Global Sales (£million)",
       x = "European Sales (£million)") +
  theme_minimal()

# Now let's check if the other assumptions of linear regression
# are met for this particular model.

# Checking the residuals follow a normal distribution:

# Plotting a histogram of the residuals
hist(model3$residuals)
# They appear to have a fairly normal distribution, but I can see
# that it is slightly positively skewed.

# Checking the skewness of the distribution of the residuals
skewness(model3$residuals)
# This tells us that the distribution is slightly positively
# skewed.

# Checking the kurtosis of the distribution of the residuals
kurtosis(model3$residuals)
# This tells us that it has heavier tails than the normal
# distribution.

# Plotting a qqplot to check for normality
qqnorm(model3$residuals)
qqline(model3$residuals)
# The tails are away from the line suggesting the residuals might
# not follow a normal distribution. 

# Carrying out the Shapiro-Wilk test for normality 
shapiro.test(model3$residuals)
# As the p-value is less than 0.05, this suggests that the
# residuals are not normally distributed. Therefore, this
# assumption cannot be met, which is a limitation of this model.

# Checking the residuals do not display a pattern:

# Plotting the residuals of this model
plot(model3$residuals)
# There appears to be a pattern going on as the graph displays a
# cone-like shape.

# Creating a new linear regression model with the log of 
# Global_Sales to try to counteract this
model4 <- lm(logGlobal_Sales ~ EU_Sales, data = no_outlier_df)

# Plotting the residuals of this model
plot(model4$residuals)
# This too appears to show a pattern in the residuals. This has
# not made our model any better. While there are limitations 
# to model3, we will use that one to understand the relationship
# between EU_Sales and Global_Sales.

#####

# Comparing models

# NA_Sales is more likely to increase Global_Sales as model1
# suggests that North American sales explain more of the
# variability (79.4%) in global sales than Europe (69%) in model3.

# According to model3, EU_Sales increase global sales more steeply
# than North American sales when comparing it to model1 (2.56
# units (£million) for every unit increase in European sales
# compared to 1.67 units (£million) for North American sales in
# model1).


