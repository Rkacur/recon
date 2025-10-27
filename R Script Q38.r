
# This is a review of supermarket data for q38. 
library(tidyverse)
library(ggplot2)
library(GGally)
library(dplyr)
#missing ggcorrplot 

# load data
df <- readRDS("~/Desktop/outlet124.rds")

# labels for variables, should we define the display catagory also?
label_map <- c(
  logsales = "Log Sales",
  p_own = "Own Price",
  p_qualig = "Competitor Price (Qualig)",
  p_qualimo = "Competitor Price (Qualimo)",
  p_qualis = "Competitor Price (Qualis)",
  display = "Display Promotion"
)

# data check/ cleaning
head(df)
str(df)
colSums(is.na(df))

#descriptive statistics
describe(select(df, logsales, p_own, p_qualig, p_qualimo, p_qualis))

#Display frequency, ratio
table(df$display)
prop.table(table(df$display))

#Pie Chart of display frequency. Had issues running the original so this code is different from the slides.
pie(table(df$display), 
    labels = c("No Display", "Display"), 
    main = "Display Frequecy",
    col = c("#F77964", "#92D282"))

#bar plot of display frequency
barplot(table(df$display), 
        main = "Display", 
        xlab = "Display Status", 
        ylab = "Frequency",
        names.arg = c("No Display (0)", "Display (1)"),
        col = c("#F77964", "#92D282"), 
        border = "black")

#Histogram. Delete this one?
df %>%
  select(logsales, p_own, p_qualig, p_qualimo, p_qualis) %>%
  gather(variable, value) %>%
  ggplot(aes(x = value)) +
  geom_histogram(fill = "steelblue", bins = 10, color = "white") +
  facet_wrap(~variable, scales = "free") +
  theme_minimal() +
  labs(title = "Distributions of numeric variables")

#Histogram, more efficent
hist(df$p_own,
     main = "Histogram of p_own", xlab = "p_own",
     col = "lightblue", border = "white")

# Bar Plot for Qualig prices, is there a way to get 4 bar plots from this one block of code?
barplot(df$p_qualig,
        main = "Qualig Prices",
        xlab = "Weeks", 
        ylab = "Price",
        ylim = c(0,4),
        col = "#F77964",
        border = "black")

# Bar Plot for Own Price, 
barplot(df$p_own,
        main = "Own Prices",
        xlab = "Weeks", 
        ylab = "Price",
        ylim = c(0,4),
        col = "#92D282",
        border = "black")

# Bar Plot for Qualimo Product,
barplot(df$p_qualimo,
        main = "Qualimo Prices",
        xlab = "Weeks", 
        ylab = "Price",
        ylim = c(0,4),
        col = "steelblue",
        border = "black")

# Bar Plot for Qualis Product, 
barplot(df$p_qualis,
        main = "Qualis Prices",
        xlab = "Weeks", 
        ylab = "Price",
        ylim = c(0,4),
        col = "#E8C170",
        border = "black")

# Bar Plot of Sales against Weeks,
barplot(df$logsales,
        main = "Log Sales Distribution",
        xlab = "Weeks",
        ylab = "Log Sales",
        ylim = c(0, max(df$logsales) + 0.5),
        col = "steelblue",
        border = "black")


# Box Plot Price comparison of the 4 products, delete?
prices_long <- df %>%
  select(p_own, p_qualig, p_qualimo, p_qualis) %>%
  tidyr::pivot_longer(everything(),
                      names_to = "Price_Type",
                      values_to = "Price")

ggplot(prices_long, aes(x = Price_Type, y = Price, fill = Price_Type)) +
  geom_boxplot() +
  labs(title = "Price Comparison Across Sources",
       x = "Price Source",
       y = "Price") +
  theme_minimal() +
  theme(legend.position = "none")


# boxplot of log sales,
boxplot(df$logsales,
        main = "Boxplot of log(sales)",
        ylab = "log(sales)",
        col = "lightblue",
        border = "gray40")


# KDE for log(sales)
plot(density(df$logsales),
     main = "Kernel Density of log(sales)",
     xlab = "log(sales)",
     col = "steelblue",
     lwd = 2)


#boxplot - how promotion affects sales, Delete this one?
ggplot(df, aes(x = factor(display, labels = c("no", "yes")),
               y = logsales, fill = factor(display))) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Effect of promotion (display) on sales",
       x = "Promotion (display)",
       y = "Log of weekly sales") +
  theme(legend.position = "none")

#Corelation Matrix
num_df <- df %>% select(logsales, display, p_own, p_qualig, p_qualimo, p_qualis)
GGally::ggcorr(num_df, label = TRUE, label_round = 2)

#Scatter own price vs log-sales, colored by promotion
ggplot(df, aes(p_own, logsales, color = display)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(title = "Log sales vs own price (colored by promotion)",
       x = "Own price", y = "Log sales", color = "Promotion")



