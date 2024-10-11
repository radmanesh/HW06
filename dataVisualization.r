# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(VIM)
library(outliers)

# Load the dataset
data <- read.csv("data/Train.csv/Train.csv")

# Visualize missing values
aggr(data, col=c('navyblue','red'), numbers=T, sortVars=T,
     # Added later
     prop=T, combined=T, bars=T,
     labels=names(data), cex.axis=.7, gap=3, ylab=c("Missing data","Pattern"))

# Summarize missing values
missing_summary <- sapply(data, function(x) sum(is.na(x)))
print(missing_summary)

# Detect outliers using boxplots
numeric_columns <- sapply(data, is.numeric)
data_numeric <- data[, numeric_columns]

# Boxplot for each numeric column
for (col in names(data_numeric)) {
  ggplot(data, aes_string(x = col)) +
    geom_boxplot() +
    ggtitle(paste("Boxplot of", col)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}

# Detect outliers using statistical methods
outlier_summary <- sapply(data_numeric, function(x) {
  if (length(unique(x)) > 1) {
    return(outlier(x))
  } else {
    return(NA)
  }
})
print(outlier_summary)
