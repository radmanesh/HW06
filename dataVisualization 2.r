# Ensure 'revenue' is numeric and replace NA with 0
data$revenue <- as.numeric(data$revenue)
data$revenue[is.na(data$revenue)] <- 0

# Convert 'date' to Date format and 'visitStartTime' to POSIXct
data$date <- as.Date(data$date)
data$visitStartTime <- as.POSIXct(data$visitStartTime, origin = '1970-01-01')

# Ensure 'newVisits' and 'bounces' are numeric
data$newVisits <- as.numeric(data$newVisits)
data$bounces <- as.numeric(data$bounces)
data$pageviews <- as.numeric(data$pageviews)
data$isMobile <- as.numeric(data$isMobile)

# 1. Number of Unique Customers and Transactions
unique_customers <- n_distinct(data$custId)
transactions_per_customer <- data %>% group_by(custId) %>% summarise(transactions = n())

# Plot Histogram of Transactions per Customer
ggplot(transactions_per_customer, aes(x = transactions)) +
  geom_histogram(binwidth = 1, fill = 'blue', alpha = 0.7) +
  ggtitle('Transactions per Customer') +
  xlab('Number of Transactions') +
  ylab('Number of Customers') +
  theme_minimal()

# 2. Average Transactions per Customer
avg_transactions <- mean(transactions_per_customer$transactions)
print(paste("Average Transactions per Customer:", round(avg_transactions, 2)))

# 3. Average Revenue per Transaction per Customer
total_revenue_per_customer <- data %>% group_by(custId) %>% summarise(total_revenue = sum(revenue))
avg_revenue_per_transaction <- total_revenue_per_customer$total_revenue / transactions_per_customer$transactions

# Combine into a data frame
avg_revenue_df <- data.frame(custId = total_revenue_per_customer$custId,
                             avg_revenue_per_transaction = avg_revenue_per_transaction)

# Plot Histogram
ggplot(avg_revenue_df, aes(x = avg_revenue_per_transaction)) +
  geom_histogram(binwidth = 10, fill = 'green', alpha = 0.7) +
  ggtitle('Average Revenue per Transaction') +
  xlab('Revenue') +
  ylab('Number of Customers') +
  theme_minimal()

# 4. Transactions Until First Purchase
transactions_until_purchase <- data %>%
  arrange(custId, visitNumber) %>%
  group_by(custId) %>%
  summarise(transactions_before_purchase = ifelse(any(revenue > 0),
                                                  min(visitNumber[revenue > 0]) - 1,
                                                  max(visitNumber)))

# Plot Histogram
ggplot(transactions_until_purchase, aes(x = transactions_before_purchase)) +
  geom_histogram(binwidth = 1, fill = 'purple', alpha = 0.7) +
  ggtitle('Transactions Until First Purchase') +
  xlab('Number of Transactions') +
  ylab('Number of Customers') +
  theme_minimal()

# 5. Pairwise Correlations
# Select numerical features
num_features <- c("visitNumber", "timeSinceLastVisit", "pageviews", "bounces", "newVisits", "revenue")

# Compute correlations
cor_data <- data[, num_features]
cor_matrix <- cor(cor_data, use = "complete.obs")

# Print the correlation matrix
print(cor_matrix)

# Visualize the correlation matrix
corrplot(cor_matrix, method = "number", tl.cex = 0.8)

# 6. Feature Distributions
# Plot histograms for numerical features
for (feature in num_features) {
  print(
    ggplot(data, aes_string(x = feature)) +
      geom_histogram(bins = 30, fill = 'skyblue', alpha = 0.7) +
      ggtitle(paste('Distribution of', feature)) +
      xlab(feature) +
      ylab('Count') +
      theme_minimal()
  )
}

# Plot bar charts for categorical features
cat_features <- c("channelGrouping", "browser", "operatingSystem", "deviceCategory",
                  "continent", "country", "source", "medium")

for (feature in cat_features) {
  print(
    ggplot(data, aes_string(x = feature)) +
      geom_bar(fill = 'orange', alpha = 0.7) +
      ggtitle(paste('Distribution of', feature)) +
      xlab(feature) +
      ylab('Count') +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme_minimal()
  )
}

# 7. Interaction Plots
# Histogram of pageviews grouped by deviceCategory
ggplot(data, aes(x = pageviews, fill = deviceCategory)) +
  geom_histogram(bins = 30, alpha = 0.7, position = 'dodge') +
  ggtitle('Pageviews by Device Category') +
  xlab('Pageviews') +
  ylab('Count') +
  theme_minimal()

# Box plot of revenue by channelGrouping
ggplot(data, aes(x = channelGrouping, y = revenue)) +
  geom_boxplot() +
  ggtitle('Revenue by Channel Grouping') +
  xlab('Channel Grouping') +
  ylab('Revenue') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal()

# 8. Time Series Analysis
# Total revenue over time (by month)
data$month <- format(data$date, "%Y-%m")
monthly_revenue <- data %>%
  group_by(month) %>%
  summarise(totalRevenue = sum(revenue))

ggplot(monthly_revenue, aes(x = as.Date(paste0(month, "-01")), y = totalRevenue)) +
  geom_line(color = 'blue') +
  ggtitle('Total Revenue Over Time') +
  xlab('Month') +
  ylab('Total Revenue') +
  theme_minimal()

# Total visits over time (by month)
monthly_visits <- data %>%
  group_by(month) %>%
  summarise(totalVisits = n())

ggplot(monthly_visits, aes(x = as.Date(paste0(month, "-01")), y = totalVisits)) +
  geom_line(color = 'red') +
  ggtitle('Total Visits Over Time') +
  xlab('Month') +
  ylab('Total Visits') +
  theme_minimal()

# 9. Feature Engineering
## 9.1 Summing Up Visits Since Last Purchase
data <- data %>%
  arrange(custId, visitStartTime) %>%
  group_by(custId) %>%
  mutate(
    purchaseOccured = ifelse(revenue > 0, 1, 0),
    visitsSinceLastPurchase = {
      visits_since_purchase <- numeric(n())
      last_purchase_idx <- 0
      for (i in seq_along(revenue)) {
        if (revenue[i] > 0) {
          visits_since_purchase[i] <- 0
          last_purchase_idx <- i
        } else {
          visits_since_purchase[i] <- i - last_purchase_idx
        }
      }
      visits_since_purchase
    }
  )

# 9.2 Lagged Variables
data <- data %>%
  group_by(custId) %>%
  arrange(visitStartTime) %>%
  mutate(
    prev_pageviews = lag(pageviews),
    prev_bounces = lag(bounces),
    prev_revenue = lag(revenue),
    prev_visitTime = lag(visitStartTime),
    time_since_last_visit = as.numeric(visitStartTime - prev_visitTime, units = "secs")
  )

# 9.3 Time-Based Features
data <- data %>%
  group_by(custId) %>%
  mutate(
    firstVisitTime = min(visitStartTime),
    time_since_first_visit = as.numeric(visitStartTime - firstVisitTime, units = "secs")
  )

data <- data %>%
  group_by(custId) %>%
  arrange(visitStartTime) %>%
  mutate(
    last_purchase_time = ifelse(revenue > 0, visitStartTime, NA),
    last_purchase_time = na.locf(last_purchase_time, na.rm = FALSE),
    time_since_last_purchase = as.numeric(visitStartTime - last_purchase_time, units = "secs")
  )

# 9.4 Cumulative Metrics
data <- data %>%
  group_by(custId) %>%
  arrange(visitStartTime) %>%
  mutate(
    cumulative_pageviews = cumsum(pageviews),
    cumulative_bounces = cumsum(bounces),
    cumulative_revenue = cumsum(revenue)
  )

# 9.5 Statistical Summaries per Customer
customer_stats <- data %>%
  group_by(custId) %>%
  summarise(
    mean_pageviews = mean(pageviews, na.rm = TRUE),
    median_pageviews = median(pageviews, na.rm = TRUE),
    max_pageviews = max(pageviews, na.rm = TRUE),
    min_pageviews = min(pageviews, na.rm = TRUE),
    sd_pageviews = sd(pageviews, na.rm = TRUE),

    mean_bounces = mean(bounces, na.rm = TRUE),
    median_bounces = median(bounces, na.rm = TRUE),
    max_bounces = max(bounces, na.rm = TRUE),
    min_bounces = min(bounces, na.rm = TRUE),
    sd_bounces = sd(bounces, na.rm = TRUE),

    mean_revenue = mean(revenue, na.rm = TRUE),
    median_revenue = median(revenue, na.rm = TRUE),
    max_revenue = max(revenue, na.rm = TRUE),
    min_revenue = min(revenue, na.rm = TRUE),
    sd_revenue = sd(revenue, na.rm = TRUE)
  )

# 9.6 Event Counts Over Time
event_counts <- data %>%
  group_by(custId) %>%
  summarise(
    total_bounces = sum(bounces, na.rm = TRUE),
    total_mobile_sessions = sum(isMobile, na.rm = TRUE)
  )

# 9.7 Trend Calculation
# Trend of pageviews over time for each customer
trend_features <- data %>%
  group_by(custId) %>%
  filter(n() >= 2) %>%
  do(
    pageviews_trend = {
      model <- lm(pageviews ~ visitNumber, data = .)
      tidy(model)$estimate[2]  # Slope
    }
  ) %>%
  ungroup() %>%
  mutate(pageviews_trend = unlist(pageviews_trend))

# 9.8 Recency, Frequency, Monetary (RFM) Analysis
# Recency: Time since last visit
latest_visit <- data %>%
  group_by(custId) %>%
  summarise(
    last_visit_time = max(visitStartTime)
  )

latest_visit <- latest_visit %>%
  mutate(
    recency_days = as.numeric(Sys.time() - last_visit_time, units = "days")
  )

# Frequency: Total number of visits
frequency <- transactions_per_customer %>%
  rename(frequency = transactions)

# Monetary: Total revenue
monetary <- total_revenue_per_customer %>%
  rename(monetary = total_revenue)

# Combine RFM
rfm <- latest_visit %>%
  select(custId, recency_days) %>%
  left_join(frequency, by = "custId") %>%
  left_join(monetary, by = "custId")

# 9.9 Combine All Features into Final Dataset
customer_features <- data %>%
  group_by(custId) %>%
  summarise(
    total_visits = n(),
    total_revenue = sum(revenue),
    avg_pageviews = mean(pageviews, na.rm = TRUE),
    avg_bounces = mean(bounces, na.rm = TRUE),
    is_new_customer = min(newVisits, na.rm = TRUE),
    visits_since_last_purchase = max(visitsSinceLastPurchase, na.rm = TRUE)
  ) %>%
  left_join(customer_stats, by = "custId") %>%
  left_join(event_counts, by = "custId") %>%
  left_join(latest_visit[, c("custId", "recency_days")], by = "custId") %>%
  left_join(trend_features, by = "custId") %>%
  left_join(rfm, by = "custId")

# View the final customer features
head(customer_features)

# 10. Visualization of Engineered Features
# Histogram of Recency Days
ggplot(customer_features, aes(x = recency_days)) +
  geom_histogram(bins = 30, fill = 'darkgreen', alpha = 0.7) +
  ggtitle('Recency (Days Since Last Visit)') +
  xlab('Days') +
  ylab('Number of Customers') +
  theme_minimal()

# Histogram of Frequency (Total Visits)
ggplot(customer_features, aes(x = frequency)) +
  geom_histogram(binwidth = 1, fill = 'steelblue', alpha = 0.7) +
  ggtitle('Frequency (Total Visits)') +
  xlab('Number of Visits') +
  ylab('Number of Customers') +
  theme_minimal()

# Histogram of Monetary Value (Total Revenue)
ggplot(customer_features, aes(x = monetary)) +
  geom_histogram(binwidth = 50, fill = 'gold', alpha = 0.7) +
  ggtitle('Monetary Value (Total Revenue)') +
  xlab('Revenue') +
  ylab('Number of Customers') +
  theme_minimal()

# Scatter plot of Pageviews Trend vs. Total Revenue
ggplot(customer_features, aes(x = pageviews_trend, y = total_revenue)) +
  geom_point(alpha = 0.5) +
  ggtitle('Pageviews Trend vs. Total Revenue') +
  xlab('Pageviews Trend (Slope)') +
  ylab('Total Revenue') +
  theme_minimal()

# Save the final dataset for modeling
write.csv(customer_features, 'customer_features.csv', row.names = FALSE)
