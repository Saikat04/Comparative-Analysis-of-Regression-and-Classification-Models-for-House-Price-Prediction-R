library(ggplot2)
path <- file.choose()
df <- read.csv(path, header = T, sep = ',')
head(df)
str(df)

# checking all properties
summary(df)

# checking for missing values
colSums(is.na(df)) 

# adding calculated field
df$date <- as.Date(df$date, format = '%Y-%m-%d')
df$year <- as.numeric(format(df$date, '%Y'))
df$month <- as.numeric(format(df$date, '%m'))
# extracting unnecessary column
df <- df[,-1]

# extra step to convert true, false to 0 and 1 for train data
df$has_basement <- as.logical(df$has_basement)
df$renovated <- as.logical(df$renovated)
df$nice_view <- as.logical(df$nice_view)
df$perfect_condition <- as.logical(df$perfect_condition)
df$has_lavatory <- as.logical(df$has_lavatory)
df$single_floor <- as.logical(df$single_floor)

# converting true, false to 0 and 1
df$has_basement <- as.numeric(df$has_basement)
df$renovated <- as.numeric(df$renovated)
df$nice_view <- as.numeric(df$nice_view)
df$perfect_condition <- as.numeric(df$perfect_condition)
df$has_lavatory <- as.numeric(df$has_lavatory)
df$single_floor <- as.numeric(df$single_floor)

# handeling outliers
# handeling price
ggplot(df, aes(y = price)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Boxplot of House Prices", y = "Price") +
  theme_minimal()
ggplot(df, aes(x = price)) +
  geom_histogram(binwidth = 150000, fill = 'blue', color = 'black', alpha = 0.7) +
  labs(title = 'Histogram of House Prices', x = 'Price', y = 'Frequency') +
  theme_minimal()
iqr_price <- IQR(df$price)
llimit_price <- quantile(df$price, 0.25) - 1.5 * iqr_price
ulimit_price <- quantile(df$price, 0.75) + 1.5 * iqr_price
dim(df[df$price > ulimit_price | df$price < llimit_price, ])
df$price <- ifelse(df$price > ulimit_price, ulimit_price, ifelse(df$price < llimit_price, llimit_price, df$price))
names(df)
# handeling area
ggplot(df, aes(y = living_in_m2)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Boxplot of House Prices", y = "Price") +
  theme_minimal()
ggplot(df, aes(x = living_in_m2)) +
  geom_histogram(binwidth = 10, fill = 'blue', color = 'black', alpha = 0.7) +
  labs(title = 'Histogram of House Prices', x = 'Price', y = 'Frequency') +
  theme_minimal()
iqr_area <- IQR(df$living_in_m2)
llimit_area <- quantile(df$living_in_m2, 0.25) - 1.5 * iqr_area
ulimit_area <- quantile(df$living_in_m2, 0.75) + 1.5 * iqr_area
dim(df[df$living_in_m2 > ulimit_area | df$living_in_m2 < llimit_area, ])
df$living_in_m2 <- ifelse(df$living_in_m2 > ulimit_area, ulimit_area, ifelse(df$living_in_m2 < llimit_area, llimit_area, df$living_in_m2))

# updating the file
path = file.choose()
write.csv(df, path, row.names = FALSE)
