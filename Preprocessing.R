setwd("C:/Users/User/Desktop/University/Machine Learning/Assignment")
dataset = read.csv("metaverse_transactions_dataset.csv", colClasses = c(sending_address = "character", receiving_address = "character"))

# Libraries

library(dplyr)

#Check if all addresses are on the ETH chain
valid_receiving_address <- subset(dataset, nchar(receiving_address) == 42 & substr(receiving_address, 1, 2) == "0x")
valid_sending_address <- subset(dataset, nchar(sending_address) == 42 & substr(sending_address, 1, 2) == "0x")

#Create New column for setting weekend/weekday
dataset$date_column <- sub("^(\\d+/\\d+/\\d+).*", "\\1", dataset$timestamp)
dataset$date_column <- as.Date(dataset$date_column, format = "%d/%m/%Y")

weekday_name <- weekdays(dataset$date_column)
dataset$weekend_or_weekday <- ifelse(weekday_name %in% c("Saturday", "Sunday"), "Weekend", "Weekday")

dataset <- dataset[, !names(dataset) %in% "date_column"]

# Normalized the session duration

dataset$session_duration_normalized <- scale(dataset$session_duration, center = min(dataset$session_duration), scale = max(dataset$session_duration) - min(dataset$session_duration))
dataset$session_duration_normalized <- round(dataset$session_duration_normalized, 2)
dataset <- dataset[, !names(dataset) %in% "session_duration"]

# Bin Time of Day

categorize_hour <- function(hour) {
  if (hour >= 6 && hour < 12) {
    return("Morning")
  } else if (hour >= 12 && hour < 18) {
    return("Afternoon")
  } else if (hour >= 18 && hour < 24) {
    return("Evening")
  } else {
    return("Late Night")
  }
}

dataset$time_of_day <- sapply(dataset$hour_of_day, categorize_hour)

dataset <- dataset[, !names(dataset) %in% "hour_of_day"]

# Bin Phishing and scam

categorize <- function(transaction) {
  if (transaction %in% c("phishing", "scam")) {
    return("scam")
  } else {
    return(transaction)
  }
}
 
dataset <- dataset %>%
  mutate(new_transactions = sapply(transaction_type, categorize))

# Bin IP Addresses

# Function to categorize based on network size
categorize_network_size <- function(ip) {
  # Remove leading zeros and convert to character
  ip <- as.character(as.numeric(ip))
  prefix_length = nchar(ip)
  if (length(prefix_length) == 3) {
    return("Large")  # /8 network (10.0.0.0/8)
  } else if (ip == "192") {
    return("Medium")  # /16 network (192.0.0.0/16)
  } else if (ip =="192.168") {
    return("Small")  # /16 network (192.168.0.0/16)
  } else if (ip == "172.16") {
    return("Medium")  # /12 network (172.16.0.0/12)
  } else if (ip == "10"||ip =="172") {
    return("Large")  # /8 network (10.0.0.0/8)
  } else {
    return("Other")
  }
}



dataset <- dataset %>% mutate("Network Size" = sapply(ip_prefix, categorize_network_size))

# View the result
print(df)


dataset <- subset(dataset, select = -c(timestamp,ip_prefix,transaction_type))

tabler = data.frame(table(dataset$receiving_address,dataset$new_transactions))
subset_df <- subset(tabler, Var2 == "scam")
subset_df <- subset(tabler, select = -Var2)
merged_dataset <- merge(dataset, subset_df, by.x = "receiving_address", by.y = "Var1", all.x = TRUE)

desired_order <- c(1,2,4,5,6,7,10,11,12,13,14,15,3,8,9)
reordered_dataset <- merged_dataset[, desired_order]
names(merged_dataset)[names(merged_dataset) == "Freq"] <- "Total Scam Occurrence"
  

write.csv(reordered_dataset,"processed metaverse dataset.csv",row.names = FALSE)
