rr_demand <- read.csv("Round Rock Oiko Demand.csv")
# Assuming your dataset is called 'data' and the date variable is named 'Date'
# and the HDD variable is named 'HDD' and the CDD variable is named 'CDD'
View(rr_demand)
colnames(rr_demand)[colnames(rr_demand) == "datetime..UTC."] <- "Date"
colnames(rr_demand)[colnames(rr_demand) == "heating_degreeday..degC.Day."] <- "HDD"
colnames(rr_demand)[colnames(rr_demand) == "cooling_degreeday..degC.Day."] <- "CDD"
rr_demand$Date <- as.Date(rr_demand$Date, format = "%m/%d/%y")
sum(rr_demand$CDD[185:213])
# Assuming your data frame is called 'rr_demand' with a 'Date' variable
# and you want to sum the HDD and CDD variables for each date range

# Create a vector of start and end dates for the ranges
ranges <- c("1/08/19 - 2/07/19", "2/08/19 - 3/09/19", "3/10/19 - 4/10/19", "4/10/19 - 5/09/19",
            "5/10/19 - 6/10/19", "6/11/19 - 7/10/19", "7/11/19 - 8/08/19", "8/09/19 - 9/10/19",
            "9/11/19 - 10/10/19", "10/11/19 - 11/10/19", "11/11/19 - 12/10/19", "12/11/19 - 1/10/20",
            "1/11/20 - 2/10/20", "2/11/20 - 3/10/20", "3/11/20 - 4/08/20", "4/09/20 - 5/10/20",
            "5/11/20 - 6/09/20", "6/10/20 - 7/12/20", "7/13/20 - 8/10/20", "8/11/20 - 9/09/20",
            "9/10/20 - 10/08/20", "10/09/20 - 11/09/20", "11/10/20 - 12/08/20", "12/09/20 - 1/08/21")

# Initialize empty vectors to store the sum of HDD and CDD for each range
sum_HDD <- numeric(length(ranges))
sum_CDD <- numeric(length(ranges))

# Loop through each range
for (i in 1:length(ranges)) {
  # Extract the start and end dates from the range string
  date_range <- unlist(strsplit(as.character(ranges[i]), " - "))
  start_date <- as.Date(date_range[1], format = "%m/%d/%y")
  end_date <- as.Date(date_range[2], format = "%m/%d/%y")
  
  # Subset the data within the current range
  subset_data <- rr_demand[rr_demand$Date >= start_date & rr_demand$Date <= end_date, ]
  
  # Sum the HDD and CDD variables within the current range
  sum_HDD[i] <- sum(subset_data$HDD)
  sum_CDD[i] <- sum(subset_data$CDD)
}

# Create a new data frame with Date, sum_HDD, and sum_CDD variables
new_data <- data.frame(Date = ranges, sum_HDD, sum_CDD)

# Print the new data frame
print(new_data)


demand_datrr <- read.csv("Round Rock Utility Data - Utility Data .csv")
combined_rr <- cbind(demand_datrr,new_data)
View(combined_rr)
subset_combined <- combined_rr[1:12,]
subset_combined2 <- combined_rr[13:24,]
# Remove non-numeric characters
combined_rr$Billed.Usage..kWh. <- as.numeric(gsub("[^0-9.]", "", combined_rr$Billed.Usage..kWh.))


rr_modelu <- lm(Billed.Usage..kWh. ~ sum_HDD + sum_CDD, data = subset_combined)
rr_modeld <- lm(subset_combined2$Billed.Demand..kW. ~ sum_HDD + sum_CDD, data = subset_combined2)
summary(rr_modeld)
summary(rr_modelu)




View(rr_demand)
rr_pred <- rr_demand[725:1635,c(1,7,8)]
View(rr_pred)
# Assuming your data frame is called 'rr_demand' with a 'Date' variable (stored as character)
# and you want to sum the HDD and CDD variables for each date range



# Create a vector of start and end dates for the ranges
ranges2 <- c("07/13/2021 - 08/11/2021", "08/11/2021 - 09/11/2021", "09/11/2021 - 10/11/2021", "10/11/2021 - 11/09/2021",
            "11/09/2021 - 12/10/2021", "12/10/2021 - 12/21/2021", "12/21/2021 - 01/10/2022", "01/10/2022 - 02/09/2022",
            "02/09/2022 - 03/11/2022", "03/11/2022 - 04/11/2022", "04/11/2022 - 05/11/2022", "05/11/2022 - 06/10/2022",
            "06/10/2022 - 07/12/2022", "07/12/2022 - 08/11/2022", "08/11/2022 - 09/12/2022", "09/12/2022 - 10/11/2022",
            "10/11/2022 - 11/09/2022", "11/09/2022 - 12/09/2022", "12/09/2022 - 01/10/2023", "01/10/2023 - 02/09/2023",
            "02/09/2023 - 03/13/2023", "03/13/2023 - 04/12/2023", "04/12/2023 - 05/11/2023", "05/11/2023 - 06/12/2023")

# Initialize empty vectors to store the sum of HDD and CDD for each range
sum_HDD <- numeric(length(ranges2))
sum_CDD <- numeric(length(ranges2))

# Loop through each range
for (i in 1:length(ranges2)) {
  # Extract the start and end dates from the range string
  date_range <- unlist(strsplit(ranges2[i], " - "))
  start_date <- as.Date(date_range[1], format = "%m/%d/%Y")
  end_date <- as.Date(date_range[2], format = "%m/%d/%Y")
  
  # Subset the data within the current range
  subset_data2 <- rr_pred[rr_pred$Date >= start_date & rr_pred$Date <= end_date, ]
  
  # Sum the HDD and CDD variables within the current range
  sum_HDD[i] <- sum(subset_data2$HDD)
  sum_CDD[i] <- sum(subset_data2$CDD)
}

# Create a new data frame with Date, sum_HDD, and sum_CDD variables
new_data2 <- data.frame(Date = ranges2, sum_HDD, sum_CDD)

# Print the new data frame
View(new_data2)


new_data2$pred_usage <- predict(rr_modelu, newdata = new_data2)
new_data2$pred_demand <- predict(rr_modeld, newdata = new_data2)



#Test data
test_rr <- read.csv("Measured Usage Demand.csv")
View(compare_rr)
compare_rr <- cbind(new_data2[,-c(2,3)],test_rr[,c(1:4)])
compare_rr$UsageDifference <- compare_rr$pred_usage-compare_rr$Actual.kWh
compare_rr$DemandDifference <- compare_rr$pred_demand-compare_rr$Metered.KW




solar_rr <- read.csv("Dogwood_Animal_Hospital_Dec_2021_to_May_2023.csv")
solar_rr <- solar_rr[,c(1,6)]
View(solar_rr)
solar_june <- read.csv("RR June 23.csv")
solar_june <- solar_june[c(1:12),c(1,2)]
View(solar_june)
solar_rr <- rbind(solar_rr,solar_june)


solar_rr$Date.and.time <- as.Date(solar_rr$Date.and.time, format = "%d.%m.%Y")
solar_dates <- solar_rr[c(172:539),]
View(solar_dates)
nrow(solar_dates)
nrow(sum_per_day)
excess_daily <- cbind(solar_dates,sum_per_day)
# Create a vector of date ranges
date_ranges <- c("12/21/2021 - 01/10/2022", "01/10/2022 - 02/09/2022", "02/09/2022 - 03/11/2022", "03/11/2022 - 04/11/2022",
                 "04/11/2022 - 05/11/2022", "05/11/2022 - 06/10/2022", "06/10/2022 - 07/12/2022", "07/12/2022 - 08/11/2022",
                 "08/11/2022 - 09/12/2022", "09/12/2022 - 10/11/2022", "10/11/2022 - 11/09/2022", "11/09/2022 - 12/09/2022",
                 "12/09/2022 - 01/10/2023", "01/10/2023 - 02/09/2023", "02/09/2023 - 03/13/2023", "03/13/2023 - 04/12/2023",
                 "04/12/2023 - 05/11/2023", "05/11/2023 - 06/12/2023")

# Function to sum Total.system within each date range
sum_total_system <- function(start_date, end_date) {
  solar_rr %>%
    filter(Date.and.time >= as.Date(start_date, format = "%m/%d/%Y") &
             Date.and.time <= as.Date(end_date, format = "%m/%d/%Y")) %>%
    summarise(sum_total_system = sum(Total.system))
}

# Apply the function to each date range and store the results in a data frame
result <- sapply(date_ranges, function(date_range) {
  range <- strsplit(date_range, " - ")[[1]]
  sum_total_system(range[1], range[2])
}, simplify = "data.frame")

# Add the date ranges as a column in the result data frame
monthly_solar <- cbind(Date.Range = date_ranges, result)

# Print the result
View(monthly_solar)














excess_rr <- read.csv("IntervalData.csv")
View(excess_rr)
# Load required library if not already loaded
if (!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
}

# Filter dataset based on condition
filtered_excess_rr <- excess_rr %>% filter(CONSUMPTION_SURPLUSGENERATION == "Surplus Generation")
View(filtered_excess_rr)
filtered_excess_rr <- filtered_excess_rr[c(865:36196),c(2,6)]




# Load required library if not already loaded
if (!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
}

# Convert USAGE_DATE to a date format
filtered_excess_rr$USAGE_DATE <- as.Date(filtered_excess_rr$USAGE_DATE, format = "%d.%m.%Y")

# Sum USAGE_KWH for each day
sum_per_day <- filtered_excess_rr %>%
  group_by(USAGE_DATE) %>%
  summarise(sum_USAGE_KWH = sum(USAGE_KWH, na.rm = TRUE))

# Print the result
View(sum_per_day)


# Create a vector of date ranges
date_ranges <- c("06/10/2022", "06/10/2022 - 07/12/2022", "07/12/2022 - 08/11/2022",
                 "08/11/2022 - 09/12/2022", "09/12/2022 - 10/11/2022", "10/11/2022 - 11/09/2022", "11/09/2022 - 12/09/2022",
                 "12/09/2022 - 01/10/2023", "01/10/2023 - 02/09/2023", "02/09/2023 - 03/13/2023", "03/13/2023 - 04/12/2023",
                 "04/12/2023 - 05/11/2023", "05/11/2023 - 06/12/2023")

View(excess_daily)
excess_daily <- excess_daily[,c(1,4)]
summary(excess_daily$sum_USAGE_KWH)
sum_total_system <- function(start_date, end_date) {
  excess_daily %>%
    filter(Date.and.time >= as.Date(start_date, format = "%m/%d/%Y") &
             Date.and.time <= as.Date(end_date, format = "%m/%d/%Y")) %>%
    summarise(sum_total_system = sum(sum_USAGE_KWH))
}

# Apply the function to each date range and store the results in a data frame
result2 <- sapply(date_ranges, function(date_range) {
  range <- strsplit(date_range, " - ")[[1]]
  sum_total_system(range[1], range[2])
}, simplify = "data.frame")

# Add the date ranges as a column in the result data frame
monthly_excess <- cbind(Date.Range = date_ranges, result2)

# Print the result
monthly_excess <- monthly_excess[-1,]
View(monthly_excess)
View(monthly_solar)
tweaked_ms <- monthly_solar[-c(1:6),]
excess_test <- cbind(tweaked_ms,monthly_excess)
View(excess_test)
excess_test <- excess_test[,-1]
excess_test$Difference <- excess_test$result - excess_test$result2
write.csv(excess_test, "Solar and Excess.csv")
















write.csv(compare_rr, "Measured vs Expected Usage and Demand RR.csv")


