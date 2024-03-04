#Read in Training Data and modify to Daily
Faircomdat <- read.csv("FaircomSAMTotal.csv")
View(Faircomdat)
Faircomdat <- Faircomdat[,-c(3, 7:11)]
# Create a new dataframe to store the aggregated values
daily_Fair_TMY <- data.frame(matrix(nrow = 365, ncol = 4))
# Generate the sequence of dates
dates_Fair <- seq(as.Date("2022-01-01"), by = "day", length.out = 365)
#Daily Average Predictor variables
for (i in seq(1, nrow(Faircomdat), 24)) {
  # Calculate the sum of each column over the 24 rows, excluding the first column
  row <- colSums(Faircomdat[i:(i+23), -1])
  row[-1] <- row[-1] / 24
  # Append the aggregated row to the new dataframe
  daily_Fair_TMY[(i-1)/24 + 1, ] <- row
}
colnames(daily_Fair_TMY) <- c("kWh", "Temp", "GHI", "WS")
# Add the Date variable to the new dataframe
daily_Fair_TMY$Date <- dates_Fair
daily_Fair_TMY <- daily_Fair_TMY[, c("Date", names(daily_Fair_TMY)[-ncol(daily_Fair_TMY)])]
View(daily_Fair_TMY)






#Daily lms
dailyFair_model1 <- lm(kWh ~ GHI + Temp + WS, data = daily_Fair_TMY)
dailyFair_model2 <- lm(kWh ~ GHI + Temp:GHI, data = daily_Fair_TMY)
dailyFair_model3 <- lm(kWh ~ GHI, data = daily_Fair_TMY)
summary(dailyFair_model1)
summary(dailyFair_model2)
summary(dailyFair_model3)
#Save Models
write.csv(dailyFair_model1$coefficients, "DailyFairModel1Coefficients.csv")
write.csv(dailyFair_model2$coefficients, "DailyFairModel2Coefficients.csv")
write.csv(dailyFair_model3$coefficients, "DailyFairModel3Coefficients.csv")








#Read in Daily Prediction Data
dailypredFair <- read.csv("OikoFaircom.csv")
View(dailypredFair)
colnames(dailypredFair)
colnames(dailypredFair)[colnames(dailypredFair) == "temperature..degC."] <- "Temp"
colnames(dailypredFair)[colnames(dailypredFair) == "wind_speed..m.s."] <- "WS"
colnames(dailypredFair)[colnames(dailypredFair) == "surface_solar_radiation..W.m.2."] <- "GHI"
colnames(dailypredFair)[colnames(dailypredFair) == "direct_normal_solar_radiation..W.m.2."] <- "DNI"
colnames(dailypredFair)[colnames(dailypredFair) == "surface_diffuse_solar_radiation..W.m.2."] <- "DHI"
#Predict kWh with models
dailypredFair$predictedkWh1 <- predict(dailyFair_model1, newdata = dailypredFair)
dailypredFair$predictedkWh2 <- predict(dailyFair_model2, newdata = dailypredFair)
dailypredFair$predictedkWh3 <- predict(dailyFair_model3, newdata = dailypredFair)











#Read in Test data21
testdat_Fair <- read.csv("FaircomCPS21.csv")
View(testdat_Fair)
#Create Daily Sums
index_Fair <- rep(1:ceiling(nrow(testdat_Fair)), each = 48, length.out = nrow(testdat_Fair))
# Use the aggregate function to calculate the mean of the 'Value' column based on the index
avg_dataset_Fair <- aggregate(activePower ~ index_Fair, data = testdat_Fair, FUN = sum)
# Rename the 'index' column to something more meaningful
names(avg_dataset_Fair)[1] <- "Date"
# Print the first few rows of the new dataset
daily_test_Fair <- avg_dataset_Fair
View(daily_test_Fair)
dates_test_Fair <- seq(as.Date("2021-03-08"), by = "day", length.out = 299)
daily_test_Fair$Date <- dates_test_Fair
daily_test_Fair$activePower <- daily_test_Fair$activePower/2


#Read in Test Data22
testdat_Fair2 <- read.csv("Faircom Jan 2022 - Jan 2023 (1).csv")
View(testdat_Fair2)
colnames(testdat_Fair2)[colnames(testdat_Fair2) == "eToday.kWh."] <- "activePower"
testdat_Fair2$Date <- as.Date(testdat_Fair2$Date, format = "%m/%d/%y")



#Read in Test data23
testdat_Fair3 <- read.csv("FaircomCPS23.csv")
View(testdat_Fair3)
#Create Daily Sums
index_Fair3 <- rep(1:ceiling(nrow(testdat_Fair3)), each = 48, length.out = nrow(testdat_Fair3))
# Use the aggregate function to calculate the mean of the 'Value' column based on the index
avg_dataset_Fair3 <- aggregate(activePower ~ index_Fair3, data = testdat_Fair3, FUN = sum)
# Rename the 'index' column to something more meaningful
names(avg_dataset_Fair3)[1] <- "Date"
# Print the first few rows of the new dataset
daily_test_Fair3 <- avg_dataset_Fair3
View(daily_test_Fair3)
dates_test_Fair3 <- seq(as.Date("2023-01-01"), by = "day", length.out = 151)
daily_test_Fair3$Date <- dates_test_Fair3
daily_test_Fair3$activePower <- daily_test_Fair3$activePower/2



#Combine all 3 into 1
dailyFair_test <- rbind(daily_test_Fair, testdat_Fair2, daily_test_Fair3)
View(dailyFair_test)
dailyFair_test <- cbind(dailyFair_test, dailypredFair$predictedkWh1, dailypredFair$predictedkWh2, dailypredFair$predictedkWh3)
colnames(dailyFair_test)
write.csv(dailyFair_test, "Faircom Measured vs Expected.csv")




#Monthly Plot Expected vs Measured
library(ggplot2)
ggplot(dailyFair_test, aes(x = as.POSIXct(Date), group = 1)) +
  geom_line(aes(y = activePower, color = "Total.system")) +
  geom_line(aes(y = `dailypredFair$predictedkWh2`, color = "predictedkWh2")) +
  labs(x = "Date and Time", y = "Value", title = "Faircom Monthly Expected vs Measured (kWh)") +
  scale_color_manual(values = c("Total.system" = "blue", "predictedkWh2" = "red")) +
  theme_bw()





library(dplyr)
library(lubridate)
#Create new Monthly dataset with ERs
# Convert the date column to a date type
# Create a new dataset with summed values by month
newFair_dataset <- dailyFair_test %>%
  mutate(month = floor_date(Date, "month")) %>%
  group_by(month) %>%
  summarise(sum_TotalOutput = sum(activePower),
            sum_kWhpred1 = sum(`dailypredFair$predictedkWh1`),
            sum_kWhpred2 = sum(`dailypredFair$predictedkWh2`),
            sum_kWhpred3 = sum(`dailypredFair$predictedkWh3`))
View(newFair_dataset)
newFair_dataset$ER1 <- newFair_dataset$sum_TotalOutput/newFair_dataset$sum_kWhpred1
newFair_dataset$ER2 <- newFair_dataset$sum_TotalOutput/newFair_dataset$sum_kWhpred2
newFair_dataset$ER3 <- newFair_dataset$sum_TotalOutput/newFair_dataset$sum_kWhpred3
summary(newFair_dataset$sum_kWhpred2)
summary(newFair_dataset$ER2)
summary(newFair_dataset$sum_TotalOutput)
cor(newFair_dataset$sum_TotalOutput, newFair_dataset$sum_kWhpred2)
rmse(newFair_dataset$sum_TotalOutput, newFair_dataset$sum_kWhpred2)
plot(newFair_dataset$sum_kWhpred2, newFair_dataset$sum_TotalOutput)
write.csv(newFair_dataset, "Faircom Monthly Sums and Energy Ratios.csv")
