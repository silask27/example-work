#Read in Training Data and modify to Daily
Baldwindat <- read.csv("BaldwinSAM.csv")
View(Baldwindat)
# Create a new dataframe to store the aggregated values
daily_Bald_TMY <- data.frame(matrix(nrow = 365, ncol = 4))
# Generate the sequence of dates
dates_Bald <- seq(as.Date("2022-01-01"), by = "day", length.out = 365)
#Daily Average Predictor variables
for (i in seq(1, nrow(Baldwindat), 24)) {
  # Calculate the sum of each column over the 24 rows, excluding the first column
  row <- colSums(Baldwindat[i:(i+23), -1])
  row[-1] <- row[-1] / 24
  # Append the aggregated row to the new dataframe
  daily_Bald_TMY[(i-1)/24 + 1, ] <- row
}
colnames(daily_Bald_TMY) <- c("kWh", "Temp", "GHI", "WS")
# Add the Date variable to the new dataframe
daily_Bald_TMY$Date <- dates_Bald
daily_Bald_TMY <- daily_Bald_TMY[, c("Date", names(daily_Bald_TMY)[-ncol(daily_Bald_TMY)])]
View(daily_Bald_TMY)






#Daily lms
dailyBald_model1 <- lm(kWh ~ GHI + Temp + WS, data = daily_Bald_TMY)
dailyBald_model2 <- lm(kWh ~ GHI + Temp:GHI, data = daily_Bald_TMY)
dailyBald_model3 <- lm(kWh ~ GHI, data = daily_Bald_TMY)
summary(dailyBald_model1)
summary(dailyBald_model2)
summary(dailyBald_model3)
#Save Models
write.csv(dailyBald_model1$coefficients, "DailyBaldModel1Coefficients.csv")
write.csv(dailyBald_model2$coefficients, "DailyBaldModel2Coefficients.csv")
write.csv(dailyBald_model3$coefficients, "DailyBaldModel3Coefficients.csv")






#Read in Oiko pred data
dailypredBald <- read.csv("OikoBaldwin.csv")
View(dailypredBald)
colnames(dailypredBald)
colnames(dailypredBald)[colnames(dailypredBald) == "temperature..degC."] <- "Temp"
colnames(dailypredBald)[colnames(dailypredBald) == "wind_speed..m.s."] <- "WS"
colnames(dailypredBald)[colnames(dailypredBald) == "surface_solar_radiation..W.m.2."] <- "GHI"
#Predict kWh with models
dailypredBald$predictedkWh1 <- predict(dailyBald_model1, newdata = dailypredBald)
dailypredBald$predictedkWh2 <- predict(dailyBald_model2, newdata = dailypredBald)
dailypredBald$predictedkWh3 <- predict(dailyBald_model3, newdata = dailypredBald)
#Tweaks
dailypredBald <- dailypredBald[147:1392,]



#Read in Measured pred data
dailypredBald2 <- read.csv("Baldwin Measured.csv")
View(dailypredBald2)
dailypredBald2 <- dailypredBald2[,c(1,3,4,6,7)]
# Count the number of NAs in the dataset
# Count NAs by variable in the dataset
na_count <- colSums(is.na(dailypredBald2))

# Print the NA counts by variable
print(na_count)
# Identify rows with NAs in the dataset
rows_with_na <- !complete.cases(dailypredBald2)

# Print the indices of rows with NAs
print(which(rows_with_na))

daily_Bald_Mea <- data.frame(matrix(nrow = 1247, ncol = 4))
# Generate the sequence of dates
dates_Bald_Mea <- seq(as.Date("2020-01-01"), by = "day", length.out = 1247)
#Daily Average Predictor variables
for (i in seq(1, nrow(dailypredBald2), 24)) {
  # Calculate the sum of each column over the 24 rows, excluding the first column
  row <- colSums(dailypredBald2[i:(i+23), -1])
  row[-1] <- row[-1] / 24
  # Append the aggregated row to the new dataframe
  daily_Bald_Mea[(i-1)/24 + 1, ] <- row
}
colnames(daily_Bald_Mea) <- c("kWh", "GHI", "Temp", "WS")
daily_Bald_Mea$Date <- dates_Bald_Mea
daily_Bald_Mea <- daily_Bald_Mea[, c("Date", names(daily_Bald_Mea)[-ncol(daily_Bald_Mea)])]
View(daily_Bald_Mea)
#Tweaks
daily_Bald_Mea <- daily_Bald_Mea[-1247,]
daily_Bald_Mea$Temp <- (daily_Bald_Mea$Temp - 32) * 5/9
daily_Bald_Mea$Mea_predictedkWh1 <- predict(dailyBald_model1, newdata = daily_Bald_Mea)
daily_Bald_Mea$Mea_predictedkWh2 <- predict(dailyBald_model2, newdata = daily_Bald_Mea)
daily_Bald_Mea$Mea_predictedkWh3 <- predict(dailyBald_model3, newdata = daily_Bald_Mea)




#Comparing OIKO vs Measured datasets
# Load necessary libraries
library(ggplot2)
library(dplyr)

summary(daily_Bald_Mea$GHI)
summary(dailypredBald$GHI)
summary(daily_Bald_Mea$Temp)
summary(dailypredBald$Temp)
summary(daily_Bald_Mea$WS)
summary(dailypredBald$WS)
# Visualize the distributions of the variables
ggplot() +
  geom_density(data = dailypredBald, aes(x = GHI, fill = "dailypredBald"), alpha = 0.5) +
  geom_density(data = daily_Bald_Mea, aes(x = GHI, fill = "daily_Bald_Mea"), alpha = 0.5) +
  labs(title = "Distribution of GHI across datasets", x = "GHI", y = "Density") +
  scale_fill_manual(values = c("dailypredBald" = "blue", "daily_Bald_Mea" = "red")) +
  theme_minimal()

ggplot() +
  geom_density(data = dailypredBald, aes(x = Temp, fill = "dailypredBald"), alpha = 0.5) +
  geom_density(data = daily_Bald_Mea, aes(x = Temp, fill = "daily_Bald_Mea"), alpha = 0.5) +
  labs(title = "Distribution of Temperature across datasets", x = "Temp", y = "Density") +
  scale_fill_manual(values = c("dailypredBald" = "blue", "daily_Bald_Mea" = "red")) +
  theme_minimal()

plot(dailypredBald$GHI, daily_Bald_Mea$GHI)
# Perform a t-test to compare the means of the variables
ttest_result <- t.test(dailypredBald$GHI, daily_Bald_Mea$GHI)
ttest_result2 <- t.test(dailypredBald$Temp, daily_Bald_Mea$Temp)
ttest_result3 <- t.test(dailypredBald$WS, daily_Bald_Mea$WS)
# Display the t-test results
print(ttest_result)
print(ttest_result2)
print(ttest_result3)

















#Combine Datasets to test
daily_Bald_test <- cbind(daily_Bald_Mea, dailypredBald)
View(daily_Bald_test)
daily_Bald_test <- daily_Bald_test[,c(1,2,6,7,8,17,18,19)]
colnames(daily_Bald_test)
plot(daily_Bald_test$Mea_predictedkWh2, daily_Bald_test$kWh)
cor(daily_Bald_test$kWh, daily_Bald_test$Mea_predictedkWh2)
cor(daily_Bald_test$kWh, daily_Bald_test$predictedkWh2)
cor(daily_Bald_test$kWh, daily_Bald_test$Mea_predictedkWh2)
plot(daily_Bald_test$predictedkWh2, daily_Bald_test$kWh)
plot(daily_Bald_test$Mea_predictedkWh2, daily_Bald_test$predictedkWh2)
write.csv(daily_Bald_test, "Baldwin Oiko and Measured Daily Predicted vs Observed.csv")
newBald_dataset <- daily_Bald_test %>%
  mutate(month = floor_date(Date, "month")) %>%
  group_by(month) %>%
  summarise(sum_TotalOutput = sum(kWh),
            sum_MeakWhpred1 = sum(Mea_predictedkWh1),
            sum_MeakWhpred2 = sum(Mea_predictedkWh2),
            sum_MeakWhpred3 = sum(Mea_predictedkWh3),
            sum_kWhpred1 = sum(predictedkWh1),
            sum_kWhpred2 = sum(predictedkWh2),
            sum_kWhpred3 = sum(predictedkWh3))
View(newBald_dataset)
newBald_dataset$ER_Mea1 <- newBald_dataset$sum_TotalOutput/newBald_dataset$sum_MeakWhpred1
newBald_dataset$ER_Mea2 <- newBald_dataset$sum_TotalOutput/newBald_dataset$sum_MeakWhpred2
newBald_dataset$ER_Mea3 <- newBald_dataset$sum_TotalOutput/newBald_dataset$sum_MeakWhpred3
newBald_dataset$ER1 <- newBald_dataset$sum_TotalOutput/newBald_dataset$sum_kWhpred1
newBald_dataset$ER2 <- newBald_dataset$sum_TotalOutput/newBald_dataset$sum_kWhpred2
newBald_dataset$ER3 <- newBald_dataset$sum_TotalOutput/newBald_dataset$sum_kWhpred3
newBald_dataset2 <- newBald_dataset[-c(17,20,29),]
View(newBald_dataset2)
#Comparisons of predicted vs Measured
mean(newBald_dataset2$ER2)
mean(newBald_dataset2$ER_Mea2)
cor(newBald_dataset2$sum_TotalOutput, newBald_dataset2$sum_MeakWhpred2)
cor(newBald_dataset2$sum_kWhpred2, newBald_dataset2$sum_TotalOutput)
cor(newBald_dataset2$sum_kWhpred2, newBald_dataset2$sum_MeakWhpred2)
plot(newBald_dataset2$sum_MeakWhpred2, newBald_dataset2$sum_TotalOutput)
plot(newBald_dataset2$sum_kWhpred2, newBald_dataset2$sum_TotalOutput)
plot(newBald_dataset2$sum_kWhpred2, newBald_dataset2$sum_MeakWhpred2)
install.packages("Metrics")
library(Metrics)
#Summary Stats on Predicted vs Measured Values (Model 2)
mean(newBald_dataset2$ER2)
mean(newBald_dataset2$ER_Mea2)
mean(newBald_dataset2$sum_MeakWhpred2)
mean(newBald_dataset2$sum_kWhpred2)
rmse(newBald_dataset2$sum_TotalOutput, newBald_dataset2$sum_MeakWhpred2)
rmse(newBald_dataset2$sum_TotalOutput, newBald_dataset2$sum_kWhpred2)
cor(newBald_dataset2$sum_TotalOutput, newBald_dataset2$sum_MeakWhpred2)
cor(newBald_dataset2$sum_kWhpred2, newBald_dataset2$sum_TotalOutput)
write.csv(newBald_dataset, "Baldwin Monthly Sums and Energy Ratios.csv")










