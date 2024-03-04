#Read in Training Data and modify to Daily
Columbiadat <- read.csv("ColumbiaSAM.csv")
View(Columbiadat)
# Create a new dataframe to store the aggregated values
daily_Col_TMY <- data.frame(matrix(nrow = 365, ncol = 4))
# Generate the sequence of dates
dates_Col <- seq(as.Date("2022-01-01"), by = "day", length.out = 365)
#Daily Average Predictor variables
for (i in seq(1, nrow(Columbiadat), 24)) {
  # Calculate the sum of each column over the 24 rows, excluding the first column
  row <- colSums(Columbiadat[i:(i+23), -1])
  row[-1] <- row[-1] / 24
  # Append the aggregated row to the new dataframe
  daily_Col_TMY[(i-1)/24 + 1, ] <- row
}
colnames(daily_Col_TMY) <- c("kWh", "Temp", "GHI", "WS")
# Add the Date variable to the new dataframe
daily_Col_TMY$Date <- dates_Col
daily_Col_TMY <- daily_Col_TMY[, c("Date", names(daily_Col_TMY)[-ncol(daily_Col_TMY)])]
View(daily_Col_TMY)






#Daily lms
dailyCol_model1 <- lm(kWh ~ GHI + Temp + WS, data = daily_Col_TMY)
dailyCol_model2 <- lm(kWh ~ GHI + Temp:GHI, data = daily_Col_TMY)
dailyCol_model3 <- lm(kWh ~ GHI, data = daily_Col_TMY)
summary(dailyCol_model1)
summary(dailyCol_model2)
summary(dailyCol_model3)
#Save Models
write.csv(dailyCol_model1$coefficients, "DailyColModel1Coefficients.csv")
write.csv(dailyCol_model2$coefficients, "DailyColModel2Coefficients.csv")
write.csv(dailyCol_model3$coefficients, "DailyColModel3Coefficients.csv")






#Read in Daily Prediction Data
dailypredCol <- read.csv("OikoColumbia.csv")
View(dailypredCol)
colnames(dailypredCol)
colnames(dailypredCol)[colnames(dailypredCol) == "temperature..degC."] <- "Temp"
colnames(dailypredCol)[colnames(dailypredCol) == "wind_speed..m.s."] <- "WS"
colnames(dailypredCol)[colnames(dailypredCol) == "surface_solar_radiation..W.m.2."] <- "GHI"
colnames(dailypredCol)[colnames(dailypredCol) == "direct_normal_solar_radiation..W.m.2."] <- "DNI"
colnames(dailypredCol)[colnames(dailypredCol) == "surface_diffuse_solar_radiation..W.m.2."] <- "DHI"
#Predict kWh with models
dailypredCol$predictedkWh1 <- predict(dailyCol_model1, newdata = dailypredCol)
dailypredCol$predictedkWh2 <- predict(dailyCol_model2, newdata = dailypredCol)
dailypredCol$predictedkWh3 <- predict(dailyCol_model3, newdata = dailypredCol)





#Read in Test data
testdat_Col <- read.csv("Jan 21 - May 23.csv")
testdat_Col <- testdat_Col[-297,]
dailypredCol <- dailypredCol[-297,]
View(testdat_Col)
dailyCol_test <- testdat_Col
View(dailyCol_test)
dailyCol_test <- cbind(dailypredCol, dailyCol_test$Total.Output..kWh.)
dailyCol_test <- dailyCol_test[,c(1,11:14)]
plot(dailyCol_test$`dailyCol_test$Total.Output..kWh.`, dailyCol_test$predictedkWh2)
write.csv(dailyCol_test, "Columbia Measured vs Expected.csv")
colnames(dailyCol_test)


#Monthly Plot Expected vs Measured
ggplot(dailyCol_test, aes(x = as.POSIXct(datetime..UTC.), group = 1)) +
  geom_line(aes(y = `dailyCol_test$Total.Output..kWh.`, color = "Total.system")) +
  geom_line(aes(y = predictedkWh2, color = "predictedkWh2")) +
  labs(x = "Date and Time", y = "Value", title = "Columbia Monthly Expected vs Measured (kWh)") +
  scale_color_manual(values = c("Total.system" = "blue", "predictedkWh2" = "red")) +
  theme_bw()



library(dplyr)
library(lubridate)
#Create new Monthly dataset with ERs
# Convert the date column to a date type
dailyCol_test$datetime..UTC. <- as.Date(dailyCol_test$datetime..UTC., format = "%m/%d/%y")
dailyCol_test$`dailyCol_test$Total.Output..kWh.` <- as.numeric(dailyCol_test$`dailyCol_test$Total.Output..kWh.`)
# Create a new dataset with summed values by month
newCol_dataset <- dailyCol_test %>%
  mutate(month = floor_date(datetime..UTC., "month")) %>%
  group_by(month) %>%
  summarise(sum_TotalOutput = sum(`dailyCol_test$Total.Output..kWh.`),
            sum_kWhpred1 = sum(predictedkWh1),
            sum_kWhpred2 = sum(predictedkWh2),
            sum_kWhpred3 = sum(predictedkWh3))
View(newCol_dataset)
newCol_dataset$ER1 <- newCol_dataset$sum_TotalOutput/newCol_dataset$sum_kWhpred1
newCol_dataset$ER2 <- newCol_dataset$sum_TotalOutput/newCol_dataset$sum_kWhpred2
newCol_dataset$ER3 <- newCol_dataset$sum_TotalOutput/newCol_dataset$sum_kWhpred3
summary(newCol_dataset$sum_kWhpred2)
summary(newCol_dataset$ER2)
summary(newCol_dataset$sum_TotalOutput)
cor(newCol_dataset$sum_TotalOutput, newCol_dataset$sum_kWhpred2)
rmse(newCol_dataset$sum_TotalOutput, newCol_dataset$sum_kWhpred2)
plot(newCol_dataset$sum_kWhpred2, newCol_dataset$sum_TotalOutput)
write.csv(newCol_dataset, "Columbia Monthly Sums and Energy Ratios.csv")



