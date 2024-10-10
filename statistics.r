############################################################
# Statistics Assignment (Stats and Probability)
#
# Zalán Tóth / ID: 20102768 / Programme: Computer Forensics & Security
#
############################################################
# Load the data from the clipboard
#Library to be used later
library(ggplot2)
#data <- read.csv("/home/zalan/Letöltések/STAT/ZalanToth.csv")
road_data <- read.table("/home/zalan/Letöltések/STAT/ZalanToth.csv",
sep= ",", header = TRUE)
attach(road_data)
############################################################
#
# Q1 #######################################################
#
#SUMMARY
anova_result <- aov(flow ~ classification, data = road_data)
summary(anova_result)
#PLOT
road_data$classification <- factor(road_data$classification)
boxplot(flow ~ classification,
data = road_data,
main = "Flow Rate Distribution by Road Classification",
xlab = "Road Classification",
ylab = "Flow Rate (hundreds of vehicles/hour)",
col = "skyblue",
border = "darkblue")
#
# Q2 #######################################################
#
#SUMMARY
road_data$services <- factor(road_data$services)
anova_result <- aov(flow ~ services, data = road_data)
summary(anova_result)
#PLOT
boxplot(flow ~ services, data = road_data,
main = "Flow Rate Distribution by Type of Services",
xlab = "Type of Services",
ylab = "Flow Rate (hundreds of vehicles/hour)",
col = "lightgreen",
border = "darkgreen")
#
# Q3 #######################################################
#
#SUMMARY
tolled_vs_class <- table(road_data$tolled, road_data$classification)
chisq.test(tolled_vs_class)
#PLOT
road_data$classification <- factor(road_data$classification)
road_data$tolled <- factor(road_data$tolled, levels = c(0, 1), labels =
c("Non-Tolled", "Tolled"))
contingency_table <- table(road_data$classification, road_data$tolled)
# Imported the ggplot library previously, and used that instead of
barplot for this!
# to install this library run: install.packages("ggplot2")
ggplot(road_data, aes(x = classification, fill = tolled)) +
geom_bar(position = "dodge") +
labs(title = "Number of Tolled and Non-Tolled Roads by
Classification",
x = "Road Classification",
y = "Number of Roads",
fill = "Tolled Status") +
theme_minimal()
#
# Q4 #######################################################
#
#PLOT
road_data$flow <- as.numeric(road_data$flow)
road_data$population <- as.numeric(road_data$population)
road_data_clean <- na.omit(road_data[, c("flow", "population")])
plot(road_data_clean$population, road_data_clean$flow,
main = "Flow Rate vs Total Population",
xlab = "Total Population (hundreds)",
ylab = "Flow Rate (hundreds of vehicles/hour)",
pch = 19, col = "blue")
lm_result <- lm(flow ~ population, data = road_data_clean)
abline(lm_result, col = "red", lwd = 2) #that is the line for the
linear regression!
#
# Q5 #######################################################
#
#PLOT
road_data$flow <- as.numeric(road_data$flow)
road_data$previously <- as.numeric(road_data$previously)
flow_data <- data.frame(
TimePeriod = rep(c("Current", "Five Years Ago"), each =
nrow(road_data)),
Flow = c(road_data$flow, road_data$previously)
)
flow_data <- na.omit(flow_data)
boxplot(Flow ~ TimePeriod, data = flow_data,
main = "Flow Rates: Current vs. Five Years Ago",
xlab = "Time Period",
ylab = "Flow Rate (hundreds of vehicles/hour)",
col = c("lightblue", "lightgreen"))
#
# Q6 #######################################################
#
#PLOT
road_data$flow <- as.numeric(road_data$flow)
road_data$residences <- as.numeric(road_data$residences)
road_data_clean <- na.omit(road_data[, c("flow", "residences")])
plot(road_data_clean$residences, road_data_clean$flow,
main = "Scatter Plot of Flow Rate vs. Number of Residences",
xlab = "Number of Residences",
ylab = "Flow Rate (hundreds of vehicles/hour)",
pch = 19, col = "blue")
lm_result <- lm(flow ~ residences, data = road_data_clean)
abline(lm_result, col = "red", lwd = 2) #Trend line (linear regression)
