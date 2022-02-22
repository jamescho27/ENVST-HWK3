# ENVST-HWK3
library(dplyr)
library(lubridate)
library(ggplot2)

temps_data <- read.csv("activity03/climate-change.csv")
temps_data$Day <- ymd(temps_data$Day)
emissions <- read.csv("activity03/annual-co-emissions-by-region.csv")
temps_data %>% with(plot(Day, temperature_anomaly, col = factor(temps_data$Entity)))
emissions$c02 <- emissions$Annual.CO2.emissions..zero.filled.

ggplot(data = temps_data, aes(x = Day, y = temperature_anomaly, color = Entity)) +
  geom_point()+
  geom_line()

total_co2 <- emissions %>% 
  group_by(Entity) %>%
  summarize(sum(c02))
colnames(total_co2) <- c("Entity", "Total_Emissions")

total_co2$billion_tons <- total_co2$Total_Emissions / 1000000000


total_northa_emissions <- total_co2[total_co2$Entity == "United States" |
                   total_co2$Entity == "Canada" |
                   total_co2$Entity == "Mexico", ]
ggplot(data = total_northa_emissions, aes(x = Entity, y = billion_tons, fill = Entity)) +
  geom_bar(stat = "identity")+
  labs(x = "Country", y= expression(Total~Emissions~"("*Billion~Tons~of~CO[2]*")"))

countries <- data.frame(country = unique(emissions$Entity))
