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
length = length(countries$country)
chosen <- countries$country[sample(1:length, 4, replace = TRUE)]

chosen_emissions <- subset(emissions, emissions$Entity %in% chosen)
chosen_emissions <- chosen_emissions %>%
  mutate(log_c02 = log(c02), million_tons = c02/1000000)
ggplot(data = chosen_emissions, aes(x = Year, y = million_tons, color = Entity)) +
  geom_point()+
  geom_line()+
  labs(y= expression(Yearly~Emissions~"("*Million~Tons~of~CO[2]*")"))+ 
  scale_color_brewer(palette=c("red", 'blue', 'green', 'yellow'))
chosen_emissions <- chosen_emissions %>%
  group_by(Entity)
cum <- data.frame(cum = cumsum(chosen_emissions$c02))
