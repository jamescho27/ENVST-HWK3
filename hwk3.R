# ENVST-HWK3
library(dplyr)
library(lubridate)
library(ggplot2)
library(patchwork)

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


#Question 1
palette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

chosen <- c("Spain", "Germany", "France", "Italy")

chosen_emissions <- subset(emissions, emissions$Entity %in% chosen)
chosen_emissions <- chosen_emissions %>%
  mutate(million_tons = c02/1000000)

ggplot(data = chosen_emissions, aes(x = Year, y = million_tons, color = Entity)) +
  geom_point()+
  geom_line()+
  labs(y= expression(Yearly~Emissions~"("*Million~Tons~of~CO[2]*")"))+
  scale_colour_manual(values=palette)+
  ggtitle("Yearly Emissions of Selected Countries")


#Question 2

anomaly_plot <- ggplot(data = filter(temps_data, Entity == "World"), 
                       aes(x = Day, y = temperature_anomaly, color = "#fff200"))+
  labs(y = "Temperature Anomalies (Degrees)")+
  ggtitle("World Temperature Anomalies")+
  theme(plot.title = element_text(size = 9))+
  geom_line(show.legend = FALSE)

world_emissions <- emissions %>% 
  group_by(Year) %>%
  summarise(sum(c02))



colnames(world_emissions) <- c("Year", "Total_C02")

world_emissions <- mutate(world_emissions, ten_million_tons = Total_C02 / 10000000)

emissions_plot <- ggplot(data = world_emissions, aes(x = Year, y = ten_million_tons))+
  geom_line()+
  geom_point()+
  labs(y= expression(Yearly~Emissions~"("*Tens~Of~Million~Tons~of~CO[2]*")"))+
  ggtitle(expression(World~Yearly~Emissions~of~CO[2]))+
  theme(plot.title = element_text(size = 9))
  
emissions_plot + anomaly_plot


#question 3

ozone = read.csv("mfe-ozone-hole-19792016-CSV/ozone-hole-19792016.csv")
colnames(ozone) <- c("year", "ozone_hole_area", "ozone_concentration")
coeff <- 20
area_color <- "#718099"
concentration_color <- "#349dba"
ggplot(data = ozone, aes(x = year))+
  geom_point(aes(y=ozone_hole_area), color = area_color)+
  geom_line(aes(y=ozone_hole_area), color = area_color)+
  geom_point(aes(y=ozone_concentration/coeff), color = concentration_color)+
  geom_line(aes(y=ozone_concentration/coeff), color = concentration_color)+
  scale_y_continuous(
    name = "Ozone Hole Area (Million square Kilometers)",
    
    sec.axis = sec_axis(~.*coeff, name = "Ozone Concentration (Dobson Units)")
  )+
  theme(
    axis.title.y = element_text(color = area_color),
    axis.title.y.right = element_text(color = concentration_color)
  )+
  ggtitle("Ozone Hole Area and Concentration")
