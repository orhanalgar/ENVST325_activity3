install.packages(c("ggplot2", "dplyr"))
library(ggplot2)
library(dplyr)
install.packages("lubridate")
library(lubridate)


datCO2 <- read.csv("/cloud/project/activity03/annual-co-emissions-by-region.csv")

colnames(datCO2)[4] <- "CO2"

datCO2$Entity <- as.factor(datCO2$Entity)
name.Ent <- levels(datCO2$Entity)

US <- datCO2 %>%
  filter(Entity == "United States")
  
#Standard Base R Plotting
plot(US$Year,US$CO2,
     type="b",
     pch=19,
     xlab= "Year",
     ylab="Fossil Fuel Emission (billions of tons)",
     yaxt="n")
axis(2, seq(0,6000000000, by=2000000000),seq(0,6, by=2), las=2)

#GGPlotting
ggplot(US, aes(x=Year, y=CO2))+
  geom_point()+
  geom_line()+
  labs(x="Year", y="US Fossil Fuel C02 Emissions (tons)")+
  theme_classic()

#Making a North America subset
NorthA <- datCO2 %>%
  filter(Entity == "United States" |
         Entity == "Mexico" |
          Entity == "Canada")

#GGplotting for North America emissions
ggplot(NorthA, aes(x=Year, y=CO2, color=Entity))+
  geom_point()+
  geom_line()+
  scale_color_manual(values=c("red","royalblue","darkgoldenrod3"))

#Violin and Box Plots
# subset CO2 to meet conditons
compCO2 <- datCO2[datCO2$Year >= 1950 & datCO2$Entity == "France" |
                    datCO2$Year >= 1950 & datCO2$Entity == "India" |
                    datCO2$Year >= 1950 &  datCO2$Entity == "Russia" , ]

ggplot(data = compCO2 , aes(x=Entity, y=CO2))+ # look at CO2 by country
  geom_violin(fill=rgb(0.933,0.953,0.98))+ # add a violin plot with blue color
  geom_boxplot(width=0.03,size=0.15, fill="grey90")+ # add grey 
  #boxplots and make them smaller to fit in the violin (width)
  #than normal with thinner lines (size_ than normal
  theme_classic()+ # get rid of ugly gridlines
  labs(x = "Country", y="Annual emissions (tons CO2)")

#Prompt 1: Make a plot of air temperature anomalies in the Northern and 
#Southern Hemisphere in base R and in ggplot2.

#Loading Climate Change Data
tempAnom <- read.csv("/cloud/project/activity03/climate-change.csv")
tempAnom$date <- ymd(tempAnom$Day)

#Base R
NHem <- tempAnom[tempAnom$Entity == "Northern Hemisphere",]
SHem <- tempAnom[tempAnom$Entity == "Southern Hemisphere",]

plot(NHem$date, NHem$temperature_anomaly,
     type="b",
     pch=19,
     xlab= "Year",
     ylab="Temperature Anamoly (celsius)",
     yaxt="n")
axis(2, seq(-5,5, by=0.5),seq(-5,5, by=0.5), las=2)

#GGPlot
ggplot(NHem, aes(x=date, y=temperature_anomaly))+
  geom_line()+
  labs(x="Year", y="Temperature Anamoly (celsius)")+
  theme_classic()

#Homework
#Question 1: Mauritius CO2 Emissions
Mau <- datCO2 %>%
  filter(Entity == "Mauritius")
Mau$SimpleCO2 <- Mau$CO2 / 1000000
ggplot(Mau, aes(x=Year, y=SimpleCO2))+
  geom_line()+
  labs(title="Mauritius Historical Emissions", x="Year", y="Fossil Fuel Emissions (million tons)")+
  theme_classic()

#Question 2: 
TotalCO2 <- datCO2 %>%
  group_by(year = Year) %>%
  summarize(total_sum = sum(CO2, na.rm = TRUE))
TotalCO2$simpleCO2 <- TotalCO2$total_sum / 1000000000
ggplot(TotalCO2, aes(x=year, y=simpleCO2))+
  geom_line()+
  labs(title="Global Historical Emissions", x="Year", y="Fossil Fuel Emissions (billion tons)")+
  theme_minimal()

World_TempAnomalies <- tempAnom %>%
  filter(Entity == "World")
ggplot(World_TempAnomalies, aes(x=date, y=temperature_anomaly))+
  geom_line()+
  labs(title="Global Historical Temperature Anamolies", x="Year", y="Temperature Anomaly (celsius)")+
  theme_minimal()

#Question 3:
datRE <- read.csv("/cloud/project/activity03/modern-renewable-energy-consumption.csv")

africa <- datRE %>%
  filter(Entity == "Africa")
ggplot(africa, aes(x=Year, y=Solar))+
  geom_line()+
  labs(title="Africa Historical Solar Energy Consumption", x="Year", y="Solar Energy Consmumption (tera-watt hours)")+
  theme_minimal()
ggplot(africa, aes(x = Year)) +
  geom_line(aes(y = Solar, color = "Solar", group = 1)) +
  geom_line(aes(y = Wind, color = "Wind", group = 1)) +
  geom_line(aes(y = Hydropower, color = "Hydropower", group = 1)) +
  geom_line(aes(y = Other.renewables, color = "Other Renewables", group = 1))+
  labs(title = "Africa Historical Renewable Energy Consumption",
       y = "Energy Consmumption (tera-watt hours)",
       color = "Renewable Type")



