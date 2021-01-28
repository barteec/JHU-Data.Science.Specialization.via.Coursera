# we will used data downloaded from plot1.R

## Plot 6 -------------------------------------------------------------

### Compare vehicle emissions between Los Angeles and Baltimore
Baltimore.NEI <- filter(summarySCC_PM25, fips=="24510" & type=="ON-ROAD")
LA.NEI <- filter(summarySCC_PM25, fips=="06037" & type=="ON-ROAD")

# Aggregate emissions
Baltimore.Emissions <- Baltimore.NEI %>% 
                        group_by(year, type) %>%
                          summarise(Total.Emissions = sum(Emissions))

LA.Emissions <- LA.NEI %>% 
                  group_by(year, type) %>%
                   summarise(Total.Emissions = sum(Emissions))

Baltimore.Emissions$city <- rep("Baltimore", nrow(Baltimore.Emissions))
LA.Emissions$city <- rep("Los Angeles", nrow(LA.Emissions))

# Rbind to a single Data.frame
Emissions <- rbind(Baltimore.Emissions, LA.Emissions)

# Make histogram plot for comparison
png("plot6.png", width = 700, height = 400)
ggplot(Emissions,
       aes(as.factor(year), Total.Emissions,
           fill = as.factor(year)))+
  xlab("Year") +
  ylab("PM2.5 emissions") +
  ggtitle("Total vehicle PM 2.5 emission from 1998 to 2008") +
  geom_histogram(stat = "identity") +
  facet_grid(. ~ city) +
  scale_fill_discrete(name = "Year")
dev.off()
