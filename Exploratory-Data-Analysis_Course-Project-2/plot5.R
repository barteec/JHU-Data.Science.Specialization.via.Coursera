# we will used data downloaded from plot1.R

## Plot 5 -------------------------------------------------------------


### Motor vehicle emission in Baltimore city over the years
Baltimore.NEI <- summarySCC_PM25
Baltimore.NEI <- filter(Baltimore.NEI, 
                        fips == 24510 & 
                        type=="ON-ROAD")

Baltimore.Motor.Emissions <- Baltimore.NEI %>% 
                              group_by(year, type) %>%
                               summarise(Total.Emissions = sum(Emissions))

# Make the plot with histogram
png("plot5.png", width = 700, height = 400)
ggplot(Baltimore.Motor.Emissions,
       aes(as.factor(year), Total.Emissions,
           label = round(Total.Emissions,digits = 0),
           fill = as.factor(year)))+
  xlab("Year") +
  ylab("PM2.5 emissions") +
  ggtitle("Total vehicle PM 2.5 emission from 1998-2008\nBaltimore, MD") +
  geom_histogram(stat = "identity") +
  geom_text(vjust = 2) +
  scale_fill_discrete(name = "Year")
dev.off()