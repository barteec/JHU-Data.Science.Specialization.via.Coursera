# we will used data downloaded from plot1.R

## Plot3 -------------------------------------------------------------
### Only Baltimore, fips=="24510"
# Aggregate emissions

### Plot3
library(ggplot2)
### Only Baltimore, fips=="24510" and divide emission by "type"
Baltimore.Emissions2 <- summarySCC_PM25
Baltimore.Emissions2 <- filter(Baltimore.Emissions2, fips == 24510)

#lets do a pivot table with dplyr package
Baltimore.Emissions3 <- Baltimore.Emissions2 %>% 
                          group_by(year, type) %>%
                            summarise(Total.Emissions = sum(Emissions))


# Make the plot and divide emission by "type"
png("plot3.png", width = 700, height = 400)
ggplot(Baltimore.Emissions2, aes(as.factor(year), log(Emissions), fill = type)) +
  xlab("Year") +
  ylab("log PM2.5 emissions") +
  ggtitle("PM 2.5 emission 1998-2008 per type\n Baltimore City, MD") +
  geom_jitter(alpha=0.1) +
  geom_boxplot() +
  labs(fill = "Type") +
  facet_grid(. ~ type)
dev.off()
