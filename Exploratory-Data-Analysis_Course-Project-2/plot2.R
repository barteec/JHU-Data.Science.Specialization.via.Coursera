# we will used data downloaded from plot1.R

## Plot2 -------------------------------------------------------------
### Only Baltimore, fips=="24510"
# Aggregate emissions
Baltimore.Emissions <- summarySCC_PM25
Baltimore.Emissions <- filter(Baltimore.Emissions, fips == 24510)

#lets do a pivot table with dplyr package
Baltimore.Emissions <- Baltimore.Emissions %>% 
                          group_by(year) %>%
                            summarise(Total.Emissions = sum(Emissions))

# Plot emission by year for Baltimore
png("plot2.png")
barplot(Baltimore.Emissions$Total.Emissions,
        Baltimore.Emissions$year,
        ylab = "PM2.5",
        xlab = "Years",
        names.arg = Baltimore.Emissions$year,
        main = "Total PM2.5 emission in Baltimore City, Maryland")
dev.off()