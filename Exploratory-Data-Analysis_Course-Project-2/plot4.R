# we will used data downloaded from plot1.R

## Plot 4 -------------------------------------------------------------

### Coal combustion-related emission changed over the years?
SCC.coal <- Source_Classification_Code[(grepl(x = Source_Classification_Code$Short.Name, pattern = "Coal", ignore.case=TRUE)),]
NEI.coal <- merge(summarySCC_PM25, SCC.coal, by = "SCC")

#lets do a pivot table with dplyr package
Emissions.coal <- NEI.coal %>% 
                    group_by(year) %>%
                      summarise(Total.Emissions = sum(Emissions))

Emissions.coal$group=rep("coal",nrow(Emissions.coal))

# Make the plot
png("plot4.png", width = 700, height = 400)
ggplot(Emissions.coal, aes(as.factor(year), Total.Emissions,
                           group = group,
                           label = round(Total.Emissions,digits = 0)))+
  xlab("Year") +
  ylab("PM2.5 emissions") +
  ggtitle("Total Coal-sourced PM 2.5 emission from 1998-2008") +
  geom_point(size = 3, shape = 2, show.legend = TRUE) +
  geom_line(col="red", linetype = 2) +
  geom_text(check_overlap = TRUE, vjust = 1.2, hjust = 1.2)
dev.off()