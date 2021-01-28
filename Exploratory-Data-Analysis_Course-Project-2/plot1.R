library(dplyr)

# frist let us get our data Data ------------------------------------------
url = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
filename = "Assignment2_data.zip"
download.file(url, filename)
 
# Unzip files so we can read them
unzip(filename)

# Now I can read Read the files
summarySCC_PM25 <- readRDS("summarySCC_PM25.rds")
Source_Classification_Code <- readRDS("Source_Classification_Code.rds")

## Plot 1-------------------------------------------------------------------
  #Total PM2.5 emission from 1999-2008
  Emissions <- summarySCC_PM25

  #lets do a pivot table with dplyr package
  Emissions <- Emissions %>% 
                group_by(year) %>%
                  summarise(Total.Emissions = sum(Emissions))
           
    
    # Plot emission by year
    png("plot1.png")
    barplot(Emissions$Total.Emissions,
            Emissions$year,
            ylab = "PM2.5",
            xlab = "Year",
            names.arg = Emissions$year,
            main = "Total PM2.5 emission"
    )
    dev.off()