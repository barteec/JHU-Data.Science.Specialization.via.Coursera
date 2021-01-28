#Reproducible_Research_Week_4

#load libraries needed
library(dplyr)
library(ggplot2)
library(data.table)
library(lubridate)
library(rmarkdown)


# downloading data needed ----------------------------------------------------------------
Url_data <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
filename <- "repdata_data_StormData.csv.bz2"
download.file(Url_data, filename)
# reading data
My_data <- fread(file = filename, sep = "auto", header = TRUE)
My_data <- data.table(My_data) #transfer back to data.table


  # Change date formats and filter data for dates
  My_data$BGN_DATE <- mdy_hms(My_data$BGN_DATE)
  My_data <- My_data[BGN_DATE > "1995-12-31"]
  
  # Select the needed columns
  My_data <- My_data[, colnames(My_data) %in% 
                       c("EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")
                     , with=FALSE]
    
  #cleaning event types names
  My_data$EVTYPE <- toupper(My_data$EVTYPE)
  
  # eliminating zero data
  My_data <- My_data[FATALITIES != 0 &
                       INJURIES != 0 & 
                       PROPDMG != 0 & 
                       CROPDMG != 0]
  
# Population health data processing-----------------------------------------------------------------
  
  #pivot table with dplyr
  My_data <- data.frame(My_data) #transfer back to data.frame
  Health_data <- My_data %>% group_by(EVTYPE) %>%
                             summarise(FATALITIES = sum(FATALITIES),
                                       INJURIES = sum(INJURIES))
  
  Health_data <- data.table(Health_data)  #transfer back to data.table
  Health_data <- Health_data[, PEOPLE_LOSS := FATALITIES + INJURIES, by = "EVTYPE"]
  # descending order by PEOPLE_LOSS                           
  Health_data <- Health_data[order(Health_data$PEOPLE_LOSS, decreasing = TRUE), ]
  #top 10 by PEOPLE_LOSS
  Top10.EVTYPE.People <- top_n(Health_data, 10)
  
  print(Top10.EVTYPE.People)
  
  
# Economic consequences data processing -------------------------------------------------
  
  #transform letters and symbols to numbers
  My_data$PROPDMGEXP <- gsub("[Hh]", "2", My_data$PROPDMGEXP)
  My_data$PROPDMGEXP <- gsub("[Kk]", "3", My_data$PROPDMGEXP)
  My_data$PROPDMGEXP <- gsub("[Mm]", "6", My_data$PROPDMGEXP)
  My_data$PROPDMGEXP <- gsub("[Bb]", "9", My_data$PROPDMGEXP)
  My_data$PROPDMGEXP <- gsub("\\+", "1", My_data$PROPDMGEXP)
  My_data$PROPDMGEXP <- gsub("\\?|\\-|\\ ", "0",  My_data$PROPDMGEXP)
  My_data$PROPDMGEXP <- as.numeric(My_data$PROPDMGEXP)
  
  My_data$CROPDMGEXP <- gsub("[Hh]", "2", My_data$CROPDMGEXP)
  My_data$CROPDMGEXP <- gsub("[Kk]", "3", My_data$CROPDMGEXP)
  My_data$CROPDMGEXP <- gsub("[Mm]", "6", My_data$CROPDMGEXP)
  My_data$CROPDMGEXP <- gsub("[Bb]", "9", My_data$CROPDMGEXP)
  My_data$CROPDMGEXP <- gsub("\\+", "1", My_data$CROPDMGEXP)
  My_data$CROPDMGEXP <- gsub("\\-|\\?|\\ ", "0", My_data$CROPDMGEXP)
  My_data$CROPDMGEXP <- as.numeric(My_data$CROPDMGEXP)
  
  My_data$PROPDMGEXP[is.na(My_data$PROPDMGEXP)] <- 0
  My_data$CROPDMGEXP[is.na(My_data$CROPDMGEXP)] <- 0
  
  #Total damage values
  My_data <- mutate(My_data, 
                      PROPDMGTOTAL = PROPDMG * (10 ^ PROPDMGEXP), 
                      CROPDMGTOTAL = CROPDMG * (10 ^ CROPDMGEXP))
  
  #Economic_data: Let us now analyze the date from above
  #pivot table with dplyr
  Economic_data <- My_data %>% group_by(EVTYPE) %>%
                               summarise(PROPDMGTOTAL = sum(PROPDMGTOTAL),
                                         CROPDMGTOTAL = sum(CROPDMGTOTAL))
  
  Economic_data <- data.table(Economic_data)  #transfer back to data.table
  Economic_data <- Economic_data[, ECONOMIC_LOSS := PROPDMGTOTAL + CROPDMGTOTAL, by = "EVTYPE"]
  # descending order by ECONOMIC_LOSS    
  Economic_data <- Economic_data[order(Economic_data$ECONOMIC_LOSS, decreasing = TRUE), ]
  #top 10 by ECONOMIC_LOSS   
  Top10.EVTYPE.economy <- top_n(Economic_data, 10)
  
  print(Top10.EVTYPE.economy)
  
  
  #plotting health loss -> HL
  png("plot1.png")
  HL <- ggplot(data = Top10.EVTYPE.People, aes(x = reorder(EVTYPE, PEOPLE_LOSS), y = PEOPLE_LOSS)) +
        geom_bar(stat = "identity", colour = "black") +
        labs(title = "USA total people loss by weather events in 1996-2011") +
        theme(plot.title = element_text(hjust = 0.5)) +
        labs(y = "Number of fatalities and injuries", x = "Event Type") +
        coord_flip()
  dev.off()
  HL
  
  #plotting economic loss -> EL
  png("plot2.png")
  EL <- ggplot(data = Top10.EVTYPE.economy, aes(x = reorder(EVTYPE, ECONOMIC_LOSS), y = ECONOMIC_LOSS)) +
        geom_bar(stat = "identity", colour = "black") +
        labs(title = "USA total economic loss by weather events in 1996-2011") +
        theme(plot.title = element_text(hjust = 0.5)) +
        labs(y = "Size of property and crop loss", x = "Event Type") +
        coord_flip()
  dev.off()
  EL
  
  render("./Events.Rmd")
  