## Reproducible Research - Week 4 Assignment

This Assignment is part of Reproducible Research Course by Coursera.
This project explores the NOAA storm database, which tracks major storms and weather events, to address the most severe types of weather events in the USA, which caused greatest damage to human population in terms of fatalities/injuries and economic loss during the years 1950 - 2011.

### Your data analysis must address the following questions:

1. Across the United States, which types of events (as indicated in the EVTYPE\color{red}{\verb|EVTYPE|}EVTYPE variable) are most harmful with respect to population health?
2. Across the United States, which types of events have the greatest economic consequences?

## Reproducible report can be accessed here: [RPUBs LINK](https://rpubs.com/BBartee/Reproducible_Research_Course_Project_2)

#
#
#
### Introduction

Storms and other severe weather events can cause both public health and economic problems for communities and municipalities. Many severe events can result in fatalities, injuries, and property damage, and preventing such outcomes to the extent possible is a key concern.

This project involves exploring the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database. This database tracks characteristics of major storms and weather events in the United States, including when and where they occur, as well as estimates of any fatalities, injuries, and property damage.

## Data

The data for this assignment come in the form of a comma-separated-value file compressed via the bzip2 algorithm to reduce its size. You can download the file from the course web site:

   [Storm Data 47mb](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2)

There is also some documentation of the database available. Here you will find how some of the variables are constructed/defined.

1. National Weather Service [Storm Data Documentation](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf)
2. National Climatic Data Center Storm Events [FAQ](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2FNCDC%20Storm%20Events-FAQ%20Page.pdf)

The events in the database start in the year 1950 and end in November 2011. In the earlier years of the database there are generally fewer events recorded, most likely due to a lack of good records. More recent years should be considered more complete.
Review criteria

Has either a (1) valid RPubs URL pointing to a data analysis document for this assignment been submitted; or (2) a complete PDF file presenting the data analysis been uploaded?
1. Is the document written in English?
2. Does the analysis include description and justification for any data transformations?
3. Does the document have a title that briefly summarizes the data analysis?
4. Does the document have a synopsis that describes and summarizes the data analysis in less than 10 sentences?
5. Is there a section titled "Data Processing" that describes how the data were loaded into R and processed for analysis?
6. Is there a section titled "Results" where the main results are presented?
7. Is there at least one figure in the document that contains a plot?
8. Are there at most 3 figures in this document?
9. Does the analysis start from the raw data file (i.e. the original .csv.bz2 file)?
10. Does the analysis address the question of which types of events are most harmful to population health?
11. Does the analysis address the question of which types of events have the greatest economic consequences?
12. Do all the results of the analysis (i.e. figures, tables, numerical summaries) appear to be reproducible?
13. Do the figure(s) have descriptive captions (i.e. there is a description near the figure of what is happening in the figure)?
14. As far as you can determine, does it appear that the work submitted for this project is the work of the student who submitted it?
