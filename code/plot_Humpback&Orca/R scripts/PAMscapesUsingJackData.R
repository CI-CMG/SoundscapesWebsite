#Getting Jack's humpback detection data for MBNMS into the PAMscapes Shiny App
#By Emma Beretta

# RUN this to make sure latest updates for PAMscapes
devtools::install_github('TaikiSan21/PAMscapes')

#install.packages("rJava") make sure Java is installed for xlsx to work

library(PAMscapes)
library(lubridate)
library(dplyr)
library(ggplot2)
library(reshape)
library(rJava)
library(xlsx)
library(openxlsx)
library(devtools)
library(plotly)

rm(list=ls()) 
#Load in
otherCsv1 <- read.csv('C:/Users/embe5980/OneDrive - UCB-O365/Desktop/sanctsound_humpback_mb/SanctSound_MB01.csv', stringsAsFactors = FALSE)
str(otherCsv1)

otherCsv2 <- read.csv('C:/Users/embe5980/OneDrive - UCB-O365/Desktop/sanctsound_humpback_mb/SanctSound_MB02.csv', stringsAsFactors = FALSE)
str(otherCsv2)

otherCsv3 <- read.csv('C:/Users/embe5980/OneDrive - UCB-O365/Desktop/sanctsound_humpback_mb/SanctSound_MB03.csv', stringsAsFactors = FALSE)
str(otherCsv3)





#Clean up!
#uu = 1

#for (uu in 3) {
  #change for what site u want
  otherCsv <- otherCsv3

  #need to add a end detection time column
  otherCsv <- otherCsv %>%
    dplyr::rename(HourStart = Hour) #need dplyr:: bc there is also a rename function in the reshape package

  otherCsv <- otherCsv %>% mutate(HourEnd = lead(HourStart))

  #filling in nd hour for last hour bin (row in dataset)
  otherCsv$HourEnd[26520] <- "11/22/2021 8:00"

  #for Song data
  otherCsvSong <- otherCsv %>% select(-NonSong)
  otherCsvSong$Humpback <- otherCsvSong$Song
  otherCsvSong <- otherCsvSong %>% select(-Song)

  #for Non Song data
  #otherCsvNonSong <- otherCsv %>% select(-Song)
  #otherCsvNonSong$Humpback <- otherCsvNonSong$NonSong
  #otherCsvNonSong <- otherCsvNonSong %>% select(-NonSong)

  #From Taiki - make detection datasets
  #change x= for if you waant Song or NonSong detection data
  #detData1 = MB01, etc
  detData3 <- loadDetectionData(x=otherCsvSong,
                  source='csv', detectionType='auto', wide=TRUE,
                  tz='UTC',
                  columnMap=list(UTC='HourStart', end='HourEnd'),
                  speciesCols='Humpback',
                  detectedValues='1')


  #effort detection dataset, doesnt work for making presence graphs
  detData3E <- loadDetectionData(x=otherCsvSong,
                              source='csv', detectionType='auto', wide=TRUE,
                              tz='UTC',
                              columnMap=list(UTC='HourStart', end='HourEnd'),
                              speciesCols='Humpback',
                              detectedValues=c('1', '0', 'N/A'))

#}

  detData1$Site <- "MB01"
  detData2$Site <- "MB02"
  detData3$Site <- "MB03"
  
  MBHump_allDet <- bind_rows(detData1, detData2, detData3)
  
  
  
#NEW PLOTS
 
#boxplot showing all three sites for Hump Song hrs/wk   
  plotDetectionBoxplot(x=MBHump_allDet, group='species', facet='Site', bin='hour/week', combineYears=TRUE)
  
#boxplot showing all three sites for Hump Song hrs/month 
  plotDetectionBoxplot(x=MBHump_allDet, group='species', facet='Site', bin='hour/month', combineYears=TRUE)
  
  
  
#TRY TO MAKE ACOUSTIC SCENE PLOT WITH ALL THREE SITES?
  
  detData1E$Site <- "MB01"
  detData2E$Site <- "MB02"
  detData3E$Site <- "MB03"
  
  MBHump_allDetE <- bind_rows(detData1E, detData2E, detData3E)
  
  
  
  
  

##PLOTS with just presence (not accounting for N/A hours with no data)
# (combined across years)

#Run PAMscapes Shiny App on detected humpback song
runDetectionExplorer(MBHump_allDetE)


#quick links to plots
#boxplot, hours in a month of humpback whale song presence combined across years
plotDetectionBoxplot(x=detData, group='species', bin='hour/month', combineYears=TRUE)

#boxplot, days in a month across years of data
plotDetectionBoxplot(x=detData, group='species', bin='day/month', combineYears=TRUE)

#boxplot, hours in a week across years
plotDetectionBoxplot(x=detData, group='species', bin='hour/week', combineYears=TRUE)

#polar plot, counting presence of song in each hour of the day over all the data days
#whales singing more at night. 3-12 am UTC -> 8pm-5am PDT or 7pm-4am PST
plotPolarDetections(x=detData, group='species', bin='detection/hour', quantity='count')

#polar plot, counting hours of song in each month over the years
#sing most in october and november
plotPolarDetections(x=detData, group='species', bin='hour/month', quantity='count')





#TIME SERIES plots
#Trying to make time series but boxplots dont work because no variation since not combining years
plotDetectionBoxplot(x=detData1, group='species', bin='hour/week', combineYears=FALSE)


#get data ready for line graph (MB01 song)
#WATCH OUT because this makes Song and NonSong numeric, which is needed for line graph, but character is needed for effort graphs
otherCsv1 <- otherCsv1 %>%
  mutate(
    datetime = mdy_hm(Hour),  # convert to POSIXct
    Song = as.numeric(Song),
    NonSong = as.numeric(NonSong)
  )

MB01Song_weekly <- otherCsv1 %>%
  mutate(
    week = floor_date(datetime, "week"),  # get the start of each week
    year = year(datetime)
  ) %>%
  group_by(week, year) %>%
  summarise(
    total_hours_present = sum(Song, na.rm = TRUE)
  ) %>%
  ungroup()


ggplot(MB01Song_weekly, aes(x = week, y = total_hours_present)) +
  geom_line(color = "steelblue") +
  labs(
    title = "Weekly Total Whale Song Hours Over Time",
    x = "Week",
    y = "Total Hours with Whale Song"
  ) +
  theme_minimal()


#All SITES one plot
otherCsv1$Site <- "MB01"
otherCsv2$Site <- "MB02"
otherCsv3$Site <- "MB03"

MBHump_all <- bind_rows(otherCsv1, otherCsv2, otherCsv3)

#Song Call presence
MBSong_weekly <- MBHump_all %>%
  mutate(
    week = floor_date(datetime, "week"),
    year = year(datetime)
  ) %>%
  group_by(Site, week, year) %>%
  summarise(total_hours_present = sum(Song, na.rm = TRUE), .groups = "drop")


#Non Song Call presence
MBNonSong_weekly <- MBHump_all %>%
  mutate(
    week = floor_date(datetime, "week"),
    year = year(datetime)
  ) %>%
  group_by(Site, week, year) %>%
  summarise(total_hours_present = sum(NonSong, na.rm = TRUE), .groups = "drop")


# can use for either Song or NonSong graph, just change dataset accordingly
ggplot(MBNonSong_weekly, aes(x = week, y = total_hours_present, color = Site)) +
  geom_line(linewidth = 1) +
 # geom_point()+
  scale_x_date(
    date_labels = "%b %Y",   # shows abbreviated month + year on x-axis
    date_breaks = "3 months"  # one tick per month
  ) +
  labs(
    title = "Weekly Humpback Whale Non Song Call Presence",
    x = "Date",
    y = "Total Hours with Whale Non Song Calls",
    color = "Site"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





#EFFORT plots for each site using PAMscapes Acoustic Scene plots

effort <- detData1E
# mark which times on vs off effort
effort$on_effort <- effort$detectedFlag != 'N/A'

# note when effort changes from on to off so we can group them
effort$eff_change <- FALSE
effort$eff_change[2:nrow(effort)] <- effort$on_effort[1:(nrow(effort)-1)] != effort$on_effort[2:nrow(effort)]

# label the groups with numbers
effort$eff_group <- cumsum(effort$eff_change)

# get the start and end times of each on effort group
effortOn1 <- effort %>%
  filter(on_effort == TRUE) %>%
  group_by(eff_group) %>%
  summarise(start=min(UTC), end=max(end))

# filter back to only "1" so that only positive detections are shown
plotAcousticScene(filter(detData3, detectedFlag=='1'), effort = effortOn)





















#trying to account for N/As
#detDataWEffort <- loadDetectionData(x=otherCsvSong,
                           #  source='csv', detectionType='auto', wide=TRUE,
                         #    tz='UTC',
                           #  columnMap=list(UTC='HourStart', end='HourEnd'),
                          #   speciesCols='Humpback',
                          #   detectedValues=c('1', '0', 'N/A'))

#Run PAMscapes Shiny App on detected humpback song (accounting for N/A)
#runDetectionExplorer(detDataWEffort)


#above accounting for N/A code didnt work so manually looking at what hours had no data
#copied these into powerpoint
otherCsv2 %>%
  count(NonSong)

OffEffort <- otherCsvSong %>%
  filter(Humpback == "N/A")

otherCsv3 %>%
  count(Song)


