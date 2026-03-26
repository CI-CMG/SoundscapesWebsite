#Fish plots of CH01, MB01, and MB02 for NOAA Report Card
#By Emma Beretta with help from Ella Kim

#Order of graphs in document:
#1. Diel Rose Plots for CH01 and MB01
#2. Seasonal Boxplots for CH01, MB01, and MB02
#3. Seasonal Comparison Histogram for MB01
#4. Map w/ Pies for CH01 and MB01

install.packages("ggplot2")
install.packages('cowplot')
install.packages('dplyr')
install.packages('tidyquant')
install.packages('tidyverse')
install.packages('lubridate')
install.packages('lunar')
install.packages('suncalc')
install.packages ('scales')
install.packages ('clock')
install.packages('rlang')
install.packages('readr')
install.packages("devtools")
install.packages("raster")
install.packages("viridis")
install.packages("sf")

library(lubridate)
library(cowplot)
library(dplyr)
library(tidyquant)
library(tidyverse)
library(lunar)
library(suncalc)
library(scales)
library(clock)
library(rlang)
library(readr)
library(devtools)
library(raster)
library(viridis)


# ><(((*>  <*)))><   ><(((*>  <*)))><  ><(((*>  <*)))><  ><(((*>  <*)))><  ><(((*>  <*)))><  ><(((*>  <*)))><  
# ><(((*>  <*)))><   ><(((*>  <*)))><  ><(((*>  <*)))><  ><(((*>  <*)))><  ><(((*>  <*)))><  ><(((*>  <*)))><  

#1. DIEL ROSE PLOTS

# ><(((*>  <*)))><   ><(((*>  <*)))><  ><(((*>  <*)))><  ><(((*>  <*)))><  ><(((*>  <*)))><  ><(((*>  <*)))><  
#CH01
#set working directory (change before making each new site graph)
setwd("C:/Users/embe5980/Indicators/WCR_fish_ella/Code/CH01")   

#read all CSVs for all sites and deployments
CH01_01_data=read.csv("CH01_01.csv",header=TRUE)
# CH01_02_data=read.csv("CH01_02 - Sheet.csv",header=TRUE) - no fish so not loading in
CH01_03_data=read.csv("CH01_03.csv",header=TRUE)
CH01_04_data=read.csv("CH01_04.csv",header=TRUE)

#Combine CSV files into one matrix for each site
CH01 <- rbind(CH01_01_data, CH01_03_data, CH01_04_data)

# Subset so that we only have the three columns we need
CH01 <- CH01 %>% 
  dplyr::select("Start.time", "End.time", "Comments")

#Creates column with start data/time in format that R can read
CH01$Start <- mdy_hms(CH01$Start.time)
CH01$End <- mdy_hms(CH01$End.time) 

#again, keeping only needed columns
CH01 <- CH01 %>% 
  dplyr::select("Start", "End", "Comments")

#fixing logging inconsistencies
CH01$Comments <- gsub('bocaccio sunset','Bocaccio sunset',CH01$Comments)
CH01$Comments <- gsub('bocaccio','Bocaccio',CH01$Comments)
CH01$Comments <- gsub('Bocaccio sunset','Bocaccio',CH01$Comments)
CH01$Comments <- gsub('Bocaccio sunrise','Bocaccio',CH01$Comments)
CH01$Comments <- gsub('midshipman','Midshipman',CH01$Comments)
CH01$Comments <- gsub('off effort','Off effort',CH01$Comments)
CH01$Comments <- gsub('Off effort','Off Effort',CH01$Comments)

#checking that we have categories we want for the Comments column
X<-split(CH01, CH01$Comments)

#formatting data so that we can plot 
CH01$start_time <- as.POSIXct(CH01$Start)
CH01$end_time <- as.POSIXct(CH01$End)

#creating a function that will expand each log so that we have a row for each hour where a fish was singing 
generate_hours <- function(fish, start_time, end_time) {
  # Create a sequence of hours between start_time and end_time
  seq_hours <- seq(from = floor_date(start_time, "hour"), 
                   to = ceiling_date(end_time, "hour") - 1, 
                   by = "hour")
  
  # Return a data frame with one row per hour
  data.frame(fish = fish, hour = seq_hours)
}

# Apply the function to each row of the data
CH01Hour <- CH01 %>%
  rowwise() %>%
  do(generate_hours(.$Comments, .$start_time, .$end_time)) %>%
  ungroup()

#pulling out just hour so we can sum across each one 
CH01Hour$ChorusHour <- hour(CH01Hour$hour)

# Keep only unique rows based on date/time and fish species. 
#this is so that we dont double count bocaccio sunset and bocaccio in teh same hour on the same day, for example
CH01HourUnique <- CH01Hour %>%
  distinct(hour, fish, .keep_all = TRUE)

#group count of fish chorusing for each hour and fish species
CH01_summary <- CH01Hour %>%
  group_by(ChorusHour, fish) %>%
  summarise(count = n())

#keep only fish rows, exclude off effort
CH01_summary <- CH01_summary %>% filter(fish %in% c("Midshipman", "Bocaccio"))

#Add 0 counts for hours with no chorusing
all_combinations <- expand.grid(ChorusHour = 0:23, fish = c("Midshipman", "Bocaccio"))

# Ensure ChorusHour is numeric for both datasets
CH01_summary$ChorusHour <- as.numeric(CH01_summary$ChorusHour)
all_combinations$ChorusHour <- as.numeric(all_combinations$ChorusHour)

# Left join the original data with all_combinations to ensure all hours are included
CH01_summary <- all_combinations %>%
  left_join(CH01_summary, by = c("ChorusHour", "fish")) %>%
  mutate(count = ifelse(is.na(count), 0, count))  # Fill missing counts with 0

#divide count of chorusing days by total effort days so that the units are in proportion, just like the histograms
CH01_summary$effortDays <- - (difftime(as.Date("2022-06-23"),as.Date("2022-10-19"), units = "day") + difftime(as.Date("2022-11-16"),as.Date("2023-02-22"), units = "day") + difftime(as.Date("2023-03-11"),as.Date("2023-11-08"), units = "day") )
CH01_summary$effortDays <- as.numeric(substr(CH01_summary$effortDays, 1,3))

CH01_summary$prop <- CH01_summary$count / CH01_summary$effortDays

#adjust utc to pst by subtracting 7 hours
CH01_summary$ChorusHour <- CH01_summary$ChorusHour -7 
CH01_summary$ChorusHour[1:7] <- CH01_summary$ChorusHour[1:7] + 24
CH01_summary$ChorusHour[25:31] <- CH01_summary$ChorusHour[25:31] + 24

#colors used for different fish
custom_colors <- c("Bocaccio" = "deepskyblue", "Midshipman" = "darkorange")

#Rose Plot! y axis is proportion of days where chorusing is present out of total effort days for that hour
CH01Rose = ggplot(CH01_summary, aes(x = factor(ChorusHour), y = prop, fill = fish)) +
  geom_bar(stat = "identity") +
  coord_polar(start = 0) +
  theme_minimal() +
  scale_fill_manual(values = custom_colors) + 
  labs(x = "Hour of the Day (PST/PDT)", 
       y = "Stacked Proportion of Daily Chorusing\n(# days with chorusing / # days recorded)", 
       title = "Hourly Proportion of Daily Fish Chorusing at CH01", 
       fill = "Fish Species") +
  theme(axis.text.x = element_text(size = 12)) + 
  #annotate("text", x = 10.5, y = 0, label = "0", color = "black") +
  annotate("text", x = 10.5, y = .4, label = "0.4", color = "black") +
  annotate("text", x = 10.5, y = .8, label = "0.8", color = "black") +
  annotate("text", x = 10.5, y = 1.2, label = "1.2", color = "black")  +
  annotate("text", x = 10.5, y = 1.6, label = "1.6", color = "black")  +
  theme(axis.text.y = element_blank()) 

CH01Rose




CH01Roses = ggplot(CH01_summary, aes(x = factor(ChorusHour), y = prop, fill = fish)) +
  geom_bar(stat = "identity") +
  # Create a separate plot for each species
  facet_wrap(~fish) + 
  coord_polar(start = 0) +
  theme_minimal() +
  scale_fill_manual(values = custom_colors, na.translate = FALSE) + 
  labs(
    x = "Hour of the Day (PST/PDT)", 
    y = "Proportion of Daily Chorusing\n(# days with chorusing / # days recorded)", 
    title = "Hourly Proportion of Daily Fish Chorusing at CH01", 
    fill = "Fish Species"
  ) +
  # Adjust y-axis to accommodate your 1.6 max
  scale_y_continuous(limits = c(0, .5), breaks = c(0.1, 0.25, 0.5)) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 10),
    axis.text.y = element_blank(),
    panel.grid.major = element_line(color = "gray95"), # Uses a very light grey
    strip.text = element_text(face = "bold", size = 12) # Species labels
  ) +
  # Annotation for scale (placed at x = 0 to avoid overlapping bars)
  annotate("text", x = 10.5, y = c(0.1, 0.25, 0.5), 
           label = c( "0.1", "0.25", "0.5"), 
           color = "black", size = 3)

CH01Roses


outDirG = "C:/Users/embe5980/SoundscapesWebsite/content/resources"
ggsave(filename = paste0(outDirG, "/CH01_FinalRosePlotV2.png"), dpi = 300)

# ><(((*>  <*)))><   ><(((*>  <*)))><  ><(((*>  <*)))><  ><(((*>  <*)))><  ><(((*>  <*)))><  ><(((*>  <*)))><  
#MB01
#set working directory (change before making each new site graph)
setwd("C:/Users/embe5980/Indicators/WCR_fish_ella/Code/MB01")   

#read all CSVs for all deployments
#no MB01_10 b/c off effort
MB01_01_data=read.csv("MB01_01.csv",header=TRUE)
MB01_02_data=read.csv("MB01_02.csv",header=TRUE)
MB01_03_data=read.csv("MB01_03.csv",header=TRUE)
MB01_04_data=read.csv("MB01_04.csv",header=TRUE)
MB01_05_data=read.csv("MB01_05.csv",header=TRUE)
MB01_06_data=read.csv("MB01_06.csv",header=TRUE)
MB01_07_data=read.csv("MB01_07.csv",header=TRUE)
MB01_08_data=read.csv("MB01_08.csv",header=TRUE)
MB01_09_data=read.csv("MB01_09.csv",header=TRUE)
MB01_11_data=read.csv("MB01_11 - Sheet1.csv",header=TRUE)
MB01_12_data=read.csv("MB01_12_NEW.csv",header=TRUE)

#midshipman was logged seperately for MBNMS, so load those in here
MB01_02_midshipman=read.csv("MB01_02_midshipman.csv")
MB01_03_midshipman=read.csv("MB01_03_midshipman.csv")
MB01_04_midshipman=read.csv("MB01_04_midshipman.csv")
MB01_05_midshipman=read.csv("MB01_05_midshipman.csv")
MB01_06_midshipman=read.csv("MB01_06_midshipman.csv")
MB01_07_midshipman=read.csv("MB01_07_midshipman.csv")
MB01_08_midshipman=read.csv("MB01_08_midshipman.csv")
MB01_09_midshipman=read.csv("MB01_09_midshipman.csv")

# Combine CSV files into one matrix for each site
MB01=rbind(MB01_01_data, MB01_02_data, MB01_03_data, MB01_04_data, MB01_05_data, MB01_06_data, MB01_07_data, MB01_08_data,MB01_09_data, MB01_11_data, MB01_12_data, MB01_02_midshipman, MB01_03_midshipman, MB01_05_midshipman, MB01_06_midshipman, MB01_07_midshipman, MB01_08_midshipman, MB01_09_midshipman)

# Subset so that we only have the three columns we need
MB01 <- MB01 %>% 
  dplyr::select("Start.time", "End.time", "Comments")

#Creates column with start data/time in format that R can read
MB01$Start <- mdy_hms(MB01$Start.time)
MB01$End <- mdy_hms(MB01$End.time) 

#again, keeping only needed columns
MB01 <- MB01 %>% 
  dplyr::select("Start", "End", "Comments")

#fixing logging inconsistencies
MB01$Comments <- gsub('Bocaccio sunset','Bocaccio', MB01$Comments)
MB01$Comments <- gsub('Bocaccio sunrise','Bocaccio',MB01$Comments)

MB01$Comments <- gsub('HF sunset','HF', MB01$Comments)
MB01$Comments <- gsub('HF sunrise','HF',MB01$Comments)

MB01$Comments <- gsub('Off effort','Off Effort',MB01$Comments)
MB01$Comments <- gsub('Lingcod','White Seabass',MB01$Comments)

MB01$Comments <- gsub('Mystery high','UF310',MB01$Comments)
MB01$Comments <- gsub('Mystery','UF310',MB01$Comments)

#check our columns
X<-split(MB01, MB01$Comments)

#UF310 being weird where there are two of them when you split(), code below manually fixes issues
missingUF310 <- MB01[1207:1221,]
missingUF310 <- missingUF310 %>% filter(missingUF310$Comments != "Bocaccio")
missingUF310$Comments <- "UF310"

#new data with the fixed UF310 problem
MB01_fix <- rbind(MB01, missingUF310)

#formatting data so that we can plot 
MB01_fix$start_time <- as.POSIXct(MB01_fix$Start)
MB01_fix$end_time <- as.POSIXct(MB01_fix$End)

#loaded in already for CH01 but leaving here just in case
#creating a function that will expand each log so that we have a row for each hour where a fish was singing 
#generate_hours <- function(fish, start_time, end_time) {
  # Create a sequence of hours between start_time and end_time
  #seq_hours <- seq(from = floor_date(start_time, "hour"), 
    #               to = ceiling_date(end_time, "hour") - 1, 
     #              by = "hour")
  
  # Return a data frame with one row per hour
  #data.frame(fish = fish, hour = seq_hours)
#}

# Apply the function to each row of the data
MB01Hour <- MB01_fix %>%
  rowwise() %>%
  do(generate_hours(.$Comments, .$start_time, .$end_time)) %>%
  ungroup()

#pulling out just hour so we can sum across each one 
MB01Hour$ChorusHour <- hour(MB01Hour$hour)

#changing utc to pst/pdt 
MB01Hour$date <- substr(MB01Hour$hour, 1, 10)
MB01Hour$date <- as.Date(MB01Hour$date)

#daylight savings -8, otherwise -7
# MB01HourPST <- MB01Hour %>%
#   mutate(
#     ChorusHourP = ifelse(date >= as.Date("2018-11-15") & date <= as.Date("2019-03-10") | date >= as.Date("2019-11-04") & date <= as.Date("2020-03-07") | date >= as.Date("2020-11-01") & date <= as.Date("2021-03-13") | date >= as.Date("2021-11-07") & date <= as.Date("2022-01-21") | date >= as.Date("2022-11-06") & date <= as.Date("2023-03-11"), ChorusHour - 8, ChorusHour - 7)
#   )
MB01HourPST <- MB01Hour %>%
  mutate(
    hour_PT = with_tz(hour, tzone = "America/Los_Angeles"),
    ChorusHourP = hour(hour_PT)
  )


# Keep only unique rows based on date/time and fish species. 
#this is so that we dont double count bocaccio sunset and bocaccio in teh same hour on the same day, for example
MB01HourUnique <- MB01Hour %>%
  distinct(hour, fish, .keep_all = TRUE)


#group count of fish chorusing for each hour and fish species
MB01_summary <- MB01HourPST %>%
  group_by(ChorusHourP, fish) %>%
  summarise(count = n())

#keep only fish rows, not off effort
MB01_summary <- MB01_summary %>% filter(fish %in% c("Midshipman", "Bocaccio", "UF310", "White Seabass", "HF"))

#changing data from utc to pst
 MB01_summary$ChorusHour <- MB01_summary$ChorusHourP
# MB01_summary <- MB01_summary[, 2:4]
# MB01_summary$ChorusHour[1:34] <- MB01_summary$ChorusHour[1:34] + 24

#get sum of days spent chorusing
MB01_summary <- MB01_summary %>%
  group_by(ChorusHour, fish) %>%
  summarise(count = sum(count))


# MAKING ACOUSTIC EFFORT PLOT BELOW

#upload deployment dates: here you need to upload deployment start and end dates (when data was being recorded at this site)
MB01_deployment_dates=read.csv("MB01Deployments.csv",header=TRUE)

#make dates readable by R
MB01_deployment_dates$Start <- mdy(MB01_deployment_dates$Start)
MB01_deployment_dates$End <- mdy(MB01_deployment_dates$End) 

#save into dataframes with just start and end times for each location (and adding comments for fish name)
MB01_effort <- MB01_deployment_dates 

#now we need to make the start datetime column just date (this deployment was already just in date)
MB01_effort$Start_date <-as.Date(MB01_effort$Start)
MB01_effort$End_date <-as.Date(MB01_effort$End)

#Now I want to make a sequence of dates from start to end date for each of the rows and then put 24 hours for each of those
MB01effort_days1 <- seq(as.Date(MB01_effort$Start_date[1]), as.Date(MB01_effort$End_date[1]), by = 'days')  
MB01effort_days2 <- seq(as.Date(MB01_effort$Start_date[2]), as.Date(MB01_effort$End_date[2]), by = 'days')  
MB01effort_days3 <- seq(as.Date(MB01_effort$Start_date[3]), as.Date(MB01_effort$End_date[3]), by = 'days')  
MB01effort_days4 <- seq(as.Date(MB01_effort$Start_date[4]), as.Date(MB01_effort$End_date[4]), by = 'days')  
MB01effort_days5 <- seq(as.Date(MB01_effort$Start_date[5]), as.Date(MB01_effort$End_date[5]), by = 'days')  
MB01effort_days6 <- seq(as.Date(MB01_effort$Start_date[6]), as.Date(MB01_effort$End_date[6]), by = 'days')  
MB01effort_days7 <- seq(as.Date(MB01_effort$Start_date[7]), as.Date(MB01_effort$End_date[7]), by = 'days')  
MB01effort_days8 <- seq(as.Date(MB01_effort$Start_date[8]), as.Date(MB01_effort$End_date[8]), by = 'days')  
MB01effort_days9 <- seq(as.Date(MB01_effort$Start_date[9]), as.Date(MB01_effort$End_date[9]), by = 'days')  
MB01effort_days11 <- seq(as.Date(MB01_effort$Start_date[10]), as.Date(MB01_effort$End_date[10]), by = 'days')  
MB01effort_days12 <- seq(as.Date(MB01_effort$Start_date[11]), as.Date(MB01_effort$End_date[11]), by = 'days')  

#combine all of these into one array
MB01_effort_dates = c(MB01effort_days1,MB01effort_days2,MB01effort_days3, MB01effort_days4, MB01effort_days5 ,MB01effort_days6 ,MB01effort_days7 ,MB01effort_days8 ,MB01effort_days9, MB01effort_days11 ,MB01effort_days12)

#save array as data frame and add a row that is 24 for each of the days
#times = length of MB01_effort_dates
test<-rep(c(24),times=1421) 
dates_test<-cbind(MB01_effort_dates,test)
MB01_date_hours=as.data.frame(dates_test)
names(MB01_date_hours)[1] <-"numdate"
names(MB01_date_hours)[2] <-"acoustic_hours"
MB01_date_hours$date<-as.Date.numeric(MB01_date_hours$numdate)
#resave just those 2 columns
MB01_effort2 <- MB01_date_hours %>% 
  dplyr::select(date,acoustic_hours)


#divide count of chorusing days by total effort days so that the units are in proportion, just like the histograms
MB01_summary$effortDays <- nrow(MB01_effort2)
MB01_summary$prop <- MB01_summary$count / MB01_summary$effortDays

#change HF to UF440 because UF440 was logged as HF (high frequency) instead of its unidentified fish frequency name (UF310)
MB01_summary$fish <- gsub('HF','UF440',MB01_summary$fish)

#colors for all 5 fish species
custom_colors <- c("Bocaccio" = "deepskyblue", "Midshipman" = "darkorange", "UF310" = "green3", "White Seabass" = "firebrick2", "UF440" = "darkorchid")


MB01_summary$fish <- factor(
  MB01_summary$fish,
  levels = c(
    "Bocaccio",
    "Midshipman",
    "UF440" ,
    "UF310",
    "White Seabass" 
  )
)

#Rose Plot!
MB01Rose = ggplot(MB01_summary, aes(x = factor(ChorusHour), y = prop, fill = fish)) +
  geom_bar(stat = "identity") +
  coord_polar(start = 0) +
  theme_minimal() +
  scale_fill_manual(values = custom_colors) + 
  labs(x = "Hour of the Day (PST/PDT)", 
       y = "Stacked Proportion of Daily Chorusing\n(# days with chorusing / # days recorded)", 
       title = "Hourly Proportion of Daily Fish Chorusing at MB01" , fill = "Fish Species") +
  theme(axis.text.x = element_text(size = 12)) +
  #can add this following line if you want the 0 on the y axis to be labeled. I removed it because it was making it hard to see bars around 0
  #annotate("text", x = 15.5, y = 0, label = "0", color = "black", size = 3) +
  annotate("text", x = 10.5, y = .4, label = "0.4", color = "black") +
  annotate("text", x = 10.5, y = .8, label = "0.8", color = "black") +
  annotate("text", x = 10.5, y = 1.2, label = "1.2", color = "black")  +
  annotate("text", x = 10.5, y = 1.6, label = "1.6", color = "black")  +
  theme(axis.text.y = element_blank()) 

MB01Rose



MB01Roses = ggplot(MB01_summary, aes(x = factor(ChorusHour), y = prop, fill = fish)) +
  geom_bar(stat = "identity") +
  # Create a separate plot for each species
  facet_wrap(~fish) + 
  coord_polar(start = 0) +
  theme_minimal() +
  scale_fill_manual(values = custom_colors, na.translate = FALSE) + 
  labs(
    x = "Hour of the Day (PST/PDT)", 
    y = "Proportion of Daily Chorusing\n(# days with chorusing / # days recorded)", 
    title = "Hourly Proportion of Daily Fish Chorusing at MB01", 
    fill = "Fish Species"
  ) +
  # Adjust y-axis to accommodate your 1.6 max
  scale_y_continuous(limits = c(0, .9), breaks = c(0.1, 0.3, 0.6, 0.9)) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 10),
    axis.text.y = element_blank(),
    panel.grid.major = element_line(color = "gray95"), # Uses a very light grey
    strip.text = element_text(face = "bold", size = 12) # Species labels
  ) +
  # Annotation for scale (placed at x = 0 to avoid overlapping bars)
  annotate("text", x = 10.5, y = c(0.1, 0.3, 0.6, 0.9), 
           label = c( "0.1", "0.3", "0.6", "0.9"), 
           color = "black", size = 3)

MB01Roses


outDirG = "C:/Users/embe5980/SoundscapesWebsite/content/resources"
ggsave(filename = paste0(outDirG, "/MB01_FinalRosePlotV2.png"), dpi = 300)

#proportion of days where fish (bocaccio) chorused out of all days where we were recording during that hour (20)

# ><(((*>  <*)))><   ><(((*>  <*)))><  ><(((*>  <*)))><  ><(((*>  <*)))><  ><(((*>  <*)))><  ><(((*>  <*)))><  
# ><(((*>  <*)))><   ><(((*>  <*)))><  ><(((*>  <*)))><  ><(((*>  <*)))><  ><(((*>  <*)))><  ><(((*>  <*)))><  

#2. SEASONAL BOXPLOTs by SITE

# ><(((*>  <*)))><   ><(((*>  <*)))><  ><(((*>  <*)))><  ><(((*>  <*)))><  ><(((*>  <*)))><  ><(((*>  <*)))><  
#CH01
#set working directory
setwd("/Users/emmaberetta/Desktop/NMSF2024/csvdetections/CHNMS Logs/CH01")   

#read all CSVs for all deployments
CH01_01_data=read.csv("CH01_01.csv",header=TRUE)
CH01_02_data=read.csv("CH01_02_no_fish.csv",header=TRUE)
CH01_03_data=read.csv("CH01_03.csv",header=TRUE)
CH01_04_data=read.csv("CH01_04.csv",header=TRUE)

# Combine CSV files into one matrix for each site
CH01=rbind(CH01_01_data, CH01_02_data, CH01_03_data, CH01_04_data)

# Subset so that we only have the three columns we need
CH01 <- CH01 %>% 
  dplyr::select("Start.time", "End.time", "Comments")

#fIXING START AND END TIMES SO THAT IN USABLE FORMAT
#Creates column with start data/time in format that R can read
CH01$combine_start <- mdy_hms(CH01$Start.time)

#Creates column with end data/time in format that R can read
CH01$combine_end <- mdy_hms(CH01$End.time) 

#make a sequence of dates from beginning of earliest deployment to end of latest deployment
dateCH01 <- seq(as.Date('2022-06-23'), as.Date('2023-11-08'), by = 'days')  # create sequence of days 

names(CH01)[4] <-"Start"
names(CH01)[5] <-"End"
names(CH01)[3] <-"Fish"

#save into dataframes with just start and end times for each location (and adding comments for fish name)
CH01new <- CH01 %>% 
  dplyr::select(Fish,Start,End)

#Time difference
CH01_diff=as.data.frame(difftime(CH01new$End,CH01new$Start))
CH01_diff=(CH01_diff)/3600 #change to hours

#add this to data frame
CH01new$Acoustic_Hours=difftime(CH01new$End,CH01new$Start)
CH01new$Acoustic_Hours=(CH01new$Acoustic_Hours)/3600

#get rid of units
CH01new$Acoustic_Hours=as.vector(CH01new$Acoustic_Hours)

#now we need to make the start datetime column just date
CH01new$Date <- ymd_hms(CH01new$Start)
CH01new$Date <-as.Date(CH01new$Date)

CH01_sort <- CH01new %>%
  dplyr::select(Fish,Date,Acoustic_Hours)

#Rename Bocaccio sunrise and Bocaccio sunset to Bocaccio, and HF sunset and HF sunset to HF. This is because we're just making one histogram for all of those together
#also fix typos and capitalization
CH01_sort$Fish <- gsub('bocaccio sunset','Bocaccio sunset',CH01_sort$Fish)
CH01_sort$Fish <- gsub('bocaccio','Bocaccio',CH01_sort$Fish)
CH01_sort$Fish <- gsub('Bocaccio sunset','Bocaccio',CH01_sort$Fish)
CH01_sort$Fish <- gsub('Bocaccio sunrise','Bocaccio',CH01_sort$Fish)

CH01_sort$Fish <- gsub('midshipman','Midshipman',CH01_sort$Fish)
CH01_sort$Fish <- gsub('off effort','Off effort',CH01_sort$Fish)
CH01_sort$Fish <- gsub('Off effort','Off Effort',CH01_sort$Fish)

#first figuring out diff names of fish
names_of_fish = CH01_sort %>% group_by(Fish) %>% 
  summarise(Acoustic_Hours = sum(Acoustic_Hours))

#separate the fish type
X<-split(CH01_sort, CH01_sort$Fish)
Bocaccio=as.data.frame(X$Bocaccio)
Midshipman=as.data.frame(X$Midshipman)
Noise=as.data.frame(X$"Off Effort")

#sorting fish seperately by date and summing acoustic hours per day (change based on what fish you have)
#Bocaccio
Bocaccio_df = Bocaccio %>% group_by(Date) %>% 
  summarise(Acoustic_Hours = sum(Acoustic_Hours))
#Midshipman
Midshipman_df = Midshipman %>% group_by(Date) %>% 
  summarise(Acoustic_Hours = sum(Acoustic_Hours))
#Noise
Noise_df = Noise %>% group_by(Date) %>% 
  summarise(Acoustic_Hours = sum(Acoustic_Hours))


#MAKING INDIVIDUAL PLOTS to then be put together 

#MIDSHIPMAN
#add dates with NA that are missing from the timeline
Midshipman_dates_fixed=Midshipman_df %>% mutate(Date = as.Date(Midshipman_df$Date)) %>% complete(Date = seq.Date(min(dateCH01), max(dateCH01), by="day"))
#replace NA with zeroes
Midshipman_dates_fixed$Acoustic_Hours[is.na(Midshipman_dates_fixed$Acoustic_Hours)]<-0 #changed this to make it show no effort times

#rename for easier use
CH01Midshipman_NEW <- Midshipman_dates_fixed 

CH01Midshipman_NEW$Date <- ymd(CH01Midshipman_NEW$Date)
CH01Midshipman_NEW$Week  <- week(CH01Midshipman_NEW$Date)

CH01Midshipman_NEW$Week = as.factor(CH01Midshipman_NEW$Week)

#Boxplot
CH01MidshipmanBox=ggplot(CH01Midshipman_NEW, aes(x=Week, y = Acoustic_Hours)) + 
  geom_boxplot(fill = "darkorange") +
  ylab("Acoustic Presence (Hrs)")+
  xlab("")+
  ggtitle("Midshipman")+ 
  theme_classic()+
  scale_x_discrete( breaks=c("1","5","9","13","18","22","26","31","35","40","44","48"),labels =c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) 

CH01MidshipmanBox


#BOCACCIO 
#add dates with NA that are missing from the timeline
Bocaccio_dates_fixed=Bocaccio_df %>% mutate(Date = as.Date(Bocaccio_df$Date)) %>% complete(Date = seq.Date(min(dateCH01), max(dateCH01), by="day"))
#replace NA with zeroes
Bocaccio_dates_fixed$Acoustic_Hours[is.na(Bocaccio_dates_fixed$Acoustic_Hours)]<-0

#rename for easier use
CH01Bocaccio_NEW <- Bocaccio_dates_fixed 

CH01Bocaccio_NEW$Date <- ymd(CH01Bocaccio_NEW$Date)
CH01Bocaccio_NEW$Week  <- week(CH01Bocaccio_NEW$Date)

CH01Bocaccio_NEW$Week = as.factor(CH01Bocaccio_NEW$Week)

#Boxplot
CH01BocaccioBox=ggplot(CH01Bocaccio_NEW, aes(x=Week, y = Acoustic_Hours)) + 
  geom_boxplot(fill = "deepskyblue") +
  ylab("")+
  xlab("")+
  ggtitle("Bocaccio")+ 
  theme_classic()+
  ylim(0,10)+
  scale_x_discrete( breaks=c("1","5","9","13","18","22","26","31","35","40","44","48"),labels =c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) 

CH01BocaccioBox


# MAKING ACOUSTIC EFFORT PLOT

#upload deployment dates: here you need to upload deployment start and end dates 
CH01_deployment_dates=read.csv("CH01Deployments.csv",header=TRUE)

#make dates readable by R
CH01_deployment_dates$Start <- mdy_hm(CH01_deployment_dates$Start) 
CH01_deployment_dates$End <- mdy_hm(CH01_deployment_dates$End) 

#save into dataframes with just start and end times for each location (and adding comments for fish name)
CH01_effort <- CH01_deployment_dates 

#now we need to make the start datetime column just date
CH01_effort$Start_date <-as.Date(CH01_effort$Start)
CH01_effort$End_date <-as.Date(CH01_effort$End)

#Now I want to make a sequence of dates from start to end date for each of the rows and then put 24 hours for each of those
CH01effort_days1 <- seq(as.Date(CH01_effort$Start_date[1]), as.Date(CH01_effort$End_date[1]), by = 'days')  
CH01effort_days2 <- seq(as.Date(CH01_effort$Start_date[2]), as.Date(CH01_effort$End_date[2]), by = 'days')  
CH01effort_days3 <- seq(as.Date(CH01_effort$Start_date[3]), as.Date(CH01_effort$End_date[3]), by = 'days')  
CH01effort_days4 <- seq(as.Date(CH01_effort$Start_date[4]), as.Date(CH01_effort$End_date[4]), by = 'days')  

#combine all of these into one array
CH01_effort_dates = c(CH01effort_days1,CH01effort_days2,CH01effort_days3, CH01effort_days4)

#save array as data frame and add a row that is 24 for each of the days
#times = length of CH01_effort_dates
test<-rep(c(24),times=462) 
dates_test<-cbind(CH01_effort_dates,test)
CH01_date_hours=as.data.frame(dates_test)
names(CH01_date_hours)[1] <-"numdate"
names(CH01_date_hours)[2] <-"acoustic_hours"
CH01_date_hours$date<-as.Date.numeric(CH01_date_hours$numdate)
#resave a just those 2 columns
CH01_effort2 <- CH01_date_hours %>% 
  dplyr::select(date,acoustic_hours)

#Now adapt to weekly sums
CH01_effort2$date <- ymd(CH01_effort2$date)
CH01_effort2$Week  <- week(CH01_effort2$date)
CH01_effort_weekly<-aggregate(acoustic_hours ~ Week,CH01_effort2, sum)


#Plot
#Effort weekly mean plot with error bars
CH01Effort=ggplot(CH01_effort_weekly, aes(x=Week, y=acoustic_hours)) + 
  ggtitle("Available Data")+ 
  geom_bar(stat = "identity",fill = "grey70")+ylim(0,400)+xlab("Month (by Week)")+ylab("Total Hours")+theme_classic()+
  #theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())
  scale_x_continuous(limits = c(0,53), expand = c(0, 0), breaks=c(1,5,9,13,18,22,26,31,35,40,44,48),labels = c("J","F","M","A","M","J","J","A","S","O","N","D")) +
  scale_y_continuous(limits = c(0,400), expand = c(0, 0), breaks=c(1,400),labels=c("0","400")) #,labels=c("0","1K") MAY NEED TO ADD THIS BACK IF SHIFTED 
#note here that I changed labels to "0" to "1k" because the existing labels shifted the whole graph to the left because they were too long and then the plots didn't line up
#but make sure that the scales are correct and appropriate. if possible make the scale the same as other plots that ella made
CH01Effort

#cant do Boxplot for Effort because all values are 24 hours because we are showing available data

#PLOT EVERYTHING TOGETHER--
grid.arrange(
  arrangeGrob(CH01BocaccioBox,CH01MidshipmanBox, CH01Effort, nrow = 3),
  top = "Seasonal Fish Chorusing at CH01")

# ><(((*>  <*)))><   ><(((*>  <*)))><  ><(((*>  <*)))><  ><(((*>  <*)))><  ><(((*>  <*)))><  ><(((*>  <*)))><  

#MB01
#set working directory
setwd("/Users/emmaberetta/Desktop/NMSF2024/csvdetections/MBNMS Logs/MB01")   

#read all CSVs for all deployments
#no MB01_10 b/c off effort
MB01_01_data=read.csv("MB01_01.csv",header=TRUE)
MB01_02_data=read.csv("MB01_02.csv",header=TRUE)
MB01_03_data=read.csv("MB01_03.csv",header=TRUE)
MB01_04_data=read.csv("MB01_04.csv",header=TRUE)
MB01_05_data=read.csv("MB01_05.csv",header=TRUE)
MB01_06_data=read.csv("MB01_06.csv",header=TRUE)
MB01_07_data=read.csv("MB01_07.csv",header=TRUE)
MB01_08_data=read.csv("MB01_08.csv",header=TRUE)
MB01_09_data=read.csv("MB01_09.csv",header=TRUE)
MB01_11_data=read.csv("MB01_11 - Sheet1.csv",header=TRUE)
MB01_12_data=read.csv("MB01_12 - Sheet1.csv",header=TRUE)

#midshipman was logged seperately for MBNMS, so load those in here
MB01_02_midshipman=read.csv("MB01_02_midshipman.csv")
MB01_03_midshipman=read.csv("MB01_03_midshipman.csv")
MB01_04_midshipman=read.csv("MB01_04_midshipman.csv")
MB01_05_midshipman=read.csv("MB01_05_midshipman.csv")
MB01_06_midshipman=read.csv("MB01_06_midshipman.csv")
MB01_07_midshipman=read.csv("MB01_07_midshipman.csv")
MB01_08_midshipman=read.csv("MB01_08_midshipman.csv")
MB01_09_midshipman=read.csv("MB01_09_midshipman.csv")

# Combine CSV files into one matrix for each site
MB01=rbind(MB01_01_data, MB01_02_data, MB01_03_data, MB01_04_data, MB01_05_data, MB01_06_data, MB01_07_data, MB01_08_data,MB01_09_data, MB01_11_data, MB01_12_data, MB01_02_midshipman, MB01_03_midshipman, MB01_05_midshipman, MB01_06_midshipman, MB01_07_midshipman, MB01_08_midshipman, MB01_09_midshipman)

# Subset so that we only have the three columns we need
MB01 <- MB01 %>% 
  dplyr::select("Start.time", "End.time", "Comments")

#fIXING START AND END TIMES SO THAT IN USABLE FORMAT
#Creates column with start data/time in format that R can read
MB01$combine_start <- mdy_hms(MB01$Start.time)

#Creates column with end data/time in format that R can read
MB01$combine_end <- mdy_hms(MB01$End.time) 

#make a sequence of dates from beginning of earliest deployment to end of latest deployment
dateMB01 <- seq(as.Date('2018-11-16'), as.Date('2023-08-17'), by = 'days')  # create sequence of days 
dateMB012019 <- seq(as.Date('2019-01-01'), as.Date('2019-12-31'), by = 'days')

names(MB01)[4] <-"Start"
names(MB01)[5] <-"End"
names(MB01)[3] <-"Fish"

#save into dataframes with just start and end times for each location (and adding comments for fish name)
MB01new <- MB01 %>% 
  dplyr::select(Fish,Start,End)

#Time difference
MB01_diff=as.data.frame(difftime(MB01new$End,MB01new$Start))
MB01_diff=(MB01_diff)/3600 #change to hours

#add this to data frame
MB01new$Acoustic_Hours=difftime(MB01new$End,MB01new$Start)
MB01new$Acoustic_Hours=(MB01new$Acoustic_Hours)/3600

#get rid of units
MB01new$Acoustic_Hours=as.vector(MB01new$Acoustic_Hours)

#now we need to make the start datetime column just date
MB01new$Date <- ymd_hms(MB01new$Start)
MB01new$Date <-as.Date(MB01new$Date)

MB01_sort <- MB01new %>%
  dplyr::select(Fish,Date,Acoustic_Hours)

#Rename Bocaccio sunrise and Bocaccio sunset to Bocaccio, and HF sunset and HF sunset to HF. THis is because we're just making one histogram for all of those together
#also fix typos and capitalization
MB01_sort$Fish <- gsub('Bocaccio sunset','Bocaccio', MB01_sort$Fish)
MB01_sort$Fish <- gsub('Bocaccio sunrise','Bocaccio',MB01_sort$Fish)

MB01_sort$Fish <- gsub('HF sunset','HF', MB01_sort$Fish)
MB01_sort$Fish <- gsub('HF sunrise','HF',MB01_sort$Fish)

MB01_sort$Fish <- gsub('Off effort','Off Effort',MB01_sort$Fish)
MB01_sort$Fish <- gsub('Lingcod','White Seabass',MB01_sort$Fish)

MB01_sort$Fish <- gsub('Mystery high','UF310',MB01_sort$Fish)
MB01_sort$Fish <- gsub('Mystery','UF310',MB01_sort$Fish)

#first figuring out diff names of fish
names_of_fish = MB01_sort %>% group_by(Fish) %>% 
  summarise(Acoustic_Hours = sum(Acoustic_Hours, na.rm = TRUE))

#For some reason not including the second UF310 (from 2020-12-21 to 2020-12-31), so manually fixing
missingUF310 <- MB01_sort[1207:1221,]
missingUF310 <- missingUF310 %>% filter(missingUF310$Fish != "Bocaccio")
missingUF310$Fish <- "UF310"

MB01_fix <- rbind(MB01_sort, missingUF310)

#seperate fish by type
X<-split(MB01_fix, MB01_fix$Fish)
Bocaccio=as.data.frame(X$Bocaccio)
Midshipman=as.data.frame(X$Midshipman)
HF=as.data.frame(X$HF)
WhiteSeabass=as.data.frame(X$"White Seabass")
Noise=as.data.frame(X$"Off Effort")
UF310=as.data.frame(X$UF310)

#sorting fish seperately by date and summing acoustic hours per day (change based on what fish you have)
#Bocaccio
Bocaccio_df = Bocaccio %>% group_by(Date) %>% 
  summarise(Acoustic_Hours = sum(Acoustic_Hours))
#Midshipman
Midshipman_df = Midshipman %>% group_by(Date) %>% 
  summarise(Acoustic_Hours = sum(Acoustic_Hours))
#HF
HF_df = HF %>% group_by(Date) %>% 
  summarise(Acoustic_Hours = sum(Acoustic_Hours))
#White Seabass
WS_df = WhiteSeabass %>% group_by(Date) %>% 
  summarise(Acoustic_Hours = sum(Acoustic_Hours))
#Noise
Noise_df = Noise %>% group_by(Date) %>% 
  summarise(Acoustic_Hours = sum(Acoustic_Hours))
#UF310
UF310_df = UF310 %>% group_by(Date) %>% 
  summarise(Acoustic_Hours = sum(Acoustic_Hours))


# MAKING INDIVIDUAL FISH PLOTS

#MIDSHIPMAN
#add dates with NA that are missing from the timeline
Midshipman_dates_fixed=Midshipman_df %>% mutate(Date = as.Date(Midshipman_df$Date)) %>% complete(Date = seq.Date(min(dateMB01), max(dateMB01), by="day"))
#replace NA with zeroes
Midshipman_dates_fixed$Acoustic_Hours[is.na(Midshipman_dates_fixed$Acoustic_Hours)]<-0 #changed this to make it show no effort times

#renaming data for easier use
MB01Midshipman_NEW <- Midshipman_dates_fixed 
MB01Midshipman_NEW$Date <- ymd(MB01Midshipman_NEW$Date)
MB01Midshipman_NEW$Week  <- week(MB01Midshipman_NEW$Date)

#Boxplot
MB01Midshipman_NEW$Week = as.factor(MB01Midshipman_NEW$Week)

MB01MidshipmanBox=ggplot(MB01Midshipman_NEW, aes(x=Week, y = Acoustic_Hours)) + 
  geom_boxplot(fill = "darkorange") +
  ylab("")+
  xlab("")+
  ggtitle("Midshipman")+ 
  theme_classic()+
  scale_x_discrete( breaks=c("1","5","9","13","18","22","26","31","35","40","44","48"),labels =c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) 

MB01MidshipmanBox


#BOCACCIO 
#add dates with NA that are missing from the timeline
Bocaccio_dates_fixed=Bocaccio_df %>% mutate(Date = as.Date(Bocaccio_df$Date)) %>% complete(Date = seq.Date(min(dateMB01), max(dateMB01), by="day"))
#replace NA with zeroes
Bocaccio_dates_fixed$Acoustic_Hours[is.na(Bocaccio_dates_fixed$Acoustic_Hours)]<-0

#renaming for easier use
MB01Bocaccio_NEW <- Bocaccio_dates_fixed 
MB01Bocaccio_NEW$Date <- ymd(MB01Bocaccio_NEW$Date)
MB01Bocaccio_NEW$Week  <- week(MB01Bocaccio_NEW$Date)


#Boxplot
MB01Bocaccio_NEW$Week = as.factor(MB01Bocaccio_NEW$Week)

sum(is.na(MB01Bocaccio_NEW$Acoustic_Hours))  # Count of NA values
sum(is.infinite(MB01Bocaccio_NEW$Acoustic_Hours))

MB01BocaccioBox=ggplot(MB01Bocaccio_NEW, aes(x=Week, y = Acoustic_Hours)) + 
  geom_boxplot(fill = "deepskyblue") +
  ylab("")+
  xlab("")+
  ggtitle("Bocaccio")+ 
  theme_classic()+
  scale_x_discrete( breaks=c("1","5","9","13","18","22","26","31","35","40","44","48"),labels =c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) 

MB01BocaccioBox


#HF 
#add dates with NA that are missing from the timeline
HF_dates_fixed=HF_df %>% mutate(Date = as.Date(HF_df$Date)) %>% complete(Date = seq.Date(min(dateMB01), max(dateMB01), by="day"))
#replace NA with zeroes
HF_dates_fixed$Acoustic_Hours[is.na(HF_dates_fixed$Acoustic_Hours)]<-0

#renaming
MB01HF_NEW <- HF_dates_fixed 
MB01HF_NEW$Date <- ymd(MB01HF_NEW$Date)
MB01HF_NEW$Week  <- week(MB01HF_NEW$Date)


#Boxplot
MB01HF_NEW$Week = as.factor(MB01HF_NEW$Week)

MB01HFBox=ggplot(MB01HF_NEW, aes(x=Week, y = Acoustic_Hours)) + 
  geom_boxplot(fill = "darkorchid3") +
  ylab("Acoustic Presence (Hrs)")+
  xlab("")+
  ggtitle("UF440")+ 
  theme_classic()+
  scale_x_discrete( breaks=c("1","5","9","13","18","22","26","31","35","40","44","48"),labels =c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) 

MB01HFBox


#WHITE SEABASS
#add dates with NA that are missing from the timeline
WS_dates_fixed=WS_df %>% mutate(Date = as.Date(WS_df$Date)) %>% complete(Date = seq.Date(min(dateMB01), max(dateMB01), by="day"))
#replace NA with zeroes
WS_dates_fixed$Acoustic_Hours[is.na(WS_dates_fixed$Acoustic_Hours)]<-0

#renaming
MB01WS_NEW <- WS_dates_fixed
MB01WS_NEW$Date <- ymd(MB01WS_NEW$Date)
MB01WS_NEW$Week  <- week(MB01WS_NEW$Date)


#Boxplot
MB01WS_NEW$Week = as.factor(MB01WS_NEW$Week)

MB01WSBox=ggplot(MB01WS_NEW, aes(x=Week, y = Acoustic_Hours)) + 
  geom_boxplot(fill = "firebrick2") +
  ylab("")+
  xlab("")+
  ggtitle("White Seabass")+ 
  theme_classic()+
  scale_x_discrete( breaks=c("1","5","9","13","18","22","26","31","35","40","44","48"),labels =c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) 

MB01WSBox


#UF310
#add dates with NA that are missing from the timeline
UF310_dates_fixed=UF310_df %>% mutate(Date = as.Date(UF310_df$Date)) %>% complete(Date = seq.Date(min(dateMB01), max(dateMB01), by="day"))
#replace NA with zeroes
UF310_dates_fixed$Acoustic_Hours[is.na(UF310_dates_fixed$Acoustic_Hours)]<-0

#renaming
MB01UF310_NEW <- UF310_dates_fixed
MB01UF310_NEW$Date <- ymd(MB01UF310_NEW$Date)
MB01UF310_NEW$Week  <- week(MB01UF310_NEW$Date)

#Boxplot
MB01UF310_NEW$Week = as.factor(MB01UF310_NEW$Week)

MB01UF310Box=ggplot(MB01UF310_NEW, aes(x=Week, y = Acoustic_Hours)) + 
  geom_boxplot(fill = "green3") +
  ylab("")+
  xlab("")+
  ggtitle("UF310")+ 
  ylim(0,30)+
  theme_classic()+
  scale_x_discrete( breaks=c("1","5","9","13","18","22","26","31","35","40","44","48"),labels =c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) 

MB01UF310Box


# MAKING ACOUSTIC EFFORT PLOT BELOW

#upload deployment dates: here you need to upload deployment start and end dates (when data was being recorded at this site)
MB01_deployment_dates=read.csv("MB01Deployments.csv",header=TRUE)

#make dates readable by R
MB01_deployment_dates$Start <- mdy(MB01_deployment_dates$Start)
MB01_deployment_dates$End <- mdy(MB01_deployment_dates$End) 

#save into dataframes with just start and end times for each location (and adding comments for fish name)
MB01_effort <- MB01_deployment_dates 

#now we need to make the start datetime column just date (this deployment was already just in date)
MB01_effort$Start_date <-as.Date(MB01_effort$Start)
MB01_effort$End_date <-as.Date(MB01_effort$End)

#Now I want to make a sequence of dates from start to end date for each of the rows and then put 24 hours for each of those
MB01effort_days1 <- seq(as.Date(MB01_effort$Start_date[1]), as.Date(MB01_effort$End_date[1]), by = 'days')  
MB01effort_days2 <- seq(as.Date(MB01_effort$Start_date[2]), as.Date(MB01_effort$End_date[2]), by = 'days')  
MB01effort_days3 <- seq(as.Date(MB01_effort$Start_date[3]), as.Date(MB01_effort$End_date[3]), by = 'days')  
MB01effort_days4 <- seq(as.Date(MB01_effort$Start_date[4]), as.Date(MB01_effort$End_date[4]), by = 'days')  
MB01effort_days5 <- seq(as.Date(MB01_effort$Start_date[5]), as.Date(MB01_effort$End_date[5]), by = 'days')  
MB01effort_days6 <- seq(as.Date(MB01_effort$Start_date[6]), as.Date(MB01_effort$End_date[6]), by = 'days')  
MB01effort_days7 <- seq(as.Date(MB01_effort$Start_date[7]), as.Date(MB01_effort$End_date[7]), by = 'days')  
MB01effort_days8 <- seq(as.Date(MB01_effort$Start_date[8]), as.Date(MB01_effort$End_date[8]), by = 'days')  
MB01effort_days9 <- seq(as.Date(MB01_effort$Start_date[9]), as.Date(MB01_effort$End_date[9]), by = 'days')  
MB01effort_days11 <- seq(as.Date(MB01_effort$Start_date[10]), as.Date(MB01_effort$End_date[10]), by = 'days')  
MB01effort_days12 <- seq(as.Date(MB01_effort$Start_date[11]), as.Date(MB01_effort$End_date[11]), by = 'days')  

#combine all of these into one array
MB01_effort_dates = c(MB01effort_days1,MB01effort_days2,MB01effort_days3, MB01effort_days4, MB01effort_days5 ,MB01effort_days6 ,MB01effort_days7 ,MB01effort_days8 ,MB01effort_days9, MB01effort_days11 ,MB01effort_days12)

#save array as data frame and add a row that is 24 for each of the days
#times = length of MB01_effort_dates
test<-rep(c(24),times=1421) 
dates_test<-cbind(MB01_effort_dates,test)
MB01_date_hours=as.data.frame(dates_test)
names(MB01_date_hours)[1] <-"numdate"
names(MB01_date_hours)[2] <-"acoustic_hours"
MB01_date_hours$date<-as.Date.numeric(MB01_date_hours$numdate)
#resave just those 2 columns
MB01_effort2 <- MB01_date_hours %>% 
  dplyr::select(date,acoustic_hours)

#Now adapt to weekly sums
MB01_effort2$date <- ymd(MB01_effort2$date)
MB01_effort2$Week  <- week(MB01_effort2$date)
MB01_effort_weekly<-aggregate(acoustic_hours ~ Week,MB01_effort2, sum)

#Plot
#Effort weekly mean plot with error bars
MB01Effort=ggplot(MB01_effort_weekly, aes(x=Week, y=acoustic_hours)) + 
  ggtitle("Available Data")+ 
  geom_bar(stat = "identity",fill = "grey70")+ylim(0,900)+xlab("Month (by Week)")+ylab("Total Hours")+theme_classic()+
  #theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())
  scale_x_continuous(limits = c(0,53), expand = c(0, 0), breaks=c(1,5,9,13,18,22,26,31,35,40,44,48),labels = c("J","F","M","A","M","J","J","A","S","O","N","D")) +
  scale_y_continuous(limits = c(0,900), expand = c(0, 0), breaks=c(1,400, 900),labels=c("0","450", "900")) #,labels=c("0","1K") MAY NEED TO ADD THIS BACK IF SHIFTED 
#note here that I changed labels to "0" to "1k" because the existing labels shifted the whole graph to the left because they were too long and then the plots didn't line up
#but make sure that the scales are correct and appropriate. if possible make the scale the same as other plots that ella made
MB01Effort


#PLOT EVERYTHING TOGETHER
#split into two plots because one wouldnt fit well in r display window
grid.arrange(
  arrangeGrob(MB01BocaccioBox,MB01MidshipmanBox, MB01HFBox, nrow = 3),
  top = "Seasonal Fish Chorusing at MB01")

grid.arrange(
  arrangeGrob( MB01WSBox, MB01UF310Box, MB01Effort, nrow = 3))


# ><(((*>  <*)))><   ><(((*>  <*)))><  ><(((*>  <*)))><  ><(((*>  <*)))><  ><(((*>  <*)))><  ><(((*>  <*)))><  

# MB02! (Just overall Histograms and Boxplots from whole deployement seperated by fish species, no comparison plots yet)
#set working directory
setwd("/Users/emmaberetta/Desktop/NMSF2024/csvdetections/MBNMS Logs/MB02")   

#read all CSVs for all deployments
#no MB02_8 or MB01_09 b/c off effort
MB02_01_data=read.csv("MB02_01.csv",header=TRUE)
MB02_02_data=read.csv("MB02_02.csv",header=TRUE)
MB02_03_data=read.csv("MB02_03.csv",header=TRUE)
MB02_04_data=read.csv("MB02_04.csv",header=TRUE)
MB02_05_data=read.csv("MB02_05.csv",header=TRUE)
MB02_06_data=read.csv("MB02_06.csv",header=TRUE)
MB02_07_data=read.csv("MB02_07.csv",header=TRUE)
MB02_10_data=read.csv("MB02_10 - Sheet1.csv",header=TRUE)
MB02_11_data=read.csv("MB02_11 - Sheet1.csv",header=TRUE)
MB02_12_data=read.csv("MB02_12 - Sheet1.csv",header=TRUE)

#MB02_10 had 13 columns so fix others so that you can merge
MB02_01_data <- MB02_01_data[1:13]
MB02_02_data <- MB02_02_data[1:13]
MB02_03_data <- MB02_03_data[1:13]
MB02_04_data <- MB02_04_data[1:13]
MB02_05_data <- MB02_05_data[1:13]
MB02_06_data <- MB02_06_data[1:13]
MB02_07_data <- MB02_07_data[1:13]
MB02_11_data <- MB02_11_data[1:13]
MB02_12_data <- MB02_12_data[1:13]

#importing midshipman logs because they were logged seperately
MB02_01_midshipman=read.csv("MB02_01_midshipman.csv")
MB02_02_midshipman=read.csv("MB02_02_midshipman.csv")
MB02_03_midshipman=read.csv("MB02_03_midshipman.csv")
MB02_04_midshipman=read.csv("MB02_04_midshipman.csv")
MB02_05_midshipman=read.csv("MB02_05_midshipman.csv")
MB02_06_midshipman=read.csv("MB02_06_midshipman.csv")

MB02_01_midshipman <- MB02_01_midshipman[1:13]
MB02_02_midshipman <- MB02_02_midshipman[1:13]
MB02_03_midshipman <- MB02_03_midshipman[1:13]
MB02_04_midshipman <- MB02_04_midshipman[1:13]
MB02_05_midshipman <- MB02_05_midshipman[1:13]
MB02_06_midshipman <- MB02_06_midshipman[1:13]

# Combine CSV files into one matrix for each site
MB02=rbind(MB02_01_data, MB02_02_data, MB02_03_data, MB02_04_data, MB02_05_data, MB02_06_data, MB02_07_data, MB02_10_data, MB02_11_data, MB02_12_data, MB02_01_midshipman, MB02_02_midshipman, MB02_03_midshipman, MB02_04_midshipman, MB02_05_midshipman, MB02_06_midshipman)

# Subset so that we only have the three columns we need
MB02 <- MB02 %>% 
  dplyr::select("Start.time", "End.time", "Comments")

#fIXING START AND END TIMES SO THAT IN USABLE FORMAT
#Creates column with start data/time in format that R can read
MB02$combine_start <- mdy_hms(MB02$Start.time)

#Creates column with end data/time in format that R can read
MB02$combine_end <- mdy_hms(MB02$End.time) 

#make a sequence of dates from beginning of earliest deployment to end of latest deployment
dateMB02 <- seq(as.Date('2018-11-16'), as.Date('2023-08-17'), by = 'days')  # create sequence of days 

names(MB02)[4] <-"Start"
names(MB02)[5] <-"End"
names(MB02)[3] <-"Fish"

#save into dataframes with just start and end times for each location (and adding comments for fish name)
MB02new <- MB02 %>% 
  dplyr::select(Fish,Start,End)

#Time difference
MB02_diff=as.data.frame(difftime(MB02new$End,MB02new$Start))
MB02_diff=(MB02_diff)/3600 #change to hours

#add this to data frame
MB02new$Acoustic_Hours=difftime(MB02new$End,MB02new$Start)
MB02new$Acoustic_Hours=(MB02new$Acoustic_Hours)/3600

#get rid of units
MB02new$Acoustic_Hours=as.vector(MB02new$Acoustic_Hours)

#now we need to make the start datetime column just date
MB02new$Date <- ymd_hms(MB02new$Start)
MB02new$Date <-as.Date(MB02new$Date)

MB02_sort <- MB02new %>%
  dplyr::select(Fish,Date,Acoustic_Hours)

#Rename Bocaccio sunrise and Bocaccio sunset to Bocaccio, and HF sunset and HF sunset to HF. THis is because we're just making one histogram for all of those together
#also fix typos and capitalization
MB02_sort$Fish <- gsub('Bocaccio sunset','Bocaccio', MB02_sort$Fish)
MB02_sort$Fish <- gsub('Bocaccio sunrise','Bocaccio',MB02_sort$Fish)
MB02_sort$Fish <- gsub('Bpcaccio sunset','Bocaccio',MB02_sort$Fish)

MB02_sort$Fish <- gsub('HF sunset','HF', MB02_sort$Fish)
MB02_sort$Fish <- gsub('HF sunrise','HF',MB02_sort$Fish)

MB02_sort$Fish <- gsub('Midhsipman','Midshipman',MB02_sort$Fish)

#replace all HF names
MB02_sort$Fish <- gsub('Mystery','UF310',MB02_sort$Fish)
MB02_sort$Fish <- gsub('Lingcod','White Seabass',MB02_sort$Fish)
MB02_sort$Fish <- gsub('Mystery high','UF310',MB02_sort$Fish)
MB02_sort$Fish <- gsub('UF310 high','UF310',MB02_sort$Fish)

MB02_sort$Fish <- gsub('Off effort','Off Effort',MB02_sort$Fish)

#first figuring out diff names of fish
names_of_fish = MB02_sort %>% group_by(Fish) %>% 
  summarise(Acoustic_Hours = sum(Acoustic_Hours))


#separate the fish type
X<-split(MB02_sort, MB02_sort$Fish)
Bocaccio=as.data.frame(X$Bocaccio)
Midshipman=as.data.frame(X$Midshipman)
HF=as.data.frame(X$HF)
WhiteSeabass=as.data.frame(X$"White Seabass")
UF310=as.data.frame(X$UF310)
Noise=as.data.frame(X$"Off Effort")

#all together CI sorted
MB02_sort2 = MB02_sort %>% group_by(Date,Fish) %>% 
  summarise(Acoustic_Hours = sum(Acoustic_Hours))


#sorting fish seperately by date and summing acoustic hours per day (change based on what fish you have)
#Bocaccio
Bocaccio_df = Bocaccio %>% group_by(Date) %>% 
  summarise(Acoustic_Hours = sum(Acoustic_Hours))
#Midshipman
Midshipman_df = Midshipman %>% group_by(Date) %>% 
  summarise(Acoustic_Hours = sum(Acoustic_Hours))
#HF
HF_df = HF %>% group_by(Date) %>% 
  summarise(Acoustic_Hours = sum(Acoustic_Hours))
#WhiteSeabass
WhiteSeabass_df = WhiteSeabass %>% group_by(Date) %>% 
  summarise(Acoustic_Hours = sum(Acoustic_Hours))
#Noise
Noise_df = Noise %>% group_by(Date) %>% 
  summarise(Acoustic_Hours = sum(Acoustic_Hours))
#UF310
UF310_df = UF310 %>% group_by(Date) %>% 
  summarise(Acoustic_Hours = sum(Acoustic_Hours))


# MAKING INDIVIDUAL BOXPLOTS

#MIDSHIPMAN
#add dates with NA that are missing from the timeline
Midshipman_dates_fixed=Midshipman_df %>% mutate(Date = as.Date(Midshipman_df$Date)) %>% complete(Date = seq.Date(min(dateMB02), max(dateMB02), by="day"))
#replace NA with zeroes
Midshipman_dates_fixed$Acoustic_Hours[is.na(Midshipman_dates_fixed$Acoustic_Hours)]<-0 #changed this to make it show no effort times

MB02Midshipman_NEW <- Midshipman_dates_fixed 
MB02Midshipman_NEW$Date <- ymd(MB02Midshipman_NEW$Date)
MB02Midshipman_NEW$Week  <- week(MB02Midshipman_NEW$Date)


#Boxplot
MB02Midshipman_NEW$Week = as.factor(MB02Midshipman_NEW$Week)

MB02MidshipmanBox=ggplot(MB02Midshipman_NEW, aes(x=Week, y = Acoustic_Hours)) + 
  geom_boxplot(fill = "darkorange") +
  ylab("")+
  xlab("")+
  ggtitle("Midshipman")+ 
  ylim(0,30)+
  theme_classic()+
  scale_x_discrete( breaks=c("1","5","9","13","18","22","26","31","35","40","44","48"),labels =c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) 

MB02MidshipmanBox


#BOCACCIO 
#add dates with NA that are missing from the timeline
Bocaccio_dates_fixed=Bocaccio_df %>% mutate(Date = as.Date(Bocaccio_df$Date)) %>% complete(Date = seq.Date(min(dateMB02), max(dateMB02), by="day"))
#replace NA with zeroes
Bocaccio_dates_fixed$Acoustic_Hours[is.na(Bocaccio_dates_fixed$Acoustic_Hours)]<-0

MB02Bocaccio_NEW <- Bocaccio_dates_fixed 
MB02Bocaccio_NEW$Date <- ymd(MB02Bocaccio_NEW$Date)
MB02Bocaccio_NEW$Week  <- week(MB02Bocaccio_NEW$Date)

#Boxplot
MB02Bocaccio_NEW$Week = as.factor(MB02Bocaccio_NEW$Week)

MB02BocaccioBox=ggplot(MB02Bocaccio_NEW, aes(x=Week, y = Acoustic_Hours)) + 
  geom_boxplot(fill = "deepskyblue") +
  ylab("")+
  xlab("")+
  ggtitle("Bocaccio")+ 
  ylim(0,30)+
  theme_classic()+
  scale_x_discrete( breaks=c("1","5","9","13","18","22","26","31","35","40","44","48"),labels =c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) 

MB02BocaccioBox


#HF 
#add dates with NA that are missing from the timeline
HF_dates_fixed=HF_df %>% mutate(Date = as.Date(HF_df$Date)) %>% complete(Date = seq.Date(min(dateMB02), max(dateMB02), by="day"))
#replace NA with zeroes
HF_dates_fixed$Acoustic_Hours[is.na(HF_dates_fixed$Acoustic_Hours)]<-0

MB02HF_NEW <- HF_dates_fixed 
MB02HF_NEW$Date <- ymd(MB02HF_NEW$Date)
MB02HF_NEW$Week  <- week(MB02HF_NEW$Date)


#Boxplot
MB02HF_NEW$Week = as.factor(MB02HF_NEW$Week)

MB02HFBox=ggplot(MB02Bocaccio_NEW, aes(x=Week, y = Acoustic_Hours)) + 
  geom_boxplot(fill = "darkorchid3") +
  ylab("Acoustic Presence (Hours)")+
  xlab("")+
  ggtitle("UF440")+ 
  ylim(0,30)+
  theme_classic()+
  scale_x_discrete( breaks=c("1","5","9","13","18","22","26","31","35","40","44","48"),labels =c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) 

MB02HFBox


#WHITE SEABASS
#add dates with NA that are missing from the timeline
WS_dates_fixed=WhiteSeabass_df %>% mutate(Date = as.Date(WhiteSeabass_df$Date)) %>% complete(Date = seq.Date(min(dateMB02), max(dateMB02), by="day"))
#replace NA with zeroes
WS_dates_fixed$Acoustic_Hours[is.na(WS_dates_fixed$Acoustic_Hours)]<-0

MB02WS_NEW <- WS_dates_fixed 
MB02WS_NEW$Date <- ymd(MB02WS_NEW$Date)
MB02WS_NEW$Week  <- week(MB02WS_NEW$Date)


#Boxplot
MB02WS_NEW$Week = as.factor(MB02WS_NEW$Week)

MB02WSBox=ggplot(MB02WS_NEW, aes(x=Week, y = Acoustic_Hours)) + 
  geom_boxplot(fill = "firebrick2") +
  ylab("")+
  xlab("")+
  ggtitle("White Seabass")+ 
  #ylim(0,30)+
  theme_classic()+
  scale_x_discrete( breaks=c("1","5","9","13","18","22","26","31","35","40","44","48"),labels =c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) 

MB02WSBox


#UF310 
#add dates with NA that are missing from the timeline
UF310_dates_fixed=UF310_df %>% mutate(Date = as.Date(UF310_df$Date)) %>% complete(Date = seq.Date(min(dateMB02), max(dateMB02), by="day"))
#replace NA with zeroes
UF310_dates_fixed$Acoustic_Hours[is.na(UF310_dates_fixed$Acoustic_Hours)]<-0

MB02UF310_NEW <- UF310_dates_fixed
MB02UF310_NEW$Date <- ymd(MB02UF310_NEW$Date)
MB02UF310_NEW$Week  <- week(MB02UF310_NEW$Date)


#Boxplot
MB02UF310_NEW$Week = as.factor(MB02UF310_NEW$Week)

MB02UF310Box=ggplot(MB02UF310_NEW, aes(x=Week, y = Acoustic_Hours)) + 
  geom_boxplot(fill = "green3") +
  ylab("")+
  xlab("")+
  ggtitle("UF310")+ 
  #ylim(0,30)+
  theme_classic()+
  scale_x_discrete( breaks=c("1","5","9","13","18","22","26","31","35","40","44","48"),labels =c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) 

MB02UF310Box


# ACOUSTIC EFFORT PLOTS BELOW

#upload deployment dates: here you need to upload deployment start and end dates 
#set working directory 
MB02_deployment_dates=read.csv("MB02Deployments.csv",header=TRUE)

#make dates readable by R
MB02_deployment_dates$Start <- mdy(MB02_deployment_dates$Start) #issue here was that it was mdy_hms and we had no seconds so it made the date all 2020 for some reason... might need to change this prior
MB02_deployment_dates$End <- mdy(MB02_deployment_dates$End) 

#save into dataframes with just start and end times for each location (and adding comments for fish name)
MB02_effort <- MB02_deployment_dates

#now we need to make the start datetime column just date
MB02_effort$Start_date <-as.Date(MB02_effort$Start)
MB02_effort$End_date <-as.Date(MB02_effort$End)

#Now I want to make a sequence of dates from start to end date for each of the rows and then put 24 hours for each of those
MB02effort_days1 <- seq(as.Date(MB02_effort$Start_date[1]), as.Date(MB02_effort$End_date[1]), by = 'days')  
MB02effort_days2 <- seq(as.Date(MB02_effort$Start_date[2]), as.Date(MB02_effort$End_date[2]), by = 'days')  
MB02effort_days3 <- seq(as.Date(MB02_effort$Start_date[3]), as.Date(MB02_effort$End_date[3]), by = 'days')  
MB02effort_days4 <- seq(as.Date(MB02_effort$Start_date[4]), as.Date(MB02_effort$End_date[4]), by = 'days')  
MB02effort_days5 <- seq(as.Date(MB02_effort$Start_date[5]), as.Date(MB02_effort$End_date[5]), by = 'days')  
MB02effort_days6 <- seq(as.Date(MB02_effort$Start_date[6]), as.Date(MB02_effort$End_date[6]), by = 'days')  
MB02effort_days7 <- seq(as.Date(MB02_effort$Start_date[7]), as.Date(MB02_effort$End_date[7]), by = 'days')  
MB02effort_days10 <- seq(as.Date(MB02_effort$Start_date[8]), as.Date(MB02_effort$End_date[8]), by = 'days')  
MB02effort_days11 <- seq(as.Date(MB02_effort$Start_date[9]), as.Date(MB02_effort$End_date[9]), by = 'days')  
MB02effort_days12 <- seq(as.Date(MB02_effort$Start_date[10]), as.Date(MB02_effort$End_date[10]), by = 'days')  

#combine all of these into one array
MB02_effort_dates = c(MB02effort_days1,MB02effort_days2,MB02effort_days3, MB02effort_days4, MB02effort_days5 ,MB02effort_days6 ,MB02effort_days7 ,MB02effort_days10 ,MB02effort_days11 ,MB02effort_days12)

#save array as data frame and add a row that is 24 for each of the days
#times = length of MB02_effort_dates
test<-rep(c(24),times=1188) 
dates_test<-cbind(MB02_effort_dates,test)
MB02_date_hours=as.data.frame(dates_test)
names(MB02_date_hours)[1] <-"numdate"
names(MB02_date_hours)[2] <-"acoustic_hours"
MB02_date_hours$date<-as.Date.numeric(MB02_date_hours$numdate)
#resave a just those 2 columns
MB02_effort2 <- MB02_date_hours %>% 
  dplyr::select(date,acoustic_hours)

#Now adapt to weekly sums
MB02_effort2$date <- ymd(MB02_effort2$date)
MB02_effort2$Week  <- week(MB02_effort2$date)
MB02_effort_weekly<-aggregate(acoustic_hours ~ Week,MB02_effort2, sum)


#Plot
#Effort weekly mean plot with error bars
MB02Effort=ggplot(MB02_effort_weekly, aes(x=Week, y=acoustic_hours)) + 
  ggtitle("Available Data")+ 
  geom_bar(stat = "identity",fill = "grey70")+ylim(0,900)+xlab("Month (by Week)")+ylab("Total Hours")+theme_classic()+
  #theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())
  scale_x_continuous(limits = c(0,53), expand = c(0, 0), breaks=c(1,5,9,13,18,22,26,31,35,40,44,48),labels = c("J","F","M","A","M","J","J","A","S","O","N","D")) +
  scale_y_continuous(limits = c(0,900), expand = c(0, 0), breaks=c(1,400, 900),labels=c("0","450", "900")) #,labels=c("0","1K") MAY NEED TO ADD THIS BACK IF SHIFTED 
#note here that I changed labels to "0" to "1k" because the existing labels shifted the whole graph to the left because they were too long and then the plots didn't line up
#but make sure that the scales are correct and appropriate. if possible make the scale the same as other plots that ella made
MB02Effort


#PLOT EVERYTHING TOGETHER
#split graph in two because wouldnt display well in r window in one
grid.arrange(
  arrangeGrob(MB02BocaccioBox,MB02MidshipmanBox, MB02HFBox, nrow = 3),
  top = "Seasonal Fish Chorusing at MB02")

grid.arrange(
  arrangeGrob( MB02WSBox, MB02UF310Box, MB02Effort, nrow = 3))


# ><(((*>  <*)))><   ><(((*>  <*)))><  ><(((*>  <*)))><  ><(((*>  <*)))><  ><(((*>  <*)))><  ><(((*>  <*)))><  
# ><(((*>  <*)))><   ><(((*>  <*)))><  ><(((*>  <*)))><  ><(((*>  <*)))><  ><(((*>  <*)))><  ><(((*>  <*)))><  

#3. SEASONAL HISTOGRAM for MONTHLY NORMALIZED TOTAL hours COMPARING MOST RECENT YEAR OF DATA TO EVERYTHING BEFORE

# ><(((*>  <*)))><   ><(((*>  <*)))><  ><(((*>  <*)))><  ><(((*>  <*)))><  ><(((*>  <*)))><  ><(((*>  <*)))><  
#only for MB01 because CH01 only currently has 1.5 yrs of data, so not enough to make comparison

#set directory
setwd("/Users/emmaberetta/Desktop/NMSF2024/csvdetections/MBNMS Logs/MB01")  

#making RECENT PLOT for August 2022-2023
#block out 2022-11-21 to 2023-02-03!!! which falls on week 47 to week 5 of the next year
dateMB01202223 <- seq(as.Date('2022-08-01'), as.Date('2023-07-31'), by = 'days')  # create sequence of days 

#format data from MB01_fix to make recent dataset
MB01_fix$Year <- str_sub(MB01_fix$Date, 1, 4)
MB01_fix$Month <- str_sub(MB01_fix$Date, 6,7)

MB01_fix$Year <- as.numeric(MB01_fix$Year)
MB01_fix$Month <- as.numeric(MB01_fix$Month)

#make 2022-23 datasets
MB01_recentA <- filter(MB01_fix, Year == 2022)
MB01_recentB <- filter(MB01_fix, Year == 2023)
MB01_recentA <- filter(MB01_recentA, Month > 7)
MB01_recentB <- filter(MB01_recentB, Month < 8)
MB01_recent <- rbind(MB01_recentA, MB01_recentB)

#split data by fish species
X<-split(MB01_recent, MB01_recent$Fish)
Bocaccio=as.data.frame(X$Bocaccio)
Midshipman=as.data.frame(X$Midshipman)
HF=as.data.frame(X$HF)
WhiteSeabass=as.data.frame(X$"White Seabass")
Noise=as.data.frame(X$"Off Effort")
UF310=as.data.frame(X$UF310)

#sorting fish seperately by date and summing acoustic hours per day (change based on what fish you have)
#Bocaccio
Bocaccio_df = Bocaccio %>% group_by(Date) %>% 
  summarise(Acoustic_Hours = sum(Acoustic_Hours))
#Midshipman
Midshipman_df = Midshipman %>% group_by(Date) %>% 
  summarise(Acoustic_Hours = sum(Acoustic_Hours))
#HF
HF_df = HF %>% group_by(Date) %>% 
  summarise(Acoustic_Hours = sum(Acoustic_Hours))
#White Seabass
WS_df = WhiteSeabass %>% group_by(Date) %>% 
  summarise(Acoustic_Hours = sum(Acoustic_Hours))
#Noise
Noise_df = Noise %>% group_by(Date) %>% 
  summarise(Acoustic_Hours = sum(Acoustic_Hours))
#UF310
UF310_df = UF310 %>% group_by(Date) %>% 
  summarise(Acoustic_Hours = sum(Acoustic_Hours))


# ACOUSTIC EFFORT data normalized (total acoustic effort/ total acoustic recorded hours)
#import deployment dates (days where data was recorded)
MB01_deployment_dates=read.csv("MB01Deployments.csv",header=TRUE)

#make dates readable by R
MB01_deployment_dates$Start <- mdy(MB01_deployment_dates$Start)
MB01_deployment_dates$End <- mdy(MB01_deployment_dates$End) 

#save into dataframes with just start and end times for each location (and adding comments for fish name)
MB01_effort <- MB01_deployment_dates 
#names(MB02_effort)[2] <-"Start"
#names(MB02_effort)[3] <-"End"

#now we need to make the start datetime column just date (this deployment was already just in date)
MB01_effort$Start_date <-as.Date(MB01_effort$Start)
MB01_effort$End_date <-as.Date(MB01_effort$End)

#Now I want to make a sequence of dates from start to end date for each of the rows and then put 24 hours for each of those
MB01effort_days1 <- seq(as.Date(MB01_effort$Start_date[1]), as.Date(MB01_effort$End_date[1]), by = 'days')  
MB01effort_days2 <- seq(as.Date(MB01_effort$Start_date[2]), as.Date(MB01_effort$End_date[2]), by = 'days')  
MB01effort_days3 <- seq(as.Date(MB01_effort$Start_date[3]), as.Date(MB01_effort$End_date[3]), by = 'days')  
MB01effort_days4 <- seq(as.Date(MB01_effort$Start_date[4]), as.Date(MB01_effort$End_date[4]), by = 'days')  
MB01effort_days5 <- seq(as.Date(MB01_effort$Start_date[5]), as.Date(MB01_effort$End_date[5]), by = 'days')  
MB01effort_days6 <- seq(as.Date(MB01_effort$Start_date[6]), as.Date(MB01_effort$End_date[6]), by = 'days')  
MB01effort_days7 <- seq(as.Date(MB01_effort$Start_date[7]), as.Date(MB01_effort$End_date[7]), by = 'days')  
MB01effort_days8 <- seq(as.Date(MB01_effort$Start_date[8]), as.Date(MB01_effort$End_date[8]), by = 'days')  
MB01effort_days9 <- seq(as.Date(MB01_effort$Start_date[9]), as.Date(MB01_effort$End_date[9]), by = 'days')  
MB01effort_days11 <- seq(as.Date(MB01_effort$Start_date[10]), as.Date(MB01_effort$End_date[10]), by = 'days')  
MB01effort_days12 <- seq(as.Date(MB01_effort$Start_date[11]), as.Date(MB01_effort$End_date[11]), by = 'days')  

#combine all of these into one array
MB01_effort_dates = c(MB01effort_days1,MB01effort_days2,MB01effort_days3, MB01effort_days4, MB01effort_days5 ,MB01effort_days6 ,MB01effort_days7 ,MB01effort_days8 ,MB01effort_days9, MB01effort_days11 ,MB01effort_days12)

#save array as data frame and add a row that is 24 for each of the days
#times = number of rows in MB01_effort_dates
test<-rep(c(24),times=1421) 
dates_test<-cbind(MB01_effort_dates,test)
MB01_date_hours=as.data.frame(dates_test)
names(MB01_date_hours)[1] <-"numdate"
names(MB01_date_hours)[2] <-"acoustic_hours"
MB01_date_hours$date<-as.Date.numeric(MB01_date_hours$numdate)
#resave just those 2 columns
MB01_effort2 <- MB01_date_hours %>% 
  dplyr::select(date,acoustic_hours)

#keep only august 2022- 2023
MB01_effort2$Year <- str_sub(MB01_effort2$date, 1, 4)
MB01_effort2$Month <- str_sub(MB01_effort2$date, 6,7)

MB01_effort2$Year <- as.numeric(MB01_effort2$Year)
MB01_effort2$Month <- as.numeric(MB01_effort2$Month)

MB01_eA <- filter(MB01_effort2, Year == 2022)
MB01_eB <- filter(MB01_effort2, Year == 2023)
MB01_eA <- filter(MB01_eA, Month > 7)
MB01_eB <- filter(MB01_eB, Month < 8)
MB01_effortR <- rbind(MB01_eA, MB01_eB)

#aggregate by month
MB01_effort_monthly<-aggregate(acoustic_hours ~ Month,MB01_effortR, sum)

#add in rows for january and december that have 0 acoustic hours
jan_row <- data.frame(
  Month = 1,
  acoustic_hours = 0,
  stringsAsFactors = FALSE
)

dec_row <- data.frame(
  Month = 12,
  acoustic_hours = 0,
  stringsAsFactors = FALSE
)

# Add the new row to the dataset
MB01_effort_monthlyR <- rbind(jan_row, MB01_effort_monthly, dec_row)


#MAKING DATA FOR RECENT DOTS on graph

#MIDSHIPMAN
#add dates with NA that are missing from the timeline
Midshipman_dates_fixed=Midshipman_df %>% mutate(Date = as.Date(Midshipman_df$Date)) %>% complete(Date = seq.Date(min(dateMB01202223), max(dateMB01202223), by="day"))
#replace NA with zeroes
Midshipman_dates_fixed$Acoustic_Hours[is.na(Midshipman_dates_fixed$Acoustic_Hours)]<-0 #changed this to make it show no effort times
#barplot
MB01Midshipman_NEW <- Midshipman_dates_fixed 

MB01Midshipman_NEW$Date <- ymd(MB01Midshipman_NEW$Date)
MB01Midshipman_NEW$Month  <- month(MB01Midshipman_NEW$Date)

Midshipman_monthR<-aggregate(Acoustic_Hours ~ Month,MB01Midshipman_NEW, sum)
Midshipman_monthR$Month = as.factor(Midshipman_monthR$Month)

#ADD 0 hours for month 1 and 12 to MB01_effort_monthly and then plug into equation below, repeat for all fish and also reference plot
#normalize acoustic hours
Midshipman_monthR$EffortHours <- MB01_effort_monthlyR$acoustic_hours
Midshipman_monthR$Acoustic_HoursNorm <- Midshipman_monthR$Acoustic_Hours / Midshipman_monthR$EffortHours

#make NaN 0 bacuse R makes 0/0 = NaN
Midshipman_monthR$Acoustic_HoursNorm[1] <- 0
Midshipman_monthR$Acoustic_HoursNorm[12] <- 0


#BOCACCIO 
#add dates with NA that are missing from the timeline
Bocaccio_dates_fixed=Bocaccio_df %>% mutate(Date = as.Date(Bocaccio_df$Date)) %>% complete(Date = seq.Date(min(dateMB01202223), max(dateMB01202223), by="day"))
#replace NA with zeroes
Bocaccio_dates_fixed$Acoustic_Hours[is.na(Bocaccio_dates_fixed$Acoustic_Hours)]<-0
#barplot
MB01Bocaccio_NEW <- Bocaccio_dates_fixed 

MB01Bocaccio_NEW$Date <- ymd(MB01Bocaccio_NEW$Date)
MB01Bocaccio_NEW$Month  <- month(MB01Bocaccio_NEW$Date)

Bocaccio_monthR<-aggregate(Acoustic_Hours ~ Month,MB01Bocaccio_NEW, sum)

Bocaccio_monthR$Month = as.factor(Bocaccio_monthR$Month)

#ADD 0 hours for month 1 and 12 to MB01_effort_monthly and then plug into equation below, repeat for all fish and also reference plot
#normalize acoustic hours
Bocaccio_monthR$EffortHours <- MB01_effort_monthlyR$acoustic_hours
Bocaccio_monthR$Acoustic_HoursNorm <- Bocaccio_monthR$Acoustic_Hours / Bocaccio_monthR$EffortHours

#make NaN 0 bacuse R makes 0/0 = NaN
Bocaccio_monthR$Acoustic_HoursNorm[1] <- 0
Bocaccio_monthR$Acoustic_HoursNorm[12] <- 0


#HF 
#add dates with NA that are missing from the timeline
HF_dates_fixed=HF_df %>% mutate(Date = as.Date(HF_df$Date)) %>% complete(Date = seq.Date(min(dateMB01202223), max(dateMB01202223), by="day"))
#replace NA with zeroes
HF_dates_fixed$Acoustic_Hours[is.na(HF_dates_fixed$Acoustic_Hours)]<-0
#barplot
MB01HF_NEW <- HF_dates_fixed 

MB01HF_NEW$Date <- ymd(MB01HF_NEW$Date)
MB01HF_NEW$Month  <- month(MB01HF_NEW$Date)

HF_df_monthR<-aggregate(Acoustic_Hours ~ Month,MB01HF_NEW, sum)
HF_df_monthR$Month = as.factor(HF_df_monthR$Month)

#ADD 0 hours for month 1 and 12 to MB01_effort_monthly and then plug into equation below, repeat for all fish and also reference plot
#normalize acoustic hours
HF_df_monthR$EffortHours <- MB01_effort_monthlyR$acoustic_hours
HF_df_monthR$Acoustic_HoursNorm <- HF_df_monthR$Acoustic_Hours / HF_df_monthR$EffortHours

#make NaN 0 bacuse R makes 0/0 = NaN
HF_df_monthR$Acoustic_HoursNorm[1] <- 0
HF_df_monthR$Acoustic_HoursNorm[12] <- 0


#WHITE SEABASS
#add dates with NA that are missing from the timeline
WS_dates_fixed=WS_df %>% mutate(Date = as.Date(WS_df$Date)) %>% complete(Date = seq.Date(min(dateMB01202223), max(dateMB01202223), by="day"))
#replace NA with zeroes
WS_dates_fixed$Acoustic_Hours[is.na(WS_dates_fixed$Acoustic_Hours)]<-0

MB01WS_NEW <- WS_dates_fixed 

MB01WS_NEW$Date <- ymd(MB01WS_NEW$Date)
MB01WS_NEW$Month  <- month(MB01WS_NEW$Date)

WS_df_monthR<-aggregate(Acoustic_Hours ~ Month,MB01WS_NEW, sum)
WS_df_monthR$Month = as.factor(WS_df_monthR$Month)

#ADD 0 hours for month 1 and 12 to MB01_effort_monthly and then plug into equation below, repeat for all fish and also reference plot
#normalize acoustic hours
WS_df_monthR$EffortHours <- MB01_effort_monthlyR$acoustic_hours
WS_df_monthR$Acoustic_HoursNorm <- WS_df_monthR$Acoustic_Hours / WS_df_monthR$EffortHours

#make NaN 0 bacuse R makes 0/0 = NaN
WS_df_monthR$Acoustic_HoursNorm[1] <- 0
WS_df_monthR$Acoustic_HoursNorm[12] <- 0


#UF310 
#add dates with NA that are missing from the timeline
UF310_dates_fixed=UF310_df %>% mutate(Date = as.Date(UF310_df$Date)) %>% complete(Date = seq.Date(min(dateMB01202223), max(dateMB01202223), by="day"))
#replace NA with zeroes
UF310_dates_fixed$Acoustic_Hours[is.na(UF310_dates_fixed$Acoustic_Hours)]<-0
#barplot
MB01UF310_NEW <- UF310_dates_fixed

MB01UF310_NEW$Date <- ymd(MB01UF310_NEW$Date)
MB01UF310_NEW$Month  <- month(MB01UF310_NEW$Date)

UF310_df_monthR<-aggregate(Acoustic_Hours ~ Month,MB01UF310_NEW, sum)

UF310_df_monthR$Month = as.factor(UF310_df_monthR$Month)

#ADD 0 hours for month 1 and 12 to MB01_effort_monthly and then plug into equation below, repeat for all fish and also reference plot
#normalize acoustic hours
UF310_df_monthR$EffortHours <- MB01_effort_monthlyR$acoustic_hours
UF310_df_monthR$Acoustic_HoursNorm <- UF310_df_monthR$Acoustic_Hours / UF310_df_monthR$EffortHours

#make NaN 0 bacuse R makes 0/0 = NaN
UF310_df_monthR$Acoustic_HoursNorm[1] <- 0
UF310_df_monthR$Acoustic_HoursNorm[12] <- 0


#Making REFERENCE (baseline) Plot with dots for recent year: 2019-2021 
#proportion of total chorusing for each month over three years divided by total effort hours over three years, for each fish
dateMB01B <- seq(as.Date('2019-01-01'), as.Date('2021-12-31'), by = 'days')  # create sequence of days 

#make 2019-2021 dataset
MB01_2019 <- filter(MB01_fix, Year == 2019)
MB01_2020 <- filter(MB01_fix, Year == 2020)
MB01_2021 <- filter(MB01_fix, Year == 2021)
MB01_baseline <- rbind(MB01_2019, MB01_2020, MB01_2021)

#plots for 2019-2021
X<-split(MB01_baseline, MB01_baseline$Fish)
Bocaccio=as.data.frame(X$Bocaccio)
Midshipman=as.data.frame(X$Midshipman)
HF=as.data.frame(X$HF)
WhiteSeabass=as.data.frame(X$"White Seabass")
Noise=as.data.frame(X$"Off Effort")
UF310=as.data.frame(X$UF310)

#sorting fish seperately by date and summing acoustic hours per day (change based on what fish you have)
#Bocaccio
Bocaccio_df = Bocaccio %>% group_by(Date) %>% 
  summarise(Acoustic_Hours = sum(Acoustic_Hours))
#Midshipman
Midshipman_df = Midshipman %>% group_by(Date) %>% 
  summarise(Acoustic_Hours = sum(Acoustic_Hours))
#HF
HF_df = HF %>% group_by(Date) %>% 
  summarise(Acoustic_Hours = sum(Acoustic_Hours))
#White Seabass
WS_df = WhiteSeabass %>% group_by(Date) %>% 
  summarise(Acoustic_Hours = sum(Acoustic_Hours))
#Noise
Noise_df = Noise %>% group_by(Date) %>% 
  summarise(Acoustic_Hours = sum(Acoustic_Hours))
#UF310
UF310_df = UF310 %>% group_by(Date) %>% 
  summarise(Acoustic_Hours = sum(Acoustic_Hours))


# ACOUSTIC EFFORT DATA BELOW
#import deployment dates (days where data was recorded)
MB01_deployment_dates=read.csv("MB01Deployments.csv",header=TRUE)

#make dates readable by R
MB01_deployment_dates$Start <- mdy(MB01_deployment_dates$Start)
MB01_deployment_dates$End <- mdy(MB01_deployment_dates$End) 

#save into dataframes with just start and end times for each location (and adding comments for fish name)
MB01_effort <- MB01_deployment_dates 
#names(MB02_effort)[2] <-"Start"
#names(MB02_effort)[3] <-"End"

#now we need to make the start datetime column just date (this deployment was already just in date)
MB01_effort$Start_date <-as.Date(MB01_effort$Start)
MB01_effort$End_date <-as.Date(MB01_effort$End)

#Now I want to make a sequence of dates from start to end date for each of the rows and then put 24 hours for each of those
MB01effort_days1 <- seq(as.Date(MB01_effort$Start_date[1]), as.Date(MB01_effort$End_date[1]), by = 'days')  
MB01effort_days2 <- seq(as.Date(MB01_effort$Start_date[2]), as.Date(MB01_effort$End_date[2]), by = 'days')  
MB01effort_days3 <- seq(as.Date(MB01_effort$Start_date[3]), as.Date(MB01_effort$End_date[3]), by = 'days')  
MB01effort_days4 <- seq(as.Date(MB01_effort$Start_date[4]), as.Date(MB01_effort$End_date[4]), by = 'days')  
MB01effort_days5 <- seq(as.Date(MB01_effort$Start_date[5]), as.Date(MB01_effort$End_date[5]), by = 'days')  
MB01effort_days6 <- seq(as.Date(MB01_effort$Start_date[6]), as.Date(MB01_effort$End_date[6]), by = 'days')  
MB01effort_days7 <- seq(as.Date(MB01_effort$Start_date[7]), as.Date(MB01_effort$End_date[7]), by = 'days')  
MB01effort_days8 <- seq(as.Date(MB01_effort$Start_date[8]), as.Date(MB01_effort$End_date[8]), by = 'days')  
MB01effort_days9 <- seq(as.Date(MB01_effort$Start_date[9]), as.Date(MB01_effort$End_date[9]), by = 'days')  
MB01effort_days11 <- seq(as.Date(MB01_effort$Start_date[10]), as.Date(MB01_effort$End_date[10]), by = 'days')  
MB01effort_days12 <- seq(as.Date(MB01_effort$Start_date[11]), as.Date(MB01_effort$End_date[11]), by = 'days')  

#combine all of these into one array
MB01_effort_dates = c(MB01effort_days1,MB01effort_days2,MB01effort_days3, MB01effort_days4, MB01effort_days5 ,MB01effort_days6 ,MB01effort_days7 ,MB01effort_days8 ,MB01effort_days9, MB01effort_days11 ,MB01effort_days12)

#save array as data frame and add a row that is 24 for each of the days
test<-rep(c(24),times=1188) 
dates_test<-cbind(MB01_effort_dates,test)
MB01_date_hours=as.data.frame(dates_test)
names(MB01_date_hours)[1] <-"numdate"
names(MB01_date_hours)[2] <-"acoustic_hours"
MB01_date_hours$date<-as.Date.numeric(MB01_date_hours$numdate)
#resave just those 2 columns
MB01_effort2 <- MB01_date_hours %>% 
  dplyr::select(date,acoustic_hours)

#getting year and month in seperate column
MB01_effort2$Year <- str_sub(MB01_effort2$date, 1, 4)
MB01_effort2$Year <- as.numeric(MB01_effort2$Year)
MB01_effort2$Month <- str_sub(MB01_effort2$date, 6, 7)
MB01_effort2$Month <- as.numeric(MB01_effort2$Month)

#keep only august 2022- 2023
MB01_e2019 <- filter(MB01_effort2, Year == 2019)
MB01_e2020 <- filter(MB01_effort2, Year == 2020)
MB01_e2021 <- filter(MB01_effort2, Year == 2021)
MB01_effortB <- rbind(MB01_e2019, MB01_e2020, MB01_e2021)

#aggregate by month
MB01_effort_monthlyB<-aggregate(acoustic_hours ~ Month,MB01_effortB, sum)


#MIDSHIPMAN
#add dates with NA that are missing from the timeline
Midshipman_dates_fixed=Midshipman_df %>% mutate(Date = as.Date(Midshipman_df$Date)) %>% complete(Date = seq.Date(min(dateMB01B), max(dateMB01B), by="day"))
#replace NA with zeroes
Midshipman_dates_fixed$Acoustic_Hours[is.na(Midshipman_dates_fixed$Acoustic_Hours)]<-0 #changed this to make it show no effort times
#barplot
MB01Midshipman_NEW <- Midshipman_dates_fixed 

MB01Midshipman_NEW$Date <- ymd(MB01Midshipman_NEW$Date)
MB01Midshipman_NEW$Month  <- month(MB01Midshipman_NEW$Date)
MB01Midshipman_NEW$Year  <- year(MB01Midshipman_NEW$Date)

#taking the monthly chorusing sum and averaging over the three years for each month
Midshipman_monthB<-aggregate(Acoustic_Hours ~ Month,MB01Midshipman_NEW, sum)

#normalize acoustic hours
Midshipman_monthB$EffortHours <- MB01_effort_monthlyB$acoustic_hours
Midshipman_monthB$Acoustic_HoursNorm <- Midshipman_monthB$Acoustic_Hours / Midshipman_monthB$EffortHours


#combine recent and reference data into one so that I can make reference histogram w/ recent dots
Midshipman_monthB$Acoustic_HoursNormDots <- Midshipman_monthR$Acoustic_HoursNorm


Midshipman_monthB$Year <- c("Off Effort", "2023", "2023", "2023", "2023", "2023", "2023", "2022", "2022", "2022", "2022", "Off Effort")
custom_colorsDots <- c("2022" = "grey", "2023" = "black", "Off Effort" = "black")


MB01MidshipmanBNorm=ggplot(Midshipman_monthB, aes(x=Month, y=Acoustic_HoursNorm)) + 
  geom_bar(stat = "identity",fill = "darkorange")+
  geom_point(aes(x = Month, y = Acoustic_HoursNormDots, color = Year, shape = Year), size = 3) +
  scale_color_manual(values = custom_colorsDots) + 
  scale_shape_manual(values = c("2022" = 16, "2023" = 16, "Off Effort" = 1)) +  # 1 for outline only
  ggtitle("Midshipman")+ xlab("")+
  ylab("")+theme_classic()+
  theme(legend.position = "none") + 
  scale_x_discrete(limits = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"), labels =c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  scale_y_continuous(limits = c(0, 0.5), expand = c(0, 0)) 
  
MB01MidshipmanBNorm



#BOCACCIO 
#add dates with NA that are missing from the timeline
Bocaccio_dates_fixed=Bocaccio_df %>% mutate(Date = as.Date(Bocaccio_df$Date)) %>% complete(Date = seq.Date(min(dateMB01B), max(dateMB01B), by="day"))
#replace NA with zeroes
Bocaccio_dates_fixed$Acoustic_Hours[is.na(Bocaccio_dates_fixed$Acoustic_Hours)]<-0
#barplot
MB01Bocaccio_NEW <- Bocaccio_dates_fixed 

MB01Bocaccio_NEW$Date <- ymd(MB01Bocaccio_NEW$Date)
MB01Bocaccio_NEW$Month  <- month(MB01Bocaccio_NEW$Date)
MB01Bocaccio_NEW$Year  <- year(MB01Bocaccio_NEW$Date)

#taking the monthly chorusing sum and averaging over the three years for each month
Bocaccio_monthB<-aggregate(Acoustic_Hours ~ Month,MB01Bocaccio_NEW, sum)

#ADD 0 hours for month 1 and 12 to MB01_effort_monthly and then plug into equation below, repeat for all fish and also reference plot
#normalize acoustic hours
Bocaccio_monthB$EffortHours <- MB01_effort_monthlyB$acoustic_hours
Bocaccio_monthB$Acoustic_HoursNorm <- Bocaccio_monthB$Acoustic_Hours / Bocaccio_monthB$EffortHours


#combine recent and reference data into one so that I can make reference histogram w/ recent dots
Bocaccio_monthB$Acoustic_HoursNormDots <- Bocaccio_monthR$Acoustic_HoursNorm


Bocaccio_monthB$Year <- c("Off Effort", "2023", "2023", "2023", "2023", "2023", "2023", "2022", "2022", "2022", "2022", "Off Effort")


MB01BocaccioBNorm=ggplot(Bocaccio_monthB, aes(x=Month, y=Acoustic_HoursNorm)) + 
  geom_bar(stat = "identity",fill = "deepskyblue")+
  geom_point(aes(x = Month, y = Acoustic_HoursNormDots, color = Year, shape = Year), size = 3) +
  scale_color_manual(values = custom_colorsDots) + 
  scale_shape_manual(values = c("2022" = 16, "2023" = 16, "Off Effort" = 1)) +  # 1 for outline only
  ggtitle("Bocaccio")+xlab("")+ylab("")+theme_classic()+
  theme(legend.position = "none")+
  scale_x_discrete(limits = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"), labels =c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  scale_y_continuous(limits = c(0,0.5), expand = c(0, 0))

MB01BocaccioBNorm



#HF 
#add dates with NA that are missing from the timeline
HF_dates_fixed=HF_df %>% mutate(Date = as.Date(HF_df$Date)) %>% complete(Date = seq.Date(min(dateMB01B), max(dateMB01B), by="day"))
#replace NA with zeroes
HF_dates_fixed$Acoustic_Hours[is.na(HF_dates_fixed$Acoustic_Hours)]<-0
#barplot
MB01HF_NEW <- HF_dates_fixed 

MB01HF_NEW$Date <- ymd(MB01HF_NEW$Date)
MB01HF_NEW$Month  <- month(MB01HF_NEW$Date)
MB01HF_NEW$Year  <- year(MB01HF_NEW$Date)

#taking the monthly chorusing sum and averaging over the three years for each month
HF_monthB<-aggregate(Acoustic_Hours ~ Month,MB01HF_NEW, sum)

#ADD 0 hours for month 1 and 12 to MB01_effort_monthly and then plug into equation below, repeat for all fish and also reference plot
#normalize acoustic hours
HF_monthB$EffortHours <- MB01_effort_monthlyB$acoustic_hours
HF_monthB$Acoustic_HoursNorm <- HF_monthB$Acoustic_Hours / HF_monthB$EffortHours


#combine recent and reference data into one so that I can make reference histogram w/ recent dots
HF_monthB$Acoustic_HoursNormDots <- HF_df_monthR$Acoustic_HoursNorm


HF_monthB$Year <- c("Off Effort", "2023", "2023", "2023", "2023", "2023", "2023", "2022", "2022", "2022", "2022", "Off Effort")


MB01HFBNorm=ggplot(HF_monthB, aes(x=Month, y=Acoustic_HoursNorm)) + 
  geom_bar(stat = "identity",fill = "darkorchid3")+
  geom_point(aes(x = Month, y = Acoustic_HoursNormDots, color = Year, shape = Year), size = 3) +
  scale_color_manual(values = custom_colorsDots) + 
  scale_shape_manual(values = c("2022" = 16, "2023" = 16, "Off Effort" = 1)) +  # 1 for outline only
  ggtitle("UF440")+ 
  ylab("Prop Chorusing Hrs")+xlab("")+theme_classic()+
  theme(legend.position = "none")+
  scale_x_discrete(limits = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"), labels =c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  scale_y_continuous(limits=c(0,0.5), expand = c(0, 0)) 

MB01HFBNorm



#WHITE SEABASS
#add dates with NA that are missing from the timeline
WS_dates_fixed=WS_df %>% mutate(Date = as.Date(WS_df$Date)) %>% complete(Date = seq.Date(min(dateMB01B), max(dateMB01B), by="day"))
#replace NA with zeroes
WS_dates_fixed$Acoustic_Hours[is.na(WS_dates_fixed$Acoustic_Hours)]<-0
#barplot
MB01WS_NEW <- WS_dates_fixed 

MB01WS_NEW$Date <- ymd(MB01WS_NEW$Date)
MB01WS_NEW$Month  <- month(MB01WS_NEW$Date)
MB01WS_NEW$Year  <- year(MB01WS_NEW$Date)

#taking the monthly chorusing sum and averaging over the three years for each month
WS_monthB<-aggregate(Acoustic_Hours ~ Month,MB01WS_NEW, sum)

#ADD 0 hours for month 1 and 12 to MB01_effort_monthly and then plug into equation below, repeat for all fish and also reference plot
#normalize acoustic hours
WS_monthB$EffortHours <- MB01_effort_monthlyB$acoustic_hours
WS_monthB$Acoustic_HoursNorm <- WS_monthB$Acoustic_Hours / WS_monthB$EffortHours

#combine recent and reference data into one so that I can make reference histogram w/ recent dots
WS_monthB$Acoustic_HoursNormDots <- WS_df_monthR$Acoustic_HoursNorm

WS_monthB$Year <- c("Off Effort", "2023", "2023", "2023", "2023", "2023", "2023", "2022", "2022", "2022", "2022", "Off Effort")


MB01WSBNorm=ggplot(WS_monthB, aes(x=Month, y=Acoustic_HoursNorm)) + 
  geom_bar(stat = "identity",fill = "firebrick2")+
  geom_point(aes(x = Month, y = Acoustic_HoursNormDots, color = Year, shape = Year), size = 3) +
  scale_color_manual(values = custom_colorsDots) + 
  scale_shape_manual(values = c("2022" = 16, "2023" = 16, "Off Effort" = 1)) +  # 1 for outline only
  ggtitle("White Seabass")+xlab("")+ylab("")+theme_classic()+
  theme(legend.position = "none")+
  scale_x_discrete(limits = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"), labels =c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  scale_y_continuous(limits=c(0,0.5), expand = c(0, 0))

MB01WSBNorm



#UF310 
#add dates with NA that are missing from the timeline
UF310_dates_fixed=UF310_df %>% mutate(Date = as.Date(UF310_df$Date)) %>% complete(Date = seq.Date(min(dateMB01B), max(dateMB01B), by="day"))
#replace NA with zeroes
UF310_dates_fixed$Acoustic_Hours[is.na(UF310_dates_fixed$Acoustic_Hours)]<-0
#barplot
MB01UF310_NEW <- UF310_dates_fixed

MB01UF310_NEW$Date <- ymd(MB01UF310_NEW$Date)
MB01UF310_NEW$Month  <- month(MB01UF310_NEW$Date)
MB01UF310_NEW$Year  <- year(MB01UF310_NEW$Date)

#taking the monthly chorusing sum and averaging over the three years for each month
UF310_monthB<-aggregate(Acoustic_Hours ~ Month,MB01UF310_NEW, sum)

#ADD 0 hours for month 1 and 12 to MB01_effort_monthly and then plug into equation below, repeat for all fish and also reference plot
#normalize acoustic hours
UF310_monthB$EffortHours <- MB01_effort_monthlyB$acoustic_hours
UF310_monthB$Acoustic_HoursNorm <- UF310_monthB$Acoustic_Hours / UF310_monthB$EffortHours

#combine recent and reference data into one so that I can make reference histogram w/ recent dots
UF310_monthB$Acoustic_HoursNormDots <- UF310_df_monthR$Acoustic_HoursNorm

UF310_monthB$Year <- c("Off Effort", "2023", "2023", "2023", "2023", "2023", "2023", "2022", "2022", "2022", "2022", "Off Effort")


MB01UF310BNorm=ggplot(UF310_monthB, aes(x=Month, y=Acoustic_HoursNorm)) + 
  geom_bar(stat = "identity",fill = "green3")+
  geom_point(aes(x = Month, y = Acoustic_HoursNormDots, color = Year, shape = Year), size = 3) +
  scale_color_manual(values = custom_colorsDots) + 
  scale_shape_manual(values = c("2022" = 16, "2023" = 16, "Off Effort" = 1)) +  # 1 for outline only
  ggtitle("UF310")+xlab("Month")+ylab("")+theme_classic()+
  theme(legend.position = "none")+
  scale_x_discrete(limits = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"), labels =c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  scale_y_continuous(limits=c(0,0.5), expand = c(0, 0)) 

MB01UF310BNorm



# Making the legend for recent year dots
MB01UF310BNorm_with_legend <- ggplot(UF310_monthB, aes(x = Month, y = Acoustic_HoursNorm)) + 
  geom_bar(stat = "identity", fill = "green3") +
  geom_point(aes(x = Month, y = Acoustic_HoursNormDots, color = Year, shape = Year), size = 3) +
  scale_color_manual(values = custom_colorsDots) + 
  scale_shape_manual(values = c("2022" = 16, "2023" = 16, "Off Effort" = 1)) +  # 1 for outline only
  ggtitle("UF310") + 
  xlab("Month") + 
  ylab("") + 
  theme_classic() +
  scale_x_discrete(
    limits = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"),
    labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  ) +
  scale_y_continuous(limits = c(0, 0.5), expand = c(0, 0))


# Helper function to extract the legend as a grob
get_legend <- function(plot) {
  g <- ggplotGrob(plot)
  legend <- g$grobs[which(sapply(g$grobs, function(x) x$name) == "guide-box")][[1]]
  return(legend)
}

# Extract the legend as a grob object
legend <- get_legend(MB01UF310BNorm_with_legend)


#final graph! putting the fish graphs and legend together
grid.arrange(
  arrangeGrob(MB01BocaccioBNorm,  MB01MidshipmanBNorm, MB01HFBNorm, MB01WSBNorm, MB01UF310BNorm, ncol = 1),  # Left column with plots
  legend,  # Right column with legend
  ncol = 2,
  widths = c(4, 1),  # Adjust the width ratio as needed
  top = "Comparing Proportion of Fish Chorusing in Most Recent Year to Rest of Recording at MB01"
)

#Figure 1: Bars represent total fish chorusing / total recording hours
#for each month of the year (proportion of chorusing for each fish in each month).
#Reference data begins in January of 2019 and ends December of 2021. 
#The recent year of data is from August 2022 to July 2023. 
#Grey dots indicate that month of recent data was from 2022, black dots indicate 
#that month of recent data was from 2023, and empty dots indicate off effort (no data).



#version of final graph split in two so bars are more visible
grid.arrange(
 arrangeGrob(MB01BocaccioBNorm,  MB01MidshipmanBNorm, MB01HFBNorm, ncol = 1),  # Left column with plots
 legend,  # Right column with legend
  ncol = 2,
  widths = c(4, 1),  # Adjust the width ratio as needed
  top = "Comparing Proportion of Fish Chorusing in Most Recent Year to Rest of Recording at MB01"
)

grid.arrange(
  arrangeGrob(MB01WSBNorm, MB01UF310BNorm, ncol = 1, nrow = 3),  # Left column with plots
    # Right column with legend
  ncol = 2,
  widths = c(4, 1)  # Adjust the width ratio as needed
 )


# ><(((*>  <*)))><   ><(((*>  <*)))><  ><(((*>  <*)))><  ><(((*>  <*)))><  ><(((*>  <*)))><  ><(((*>  <*)))><  
# ><(((*>  <*)))><   ><(((*>  <*)))><  ><(((*>  <*)))><  ><(((*>  <*)))><  ><(((*>  <*)))><  ><(((*>  <*)))><  

#4. MAKING PIES to eventually add onto map

# ><(((*>  <*)))><   ><(((*>  <*)))><  ><(((*>  <*)))><  ><(((*>  <*)))><  ><(((*>  <*)))><  ><(((*>  <*)))><  
#CH01
setwd("/Users/emmaberetta/Desktop/NMSF2024/csvdetections/CHNMS Logs/CH01")   

#read all CSVs for all sites and deployments
CH01_01_data=read.csv("CH01_01.csv",header=TRUE)
# CH01_02_data=read.csv("CH01_02 - Sheet.csv",header=TRUE) - no fish
CH01_03_data=read.csv("CH01_03.csv",header=TRUE)
CH01_04_data=read.csv("CH01_04.csv",header=TRUE)

#Combine CSV files into one matrix for each site
CH01 <- rbind(CH01_01_data, CH01_03_data, CH01_04_data)

# Subset so that we only have the three columns we need
CH01 <- CH01 %>% 
  dplyr::select("Start.time", "End.time", "Comments")

#CH01
#Creates column with start data/time in format that R can read
CH01$Start <- mdy_hms(CH01$Start.time)
CH01$End <- mdy_hms(CH01$End.time) 

#again, keeping only needed columns
CH01 <- CH01 %>% 
  dplyr::select("Start", "End", "Comments")

#fixing logging inconsistencies
CH01$Comments <- gsub('bocaccio sunset','Bocaccio sunset',CH01$Comments)
CH01$Comments <- gsub('bocaccio','Bocaccio',CH01$Comments)
CH01$Comments <- gsub('Bocaccio sunset','Bocaccio',CH01$Comments)
CH01$Comments <- gsub('Bocaccio sunrise','Bocaccio',CH01$Comments)
CH01$Comments <- gsub('midshipman','Midshipman',CH01$Comments)
CH01$Comments <- gsub('off effort','Off effort',CH01$Comments)
CH01$Comments <- gsub('Off effort','Off Effort',CH01$Comments)

#check if you have the right categories for the comments column
X<-split(CH01, CH01$Comments)

#formatting data so that we can plot 
CH01$start_time <- as.POSIXct(CH01$Start)
CH01$end_time <- as.POSIXct(CH01$End)

#creating a function that will expand each log so that we have a row for each hour where a fish was singing 
generate_hours <- function(fish, start_time, end_time) {
  # Create a sequence of hours between start_time and end_time
  seq_hours <- seq(from = floor_date(start_time, "hour"), 
                   to = ceiling_date(end_time, "hour") - 1, 
                   by = "hour")
  
  # Return a data frame with one row per hour
  data.frame(fish = fish, hour = seq_hours)
}

# Apply the function to each row of the data
CH01Hour <- CH01 %>%
  rowwise() %>%
  do(generate_hours(.$Comments, .$start_time, .$end_time)) %>%
  ungroup()

#pulling out just hour so we can sum across each one 
CH01Hour$ChorusHour <- hour(CH01Hour$hour)

#group count of fish chorusing for each hour and fish species
CH01_summaryP <- CH01Hour %>%
  group_by(fish) %>%
  summarise(count = n())

#keep only fish rows, not off effort
CH01_summaryP <- CH01_summaryP %>% filter(fish %in% c("Midshipman", "Bocaccio"))

custom_colors <- c("deepskyblue", "darkorange")

# Create a pie chart with custom colors
par(cex = .1)
CH01_pie<-pie(CH01_summaryP$count, labels = rep("", length(CH01_summaryP$count)), col = custom_colors)


#divide count of chorusing days by total effort days so that the units are in proportion, just like the histograms
CH01_summaryP$effortHours <- - (difftime(as.Date("2022-06-23"),as.Date("2022-10-19"), units = "hour") + difftime(as.Date("2022-11-16"),as.Date("2023-02-22"), units = "hour") + difftime(as.Date("2023-03-11"),as.Date("2023-11-08"), units = "hour") )
CH01_summaryP$effortHours <- as.numeric(substr(CH01_summaryP$effortHours, 1,5))

CH01_fish_sum_norm <- CH01_summaryP$count / CH01_summaryP$effortHours

#setting up data so that we can use it to make pies in ggplot
CH01_summaryP$value <- CH01_summaryP$count / CH01_summaryP$effortHours
CH01_summaryP <- CH01_summaryP %>% dplyr::select(1,4)

# Adjust the size based on the total_normalized
CH01_total_normalized<-sum(CH01_fish_sum_norm)
CH01_size_factor <- .8*CH01_total_normalized

# Create a pie chart with adjusted size and custom colors
par(mfrow = c(1, 1))  # Resetting mfrow
CH01_pie_new = pie(CH01_fish_sum_norm, labels = rep("", length(CH01_fish_sum_norm)),
                    col = custom_colors, radius = CH01_size_factor)


# ><(((*>  <*)))><   ><(((*>  <*)))><  ><(((*>  <*)))><  ><(((*>  <*)))><  ><(((*>  <*)))><  ><(((*>  <*)))><  
#MB01
#set working directory (change before making each new site graph)
setwd("/Users/emmaberetta/Desktop/NMSF2024/csvdetections/MBNMS Logs/MB01")   

#read all CSVs for all deployments
#no MB01_10 b/c off effort
MB01_01_data=read.csv("MB01_01.csv",header=TRUE)
MB01_02_data=read.csv("MB01_02.csv",header=TRUE)
MB01_03_data=read.csv("MB01_03.csv",header=TRUE)
MB01_04_data=read.csv("MB01_04.csv",header=TRUE)
MB01_05_data=read.csv("MB01_05.csv",header=TRUE)
MB01_06_data=read.csv("MB01_06.csv",header=TRUE)
MB01_07_data=read.csv("MB01_07.csv",header=TRUE)
MB01_08_data=read.csv("MB01_08.csv",header=TRUE)
MB01_09_data=read.csv("MB01_09.csv",header=TRUE)
MB01_11_data=read.csv("MB01_11 - Sheet1.csv",header=TRUE)
MB01_12_data=read.csv("MB01_12 - Sheet1.csv",header=TRUE)

#midshipman was logged seperately for MBNMS, so load those in here
MB01_02_midshipman=read.csv("MB01_02_midshipman.csv")
MB01_03_midshipman=read.csv("MB01_03_midshipman.csv")
MB01_04_midshipman=read.csv("MB01_04_midshipman.csv")
MB01_05_midshipman=read.csv("MB01_05_midshipman.csv")
MB01_06_midshipman=read.csv("MB01_06_midshipman.csv")
MB01_07_midshipman=read.csv("MB01_07_midshipman.csv")
MB01_08_midshipman=read.csv("MB01_08_midshipman.csv")
MB01_09_midshipman=read.csv("MB01_09_midshipman.csv")

# Combine CSV files into one matrix for each site
MB01=rbind(MB01_01_data, MB01_02_data, MB01_03_data, MB01_04_data, MB01_05_data, MB01_06_data, MB01_07_data, MB01_08_data,MB01_09_data, MB01_11_data, MB01_12_data, MB01_02_midshipman, MB01_03_midshipman, MB01_05_midshipman, MB01_06_midshipman, MB01_07_midshipman, MB01_08_midshipman, MB01_09_midshipman)

# Subset so that we only have the three columns we need
MB01 <- MB01 %>% 
  dplyr::select("Start.time", "End.time", "Comments")

#Creates column with start data/time in format that R can read
MB01$Start <- mdy_hms(MB01$Start.time)
MB01$End <- mdy_hms(MB01$End.time) 

#again, keeping only needed columns
MB01 <- MB01 %>% 
  dplyr::select("Start", "End", "Comments")

#fixing logging inconsistencies
MB01$Comments <- gsub('Bocaccio sunset','Bocaccio', MB01$Comments)
MB01$Comments <- gsub('Bocaccio sunrise','Bocaccio',MB01$Comments)

MB01$Comments <- gsub('HF sunset','HF', MB01$Comments)
MB01$Comments <- gsub('HF sunrise','HF',MB01$Comments)

MB01$Comments <- gsub('Off effort','Off Effort',MB01$Comments)
MB01$Comments <- gsub('Lingcod','White Seabass',MB01$Comments)

MB01$Comments <- gsub('Mystery high','UF310',MB01$Comments)
MB01$Comments <- gsub('Mystery','UF310',MB01$Comments)

#UF310 being weird and there are two of them when you split(), below code manually fixes issues
missingUF310 <- MB01[1207:1221,]
missingUF310 <- missingUF310 %>% filter(missingUF310$Comments != "Bocaccio")
missingUF310$Comments <- "UF310"

MB01_fix <- rbind(MB01, missingUF310)

X<-split(MB01_fix, MB01_fix$Comments)

#formatting data so that we can plot 
MB01_fix$start_time <- as.POSIXct(MB01_fix$Start)
MB01_fix$end_time <- as.POSIXct(MB01_fix$End)

#creating a function that will expand each log so that we have a row for each hour where a fish was singing 
#generate_hours <- function(fish, start_time, end_time) {
  # Create a sequence of hours between start_time and end_time
 # seq_hours <- seq(from = floor_date(start_time, "hour"), 
      #             to = ceiling_date(end_time, "hour") - 1, 
       #            by = "hour")
  
  # Return a data frame with one row per hour
 # data.frame(fish = fish, hour = seq_hours)
#}

# Apply the function to each row of the data
MB01Hour <- MB01_fix %>%
  rowwise() %>%
  do(generate_hours(.$Comments, .$start_time, .$end_time)) %>%
  ungroup()

#pulling out just hour so we can sum across each one 
MB01Hour$ChorusHour <- hour(MB01Hour$hour)


#group count of fish chorusing for each hour and fish species
MB01_summary <- MB01HourPST %>%
  group_by( fish) %>%
  summarise(count = n())

#keep only fish rows, not off effort
MB01_summary <- MB01_summary %>% filter(fish %in% c("Midshipman", "Bocaccio", "UF310", "White Seabass", "HF"))

# Specify custom colors for each part of the pie
custom_colors <- c("deepskyblue", "darkorchid3", "darkorange","green3", "firebrick2")

# Create a pie chart with custom colors
par(cex = .1)
MB01_pie<-pie(MB01_summary$count, labels = rep("", length(MB01_summary$count)), col = custom_colors)

#save pies

#divide count of chorusing days by total effort days so that the units are in proportion, just like the histograms
MB01_summary$effortDays <- nrow(MB01_effort2)
MB01_summary$effortHours <- MB01_summary$effortDays *24

MB01_fish_sum_norm <- MB01_summary$count / MB01_summary$effortHours

MB01_summary$prop <- MB01_summary$count / MB01_summary$effortHours
MB01_summary <- MB01_summary %>% dplyr::select(1,5)

# Adjust the size based on the total_normalized
MB01_total_normalized<-sum(MB01_fish_sum_norm)
MB01_size_factor <- .8*MB01_total_normalized
# Create a pie chart with adjusted size and custom colors
par(mfrow = c(1, 1))  # Resetting mfrow
MB01_pie_new <- pie(MB01_fish_sum_norm, labels = rep("", length(MB01_fish_sum_norm)),
                    col = custom_colors, radius = MB01_size_factor)


# ><(((*>  <*)))><   ><(((*>  <*)))><  ><(((*>  <*)))><  ><(((*>  <*)))><  ><(((*>  <*)))><  ><(((*>  <*)))><  

#MAKING MAP with pie charts and Sanctuary boundaries!

#making map for MBNMS, CHNMS, and CINMS

library(marmap)
library(ggforce)
library(sf)
library(gridExtra)
library(ggplot2)
library(grid)
library(patchwork)

#loading in bathymetric data for my desired area 
westcoastmap <- getNOAA.bathy(lon1 = -118.58, lon2 = -122.5,
                              lat1 = 33.3, lat2 = 36.98, resolution = 2)
westcoastmapgg <- fortify(westcoastmap)

#make it so any land elevation is 0
westcoastmapgg$z[westcoastmapgg$z > 0] <- 0

#loading in shape files for sanctuary boundaries for MBNMS CINMS and CHNMS
#use this link to get sanctuary shape files for CINMS MBNMS: https://hub.marinecadastre.gov/datasets/da25ca98e26b495a8a06cb72f120dfa6_0/explore
setwd("/Users/emmaberetta/Desktop/NMSF2024/MAP")  

CINMS_shape <- st_read(dsn = "Channel_Islands_National_Marine_Sanctuary")
CINMS_shape <- st_transform(CINMS_shape, crs = 4326)

MBNMS_shape <- st_read(dsn = "Monterey_Bay_National_Marine_Sanctuary")
MBNMS_shape <- st_transform(MBNMS_shape, crs = 4326)

CHNMS_shape <- st_read(dsn = "CHNMS_FinalPreferredAlternative")
CHNMS_shape <- st_transform(CHNMS_shape, crs = 4326)


# Plot bathymetry map and overlay shapefile boundary
bathy_plot <- ggplot() +
  # Bathymetry layer
  geom_contour_filled(data = westcoastmapgg, aes(x = x, y = y, z = z), binwidth = 400)+
  #geom_raster(data = westcoastmapgg, fill = z) +
  #scale_colour_manual(values = c("red", "blue", "green")) +
  
  # Overlay shapefile sanctuary boundary
  geom_sf(data = CHNMS_shape, color = "red", fill = NA, size = 2) +
  geom_sf(data = CINMS_shape, color = "blue", fill = NA, size = 2) +
  geom_sf(data = MBNMS_shape, color = "purple", fill = NA, size = 2) +
  geom_contour(data = westcoastmapgg, aes(x = x, y = y, z = z), breaks = 0, color = "black") +
  
  # Use coord_sf to handle geographic data properly
  coord_sf(xlim = c( -122.5, -118.58), ylim = c(33.3, 36.98), expand = FALSE) +
  
  #customizing labels
  labs(title = "Spatial Patterns of Fish Chorusing in California NMS", x = "Longitude", y = "Latitude", fill = "Contour Levels (Meters)") +
  theme_minimal() 


#make pie charts using ggplot so that you can combine them onto map
#only for CH01 and MB01 so far, but repeat for other desired sites in the future

#CH01
pie_chart_CH01 <- ggplot(CH01_summaryP, aes(x = "", y = value, fill = fish)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar("y") +
  theme_void() +
  theme(legend.position = "none")+
  scale_fill_manual(values = c("Bocaccio" = "deepskyblue", "Midshipman" = "darkorange", "White Seabass" = "firebrick2", "HF" = "darkorchid3","UF310" = "green3"))

#MB01
pie_chart_MB01 <- ggplot(MB01_summary, aes(x = "", y = prop, fill = fish)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar("y") +
  theme_void() +
  theme(legend.position = "none")+
  scale_fill_manual(values = c("Bocaccio" = "deepskyblue", "Midshipman" = "darkorange", "White Seabass" = "firebrick2", "HF" = "darkorchid3","UF310" = "green3"))

# Convert pie charts to grobs
pie_grob_CH01 <- ggplotGrob(pie_chart_CH01)
pie_grob_MB01 <- ggplotGrob(pie_chart_MB01)


#extracting the legend for the fish pies
MB01_summary$fish <-  gsub( "HF", "UF440", MB01_summary$fish)
                            
pie_chart_legend <- ggplot(MB01_summary, aes(x = "", y = prop, fill = fish)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar("y") +
  theme_void() +
  scale_fill_manual(values = c("Bocaccio" = "deepskyblue", "Midshipman" = "darkorange", 
                               "White Seabass" = "firebrick2", "UF440" = "darkorchid3", "UF310" = "green3")) +
  guides(fill = guide_legend(title = "Fish Species")) +
  theme(legend.position = "right")  # Ensure the legend appears to the right

pie_legend <- cowplot::get_legend(pie_chart_legend)


#making points to mark site positions (only CH01 and MB01 for now)
site_data <- data.frame(
  lon = c(-121.02, -121.9),   # Longitudes of the sites
  lat = c(35.47, 36.7),       # Latitudes of the sites
  site_name = c("CH01", "MB01"),  # Labels for each site
  radius = c("r = 0.134", "r = 0.289"),
  rlon = c(-120.52, -121.4),   # minus .5
  rlat = c(35.17, 36.3), #minus .4 for MB01 and .3 for CH01 so labels line up under pir well
  #pielon = c(-120.52-0.134, -121.4-0.289)
  pielat = c(35.47,36.7)
)


#FINAL MAP PLOT
# Add pie charts, pie legend, and site markers to bathymetry map
#radii for each pie were calculated using the total chorusing hours/ total time spent recording for that site. Thus, a larger pie = more available data at that site.
final_plot <- bathy_plot  +
  
  geom_segment(data = site_data, 
               aes(x = lon, y = lat, xend = rlon, yend = pielat),
               color = "black", size = 0.5, linetype = "dotdash") +
  
  # First pie chart at (35.47, -121.02) with approximate radius 0.134
  #shifted pie over by -0.5 longitutde so that its next to the site instead of ontop
  annotation_custom(grob = pie_grob_CH01, 
                    xmin = -120.52 - 0.134, xmax = -120.52 + 0.134, 
                    ymin = 35.47 - 0.134, ymax = 35.47 + 0.134) +
  
  # Second pie chart at (36.7, -121.9) with approximate radius 0.289
  #shifted pie over by -0.5 longitutde so that its next to the site instead of ontop
  annotation_custom(grob = pie_grob_MB01, 
                    xmin = -121.4 - 0.289, xmax = -121.4 + 0.289, 
                    ymin = 36.7 - 0.289, ymax = 36.7 + 0.289) +
  
  #adding pie chart legend
  annotation_custom(grob = pie_legend, 
                    xmin = -119.25 - .05, xmax = -119.25 + 0.05, 
                    ymin = 36.35 - .05, ymax = 36.35 + 0.05) +
  
  # adding points for sites and labeling them 
  geom_point(data = site_data, aes(x = lon, y = lat), 
             color = "black", size = 1, shape = 21, fill = "black") +  # Customize color and size
  geom_text(data = site_data, aes(x = lon, y = lat, label = site_name), 
            nudge_y = 0.1, size = 3, color = "black") +
  geom_text(data = site_data, aes(x = rlon, y = rlat, label = radius), 
          nudge_y = 0.1, size = 3, color = "black")

final_plot




