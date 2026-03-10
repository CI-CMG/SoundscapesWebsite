#Second Draft of MBNMS Humpback Detection graphs


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
library(tidyr)

library(grid)
library(patchwork)

rm(list=ls()) 

#Load in
otherCsv1OG <- read.csv('C:/Users/embe5980/Indicators/WCR_humpbacks_jack_orca_ss/data/CINMS/SanctSound_CI01.csv', stringsAsFactors = FALSE)

otherCsv2OG <- read.csv('C:/Users/embe5980/Indicators/WCR_humpbacks_jack_orca_ss/data/CINMS/SanctSound_CI04.csv', stringsAsFactors = FALSE)


#CINMS data is different from MB and OCNMS, so refromat to match other data
otherCsv1 <- otherCsv1OG %>%
  mutate(across(c(Song.Presence, Non.Song.Presence), 
                ~ ifelse(NoData == 1, NA, .)))  %>%
  rename(Song = Song.Presence,
         NonSong = Non.Song.Presence) %>%
  select(1:3)

otherCsv2 <- otherCsv2OG %>%
  mutate(across(c(Song.Presence, Non.Song.Presence), 
                ~ ifelse(NoData == 1, NA, .))) %>%
  rename(Song = Song.Presence,
         NonSong = Non.Song.Presence) %>%
  select(1:3)



#CI01
#change for what site u want
otherCsv <- otherCsv1

#need to add a end detection time column
otherCsv <- otherCsv %>%
  dplyr::rename(HourStart = Hour) #need dplyr:: bc there is also a rename function in the reshape package

otherCsv <- otherCsv %>% mutate(HourEnd = lead(HourStart))

#filling in nd hour for last hour bin (row in dataset)
#manually grab last start date/hour and then add one hour to it to get...
otherCsv$HourEnd[26651] <- "11/14/2021 13:00"

#for Song data
otherCsvSong <- otherCsv %>% select(-NonSong)
otherCsvSong$Humpback <- otherCsvSong$Song
otherCsvSong <- otherCsvSong %>% select(-Song)

#for Non Song data
otherCsvNonSong <- otherCsv %>% select(-Song)
otherCsvNonSong$Humpback <- otherCsvNonSong$NonSong
otherCsvNonSong <- otherCsvNonSong %>% select(-NonSong)

#From Taiki - make detection datasets
#change x= for if you want Song or NonSong detection data
#detData1 = MB01, etc
detData1 <- loadDetectionData(x=otherCsvSong,
                              source='csv', detectionType='auto', wide=TRUE,
                              tz='UTC',
                              columnMap=list(UTC='HourStart', end='HourEnd'),
                              speciesCols='Humpback',
                              detectedValues='1')


#effort detection dataset, doesnt work for making presence graphs
detData1N <- loadDetectionData(x=otherCsvNonSong,
                                source='csv', detectionType='auto', wide=TRUE,
                                tz='UTC',
                                columnMap=list(UTC='HourStart', end='HourEnd'),
                                speciesCols='Humpback',
                                detectedValues=c('1'))



#CI04
#change for what site u want
otherCsv <- otherCsv2

#need to add a end detection time column
otherCsv <- otherCsv %>%
  dplyr::rename(HourStart = Hour) #need dplyr:: bc there is also a rename function in the reshape package

otherCsv <- otherCsv %>% mutate(HourEnd = lead(HourStart))

#filling in nd hour for last hour bin (row in dataset)
otherCsv$HourEnd[26651] <- "11/14/2021 13:00"

#for Song data
otherCsvSong <- otherCsv %>% select(-NonSong)
otherCsvSong$Humpback <- otherCsvSong$Song
otherCsvSong <- otherCsvSong %>% select(-Song)

#for Non Song data
otherCsvNonSong <- otherCsv %>% select(-Song)
otherCsvNonSong$Humpback <- otherCsvNonSong$NonSong
otherCsvNonSong <- otherCsvNonSong %>% select(-NonSong)

#From Taiki - make detection datasets
detData2 <- loadDetectionData(x=otherCsvSong,
                              source='csv', detectionType='auto', wide=TRUE,
                              tz='UTC',
                              columnMap=list(UTC='HourStart', end='HourEnd'),
                              speciesCols='Humpback',
                              detectedValues='1')


#effort detection dataset, doesnt work for making presence graphs
detData2N <- loadDetectionData(x=otherCsvNonSong,
                                source='csv', detectionType='auto', wide=TRUE,
                                tz='UTC',
                                columnMap=list(UTC='HourStart', end='HourEnd'),
                                speciesCols='Humpback',
                                detectedValues=c('1'))

#c('1', '0', 'N/A') for curve graph, 1 for boxplots


#for new plots, need all data together with new column for site name
detData1$Site <- "CI01"
detData2$Site <- "CI04"


#for new plots, need all data together with new column for site name
detData1N$Site <- "CI01"
detData2N$Site <- "CI04"

CIHump_allDet <- bind_rows(detData1, detData2)
CIHump_allDetN <- bind_rows(detData1N, detData2N)



#NEW PLOTS

#BOXPLOTS ####

# ##MB01 vs MB02 vs MB03 ####
# #boxplot showing all three sites for Hump SONG hrs/wk   
# plotDetectionBoxplot(x=CIHump_allDet, group='species', facet='Site', bin='hour/week', combineYears=TRUE) + theme(legend.position = "none")
# 
# #boxplot showing all three sites for Hump SONG hrs/month 
# plotDetectionBoxplot(x=MBHump_allDet, group='species', facet='Site', bin='hour/month', combineYears=TRUE) + theme(legend.position = "none")
# 
# #boxplot showing all three sites for Hump NON SONG hrs/wk   
# plotDetectionBoxplot(x=MBHump_allDetN, group='species', facet='Site', bin='hour/week', combineYears=TRUE) + theme(legend.position = "none")
# 
# #boxplot showing all three sites for Hump NON SONG hrs/month 
# plotDetectionBoxplot(x=MBHump_allDetN, group='species', facet='Site', bin='hour/month', combineYears=TRUE) + theme(legend.position = "none")


## Song vs NonSong ####
#trying to get song and non song on one graph
#YOU CANT^ because you can only facet by one variable at a time on PAMscapes
CIHump_allDet$Call <- "Song"
CIHump_allDetN$Call <- "Non Song"

CIHump_Detections <- bind_rows(CIHump_allDet, CIHump_allDetN)
CI04Hump_Detections <- filter(CIHump_Detections, Site == "CI04")
CI01Hump_Detections <- filter(CIHump_Detections, Site == "CI01")


#Each site, showing song vs non song in facetted boxplot

plotDetectionBoxplot(x = CI01Hump_Detections, group = 'species', facet = 'Call',  bin = 'hour/week', combineYears = TRUE) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold")) +
  labs(title = " CI01: Variation of weekly humpback whale song calls vs non song calls b/w 2018 and 2021")

#2018-11-15 and 2021-11-20

plotDetectionBoxplot(x= CI04Hump_Detections, group='species', facet='Call', bin='hour/week', combineYears=TRUE) + 
  theme(legend.position = "none",
        plot.title = element_text(face = "bold")) +
  labs(title = " CI04: Variation of weekly humpback whale song calls vs non song calls b/w 2018 and 2021")
#2018-11-15 and 2021-04-17


runDetectionExplorer(MB01Hump_Detections)



#checking effort for both sites
#CI01 has 10000 days off effort while CI04 has 4000. explains CI01 boxplot emptiness
test1 = otherCsv1 %>% filter(is.na(Song))
test2 = otherCsv2 %>% filter(is.na(Song))




#LINE GRAPHS ####

##initial formatting####
#Load in
otherCsv1OG <- read.csv('C:/Users/embe5980/Indicators/WCR_humpbacks_jack_orca_ss/data/CINMS/SanctSound_CI01.csv', stringsAsFactors = FALSE)

otherCsv2OG <- read.csv('C:/Users/embe5980/Indicators/WCR_humpbacks_jack_orca_ss/data/CINMS/SanctSound_CI04.csv', stringsAsFactors = FALSE)


#CINMS data is different from MB and OCNMS, so refromat to match other data
otherCsv1 <- otherCsv1OG %>%
  mutate(across(c(Song.Presence, Non.Song.Presence), 
                ~ ifelse(NoData == 1, NA, .)))  %>%
  rename(Song = Song.Presence,
         NonSong = Non.Song.Presence) %>%
  select(1:3)

otherCsv2 <- otherCsv2OG %>%
  mutate(across(c(Song.Presence, Non.Song.Presence), 
                ~ ifelse(NoData == 1, NA, .))) %>%
  rename(Song = Song.Presence,
         NonSong = Non.Song.Presence) %>%
  select(1:3)



#get data ready for line graph (MB01 song)
#WATCH OUT because this makes Song and NonSong numeric, which is needed for line graph, but character is needed for effort graphs
otherCsv1 <- otherCsv1 %>%
  mutate(
    datetime = mdy_hm(Hour),  # convert to POSIXct
    Song = as.numeric(Song),
    NonSong = as.numeric(NonSong)
  )

otherCsv2 <- otherCsv2 %>%
  mutate(
    datetime = mdy_hm(Hour),  # convert to POSIXct
    Song = as.numeric(Song),
    NonSong = as.numeric(NonSong)
  )



#get detection data for all three sites to then be able to get off effort
# Put the three data frames into a list:
otherCsv_list <- list(otherCsv1, otherCsv2)

# List to store outputs
detData_list <- list()

#format data to get detection dataset
for (i in seq_along(otherCsv_list)) { #i = 1
  
  otherCsv <- otherCsv_list[[i]]
  
  otherCsv <- otherCsv %>%
    rename(HourStart = Hour)
  
  otherCsv <- otherCsv %>% 
    mutate(HourEnd = lead(HourStart))
  
  #filling in end hour for last hour bin 
  otherCsv$HourEnd[26651] <- "11/14/2021 13:00"
  
  otherCsvSong <- otherCsv %>%
    select(-NonSong) %>%
    mutate(Humpback = Song) %>%
    select(-Song)
  
  # Create detection dataset w/ effort
  detData_list[[i]] <- loadDetectionData(
    x = otherCsvSong,
    source = "csv",
    detectionType = "auto",
    wide = TRUE,
    tz = "UTC",
    columnMap = list(UTC = "HourStart", end = "HourEnd"),
    speciesCols = "Humpback",
    detectedValues = c("1", "0", NA))
}

names(detData_list) <- c("detData1E", "detData2E")



##CI01 ####

###format data ####

#song
CI01Song_weekly <- otherCsv1 %>%
  mutate(
    week = floor_date(datetime, "week"),  # get the start of each week
    year = year(datetime)
  ) %>%
  group_by(week, year) %>%
  summarise(
    total_hours_present = sum(Song, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(CallType = "Song")


#nonsong
CI01NonSong_weekly <- otherCsv1 %>%
  mutate(
    week = floor_date(datetime, "week"),  # get the start of each week
    year = year(datetime)
  ) %>%
  group_by(week, year) %>%
  summarise(
    total_hours_present = sum(NonSong, na.rm = TRUE)
  ) %>%
  ungroup()%>%
  mutate(CallType = "NonSong")

#combine song and non song into one dataset
CI01_weekly <- bind_rows(CI01Song_weekly, CI01NonSong_weekly)



###effort ####

#NEW EFFORT for MB01 when na > 75% of hours in week
effort <- detData_list[[1]]

# Mark on-effort (non-NA)
effort$on_effort <- !is.na(effort$detectedFlag)

# Extract week
effort$week <- floor_date(effort$UTC, "week")

# Count NA hours per week
weekly_effort_summary <- effort %>%
  group_by(week) %>%
  summarize(
    na_hours = sum(is.na(detectedFlag)),
    total_hours = n()
  ) %>%
  mutate(
    off_effort = na_hours >= 126 
  )

#dataset with off effort weeks
off_effort_weeksCI01 <- weekly_effort_summary %>%
  filter(off_effort) %>%
  mutate(
    xmin = week,
    xmax = week + weeks(1)
  )


#get rid of off effort data points
CI01_weekly_clean <- CI01_weekly %>%
  anti_join(weekly_effort_summary %>% filter(off_effort),
            by = c("week"))


###plot ####
# 
# ####line graph MB01 ####
# lineMB01 <- ggplot(CI01_weekly_clean, aes(x = week, y = total_hours_present, color = CallType)) +
#   geom_line(size = 1) +
#   geom_point(size = 2)+
#   geom_rect(
#     data = off_effort_weeksMB01,
#     aes(xmin = xmin - as.difftime(3.5, units = "days"), xmax = xmax- as.difftime(3.5, units = "days"), ymin = -1, ymax = Inf),
#     inherit.aes = FALSE,
#     fill = "grey"
#   ) + 
#   scale_x_date(
#     date_labels = "%b %Y",   # shows abbreviated month + year on x-axis
#     date_breaks = "3 months",
#     date_minor_breaks = "1 month"  # one tick per month
#   ) +
#   scale_color_manual(
#     values = c(
#       "Song" = "steelblue",
#       "NonSong" = "firebrick"
#     )
#   )+
#   labs(
#     title = "MB01",
#     x = "Week",
#     y = "",
#     color = "Call Type"
#   ) +
#   coord_cartesian(ylim = c(0, 168))+
#   theme_minimal() +
#   scale_y_continuous(expand = c(0,0), breaks = c(24, 48, 72, 96, 120, 144, 168))+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   theme(axis.title.x = element_blank(),
#         axis.text.x  = element_blank(),
#         axis.ticks.x = element_blank())
# 
# 
# lineMB01
# 

####curve graph CI01 ####
#with GAM curve of best fit
curveCI01 <- ggplot(CI01_weekly_clean, aes(x = week, y = total_hours_present, color = CallType)) +
  geom_point() +
  scale_color_manual(
    values = c(
      "Song" = "steelblue",
      "NonSong" = "firebrick"
    )
  )+
  stat_smooth(method = "gam", formula = y ~ s(x, k = 20), se = TRUE,
              aes(fill = CallType),   # NEW: fill follows CallType
              alpha = 0.2  ) +
  geom_rect(
    data = off_effort_weeksCI01,
    aes(xmin = xmin - days(4), xmax = xmax-days(4), ymin = 0, ymax = Inf),
    inherit.aes = FALSE,
    fill = "grey"
  ) + scale_x_date(
    date_labels = "%b %Y",   # shows abbreviated month + year on x-axis
    date_breaks = "3 months",
    date_minor_breaks = "1 month"  # one tick per month
  ) +
  coord_cartesian(ylim = c(0, 168))+
  scale_y_continuous(expand = c(0, 0),
                     breaks = c(24, 48, 72, 96, 120, 144, 168))+
  labs(
    title = "CI01",
    x = "",
    y = "",
    color = "Call Type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = "none")+
  theme(axis.title.x = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank())

curveCI01


# Save a data frame to CSV
write.csv(CI01_weekly_clean, "CI01_weekly_clean.csv", row.names = FALSE)


#chcking if GAM is appropraite
#acf(MB01_weekly_clean$total_hours_present)

#library(mgcv)

# Create a time variable if you don't already have one
#MB01_weekly_clean$time <- 1:nrow(MB01_weekly_clean)

# Fit the GAM with AR(1) correlation
#gamm_fit <- gamm(total_hours_present ~ s(week, k=20), 
# correlation = corAR1(form = ~ week),
# data = MB01_weekly_clean)

#MB01_weekly_clean$fit <- gamm_fit$gam$fitted.values

#ggplot(MB01_weekly_clean, aes(x = week, y = total_hours_present)) +
# geom_point(alpha = 0.5) +
#geom_line(aes(y = fit), color = "blue", size = 1)






##MB02 ####

###format data ####
#Line graph with ONLY MB02 (song vs non song)

#song
CI04Song_weekly <- otherCsv2 %>%
  mutate(
    week = floor_date(datetime, "week"),  # get the start of each week
    year = year(datetime)
  ) %>%
  group_by(week, year) %>%
  summarise(
    total_hours_present = sum(Song, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(CallType = "Song")

#nonsong
CI04NonSong_weekly <- otherCsv2 %>%
  mutate(
    week = floor_date(datetime, "week"),  # get the start of each week
    year = year(datetime)
  ) %>%
  group_by(week, year) %>%
  summarise(
    total_hours_present = sum(NonSong, na.rm = TRUE)
  ) %>%
  ungroup()%>%
  mutate(CallType = "NonSong")


CI04_weekly <- bind_rows(CI04Song_weekly, CI04NonSong_weekly)


### effort ####
#NEW: when na > 75% of hours in week

effort <- detData_list[[2]]

# Mark on-effort (non-NA)
effort$on_effort <- !is.na(effort$detectedFlag)

# Extract week
effort$week <- floor_date(effort$UTC, "week")

# Count NA hours per week
weekly_effort_summary <- effort %>%
  group_by(week) %>%
  summarize(
    na_hours = sum(is.na(detectedFlag)),
    total_hours = n()
  ) %>%
  mutate(
    off_effort = na_hours >= 126 
  )

#dataset with off effort weeks
off_effort_weeksCI04 <- weekly_effort_summary %>%
  filter(off_effort) %>%
  mutate(
    xmin = week,
    xmax = week + weeks(1)
  )

#only keep data points from on effort weeks
CI04_weekly_clean <- CI04_weekly %>%
  anti_join(weekly_effort_summary %>% filter(off_effort),
            by = c("week"))


### plot ####

####line graph MB02 ####
# lineMB02 <-ggplot(MB02_weekly_clean, aes(x = week, y = total_hours_present, color = CallType)) +
#   geom_line(size = 1) +
#   geom_point(size = 2)+
#   geom_rect(
#     data = off_effort_weeksMB02,
#     aes(xmin = xmin - as.difftime(3.5, units = "days"), xmax = xmax- as.difftime(3.5, units = "days"), ymin = -1, ymax = Inf),
#     inherit.aes = FALSE,
#     fill = "grey"
#   ) + 
#   scale_x_date(
#     date_labels = "%b %Y",   # shows abbreviated month + year on x-axis
#     date_breaks = "3 months",
#     date_minor_breaks = "1 month"  # one tick per month
#   ) +
#   scale_color_manual(
#     values = c(
#       "Song" = "steelblue",
#       "NonSong" = "firebrick"
#     )
#   )+
#   labs(
#     title = "MB02",
#     x = "Week",
#     y = "Total Hours with Whale Call",
#     color = "Call Type"
#   ) +
#   coord_cartesian(ylim = c(0, 168))+
#   theme_minimal() +
#   scale_y_continuous(expand = c(0,0), breaks = c(24, 48, 72, 96, 120, 144, 168))+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   theme(axis.title.x = element_blank(),
#         axis.text.x  = element_blank(),
#         axis.ticks.x = element_blank())




#### curve graph CI04 ####
#with curve of best fit
curveCI04 <- ggplot(CI04_weekly_clean, aes(x = week, y = total_hours_present, color = CallType)) +
  geom_point() +
  scale_color_manual(
    values = c(
      "Song" = "steelblue",
      "NonSong" = "firebrick"
    )
  )+
  stat_smooth(method = "gam", formula = y ~ s(x, k = 20), se = TRUE,
              aes(fill = CallType),   # NEW: fill follows CallType
              alpha = 0.2  ) +
  geom_rect(
    data = off_effort_weeksCI04,
    aes(xmin = xmin - days(4), xmax = xmax-days(4), ymin = 0, ymax = Inf),
    inherit.aes = FALSE,
    fill = "grey"
  ) + scale_x_date(
    date_labels = "%b %Y",   # shows abbreviated month + year on x-axis
    date_breaks = "3 months",
    date_minor_breaks = "1 month"  # one tick per month
  ) +
  coord_cartesian(ylim = c(0, 168))+
  scale_y_continuous(expand = c(0, 0),
                     breaks = c(24, 48, 72, 96, 120, 144, 168))+
  labs(
    title = "CI04",
    x = "Week",
    y = "Total hours with humpback whale call presence",
    color = "Call Type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = "none") 

curveCI04

# Save a data frame to CSV
write.csv(CI04_weekly_clean, "CI04_weekly_clean.csv", row.names = FALSE)






##Stacked graphs for all sites! ####
#to go on Soundscape report and Sanctuary Watch

#gridline <- grid.arrange(lineMB01, lineMB02, lineMB03, heights =c(1, 1, 1))
#gridcurve <- grid.arrange(curveMB01, curveMB02, curveMB03, heights =c(1, 1, 1))

#Stacked Line graph
# patchedL <- (lineMB01 / lineMB02 / lineMB03) + 
#   plot_layout(guides = "collect") +
#   plot_annotation(
#     title = " Passive acoustic monitoring of humpback whales in Monterey Bay National Marine Sanctuary",
#   )


#Stacked Curve of best fit (GAM) graph
patchedC <- (curveCI01 / curveCI04 ) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Passive acoustic monitoring of humpback whales in Channel Islands National Marine Sanctuary",
    theme = theme(
      plot.title = element_text(face = "bold")
    )
  )

patchedC


#save
outDir = "C:/Users/embe5980/Indicators/WCR_humpbacks_jack_orca_ss/plots"

ggsave(filename = paste0(outDir, "/plot_CINMSHumpbackWhaleDetections.jpg"), plot = patchedC, dpi = 300)




