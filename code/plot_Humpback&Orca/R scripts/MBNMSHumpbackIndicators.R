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
otherCsv1MB <- read.csv('C:/Users/embe5980/Indicators/WCR_humpbacks_jack_orca_ss/data/MBNMS/SanctSound_MB01.csv', stringsAsFactors = FALSE)

otherCsv2 <- read.csv('C:/Users/embe5980/Indicators/WCR_humpbacks_jack_orca_ss/data/MBNMS/SanctSound_MB02.csv', stringsAsFactors = FALSE)

otherCsv3 <- read.csv('C:/Users/embe5980/Indicators/WCR_humpbacks_jack_orca_ss/data/MBNMS/SanctSound_MB03.csv', stringsAsFactors = FALSE)




#Clean up! FOR BOXPLOTS
#uu = 1

#for (uu in 3) {
#change for what site u want
otherCsv <- otherCsv1

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
# otherCsvNonSong <- otherCsv %>% select(-Song)
# otherCsvNonSong$Humpback <- otherCsvNonSong$NonSong
# otherCsvNonSong <- otherCsvNonSong %>% select(-NonSong)

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
detData1EN <- loadDetectionData(x=otherCsvNonSong,
                               source='csv', detectionType='auto', wide=TRUE,
                               tz='UTC',
                               columnMap=list(UTC='HourStart', end='HourEnd'),
                               speciesCols='Humpback',
                               detectedValues=c('1', '0', 'N/A'))

#}

#for new plots, need all data together with new column for site name
detData1$Site <- "MB01"
detData2$Site <- "MB02"
detData3$Site <- "MB03"


#for new plots, need all data together with new column for site name
detData1N$Site <- "MB01"
detData2N$Site <- "MB02"
detData3N$Site <- "MB03"

MBHump_allDet <- bind_rows(detData1, detData2, detData3)
MBHump_allDetN <- bind_rows(detData1N, detData2N, detData3N)



#NEW PLOTS

#BOXPLOTS ####

##MB01 vs MB02 vs MB03 ####
#boxplot showing all three sites for Hump SONG hrs/wk   
plotDetectionBoxplot(x=MBHump_allDet, group='species', facet='Site', bin='hour/week', combineYears=TRUE) + theme(legend.position = "none")

#boxplot showing all three sites for Hump SONG hrs/month 
plotDetectionBoxplot(x=MBHump_allDet, group='species', facet='Site', bin='hour/month', combineYears=TRUE) + theme(legend.position = "none")

#boxplot showing all three sites for Hump NON SONG hrs/wk   
plotDetectionBoxplot(x=MBHump_allDetN, group='species', facet='Site', bin='hour/week', combineYears=TRUE) + theme(legend.position = "none")

#boxplot showing all three sites for Hump NON SONG hrs/month 
plotDetectionBoxplot(x=MBHump_allDetN, group='species', facet='Site', bin='hour/month', combineYears=TRUE) + theme(legend.position = "none")


## Song vs NonSong ####
#trying to get song and non song on one graph
#YOU CANT^ because you can only facet by one variable at a time on PAMscapes
MBHump_allDet$Call <- "Song"
MBHump_allDetN$Call <- "Non Song"

MBHump_Detections <- bind_rows(MBHump_allDet, MBHump_allDetN)
MB01Hump_Detections <- filter(MBHump_Detections, Site == "MB01")
MB02Hump_Detections <- filter(MBHump_Detections, Site == "MB02")
MB03Hump_Detections <- filter(MBHump_Detections, Site == "MB03")


#Each site, showing song vs non song in facetted boxplot

plotDetectionBoxplot(x = MB01Hump_Detections, group = 'species', facet = 'Call',  bin = 'hour/week', combineYears = TRUE) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold")) +
  labs(title = " MB01: Variation of weekly humpback whale song calls vs non song calls b/w 2018 and 2021")

#2018-11-15 and 2021-11-20

plotDetectionBoxplot(x=MB02Hump_Detections, group='species', facet='Call', bin='hour/week', combineYears=TRUE) + 
  theme(legend.position = "none",
  plot.title = element_text(face = "bold")) +
   labs(title = " MB02: Variation of weekly humpback whale song calls vs non song calls b/w 2018 and 2021")
#2018-11-15 and 2021-04-17

plotDetectionBoxplot(x=MB03Hump_Detections, group='species', facet='Call', bin='hour/week', combineYears=TRUE) + 
  theme(legend.position = "none",
        plot.title = element_text(face = "bold")) +
  labs(title = " MB03: Variation of weekly humpback whale song calls vs non song calls b/w 2018 and 2021")
#2018-11-15 and 2021-05-31

runDetectionExplorer(MB01Hump_Detections)


##Everything ####
#song vs non song for all sites NOT using pamscapes
ggplot(MBHump_Detections, aes(x = as.factor(Week), y = CallCount, fill = CallType)) +
  geom_boxplot(outlier.alpha = 0.3) +
  facet_wrap(~ Site, ncol = 1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  xlab("Week of Year")





#ACOUSTIC SCENE?
detData1E$Site <- "MB01"
detData2E$Site <- "MB02"
detData3E$Site <- "MB03"

MBHump_allDetE <- bind_rows(detData1E, detData2E, detData3E)






#LINE GRAPHS ####

##initial formatting####
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

otherCsv3 <- otherCsv3 %>%
  mutate(
    datetime = mdy_hm(Hour),  # convert to POSIXct
    Song = as.numeric(Song),
    NonSong = as.numeric(NonSong)
  )


#get detection data for all three sites to then be able to get off effort
# Put the three data frames into a list:
otherCsv_list <- list(otherCsv1, otherCsv2, otherCsv3)

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
  otherCsv$HourEnd[26520] <- "11/22/2021 8:00"
  
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

names(detData_list) <- c("detData1E", "detData2E", "detData3E")



##MB01 ####

###format data ####
#format data for Line graph with ONLY MB01

#song
MB01Song_weekly <- otherCsv1 %>%
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
MB01NonSong_weekly <- otherCsv1 %>%
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
MB01_weekly <- bind_rows(MB01Song_weekly, MB01NonSong_weekly)



###effort ####

#OLD WAY to get effort when off effort week = when 1 hour of na or more
  #use detection dataset to mark when off effort
  #effort <- detData_list[[1]]
  # mark which times on vs off effort
  #effort$on_effort <- !is.na(effort$detectedFlag)
  # note when effort changes from on to off so we can group them
  #effort$eff_change <- FALSE
  #effort$eff_change[2:nrow(effort)] <- effort$on_effort[1:(nrow(effort)-1)] != effort$on_effort[2:nrow(effort)]
  # label the groups with numbers
  #effort$eff_group <- cumsum(effort$eff_change)
  # get the start and end times of each off effort group
  #effortOff1 <- effort %>%
  # filter(on_effort == FALSE) %>%
  # group_by(eff_group) %>%
  # summarise(start=min(UTC), end=max(end))
  #off_effort_weeks <- effortOff1 %>%
  # rowwise() %>%
  # mutate(
  #  week_seq = list(seq(
  #    from = floor_date(start, "week"),
  #    to   = floor_date(end,   "week"),
  #     by   = "1 week"
  #   ))
  #  ) %>%
  # unnest(week_seq) %>%
  # ungroup() %>%
  # distinct(week_seq) %>%     # avoid duplicates
  # mutate(
  #   xmin = week_seq,
  #   xmax = week_seq + weeks(1)
  # )
  #plot after removing data from off effort weeks
  #MB01_weekly_clean <- MB01_weekly %>%
  # anti_join(off_effort_weeks, by = c("week" = "week_seq"))


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
off_effort_weeksMB01 <- weekly_effort_summary %>%
  filter(off_effort) %>%
  mutate(
    xmin = week,
    xmax = week + weeks(1)
  )


#get rid of off effort data points
MB01_weekly_clean <- MB01_weekly %>%
  anti_join(weekly_effort_summary %>% filter(off_effort),
            by = c("week"))


###plot ####

####line graph MB01 ####
lineMB01 <- ggplot(MB01_weekly_clean, aes(x = week, y = total_hours_present, color = CallType)) +
  geom_line(size = 1) +
  geom_point(size = 2)+
  geom_rect(
    data = off_effort_weeksMB01,
    aes(xmin = xmin - as.difftime(3.5, units = "days"), xmax = xmax- as.difftime(3.5, units = "days"), ymin = -1, ymax = Inf),
    inherit.aes = FALSE,
    fill = "grey"
  ) + 
  scale_x_date(
    date_labels = "%b %Y",   # shows abbreviated month + year on x-axis
    date_breaks = "3 months",
    date_minor_breaks = "1 month"  # one tick per month
  ) +
  scale_color_manual(
    values = c(
      "Song" = "steelblue",
      "NonSong" = "firebrick"
    )
  )+
  labs(
    title = "MB01",
    x = "Week",
    y = "",
    color = "Call Type"
  ) +
  coord_cartesian(ylim = c(0, 168))+
  theme_minimal() +
  scale_y_continuous(expand = c(0,0), breaks = c(24, 48, 72, 96, 120, 144, 168))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(axis.title.x = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank())


lineMB01


####curve graph MB01 ####
#with GAM curve of best fit
curveMB01 <- ggplot(MB01_weekly_clean, aes(x = week, y = total_hours_present, color = CallType)) +
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
    data = off_effort_weeksMB01,
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
    title = "MB01",
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

curveMB01


# Save a data frame to CSV
write.csv(MB01_weekly_clean, "MB01_weekly_clean.csv", row.names = FALSE)


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
MB02Song_weekly <- otherCsv2 %>%
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
MB02NonSong_weekly <- otherCsv2 %>%
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


MB02_weekly <- bind_rows(MB02Song_weekly, MB02NonSong_weekly)


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
off_effort_weeksMB02 <- weekly_effort_summary %>%
  filter(off_effort) %>%
  mutate(
    xmin = week,
    xmax = week + weeks(1)
  )

#only keep data points from on effort weeks
MB02_weekly_clean <- MB02_weekly %>%
  anti_join(weekly_effort_summary %>% filter(off_effort),
            by = c("week"))


### plot ####

####line graph MB02 ####
lineMB02 <-ggplot(MB02_weekly_clean, aes(x = week, y = total_hours_present, color = CallType)) +
  geom_line(size = 1) +
  geom_point(size = 2)+
  geom_rect(
    data = off_effort_weeksMB02,
    aes(xmin = xmin - as.difftime(3.5, units = "days"), xmax = xmax- as.difftime(3.5, units = "days"), ymin = -1, ymax = Inf),
    inherit.aes = FALSE,
    fill = "grey"
  ) + 
  scale_x_date(
    date_labels = "%b %Y",   # shows abbreviated month + year on x-axis
    date_breaks = "3 months",
    date_minor_breaks = "1 month"  # one tick per month
  ) +
  scale_color_manual(
    values = c(
      "Song" = "steelblue",
      "NonSong" = "firebrick"
    )
  )+
  labs(
    title = "MB02",
    x = "Week",
    y = "Total Hours with Whale Call",
    color = "Call Type"
  ) +
  coord_cartesian(ylim = c(0, 168))+
  theme_minimal() +
  scale_y_continuous(expand = c(0,0), breaks = c(24, 48, 72, 96, 120, 144, 168))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(axis.title.x = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank())




#### curve graph MB02 ####
#with curve of best fit
curveMB02 <- ggplot(MB02_weekly_clean, aes(x = week, y = total_hours_present, color = CallType)) +
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
    data = off_effort_weeksMB02,
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
    title = "MB02",
    x = "",
    y = "Total Hours with Whale Call",
    color = "Call Type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = "none") +
  theme(axis.title.x = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank())



# Save a data frame to CSV
write.csv(MB02_weekly_clean, "MB02_weekly_clean.csv", row.names = FALSE)




##MB03 ####

###format data#### 
#for Line graphs with ONLY MB03 (song vs non song)

#Song
MB03Song_weekly <- otherCsv3 %>%
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

#NonSong
MB03NonSong_weekly <- otherCsv3 %>%
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

#combine song and nonsong to one dataset
MB03_weekly <- bind_rows(MB03Song_weekly, MB03NonSong_weekly)


###effort ####
#new: when na > 75% of hours in week

effort <- detData_list[[3]]

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

#identify only weeks that were off effort
off_effort_weeksMB03 <- weekly_effort_summary %>%
  filter(off_effort) %>%
  mutate(
    xmin = week,
    xmax = week + weeks(1)
  )


#remove data points from off effort weeks
MB03_weekly_clean <- MB03_weekly %>%
  anti_join(weekly_effort_summary %>% filter(off_effort),
            by = c("week"))



### plot ####

#to make vertical grid line darker every three months (above x axes)
three_month_lines <- seq(
  from = as.POSIXct("2018-10-01", tz = "UTC"),
  to   = max(MB03_weekly_clean$week, na.rm = TRUE),
  by   = "3 months"
)



####line graph MB03 ####
lineMB03 <- ggplot(MB03_weekly_clean, aes(x = week, y = total_hours_present, color = CallType)) +
  geom_vline(xintercept =three_month_lines,
             color = "grey",
             linewidth = 0.7,
             alpha = 0.5)+
  geom_line(size = 1) +
  geom_point(size = 2)+
  geom_rect(
    data = off_effort_weeksMB03,
    aes(xmin = xmin - as.difftime(3.5, units = "days"), xmax = xmax- as.difftime(3.5, units = "days"), ymin = -1, ymax = Inf),
    inherit.aes = FALSE,
    fill = "grey"
  ) + 
  scale_x_date(
    date_labels = "%b %Y",   # shows abbreviated month + year on x-axis
    date_breaks = "3 months",
    date_minor_breaks = "1 month"  # one tick per month
  )  +
  scale_color_manual(
    values = c(
      "Song" = "steelblue",
      "NonSong" = "firebrick"
    )
  )+
  labs(
    title = "MB03",
    x = "Week",
    y = "",
    color = "Call Type"
  ) +
  coord_cartesian(xlim = c(as.Date("2018-11-25"), as.Date("2021-12-01")) ,ylim = c(0, 168))+
  theme_minimal() +
  scale_y_continuous(expand = c(0,0), breaks = c(24, 48, 72, 96, 120, 144, 168))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

lineMB03


####curve graph MB03 ####
#with curve of best fit
curveMB03 <- ggplot(MB03_weekly_clean, aes(x = week, y = total_hours_present, color = CallType)) +
  scale_x_date(
    date_labels = "%b %Y",   # shows abbreviated month + year on x-axis
    date_breaks = "3 months",
    date_minor_breaks = "1 month"  # one tick per month
  ) +
  geom_point() +
  scale_color_manual(
    values = c(
      "Song" = "steelblue",
      "NonSong" = "firebrick"
    )
  )+
  stat_smooth(method = "gam", formula = y ~ s(x, k = 20), se = TRUE,
              aes(fill = CallType), 
              alpha = 0.2  ) +
  geom_rect(
    data = off_effort_weeksMB03,
    aes(xmin = xmin - days(4), xmax = xmax-days(4), ymin = 0, ymax = Inf),
    inherit.aes = FALSE,
    fill = "grey"
  )  +
  coord_cartesian(ylim = c(0, 168))+
  scale_y_continuous(expand = c(0, 0),
                     breaks = c(24, 48, 72, 96, 120, 144, 168))+
  labs(
    title = "MB03",
    x = "Week",
    y = "",
    color = "Call Type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = "none") 


curveMB03


# Save a data frame to CSV
write.csv(MB03_weekly_clean, "MB03_weekly_clean.csv", row.names = FALSE)








##Stacked graphs for all sites! ####
#to go on Soundscape report and Sanctuary Watch

#gridline <- grid.arrange(lineMB01, lineMB02, lineMB03, heights =c(1, 1, 1))
#gridcurve <- grid.arrange(curveMB01, curveMB02, curveMB03, heights =c(1, 1, 1))

#Stacked Line graph
patchedL <- (lineMB01 / lineMB02 / lineMB03) + 
  plot_layout(guides = "collect") +
  plot_annotation(
    title = " Passive acoustic monitoring of humpback whales in Monterey Bay National Marine Sanctuary",
  )


#Stacked Curve of best fit (GAM) graph
patchedC <- (curveMB01 / curveMB02 / curveMB03) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Passive acoustic monitoring of humpback whales in Monterey Bay National Marine Sanctuary",
    theme = theme(
      plot.title = element_text(face = "bold")
    )
  )

patchedC

 
 #save
 outDir = "C:/Users/embe5980/OneDrive - UCB-O365/Desktop/sanctsound_humpback_mb/plots"
 
 ggsave(filename = paste0(outDir, "/plot_MBNMSHumpbackWhaleDetections.jpg"), plot = patchedC, dpi = 300)
 



