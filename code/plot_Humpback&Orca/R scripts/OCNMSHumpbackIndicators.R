#Second Draft of MBNMS Humpback Detection graphs


# RUN this to make sure latest updates for PAMscapes
devtools::install_github('TaikiSan21/PAMscapes')

#install.packages("rJava") make sure Java is installed for xlsx to work


library(promises)
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
otherCsv <- read.csv('C:/Users/embe5980/Indicators/WCR_humpbacks_jack_orca_ss/data/OCNMS/SanctSound_OC02.csv', stringsAsFactors = FALSE)



#Clean up! FOR BOXPLOTS

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
otherCsvNonSong <- otherCsv %>% select(-Song)
otherCsvNonSong$Humpback <- otherCsvNonSong$NonSong
otherCsvNonSong <- otherCsvNonSong %>% select(-NonSong)

#From Taiki - make detection datasets
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
                                detectedValues='1')

#c('1', '0', 'N/A') for curve graph, 1 for boxplots


detData1$Site <- "OC02"
detData1N$Site <- "OC02"

OCHump_allDet <- detData1
OCHump_allDetN <- detData1N



#NEW PLOTS

#BOXPLOTS ####

## Song vs NonSong ####
#trying to get song and non song on one graph
#YOU CANT^ because you can only facet by one variable at a time on PAMscapes
OCHump_allDet$Call <- "Song"
OCHump_allDetN$Call <- "Non Song"

OCHump_Detections <- bind_rows(OCHump_allDet, OCHump_allDetN)


#Each site, showing song vs non song in facetted boxplot

plotDetectionBoxplot(x = OCHump_Detections, group = 'species', facet = 'Call',  bin = 'hour/week', combineYears = TRUE) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold")) +
  labs(title = " OC02: Variation of weekly humpback whale song calls vs non song calls b/w 2019 and 2021")


runDetectionExplorer(OCHump_Detections)






#LINE GRAPHS ####

##initial formatting####
#get data ready for line graph (MB01 song)

#Load in
otherCsv <- read.csv('C:/Users/embe5980/Indicators/WCR_humpbacks_jack_orca_ss/data/OCNMS/SanctSound_OC02.csv', stringsAsFactors = FALSE)


#WATCH OUT because this makes Song and NonSong numeric, which is needed for line graph, but character is needed for effort graphs
otherCsv1 <- otherCsv %>%
  mutate(
    datetime = mdy_hm(Hour),  # convert to POSIXct
    Song = as.numeric(Song),
    NonSong = as.numeric(NonSong)
  )


#get detection data for all three sites to then be able to get off effort
# Put the three data frames into a list:
otherCsv_list <- list(otherCsv1)

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

names(detData_list) <- c("detData1E")



##OC02 ####

###format data ####
#format data for Line graph with ONLY MB01

#song
OC02Song_weekly <- otherCsv1 %>%
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
OC02NonSong_weekly <- otherCsv1 %>%
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
OC02_weekly <- bind_rows(OC02Song_weekly, OC02NonSong_weekly)



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
off_effort_weeksOC02 <- weekly_effort_summary %>%
  filter(off_effort) %>%
  mutate(
    xmin = week,
    xmax = week + weeks(1)
  )


#get rid of off effort data points
OC02_weekly_clean <- OC02_weekly %>%
  anti_join(weekly_effort_summary %>% filter(off_effort),
            by = c("week"))


###plot ####

####line graph MB01 ####
lineMB01 <- ggplot(OC02_weekly_clean, aes(x = week, y = total_hours_present, color = CallType)) +
  geom_line(size = 1) +
  geom_point(size = 2)+
  geom_rect(
    data = off_effort_weeksOC02,
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
    title = "OC02",
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
curveMB01 <- ggplot(OC02_weekly_clean, aes(x = week, y = total_hours_present, color = CallType)) +
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
    data = off_effort_weeksOC02,
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
    title = "OC02",
    x = "Week",
    y = "Total hours with humpback whale call presence",
    color = "Call Type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = "none")

curveMB01


# Save a data frame to CSV
write.csv(OC02_weekly_clean, "OC02_weekly_clean.csv", row.names = FALSE)


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







##Stacked graphs for all sites! ####
#to go on Soundscape report and Sanctuary Watch

# #Stacked Line graph
# patchedL <- (lineMB01 / lineMB02 / lineMB03) + 
#   plot_layout(guides = "collect") +
#   plot_annotation(
#     title = " Passive acoustic monitoring of humpback whales in Monterey Bay National Marine Sanctuary",
#   )


#Stacked Curve of best fit (GAM) graph
patchedC <- (curveMB01) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Passive acoustic monitoring of humpback whales in Olympic Coast National Marine Sanctuary",
    theme = theme(
      plot.title = element_text(face = "bold")
    )
  )

patchedC


#save
outDir = "C:/Users/embe5980/Indicators/WCR_humpbacks_jack_orca_ss/plots"

ggsave(filename = paste0(outDir, "/plot_OCNMSHumpbackWhaleDetections.jpg"), plot = patchedC, dpi = 300)




