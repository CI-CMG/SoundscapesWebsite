# OCNMS Killer Whale Detection graphs

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
OC02_01 <- read.csv('C:/Users/embe5980/Indicators/WCR_humpbacks_jack_orca_ss/data/OCNMS/noaaSanctSound_OC02_01_killerwhale.csv', stringsAsFactors = FALSE)

OC02_02 <- read.csv('C:/Users/embe5980/Indicators/WCR_humpbacks_jack_orca_ss/data/OCNMS/noaaSanctSound_OC02_02_killerwhale.csv', stringsAsFactors = FALSE)

OC02_04 <- read.csv('C:/Users/embe5980/Indicators/WCR_humpbacks_jack_orca_ss/data/OCNMS/noaaSanctSound_OC02_04_killerwhale.csv', stringsAsFactors = FALSE)

OC02_05 <- read.csv('C:/Users/embe5980/Indicators/WCR_humpbacks_jack_orca_ss/data/OCNMS/noaaSanctSound_OC02_05_killerwhale.csv', stringsAsFactors = FALSE)


#Clean up! 
OC02_01 <- OC02_01[-1,]
OC02_02 <- OC02_02[-1,]
OC02_04 <- OC02_04[-1,]
OC02_05 <- OC02_05[-1,]

OC02 <- rbind(OC02_01, OC02_02, OC02_04, OC02_05)

#fixing error found in OC02_01 log
OC02$end_time[4] <- "2019-03-10T21:50:28Z"

OC02$start_time <- ymd_hms(OC02$start_time)
OC02$end_time <- ymd_hms(OC02$end_time)

# remove columns killerwhale_presence (always 1) and killerwhale_echo_types (always NaN) 
OC02 <- OC02[, c(1, 4)]

#format
OC02$start_time <- as.POSIXct(OC02$start_time)
OC02$end_time <- as.POSIXct(OC02$end_time)


#creating a function that will expand each log so that we have a row for each hour where a fish was singing 
generate_hours <- function(start_time, end_time) {
  # Create a sequence of hours between start_time and end_time
  seq_hours <- seq(from = floor_date(start_time, "hour"), 
                   to = ceiling_date(end_time, "hour") - 1, 
                   by = "hour")
  
  # Return a data frame with one row per hour
  data.frame(hour_presence = seq_hours)
}

# Apply the function to each row of the data
OC02Hour <- OC02 %>%
  rowwise() %>%
  do(generate_hours( .$start_time, .$end_time)) %>%
  ungroup() %>%
  #remove duplicate hours so that we have one row for every hour where there were orca calls
  distinct()


#pulling out just hour so we can sum across each one 
#MB05Hour$ChorusHour <- hour(MB05Hour$hour)

#changing utc to pst/pdt 
# MB05Hour$date <- substr(MB05Hour$hour, 1, 10)
# MB05Hour$date <- as.Date(MB05Hour$date)

#daylight savings -8, otherwise -7
# MB05HourPST <- MB05Hour %>%
#   mutate(
#     ChorusHourP = ifelse(date >= as.Date("2022-11-06") & date <= as.Date("2023-03-11"), ChorusHour - 8, ChorusHour - 7)
#   )

#make pt from utc
# MB05HourPST <- MB05Hour %>%
#   mutate(
#     hour_PT = with_tz(hour, tzone = "America/Los_Angeles"),
#     ChorusHourPT = hour(hour_PT)
#   )

#group count of fish chorusing for each hour and fish species
# MB05_summary <- MB05HourPST %>%
#  group_by(ChorusHourPT, fish) %>%
#  summarise(count = n())


#need to add a end detection time column
OC02Hour <- OC02Hour %>% mutate(end_time = hour_presence + hours(1))

OC02Hour$Orca <- 1

#From Taiki - make detection datasets
detData <- loadDetectionData(x= OC02Hour,
                               source='csv', detectionType='auto', wide=TRUE,
                               tz='UTC',
                               columnMap=list(UTC='hour_presence', end='end_time'),
                               speciesCols='Orca',
                               detectedValues='1')

runDetectionExplorer(detData)




#NEW PLOTS - INDICATOR GRAPHICS

#BOXPLOT ####
# 03/08/2019 to 10/21/2021

plotDetectionBoxplot(x=detData, group='species', bin='hour/week', combineYears=TRUE)+
  theme(legend.position = "none",
        plot.title = element_text(face = "bold")) +
  labs(title = " OC02: Weekly variation of killer whale calls b/w Mar 2019 and Oct 2021")






#LINE GRAPH ####

##initial formatting####
#get data ready for line graph 
#WATCH OUT because this makes Song and NonSong numeric, which is needed for line graph, but character is needed for effort graphs
# otherCsv1 <- otherCsv1 %>%
#   mutate(
#     datetime = mdy_hm(Hour),  # convert to POSIXct
#     Song = as.numeric(Song),
#     NonSong = as.numeric(NonSong)
#   )
# 
# 
# 
# #get detection data for all three sites to then be able to get off effort
# # Put the three data frames into a list:
# otherCsv_list <- list(otherCsv1, otherCsv2, otherCsv3)
# 
# # List to store outputs
# detData_list <- list()
# 
# #format data to get detection dataset
# for (i in seq_along(otherCsv_list)) { #i = 1
#   
#   otherCsv <- otherCsv_list[[i]]
#   
#   otherCsv <- otherCsv %>%
#     rename(HourStart = Hour)
#   
#   otherCsv <- otherCsv %>% 
#     mutate(HourEnd = lead(HourStart))
#   
#   #filling in end hour for last hour bin 
#   otherCsv$HourEnd[26520] <- "11/22/2021 8:00"
#   
#   otherCsvSong <- otherCsv %>%
#     select(-NonSong) %>%
#     mutate(Humpback = Song) %>%
#     select(-Song)
#   
#   # Create detection dataset w/ effort
#   detData_list[[i]] <- loadDetectionData(
#     x = otherCsvSong,
#     source = "csv",
#     detectionType = "auto",
#     wide = TRUE,
#     tz = "UTC",
#     columnMap = list(UTC = "HourStart", end = "HourEnd"),
#     speciesCols = "Humpback",
#     detectedValues = c("1", "0", NA))
# }
# 
# names(detData_list) <- c("detData1E", "detData2E", "detData3E")
# 
# 

###format data ####
#format data for Line graph 

OC02_weekly <- OC02Hour %>%
  mutate(
    week = floor_date(hour_presence, "week")
  ) %>%
  group_by(week) %>%
  summarise(
    total_hours_present = sum(Orca, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  complete(
    week = seq(min(week), max(week), by = "1 week"),
    fill = list(total_hours_present = 0)
  ) %>%
  arrange(week)




OC02_weekly <- OC02_weekly %>%
  filter(
    !(
      (week >= as.Date("2019-04-25") & week <= as.Date("2019-07-10")) |
        (week >= as.Date("2019-10-31") & week <= as.Date("2020-07-11")) |
        (week >= as.Date("2020-10-02") & week <= as.Date("2021-06-05"))
    )
  )





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

# 
# #NEW EFFORT for MB01 when na > 75% of hours in week
# effort <- detData_list[[1]]
# 
# # Mark on-effort (non-NA)
# effort$on_effort <- !is.na(effort$detectedFlag)
# 
# # Extract week
# effort$week <- floor_date(effort$UTC, "week")
# 
# # Count NA hours per week
# weekly_effort_summary <- effort %>%
#   group_by(week) %>%
#   summarize(
#     na_hours = sum(is.na(detectedFlag)),
#     total_hours = n()
#   ) %>%
#   mutate(
#     off_effort = na_hours >= 126 
#   )
# 
# #dataset with off effort weeks
# off_effort_weeksMB01 <- weekly_effort_summary %>%
#   filter(off_effort) %>%
#   mutate(
#     xmin = week,
#     xmax = week + weeks(1)
#   )
# 
# 
# #get rid of off effort data points
# MB01_weekly_clean <- MB01_weekly %>%
#   anti_join(weekly_effort_summary %>% filter(off_effort),
#             by = c("week"))
# 
# 
# 



#off effort weeks
OC02_offeffort <- data.frame(
  start = c("04-25-2019", "10-31-2019", "10-02-2020"),
  end = c("07-10-2019", "07-11-2020", "06-05-2021")
)


OC02_offeffort$start <- as.Date(OC02_offeffort$start, format = "%m-%d-%Y")
OC02_offeffort$end <- as.Date(OC02_offeffort$end, format = "%m-%d-%Y")


###plot ####

####curve graph MB01 ####
#with GAM curve of best fit
curveOC02 <- ggplot(OC02_weekly, aes(x = week, y = total_hours_present)) +
  geom_point() +
  stat_smooth(method = "gam", formula = y ~ s(x, k = 20),
              se = TRUE,
              color = "steelblue",
              fill = "steelblue",
              alpha = 0.2) +
  geom_rect(
    data = OC02_offeffort,
    aes(xmin = start , xmax = end - days(4), ymin = 0, ymax = Inf),
    inherit.aes = FALSE,
    fill = "grey") +
    scale_x_date(
    date_labels = "%b %Y",   # shows abbreviated month + year on x-axis
    date_breaks = "3 months",
    date_minor_breaks = "1 month"  # one tick per month
  ) +
  coord_cartesian(ylim = c(0, 168))+
  scale_y_continuous(expand = c(0, 0),
                     breaks = c(24, 48, 72, 96, 120, 144, 168))+
  labs(
    title = "Passive acoustic monitoring of orca at OC02",
    x = "Week",
    y = "Total hours with orca call presence"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold")) +
  guides(fill = "none")


curveOC02



#save
outDir = "C:/Users/embe5980/Indicators/WCR_humpbacks_jack_orca_ss/plots"

ggsave(filename = paste0(outDir, "/plot_OC02OrcaDetections.jpg"), plot = curveOC02, dpi = 300)






# Save a data frame to CSV
write.csv(OC02_weekly, ".csv", row.names = FALSE)


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


