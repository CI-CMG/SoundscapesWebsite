#CI01, CI04, MB02, and MB05 rose graphs
#CI01, CI04, MB02, MB05, and CH01 histogram graphs
#By Emma Beretta w/ help from Ella Kim

# ><(((*>  <*)))><   ><(((*>  <*)))><  ><(((*>  <*)))><  ><(((*>  <*)))><  ><(((*>  <*)))><  ><(((*>  <*)))><  
#MB05
#set working directory (change before making each new site graph)
setwd("C:/Users/embe5980/SoundscapesWebsite/code/plot_FishChorus/MB05/")   

#read all CSVs for all deployments
#no MB01_10 b/c off effort
MB05_01_data= read_csv("MB05_01.csv") %>%
  filter(if_any(everything(), ~ !is.na(.) & . != ""))
MB05_02_data= read_csv("MB05_02.csv") %>%
  filter(if_any(everything(), ~ !is.na(.) & . != ""))
MB05_03_data= read_csv("MB05_03.csv") %>%
  filter(if_any(everything(), ~ !is.na(.) & . != ""))
MB05_04_data= read_csv("MB05_04.csv") %>%
  filter(if_any(everything(), ~ !is.na(.) & . != ""))
MB05_05_data= read_csv("MB05_05.csv") %>%
  filter(if_any(everything(), ~ !is.na(.) & . != ""))


#MB02_02 and 05 have wrong UF440
#MB05_02
MB05_02_UF440= read_csv("MB05_02_UF440_new_log_ella.csv") %>%
  filter(if_any(everything(), ~ !is.na(.) & . != ""))

MB05_05_UF440= read_csv("MB05_05_UF440_new_log_ella.csv") %>%
  filter(if_any(everything(), ~ !is.na(.) & . != ""))

# MB05_12_UF440= read_csv("MB05_06_UF440_new_log_ella.csv") %>%
#   filter(if_any(everything(), ~ !is.na(.) & . != ""))

MB05_02_data <- MB05_02_data %>% filter(Comments != "HF")
MB05_02_data <- MB05_02_data %>% filter(Comments != "HF sunset")
MB05_02_data <- MB05_02_data %>% filter(Comments != "HF sunrise")

MB05_02_UF440 <- MB05_02_UF440[,-c(7:12)]

MB05_02_data= rbind(MB05_02_data, MB05_02_UF440)


#MB05_05
MB05_05_data <- MB05_05_data %>% filter(Comments != "HF")
MB05_05_data <- MB05_05_data %>% filter(Comments != "HF sunset")
MB05_05_data <- MB05_05_data %>% filter(Comments != "HF sunrise")

MB05_05_UF440 <- MB05_05_UF440[,-c(7:12)]

MB05_05_data= rbind(MB05_05_data, MB05_05_UF440)


write.csv(MB05_02_data, "MB05_02_NEW.csv", row.names = FALSE)
write.csv(MB05_05_data, "MB05_05_NEW.csv", row.names = FALSE)



# Combine CSV files into one matrix for each site
MB05=rbind(MB05_01_data, MB05_02_data, MB05_03_data, MB05_04_data, MB05_05_data)

# Subset so that we only have the three columns we need
MB05 <- MB05 %>% 
  dplyr::select("Start time", "End time", "Comments")

#Creates column with start data/time in format that R can read
MB05$Start <- mdy_hms(MB05$"Start time")
MB05$End <- mdy_hms(MB05$"End time") 

#again, keeping only needed columns
MB05 <- MB05 %>% 
  dplyr::select("Start", "End", "Comments")

#check our Comment categories 
X<-split(MB05, MB05$Comments)

#fixing logging inconsistencies
MB05$Comments <- gsub('bocaccio sunset','Bocaccio', MB05$Comments)
#MB05$Comments <- gsub(' bocaccio sunset','Bocaccio', MB05$Comments)
MB05$Comments <- gsub('bocaccio sunrise','Bocaccio',MB05$Comments)
MB05$Comments <- gsub('bocaccio','Bocaccio',MB05$Comments)

MB05$Comments <- gsub('HF sunset','HF', MB05$Comments)
MB05$Comments <- gsub('HF sunrise','HF',MB05$Comments)
MB05$Comments <- gsub('HF ','HF',MB05$Comments)

#change HF to UF440 because UF440 was logged as HF (high frequency) instead of its unidentified fish frequency name (UF310)
MB05$Comments <- gsub('HF','UF440',MB05$Comments)

MB05$Comments <- gsub('off effort','Off Effort',MB05$Comments)
#MB05$Comments <- gsub('Lingcod','White Seabass',MB01$Comments)

#MB05$Comments <- gsub('Mystery high','UF310',MB01$Comments)
#MB05$Comments <- gsub('Mystery','UF310',MB01$Comments)

#UF310 being weird where there are two of them when you split(), code below manually fixes issues
#missingUF310 <- MB01[1207:1221,]
#missingUF310 <- missingUF310 %>% filter(missingUF310$Comments != "Bocaccio")
#missingUF310$Comments <- "UF310"

#new data with the fixed UF310 problem
#MB01_fix <- rbind(MB01, missingUF310)

#formatting data so that we can plot 
MB05$start_time <- as.POSIXct(MB05$Start)
MB05$end_time <- as.POSIXct(MB05$End)

#loaded in already for CH01 but leaving here just in case
#creating a function that will expand each log so that we have a row for each hour where a fish was singing 
generate_hours <- function(fish, start_time, end_time) {
  # Create a sequence of hours between start_time and end_time
  seq_hours <- seq(from = floor_date(start_time, "hour"), 
                   to = ceiling_date(end_time, "hour") - 1, 
                   by = "hour")
  
  # Return a data frame with one row per hour
  data.frame(fish = fish, hour = seq_hours)
}

MB05 <- MB05[-529,]

# Apply the function to each row of the data
MB05Hour <- MB05 %>%
  rowwise() %>%
  do(generate_hours(.$Comments, .$start_time, .$end_time)) %>%
  ungroup()

#pulling out just hour so we can sum across each one 
MB05Hour$ChorusHour <- hour(MB05Hour$hour)

#changing utc to pst/pdt 
MB05Hour$date <- substr(MB05Hour$hour, 1, 10)
MB05Hour$date <- as.Date(MB05Hour$date)

#daylight savings -8, otherwise -7
# MB05HourPST <- MB05Hour %>%
#   mutate(
#     ChorusHourP = ifelse(date >= as.Date("2022-11-06") & date <= as.Date("2023-03-11"), ChorusHour - 8, ChorusHour - 7)
#   )

#make pt from utc
MB05HourPST <- MB05Hour %>%
  mutate(
    hour_PT = with_tz(hour, tzone = "America/Los_Angeles"),
    ChorusHourPT = hour(hour_PT)
  )

#group count of fish chorusing for each hour and fish species
MB05_summary <- MB05HourPST %>%
  group_by(ChorusHourPT, fish) %>%
  summarise(count = n())

#keep only fish rows, not off effort
MB05_summary <- MB05_summary %>% filter(fish %in% c("Midshipman", "Bocaccio", "UF310", "White Seabass", "UF440"))

#get sum of days spent chorusing
MB05_summary <- MB05_summary %>%
  group_by(ChorusHourPT, fish) %>%
  summarise(count = sum(count), .groups = "drop")


MB05_summary <- MB05_summary %>%
  complete(
    ChorusHourPT = 0:23,
   # fish = c("Midshipman", "Bocaccio", "UF310", "White Seabass", "UF440"),
    fill = list(count = 0)
  )



# ACOUSTIC EFFORT data normalized (total acoustic effort/ total acoustic recorded hours)
#import deployment dates (days where data was recorded)
MB05_deployment_dates=read.csv("MB05Deployments.csv",header=TRUE)

#make dates readable by R
MB05_deployment_dates$Start <- mdy_hm(MB05_deployment_dates$Start)
MB05_deployment_dates$End <- mdy_hm(MB05_deployment_dates$End) 

#save into dataframes with just start and end times for each location (and adding comments for fish name)
MB05_effort <- MB05_deployment_dates 
#names(MB02_effort)[2] <-"Start"
#names(MB02_effort)[3] <-"End"

#now we need to make the start datetime column just date (this deployment was already just in date)
MB05_effort$Start_date <-as.Date(MB05_effort$Start)
MB05_effort$End_date <-as.Date(MB05_effort$End)

#Now I want to make a sequence of dates from start to end date for each of the rows and then put 24 hours for each of those
MB05effort_days1 <- seq(as.Date(MB05_effort$Start_date[1]), as.Date(MB05_effort$End_date[1]), by = 'days')  
MB05effort_days2 <- seq(as.Date(MB05_effort$Start_date[2]), as.Date(MB05_effort$End_date[2]), by = 'days')  
MB05effort_days3 <- seq(as.Date(MB05_effort$Start_date[3]), as.Date(MB05_effort$End_date[3]), by = 'days')  
MB05effort_days4 <- seq(as.Date(MB05_effort$Start_date[4]), as.Date(MB05_effort$End_date[4]), by = 'days')  
MB05effort_days5 <- seq(as.Date(MB05_effort$Start_date[5]), as.Date(MB05_effort$End_date[5]), by = 'days')  
# MB01effort_days6 <- seq(as.Date(MB01_effort$Start_date[6]), as.Date(MB01_effort$End_date[6]), by = 'days')  
# MB01effort_days7 <- seq(as.Date(MB01_effort$Start_date[7]), as.Date(MB01_effort$End_date[7]), by = 'days')  
# MB01effort_days8 <- seq(as.Date(MB01_effort$Start_date[8]), as.Date(MB01_effort$End_date[8]), by = 'days')  
# MB01effort_days9 <- seq(as.Date(MB01_effort$Start_date[9]), as.Date(MB01_effort$End_date[9]), by = 'days')  
# MB01effort_days11 <- seq(as.Date(MB01_effort$Start_date[10]), as.Date(MB01_effort$End_date[10]), by = 'days')  
# MB01effort_days12 <- seq(as.Date(MB01_effort$Start_date[11]), as.Date(MB01_effort$End_date[11]), by = 'days')  

#combine all of these into one array
MB05_effort_dates = c(MB05effort_days1,MB05effort_days2,MB05effort_days3, MB05effort_days4, MB05effort_days5)

#save array as data frame and add a row that is 24 for each of the days

#times = number of rows in MB05_effort_dates
test<-rep(c(24),times=446) 
dates_test<-cbind(MB05_effort_dates,test)
MB05_date_hours=as.data.frame(dates_test)
names(MB05_date_hours)[1] <-"numdate"
names(MB05_date_hours)[2] <-"acoustic_hours"
MB05_date_hours$date<-as.Date.numeric(MB05_date_hours$numdate)
#resave just those 2 columns
MB05_effort2 <- MB05_date_hours %>% 
  dplyr::select(date,acoustic_hours)

#removing duplicate day
MB05_effort2 <- MB05_effort2[-322, ]



#divide count of chorusing days by total effort days so that the units are in proportion, just like the histograms
MB05_summary$effortDays <- nrow(MB05_effort2)
MB05_summary$prop <- MB05_summary$count / MB05_summary$effortDays

#colors for all 5 fish species
custom_colors <- c("Bocaccio" = "deepskyblue", "Midshipman" = "darkorange", "UF310" = "green3", "White Seabass" = "firebrick2", "UF440" = "darkorchid")

#Rose Plot!
MB05Rose = ggplot(MB05_summary, aes(x = factor(ChorusHourPT), y = prop, fill = fish)) +
  geom_bar(stat = "identity") +
  coord_polar(start = 0) +
  theme_minimal() +
  scale_fill_manual(values = custom_colors, na.translate = FALSE) + 
  labs(x = "Hour of the Day (PST/PDT)", 
       y = "Stacked Proportion of Daily Chorusing\n(# days with chorusing / # days recorded)", 
       title = "Hourly Proportion of Daily Fish Chorusing at MB05" , fill = "Fish Species") +
  theme(axis.text.x = element_text(size = 12)) +
  scale_y_continuous(
    breaks = c(0.4, 0.8, 1.2, 1.6),
    expand = c(0, 0))+
  #can add this following line if you want the 0 on the y axis to be labeled. I removed it because it was making it hard to see bars around 0
  #annotate("text", x = 10.5, y = 0.4, label = "0.4", color = "black") +
  annotate("text", x = 10.5, y = .4, label = "0.4", color = "black") +
  annotate("text", x = 10.5, y = .8, label = "0.8", color = "black") +
  annotate("text", x = 10.5, y = 1.6, label = "1.6", color = "black")  +
  annotate("text", x = 10.5, y = 1.2, label = "1.2", color = "black")  +
  theme(axis.text.y = element_blank()) 

MB05Rose

outDirG = "C:/Users/embe5980/SoundscapesWebsite/content/resources"
ggsave(filename = paste0(outDirG, "/MB05_FinalRosePlot.png"), dpi = 300)


#proportion of days where fish (bocaccio) chorused out of all days where we were recording during that hour (20)

# ><(((*>  <*)))><   ><(((*>  <*)))><  ><(((*>  <*)))><  ><(((*>  <*)))><  ><(((*>  <*)))><  ><(((*>  <*)))><  
#MB02
#set working directory (change before making each new site graph)
outDirG = setwd("C:/Users/embe5980/Indicators/WCR_fish_ella/Code/MB02")   

#read all CSVs for all deployments
MB02_01_data= read_csv("MB02_01.csv") %>%
  filter(if_any(everything(), ~ !is.na(.) & . != ""))
MB02_02_data= read_csv("MB02_02.csv") %>%
  filter(if_any(everything(), ~ !is.na(.) & . != ""))
MB02_03_data= read_csv("MB02_03.csv") %>%
  filter(if_any(everything(), ~ !is.na(.) & . != ""))
MB02_04_data= read_csv("MB02_04.csv") %>%
  filter(if_any(everything(), ~ !is.na(.) & . != ""))
MB02_05_data= read_csv("MB02_05.csv") %>%
  filter(if_any(everything(), ~ !is.na(.) & . != ""))
MB02_06_data= read_csv("MB02_06.csv") %>%
  filter(if_any(everything(), ~ !is.na(.) & . != ""))
MB02_07_data= read_csv("MB02_07.csv") %>%
  filter(if_any(everything(), ~ !is.na(.) & . != ""))
MB02_10_data= read_csv("MB02_10 - Sheet1.csv") %>%
  filter(if_any(everything(), ~ !is.na(.) & . != ""))
MB02_11_data= read_csv("MB02_11_NEW.csv") %>%
  filter(if_any(everything(), ~ !is.na(.) & . != ""))
MB02_12_data= read_csv("MB02_12_NEW.csv") %>%
  filter(if_any(everything(), ~ !is.na(.) & . != ""))

# midshipman was logged seperately for MBNMS, so load those in here
MB02_01_midshipman=read_csv("MB02_01_midshipman.csv")
MB02_02_midshipman=read_csv("MB02_01_midshipman.csv")
MB02_03_midshipman=read_csv("MB02_01_midshipman.csv")
MB02_04_midshipman=read_csv("MB02_01_midshipman.csv")
MB02_05_midshipman=read_csv("MB02_01_midshipman.csv")
MB02_06_midshipman=read_csv("MB02_01_midshipman.csv")

#MB02_10 missing last two columns so cant bind. fix
MB02_10_data$Image <- NA
MB02_10_data$Audio <- NA

#MB02_11 and 12 have wrong UF440
#mb02_11
# MB02_11_UF440= read_csv("MB02_11_ella_uf440_fixed.csv") %>%
#   filter(if_any(everything(), ~ !is.na(.) & . != ""))
# 
# MB02_12_UF440= read_csv("MB02_12_ella_uf440_fixed.csv") %>%
#   filter(if_any(everything(), ~ !is.na(.) & . != ""))
# 
# MB02_11_data <- MB02_11_data %>% filter(Comments != "HF")
# MB02_11_data <- MB02_11_data %>% filter(Comments != "HF sunset")
# MB02_11_data <- MB02_11_data %>% filter(Comments != "HF sunrise")
# 
# MB02_11_data= rbind(MB02_11_data, MB02_11_UF440)
# 
# 
# #mb02_12
# MB02_12_UF440= read_csv("MB02_12_ella_uf440_fixed.csv") %>%
#   filter(if_any(everything(), ~ !is.na(.) & . != ""))
# 
# MB02_12_UF440= read_csv("MB02_12_ella_uf440_fixed.csv") %>%
#   filter(if_any(everything(), ~ !is.na(.) & . != ""))
# 
# MB02_12_data <- MB02_12_data %>% filter(Comments != "HF")
# MB02_12_data <- MB02_12_data %>% filter(Comments != "HF sunset")
# MB02_12_data <- MB02_12_data %>% filter(Comments != "HF sunrise")
# 
# MB02_12_data= rbind(MB02_12_data, MB02_12_UF440)
# 
# 
# write.csv(MB02_11_data, "MB02_11_NEW.csv", row.names = FALSE)
# write.csv(MB02_12_data, "MB03_12_NEW.csv", row.names = FALSE)



# Combine CSV files into one matrix for each site
MB02=rbind(MB02_01_data, 
           MB02_02_data, 
           MB02_03_data, 
           MB02_04_data, 
           MB02_05_data, 
           MB02_06_data, 
           MB02_07_data, 
           MB02_10_data, 
           MB02_11_data, 
           MB02_12_data, 
           MB02_01_midshipman, 
           MB02_02_midshipman,
           MB02_03_midshipman,
           MB02_04_midshipman,
           MB02_05_midshipman,
           MB02_06_midshipman)

# Subset so that we only have the three columns we need
MB02 <- MB02 %>% 
  dplyr::select("Start time", "End time", "Comments")

#Creates column with start data/time in format that R can read
MB02$Start <- mdy_hms(MB02$"Start time")
MB02$End <- mdy_hms(MB02$"End time") 

#again, keeping only needed columns
MB02 <- MB02 %>% 
  dplyr::select("Start", "End", "Comments")

#check our Comment categories 
X<-split(MB02, MB02$Comments)

#fixing logging inconsistencies
MB02$Comments <- gsub('Bocaccio sunset','Bocaccio', MB02$Comments)
MB02$Comments <- gsub('Bpcaccio sunset','Bocaccio', MB02$Comments)
MB02$Comments <- gsub('Bocaccio sunrise','Bocaccio',MB02$Comments)
#MB02$Comments <- gsub('bocaccio','Bocaccio',MB02$Comments)

MB02$Comments <- gsub('HF sunset','HF', MB02$Comments)
MB02$Comments <- gsub('HF sunrise','HF',MB02$Comments)
#MB02$Comments <- gsub('HF ','HF',MB02$Comments)

MB02$Comments <- gsub('Off effort','Off Effort',MB02$Comments)
MB02$Comments <- gsub('Midhsipman','Midshipman',MB02$Comments)

MB02$Comments <- gsub('Mystery high','UF310',MB02$Comments)
MB02$Comments <- gsub('Mystery','UF310',MB02$Comments)

#UF310 being weird where there are two of them when you split(), code below manually fixes issues
#missingUF310 <- MB01[1207:1221,]
#missingUF310 <- missingUF310 %>% filter(missingUF310$Comments != "Bocaccio")
#missingUF310$Comments <- "UF310"

#new data with the fixed UF310 problem
#MB01_fix <- rbind(MB01, missingUF310)

#formatting data so that we can plot 
MB02$start_time <- as.POSIXct(MB02$Start)
MB02$end_time <- as.POSIXct(MB02$End)

#loaded in already for CH01 but leaving here just in case
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
MB02Hour <- MB02 %>%
  rowwise() %>%
  do(generate_hours(.$Comments, .$start_time, .$end_time)) %>%
  ungroup()

#pulling out just hour so we can sum across each one 
MB02Hour$ChorusHour <- hour(MB02Hour$hour)

#changing utc to pst/pdt 
MB02Hour$date <- substr(MB02Hour$hour, 1, 10)
MB02Hour$date <- as.Date(MB02Hour$date)

#daylight savings -8, otherwise -7
#make pt from utc
MB02HourPST <- MB02Hour %>%
  mutate(
    hour_PT = with_tz(hour, tzone = "America/Los_Angeles"),
    ChorusHourPT = hour(hour_PT)
  )

#group count of fish chorusing for each hour and fish species
MB02_summary <- MB02HourPST %>%
  group_by(ChorusHourPT, fish) %>%
  summarise(count = n())

#keep only fish rows, not off effort
MB02_summary <- MB02_summary %>% filter(fish %in% c("Midshipman", "Bocaccio", "UF310", "White Seabass", "HF"))

#get sum of days spent chorusing
MB02_summary <- MB02_summary %>%
  group_by(ChorusHourPT, fish) %>%
  summarise(count = sum(count), .groups = "drop")


MB02_summary <- MB02_summary %>%
  complete(
    ChorusHourPT = 0:23,
    # fish = c("Midshipman", "Bocaccio", "UF310", "White Seabass", "UF440"),
    fill = list(count = 0)
  )


# ACOUSTIC EFFORT data normalized (total acoustic effort/ total acoustic recorded hours)
#import deployment dates (days where data was recorded)
MB02_deployment_dates=read.csv("MB02Deployments.csv",header=TRUE)

#make dates readable by R
MB02_deployment_dates$Start <- mdy(MB02_deployment_dates$Start)
MB02_deployment_dates$End <- mdy(MB02_deployment_dates$End) 

#save into dataframes with just start and end times for each location (and adding comments for fish name)
MB02_effort <- MB02_deployment_dates 
#names(MB02_effort)[2] <-"Start"
#names(MB02_effort)[3] <-"End"

#now we need to make the start datetime column just date (this deployment was already just in date)
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
MB02_effort_dates = c(MB02effort_days1,MB02effort_days2,MB02effort_days3, MB02effort_days4, MB02effort_days5, MB02effort_days6, MB02effort_days7,MB02effort_days10, MB02effort_days11, MB02effort_days12)

#save array as data frame and add a row that is 24 for each of the days

#times = number of rows in MB02_effort_dates
test<-rep(c(24),times=1188) 
dates_test<-cbind(MB02_effort_dates,test)
MB02_date_hours=as.data.frame(dates_test)
names(MB02_date_hours)[1] <-"numdate"
names(MB02_date_hours)[2] <-"acoustic_hours"
MB02_date_hours$date<-as.Date.numeric(MB02_date_hours$numdate)
#resave just those 2 columns
MB02_effort2 <- MB02_date_hours %>% 
  dplyr::select(date,acoustic_hours)

#removing duplicate day (overlap between end date of one deployment and start datae of next, so double counting that in between day)
MB02_effort2 <- MB02_effort2[-609, ]



#divide count of chorusing days by total effort days so that the units are in proportion, just like the histograms
MB02_summary$effortDays <- nrow(MB02_effort2)
MB02_summary$prop <- MB02_summary$count / MB02_summary$effortDays

#change HF to UF440 because UF440 was logged as HF (high frequency) instead of its unidentified fish frequency name (UF310)
MB02_summary$fish <- gsub('HF','UF440',MB02_summary$fish)

#colors for all 5 fish species
custom_colors <- c("Bocaccio" = "deepskyblue", "Midshipman" = "darkorange", "UF310" = "green3", "White Seabass" = "firebrick2", "UF440" = "darkorchid")

MB02_summary$fish <- factor(
  MB02_summary$fish,
  levels = c(
    "Bocaccio",
    "Midshipman",
    "UF440" ,
    "UF310",
    "White Seabass" 
  )
)

#Rose Plot!
MB02Rose = ggplot(MB02_summary, aes(x = factor(ChorusHourPT), y = prop, fill = fish)) +
  geom_bar(stat = "identity") +
  coord_polar(start = 0) +
  theme_minimal() +
  scale_fill_manual(values = custom_colors, na.translate = FALSE) + 
  labs(x = "Hour of the Day (PST/PDT)", y = "Stacked Proportion of Daily Chorusing\n(# days with chorusing / # days recorded)", title = "Hourly Proportion of Daily Fish Chorusing at MB02" , fill = "Fish Species") +
  theme(axis.text.x = element_text(size = 12)) +
  #can add this following line if you want the 0 on the y axis to be labeled. I removed it because it was making it hard to see bars around 0
  #annotate("text", x = 10.5, y = .2, label = "0.2", color = "black") +
  annotate("text", x = 10.5, y = .4, label = "0.4", color = "black") +
  annotate("text", x = 10.5, y = .8, label = "0.8", color = "black") +
  annotate("text", x = 10.5, y = 1.2, label = "1.2", color = "black")  +
  annotate("text", x = 10.5, y = 1.6, label = "1.6", color = "black")  +
  theme(axis.text.y = element_blank()) 

MB02Rose


outDirG = "C:/Users/embe5980/SoundscapesWebsite/content/resources"
ggsave(filename = paste0(outDirG, "/MB02_FinalRosePlot.png"), dpi = 300)

#proportion of days where fish (bocaccio) chorused out of all days where we were recording during that hour (20)

# ><(((*>  <*)))><   ><(((*>  <*)))><  ><(((*>  <*)))><  ><(((*>  <*)))><  ><(((*>  <*)))><  ><(((*>  <*)))><  
#CI01
#set working directory (change before making each new site graph)
setwd("C:/Users/embe5980/Indicators/WCR_fish_ella/Code/CI01")   

#read all CSVs for all deployments
CI01_01_data= read_csv("CI01_01_fish.csv") %>%
  filter(if_any(everything(), ~ !is.na(.) & . != ""))
CI01_02_data= read_csv("CI01_02_fish.csv") %>%
  filter(if_any(everything(), ~ !is.na(.) & . != ""))
CI01_03_data= read_csv("CI01_03_fish.csv") %>%
  filter(if_any(everything(), ~ !is.na(.) & . != ""))
CI01_04_data= read_csv("CI01_04_fish.csv") %>%
  filter(if_any(everything(), ~ !is.na(.) & . != ""))
CI01_05_data= read_csv("CI01_05_fish.csv") %>%
  filter(if_any(everything(), ~ !is.na(.) & . != ""))
CI01_06_data= read_csv("CI01_06_fish.csv") %>%
  filter(if_any(everything(), ~ !is.na(.) & . != ""))
CI01_07_data= read_csv("CI01_07_fish.csv") %>%
  filter(if_any(everything(), ~ !is.na(.) & . != ""))
CI01_08_data= read_csv("CI01_08_fish.csv") %>%
  filter(if_any(everything(), ~ !is.na(.) & . != ""))


# midshipman was logged seperately for MBNMS, so load those in here
# CI01_01_midshipman=read_csv("CI01_01_midshipman.csv")
# CI01_02_midshipman=read_csv("CI01_01_midshipman.csv")


# Combine CSV files into one matrix for each site
CI01=rbind(CI01_01_data, 
           CI01_02_data, 
           CI01_03_data, 
           CI01_04_data, 
           CI01_05_data, 
           CI01_06_data, 
           CI01_07_data, 
           CI01_08_data)

# Subset so that we only have the three columns we need
CI01 <- CI01 %>% 
  dplyr::select("Start time", "End time", "Comments")

#Creates column with start data/time in format that R can read
CI01$Start <- mdy_hms(CI01$"Start time")
CI01$End <- mdy_hms(CI01$"End time") 

#again, keeping only needed columns
CI01 <- CI01 %>% 
  dplyr::select("Start", "End", "Comments")

#check our Comment categories 
X<-split(CI01, CI01$Comments)

#fixing logging inconsistencies
CI01$Comments <- gsub('Bocaccio sunset','Bocaccio', CI01$Comments)
CI01$Comments <- gsub('Bocacio sunrise','Bocaccio', CI01$Comments)
CI01$Comments <- gsub('Bocaccio sunrise','Bocaccio',CI01$Comments)
#CI01$Comments <- gsub('bocaccio','Bocaccio',CI01$Comments)

CI01$Comments <- gsub('HF Sunset','HF', CI01$Comments)
CI01$Comments <- gsub('HF sunrise','HF',CI01$Comments)
CI01$Comments <- gsub('HF sunset','HF', CI01$Comments)

CI01$Comments <- gsub('Off effort','Off Effort',CI01$Comments)
CI01$Comments <- gsub('Midshipman sunrise','Midshipman',CI01$Comments)
CI01$Comments <- gsub('Midhsipman','Midshipman',CI01$Comments)

CI01$Comments <- gsub('Mystery high','UF310',CI01$Comments)
CI01$Comments <- gsub('Mystery','UF310',CI01$Comments)

#UF310 being weird where there are two of them when you split(), code below manually fixes issues
#missingUF310 <- MB01[1207:1221,]
#missingUF310 <- missingUF310 %>% filter(missingUF310$Comments != "Bocaccio")
#missingUF310$Comments <- "UF310"

#new data with the fixed UF310 problem
#MB01_fix <- rbind(MB01, missingUF310)

#formatting data so that we can plot 
CI01$start_time <- as.POSIXct(CI01$Start)
CI01$end_time <- as.POSIXct(CI01$End)

#loaded in already for CH01 but leaving here just in case
#creating a function that will expand each log so that we have a row for each hour where a fish was singing 
generate_hours <- function(fish, start_time, end_time) {
  # Create a sequence of hours between start_time and end_time
  seq_hours <- seq(from = floor_date(start_time, "hour"),
                   to = ceiling_date(end_time, "hour") - 1,
                   by = "hour")

  # Return a data frame with one row per hour
  data.frame(fish = fish, hour = seq_hours)
}

#there is one log that accidentally had the start and end time switched, so fixing that here?
#line 117
# swap the values in row 117
tmp <- CI01$start_time[117]
CI01$start_time[117] <- CI01$end_time[117]
CI01$end_time[117] <- tmp


# Apply the function to each row of the data
CI01Hour <- CI01 %>%
  rowwise() %>%
  do(generate_hours(.$Comments, .$start_time, .$end_time)) %>%
  ungroup()

#pulling out just hour so we can sum across each one 
CI01Hour$ChorusHour <- hour(CI01Hour$hour)

# changing utc to pst/pdt 
# CI01Hour$date <- substr(CI01Hour$hour, 1, 10)
# CI01Hour$date <- as.Date(CI01Hour$date)

#daylight savings -8, otherwise -7
#make pt from utc
CI01HourPST <- CI01Hour %>%
  mutate(
    hour_PT = with_tz(hour, tzone = "America/Los_Angeles"),
    ChorusHourPT = hour(hour_PT)
  )

#group count of fish chorusing for each hour and fish species
CI01_summary <- CI01HourPST %>%
  group_by(ChorusHourPT, fish) %>%
  summarise(count = n())

#keep only fish rows, not off effort
CI01_summary <- CI01_summary %>% filter(fish %in% c("Midshipman", "Bocaccio", "UF310", "White Seabass", "HF"))

#get sum of days spent chorusing
CI01_summary <- CI01_summary %>%
  group_by(ChorusHourPT, fish) %>%
  summarise(count = sum(count), .groups = "drop")


CI01_summary <- CI01_summary %>%
  complete(
    ChorusHourPT = 0:23,
    # fish = c("Midshipman", "Bocaccio", "UF310", "White Seabass", "UF440"),
    fill = list(count = 0)
  )


# ACOUSTIC EFFORT data normalized (total acoustic effort/ total acoustic recorded hours)
#import deployment dates (days where data was recorded)
CI01_deployment_dates=read.csv("CINMS Deployment Dates.csv",header=TRUE)

#only keep details for this site. file loaded in above has all sites deployment dates
CI01_deployment_dates = CI01_deployment_dates[1:8,]

#make dates readable by R
CI01_deployment_dates$Start <- mdy_hm(CI01_deployment_dates$Start)
CI01_deployment_dates$End <- mdy_hm(CI01_deployment_dates$End) 

#save into dataframes with just start and end times for each location (and adding comments for fish name)
CI01_effort <- CI01_deployment_dates 
#names(CI01_effort)[2] <-"Start"
#names(CI01_effort)[3] <-"End"

#now we need to make the start datetime column just date (this deployment was already just in date)
CI01_effort$Start_date <-as.Date(CI01_effort$Start)
CI01_effort$End_date <-as.Date(CI01_effort$End)

#Now I want to make a sequence of dates from start to end date for each of the rows and then put 24 hours for each of those
CI01effort_days1 <- seq(as.Date(CI01_effort$Start_date[1]), as.Date(CI01_effort$End_date[1]), by = 'days')  
CI01effort_days2 <- seq(as.Date(CI01_effort$Start_date[2]), as.Date(CI01_effort$End_date[2]), by = 'days')  
CI01effort_days3 <- seq(as.Date(CI01_effort$Start_date[3]), as.Date(CI01_effort$End_date[3]), by = 'days')  
CI01effort_days4 <- seq(as.Date(CI01_effort$Start_date[4]), as.Date(CI01_effort$End_date[4]), by = 'days')  
CI01effort_days5 <- seq(as.Date(CI01_effort$Start_date[5]), as.Date(CI01_effort$End_date[5]), by = 'days')  
CI01effort_days6 <- seq(as.Date(CI01_effort$Start_date[6]), as.Date(CI01_effort$End_date[6]), by = 'days')
CI01effort_days7 <- seq(as.Date(CI01_effort$Start_date[7]), as.Date(CI01_effort$End_date[7]), by = 'days')
CI01effort_days8 <- seq(as.Date(CI01_effort$Start_date[8]), as.Date(CI01_effort$End_date[8]), by = 'days')

#combine all of these into one array
CI01_effort_dates = c(CI01effort_days1,CI01effort_days2,CI01effort_days3, CI01effort_days4, CI01effort_days5, CI01effort_days6, CI01effort_days7, CI01effort_days8)

#save array as data frame and add a row that is 24 for each of the days

#times = number of rows in CI01_effort_dates
test<-rep(c(24),times=832) 
dates_test<-cbind(CI01_effort_dates,test)
CI01_date_hours=as.data.frame(dates_test)
names(CI01_date_hours)[1] <-"numdate"
names(CI01_date_hours)[2] <-"acoustic_hours"
CI01_date_hours$date<-as.Date.numeric(CI01_date_hours$numdate)
#resave just those 2 columns
CI01_effort2 <- CI01_date_hours %>% 
  dplyr::select(date,acoustic_hours)

#removing duplicate day (overlap between end date of one deployment and start datae of next, so double counting that in between day)
CI01_effort2 <- CI01_effort2[-393, ]



#divide count of chorusing days by total effort days so that the units are in proportion, just like the histograms
CI01_summary$effortDays <- nrow(CI01_effort2)
CI01_summary$prop <- CI01_summary$count / CI01_summary$effortDays

#change HF to UF440 because UF440 was logged as HF (high frequency) instead of its unidentified fish frequency name (UF310)
CI01_summary$fish <- gsub('HF','UF440',CI01_summary$fish)

#colors for all 5 fish species
custom_colors <- c("Bocaccio" = "deepskyblue", "Midshipman" = "darkorange", "UF310" = "green3", "White Seabass" = "firebrick2", "UF440" = "darkorchid")


#fixing order of colors so that red and purple are not next to eachother
#easier to distinguish
CI01_summary$fish <- factor(
  CI01_summary$fish,
  levels = c(
    "Bocaccio",
    "UF440" ,
    "Midshipman",
    "White Seabass" 
  )
)

#Rose Plot!
CI01Rose = ggplot(CI01_summary, aes(x = factor(ChorusHourPT), y = prop, fill = fish)) +
  geom_bar(stat = "identity") +
  coord_polar(start = 0) +
  theme_minimal() +
  scale_fill_manual(values = custom_colors, na.translate = FALSE) + 
  labs(x = "Hour of the Day (PST/PDT)", 
       y = "Stacked Proportion of Daily Chorusing\n(# days with chorusing / # days recorded)", 
       title = "Hourly Proportion of Daily Fish Chorusing at CI01" , fill = "Fish Species") +
  theme(axis.text.x = element_text(size = 12)) +
  #can add this following line if you want the 0 on the y axis to be labeled. I removed it because it was making it hard to see bars around 0
  #annotate("text", x = 10.5, y = .2, label = "0.2", color = "black") +
  annotate("text", x = 10.5, y = .4, label = "0.4", color = "black") +
  annotate("text", x = 10.5, y = .8, label = "0.8", color = "black") +
  annotate("text", x = 10.5, y = 1.2, label = "1.2", color = "black")  +
  annotate("text", x = 10.5, y = 1.6, label = "1.6", color = "black")  +
  theme(axis.text.y = element_blank()) 

CI01Rose


outDirG = "C:/Users/embe5980/SoundscapesWebsite/content/resources"
ggsave(filename = paste0(outDirG, "/CI01_FinalRosePlot.jpg"), dpi = 300)

#proportion of days where fish (bocaccio) chorused out of all days where we were recording during that hour (20)



# ><(((*>  <*)))><   ><(((*>  <*)))><  ><(((*>  <*)))><  ><(((*>  <*)))><  ><(((*>  <*)))><  ><(((*>  <*)))><  
#CI04
#set working directory (change before making each new site graph)
setwd("C:/Users/embe5980/Indicators/WCR_fish_ella/Code/CI04")   

#read all CSVs for all deployments
CI04_01_data= read_csv("CI04_01_fish.csv") %>%
  filter(if_any(everything(), ~ !is.na(.) & . != ""))
CI04_02_data= read_csv("CI04_02_fish.csv") %>%
  filter(if_any(everything(), ~ !is.na(.) & . != ""))
CI04_03_data= read_csv("CI04_03_fish.csv") %>%
  filter(if_any(everything(), ~ !is.na(.) & . != ""))
CI04_04_data= read_csv("CI04_04_fish.csv") %>%
  filter(if_any(everything(), ~ !is.na(.) & . != ""))
CI04_05_data= read_csv("CI04_05_fish.csv") %>%
  filter(if_any(everything(), ~ !is.na(.) & . != ""))
CI04_06_data= read_csv("CI04_06_fish.csv") %>%
  filter(if_any(everything(), ~ !is.na(.) & . != ""))
CI04_07_data= read_csv("CI04_07_fish.csv") %>%
  filter(if_any(everything(), ~ !is.na(.) & . != ""))
CI04_08_data= read_csv("CI04_08_fish.csv") %>%
  filter(if_any(everything(), ~ !is.na(.) & . != ""))


# midshipman was logged seperately for MBNMS, so load those in here
# CI04_01_midshipman=read_csv("CI04_01_midshipman.csv")
# CI04_02_midshipman=read_csv("CI04_01_midshipman.csv")


# Combine CSV files into one matrix for each site
CI04=rbind(CI04_01_data, 
           CI04_02_data, 
           CI04_03_data, 
           CI04_04_data, 
           CI04_05_data, 
           CI04_06_data, 
           CI04_07_data, 
           CI04_08_data)

# Subset so that we only have the three columns we need
CI04 <- CI04 %>% 
  dplyr::select("Start time", "End time", "Comments")

#Creates column with start data/time in format that R can read
CI04$Start <- mdy_hms(CI04$"Start time")
CI04$End <- mdy_hms(CI04$"End time") 

#again, keeping only needed columns
CI04 <- CI04 %>% 
  dplyr::select("Start", "End", "Comments")

#check our Comment categories 
X<-split(CI04, CI04$Comments)

#fixing logging inconsistencies
CI04$Comments <- gsub('Bocaccio sunset','Bocaccio', CI04$Comments)
CI04$Comments <- gsub('Boaccio sunset','Bocaccio', CI04$Comments)
CI04$Comments <- gsub('Bocaccio susnet','Bocaccio', CI04$Comments)
CI04$Comments <- gsub('Bocacio sunrise','Bocaccio', CI04$Comments)
CI04$Comments <- gsub('Bocaccio sunrise','Bocaccio',CI04$Comments)

# CI04$Comments <- gsub('HF Sunset','HF', CI04$Comments)
# CI04$Comments <- gsub('HF sunrise','HF',CI04$Comments)
# CI04$Comments <- gsub('HF sunset','HF', CI04$Comments)

CI04$Comments <- gsub('Lincod','White Seabass',CI04$Comments)
CI04$Comments <- gsub('Lingcod','White Seabass',CI04$Comments)
CI04$Comments <- gsub('Lingcod sunrise','White Seabass',CI04$Comments)
CI04$Comments <- gsub('Midshipman sunrise','Midshipman',CI04$Comments)
CI04$Comments <- gsub('Midhsipman','Midshipman',CI04$Comments)

CI04$Comments <- gsub('Mystery high','UF310',CI04$Comments)
CI04$Comments <- gsub('Mystery','UF310',CI04$Comments)

#UF310 being weird where there are two of them when you split(), code below manually fixes issues
#missingUF310 <- MB01[1207:1221,]
#missingUF310 <- missingUF310 %>% filter(missingUF310$Comments != "Bocaccio")
#missingUF310$Comments <- "UF310"

#new data with the fixed UF310 problem
#MB01_fix <- rbind(MB01, missingUF310)

#formatting data so that we can plot 
CI04$start_time <- as.POSIXct(CI04$Start)
CI04$end_time <- as.POSIXct(CI04$End)

#loaded in already for CH01 but leaving here just in case
#creating a function that will expand each log so that we have a row for each hour where a fish was singing 
generate_hours <- function(fish, start_time, end_time) {
  # Create a sequence of hours between start_time and end_time
  seq_hours <- seq(from = floor_date(start_time, "hour"),
                   to = ceiling_date(end_time, "hour") - 1,
                   by = "hour")
  
  # Return a data frame with one row per hour
  data.frame(fish = fish, hour = seq_hours)
}

#there is one log iin CI01 that accidentally had the start and end time switched, so fixing that here?
#line 117
# swap the values in row 117
# tmp <- CI04$start_time[117]
# CI04$start_time[117] <- CI04$end_time[117]
# CI04$end_time[117] <- tmp


# Apply the function to each row of the data
CI04Hour <- CI04 %>%
  rowwise() %>%
  do(generate_hours(.$Comments, .$start_time, .$end_time)) %>%
  ungroup()

#pulling out just hour so we can sum across each one 
CI04Hour$ChorusHour <- hour(CI04Hour$hour)

# changing utc to pst/pdt 
# CI04Hour$date <- substr(CI04Hour$hour, 1, 10)
# CI04Hour$date <- as.Date(CI04Hour$date)

#daylight savings -8, otherwise -7
#make pt from utc
CI04HourPST <- CI04Hour %>%
  mutate(
    hour_PT = with_tz(hour, tzone = "America/Los_Angeles"),
    ChorusHourPT = hour(hour_PT)
  )

#group count of fish chorusing for each hour and fish species
CI04_summary <- CI04HourPST %>%
  group_by(ChorusHourPT, fish) %>%
  summarise(count = n())

#keep only fish rows, not off effort
CI04_summary <- CI04_summary %>% filter(fish %in% c("Midshipman", "Lingcod", "Bocaccio", "UF310", "White Seabass", "HF"))

#get sum of days spent chorusing
CI04_summary <- CI04_summary %>%
  group_by(ChorusHourPT, fish) %>%
  summarise(count = sum(count), .groups = "drop")


CI04_summary <- CI04_summary %>%
  complete(
    ChorusHourPT = 0:23,
    fill = list(count = 0)
  )


# ACOUSTIC EFFORT data normalized (total acoustic effort/ total acoustic recorded hours)
#import deployment dates (days where data was recorded)
CI04_deployment_dates=read.csv("CINMS Deployment Dates.csv",header=TRUE)

#only keep details for this site. file loaded in above has all sites deployment dates
CI04_deployment_dates = CI04_deployment_dates[22:29,]

#make dates readable by R
CI04_deployment_dates$Start <- mdy_hm(CI04_deployment_dates$Start)
CI04_deployment_dates$End <- mdy_hm(CI04_deployment_dates$End) 

#save into dataframes with just start and end times for each location (and adding comments for fish name)
CI04_effort <- CI04_deployment_dates 
#names(CI04_effort)[2] <-"Start"
#names(CI04_effort)[3] <-"End"

#now we need to make the start datetime column just date (this deployment was already just in date)
CI04_effort$Start_date <-as.Date(CI04_effort$Start)
CI04_effort$End_date <-as.Date(CI04_effort$End)

#Now I want to make a sequence of dates from start to end date for each of the rows and then put 24 hours for each of those
CI04effort_days1 <- seq(as.Date(CI04_effort$Start_date[1]), as.Date(CI04_effort$End_date[1]), by = 'days')  
CI04effort_days2 <- seq(as.Date(CI04_effort$Start_date[2]), as.Date(CI04_effort$End_date[2]), by = 'days')  
CI04effort_days3 <- seq(as.Date(CI04_effort$Start_date[3]), as.Date(CI04_effort$End_date[3]), by = 'days')  
CI04effort_days4 <- seq(as.Date(CI04_effort$Start_date[4]), as.Date(CI04_effort$End_date[4]), by = 'days')  
CI04effort_days5 <- seq(as.Date(CI04_effort$Start_date[5]), as.Date(CI04_effort$End_date[5]), by = 'days')  
CI04effort_days6 <- seq(as.Date(CI04_effort$Start_date[6]), as.Date(CI04_effort$End_date[6]), by = 'days')
CI04effort_days7 <- seq(as.Date(CI04_effort$Start_date[7]), as.Date(CI04_effort$End_date[7]), by = 'days')
CI04effort_days8 <- seq(as.Date(CI04_effort$Start_date[8]), as.Date(CI04_effort$End_date[8]), by = 'days')

#combine all of these into one array
CI04_effort_dates = c(CI04effort_days1,CI04effort_days2,CI04effort_days3, CI04effort_days4, CI04effort_days5, CI04effort_days6, CI04effort_days7, CI04effort_days8)

#save array as data frame and add a row that is 24 for each of the days

#times = number of rows in CI04_effort_dates
test<-rep(c(24),times=1019) 
dates_test<-cbind(CI04_effort_dates,test)
CI04_date_hours=as.data.frame(dates_test)
names(CI04_date_hours)[1] <-"numdate"
names(CI04_date_hours)[2] <-"acoustic_hours"
CI04_date_hours$date<-as.Date.numeric(CI04_date_hours$numdate)
#resave just those 2 columns
CI04_effort2 <- CI04_date_hours %>% 
  dplyr::select(date,acoustic_hours)

#removing duplicate day (overlap between end date of one deployment and start datae of next, so double counting that in between day)
CI04_effort2 <- CI04_effort2[-910, ]



#divide count of chorusing days by total effort days so that the units are in proportion, just like the histograms
CI04_summary$effortDays <- nrow(CI04_effort2)
CI04_summary$prop <- CI04_summary$count / CI04_summary$effortDays

#change HF to UF440 because UF440 was logged as HF (high frequency) instead of its unidentified fish frequency name (UF310)
CI04_summary$fish <- gsub('HF','UF440',CI04_summary$fish)

#colors for all 5 fish species
custom_colors <- c( "Bocaccio" = "deepskyblue", "Midshipman" = "darkorange", "UF310" = "green3", "White Seabass" = "firebrick2", "UF440" = "darkorchid")

CI04_summary$fish <- factor(
  CI04_summary$fish,
  levels = c(
    "White Seabass" ,
    "Bocaccio",
    "Midshipman",
    "UF310",
    "UF440" 
  )
)


#Rose Plot!
CI04Rose = ggplot(CI04_summary, aes(x = factor(ChorusHourPT), y = prop, fill = fish)) +
  geom_bar(stat = "identity") +
  coord_polar(start = 0) +
  theme_minimal() +
  scale_fill_manual(values = custom_colors, na.translate = FALSE) + 
  labs(x = "Hour of the Day (PST/PDT)", y = "Stacked Proportion of Daily Chorusing\n(# days with chorusing / # days recorded)", title = "Hourly Proportion of Daily Fish Chorusing at CI04" , fill = "Fish Species") +
  theme(axis.text.x = element_text(size = 12)) +
  scale_y_continuous(
    breaks = c(0.4, 0.8, 1.2, 1.6),
    expand = c(0, 0))+
  #can add this following line if you want the 0 on the y axis to be labeled. I removed it because it was making it hard to see bars around 0
  #annotate("text", x = 10.5, y = 0.4, label = "0.4", color = "black") +
  annotate("text", x = 10.5, y = .4, label = "0.4", color = "black") +
  annotate("text", x = 10.5, y = .8, label = "0.8", color = "black") +
  annotate("text", x = 10.5, y = 1.6, label = "1.6", color = "black")  +
  annotate("text", x = 10.5, y = 1.2, label = "1.2", color = "black")  +
  theme(axis.text.y = element_blank()) 

CI04Rose


outDirG = "C:/Users/embe5980/SoundscapesWebsite/content/resources"
ggsave(filename = paste0(outDirG, "/CI04_FinalRosePlot.jpg"), dpi = 300)

#proportion of days where fish (bocaccio) chorused out of all days where we were recording during that hour (20)


# ><(((*>  <*)))><   ><(((*>  <*)))><  ><(((*>  <*)))><  ><(((*>  <*)))><  ><(((*>  <*)))><  ><(((*>  <*)))><  
# ><(((*>  <*)))><   ><(((*>  <*)))><  ><(((*>  <*)))><  ><(((*>  <*)))><  ><(((*>  <*)))><  ><(((*>  <*)))><  

#3. SEASONAL HISTOGRAM for MONTHLY NORMALIZED TOTAL hours COMPARING MOST RECENT YEAR OF DATA TO EVERYTHING BEFORE

# ><(((*>  <*)))><   ><(((*>  <*)))><  ><(((*>  <*)))><  ><(((*>  <*)))><  ><(((*>  <*)))><  ><(((*>  <*)))><  

#set directory
setwd("C:/Users/embe5980/Indicators/WCR_fish_ella/Code/MB02")  

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
