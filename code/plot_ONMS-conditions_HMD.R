# Soundscape Condition | tracking how sound levels change in different frequencies using the soundscape metrics

# ADDS context metadata to hourly HMD-- season
# INPUTS: output of HrHMDONMS.R, loads the most recent file; ONMS metadata; wind Model
# works for each monitoring site

# LIBRARIES ####
#devtools::install_github('TaikiSan21/PAMscapes')

library(patchwork)
library(reshape2)
library(PAMscapes)
library(scales)
library(gridExtra)
library(ggplot2)
library(tidyverse)
library(openxlsx)
library(reshape)

library(gtable)
library(grid)
library(ggtext)
library(plotly)
library(viridis)
library(ggpp)


rm(list=ls()) 

#SITES ####
# ONMSsites = c("sb01", "sb03", "hi01", "hi03", "hi04", "hi08", "pm01", "pm02", "fgb01", "fk01", "fk05", "fk06", "fk08", "as01", "mb01", "mb02", "oc02")
# NRSsites "oc03", "hi00","ci05","sb09","as10","cb11","ch13","fgb06" 


ONMSsites = c("fgb06")


## directories ####
outDir   =  "C:/Users/embe5980/SoundscapesWebsite/" # Emma local git repo 
#outDir   =  "F:/CODE/GitHub/SoundscapesWebsite/" # your local git repo 
#outDir   =  "/Users/quca3108/SoundscapesWebsite/" # Quincy local git repo
#outDir = "X:/Emma_Beretta/SoundscapesWebsite/" #for GCP workstation remote desktop Emma
#outDir   = "~/GitHub/SoundscapesWebsite/" #GCP WW


outDirG  =  paste0(outDir,"content/resources/") #where save graphics
outDirGe =  paste0(outDir,"content/resources/extra") #where extra save graphics
outDirC  =  paste0(outDir,"context/") #where to get context

#PARAMETERS ####
DC = Sys.Date()
project = "ONMS"
# fqIn = "TOL_125"
# fqInN = "125 Hz"
# ab = 65 # threshold for above frequency in
# fqIn2 = "TOL_500" # wind model comparison- wind dominated frequency
# fqIn2name = "500 Hz" # wind model comparison- wind dominated frequency
ab2 = 0 #threshold for dB above a reference value
windUpp = 22.6 #which wind model result to show on plot
windLow = 1 #which wind model result to show on plot
windH = 10 #wind speeds categories
windL = 5 #wind speeds categories
removess = 0 # set to 1 if you want to truncate the time series by removing sanctsound data
removeShort = 360 #minimum number of hours needed to included a year in the graphics 

# CONTEXT ####
#reads information for all sites
metaFile = paste0(outDirG,"ONMSSound_IndicatorCategories.xlsx")
lookup = as.data.frame ( openxlsx :: read.xlsx(metaFile, sheet  = "Summary") ) #xlsx::read.xlsx(metaFile, sheetName = "Summary")
colnames(lookup) = lookup[1, ]         # Set first row as column names
lookup = as.data.frame( lookup[-1, ] ) # Remove the first row
lookup = as.data.frame( lookup[!apply(lookup, 1, function(row) all(is.na(row))), ] )

## TIMES OF INTEREST ####
TOI = as.data.frame (openxlsx :: read.xlsx(metaFile, sheet = "Time period of interest") )
TOI = TOI[!apply(TOI, 1, function(row) all(is.na(row))), ]
endSS = as.data.frame ( openxlsx :: read.xlsx(metaFile, sheet  = "SanctSound") )
endSS$endSS =  as.Date(endSS$endSS, origin = "1899-12-30")

## FREQUENCIES OF INTEREST ####
FOI = as.data.frame ( openxlsx ::read.xlsx(metaFile, sheet = "Frequency of Interest") )
FOI = FOI[!apply(FOI, 1, function(row) all(is.na(row))), ]
FOI$Sanctuary = tolower(FOI$Sanctuary)
FOI = FOI[FOI$`Show.on.plot?` == "Y",]
FOIp = FOI[FOI$`Track.this.FQ.as.indicator.for.sources?` == "Y",]

## TOL CONVERSION ####
# TOL_convert = read.csv(paste0(outDirC,"TOLconvert.csv"))
# TOL_convert$Nominal = paste0("TOL_",TOL_convert$Center)

## WIND NOISE MODEL ####
windFile = list.files(outDirC, pattern = paste0("WindModel_", project), full.names = T)
file_info = file.info(windFile)
load( windFile[which.max(file_info$ctime)] ) #only load the most recent!




# PROCESS BY SITE #### 
for (uu in 1:length(ONMSsites)) { # uu = 1
  
  suppressWarnings ( rm(gps, outData) )
  cat("Processing... ", ONMSsites[uu],"\n" )
  site =  ONMSsites[uu]
  
  #renaming for NRS sites
  if (site == "oc03") {
    outDirP = paste0("Y:/soundscape_website_products/", substr(tolower(site),start = 1, stop =2),"/" ) #products
    site1 = "nrs03"
    site3 = "ocnrs03"
    site  = "NRS03"
    site5 = "oc03"
  } else if (site == "ac01"){
    outDirP = paste0("Y:/soundscape_website_products/", substr(tolower(site), start = 1, stop =2),"/" ) #products
    site1 = "nrs01"
    site3 = "acnrs01"
    site  = "NRS01"
    site5 = "ac01"
  } else if (site == "np02"){
    outDirP = paste0("Y:/soundscape_website_products/", substr(tolower(site), start = 1, stop =2),"/" ) #products
    site1 = "nrs02"
    site3 = "npnrs02"
    site  = "NRS02"
    site5 = "np02"
  } else if (site == "hi00"){
    outDirP = paste0("Y:/soundscape_website_products/", substr(tolower(site),start = 1, stop =2),"/" )
    site1 = "nrs04"
    site3 = "hinrs04"
    site  = "NRS04"
    site5 = "hi00"
  } else if (site == "ci05"){
    outDirP = paste0("Y:/soundscape_website_products/", substr(tolower(site),start = 1, stop =2),"/" ) #products
    site1 = "nrs05"
    site3 = "cinrs05"
    site  = "NRS05"
    site5 = "ci05"
  } else if (site == "fgb06"){
    outDirP = paste0("Y:/soundscape_website_products/", substr(tolower(site),start = 1, stop =2),"/" )
    site1 = "nrs06"
    site3 = "fgbnrs06"
    site  = "NRS06"
    site5 = "fgb06"
  } else if (site == "se07"){
    outDirP = paste0("Y:/soundscape_website_products/", substr(tolower(site), start = 1, stop =2),"/" ) #products
    site1 = "nrs07"
    site3 = "senrs07"
    site  = "NRS07"
    site5 = "se07"
  } else if (site == "ne08"){
    outDirP = paste0("Y:/soundscape_website_products/", substr(tolower(site), start = 1, stop =2),"/" ) #products
    site1 = "nrs08"
    site3 = "nenrs08"
    site  = "NRS08"
    site5 = "ne08"
  } else if (site == "sb09"){
    outDirP = paste0("Y:/soundscape_website_products/", substr(tolower(site), start = 1, stop =2),"/" ) #products
    site1 = "nrs09"
    site3 = "sbnrs09"
    site  = "NRS09"
    site5 = "sb09"
  } else if (site == "as10"){
    outDirP = paste0("Y:/soundscape_website_products/", substr(tolower(site),start = 1, stop =2),"/" )
    site1 = "nrs10"
    site3 = "asnrs10"
    site  = "NRS10"
    site5 = "as10"
  } else if (site == "cb11"){
    outDirP = paste0("Y:/soundscape_website_products/", substr(tolower(site),start = 1, stop =2),"/" )
    site1 = "nrs11"
    site3 = "cbnrs11"
    site  = "NRS11"
    site5 = "cb11"
  } else if (site == "vi12"){
    outDirP = paste0("Y:/soundscape_website_products/", substr(tolower(site),start = 1, stop =2),"/" ) #products
    site1 = "nrs12"
    site3 = "vinrs12"
    site  = "NRS12"
    site5 = "vi12"
  } else if (site == "ch13"){
    outDirP = paste0("Y:/soundscape_website_products/", substr(tolower(site),start = 1, stop =2),"/" ) #products
    site1 = "nrs13"
    site3 = "chnrs13"
    site  = "NRS13"
    site5 = "ch13"
  }else if (site == "fk08"){
    site1 = site
    site3 = "fk08"
    site5 = site
    outDirP = paste0( outDir,"products/", substr(tolower(site), start = 1, stop =2),"/" )#products
  } else {
    site1 = site
    site3 = site
    site5 = site
    outDirP = paste0( outDir,"products/", substr(tolower(site), start = 1, stop =2),"/" )#products
  }
  
  ## SITE PARAMETERS ####
  siteInfo = lookup[lookup$`NCEI ID` == tolower(site1),]
  siteInfo = siteInfo[!is.na(siteInfo$`NCEI ID`), ]
  siteInfo$MThreshold = as.numeric(siteInfo$"Threshold for excluding a month of data (default is 23 days)")
  
  
  # ##frequency of interest ####
  if (substr(site, 1,3) == "fgb"){
    FOIs = FOI [ FOI$Sanctuary == substr(site5, 1,3), ]
  } else if (substr(site, 1,5) == "NRS06"){
    FOIs = FOI [ FOI$Sanctuary == substr(site5, 1,3), ]
   } else {
    FOIs = FOI [ FOI$Sanctuary == substr(site5, 1,2), ]
  }
  
  FOIs <- FOIs [FOIs$Site == toupper(site5) | is.na(FOIs$Site), ]
  
  ##frequency(s) to track
  #any rows with blank site column apply to all sites in NMS, any rows with just on site apply only to that site
  FOIst = FOIs [ FOIs$`Track.this.FQ.as.indicator.for.sources?` == "Y", ]

  
  
  ##times of interest ####
  TOIs = TOI [ TOI$Site == (site1), ]
  TOIs <- TOIs %>%
    mutate(
      Start_Julian = as.numeric(format(as.Date(start_date), "%j")),
      End_Julian = as.numeric(format(as.Date(end_date), "%j")),
      Mid_Julian = (Start_Julian + End_Julian) / 2  # Midpoint for annotation
    )
  TOIs$yr = TOIs$Year
  
  ##seasonality ####
  sidx = siteInfo$Seasonality
  #put in alphabetical order so plots line up!!!
  if ( length(sidx) == 0 ) { # default
    season = data.frame(
      Season = c("Winter", "Spring", "Summer" , "Fall"),
      Months = c("1,2,3" , "4,5,6",  "7,8,9", "10,11,12"),
      values = c( "#56B4E9", "#009E73",  "#CC79A7", "#E69F00") )
    sidx = "wssf"
    seasonLabel = "Winter = Jan-Mar, Spring = Apr-Jun, Summer = Jul-Sep, Fall = Oct-Dec"
    seasonShift = 0
  }else if  ( sidx == "biological") {
    season = data.frame(
      Season = c("Early", "Peak", "Late", "Non"),
      Months = c("12,1", "2,3", "4,5", "6,7,8,9,10,11") ,
      values = c(  "#56B4E9", "#CC79A7", "#009E73", "#B3B3B3") )
    seasonLabel = "Early = Dec-Jan, Peak = Feb-Mar, Late = Apr-May, Non = Jun-Nov"
    seasonShift = 1 # this is the offset for HI sites- where the december is part of the next year
  }else if  ( sidx == "upwelling") {
    season = data.frame(
      Season = c( "Upwelling", "Post-Upwelling", "Winter"),
      Months = c( "3,4,5,6", "7,8,9,10,11", "12,1,2") ,
      values = c(  "#009E73", "#CC79A7",  "#56B4E9") )
    seasonLabel = "Upwelling = Mar-Jun, Post-Upwelling = Jul-Nov, Winter = Dec-Feb"
    seasonShift = 0
  }else if ( sidx == "wssf") {
    season = data.frame(
      Season = c("Winter", "Spring", "Summer" , "Fall"),
      Months = c("1,2,3" , "4,5,6",  "7,8,9", "10,11,12"),
      values = c( "#56B4E9", "#009E73",  "#CC79A7", "#E69F00"))
    seasonLabel = "Winter = Jan-Mar, Spring = Apr-Jun, Summer = Jul-Sep, Fall = Oct-Dec"
    seasonShift = 0
  }else if ( sidx == "southernHem") {
    season = data.frame(
      Season = c(  "Hurricane", "Tradewind", "Tradewind & Humpback" ),
      Months = c("11,12,1,2,3,4",  "5,6,7", "8,9,10" ) ,
      values = c(  "#CC79A7", "#E69F00",  "#009E73") )
    seasonLabel = "Hurricane = Nov-Apr, Tradewind = May-Jul, Tradewind & Humpback = Aug-Oct"
    seasonShift = 0
  }
  
  ## SPL data product ####
  inFile = list.files(outDirP, pattern = paste0("HMDdata_", tolower(site1), "_HourlySPL-gfs_.*\\.Rda$"), full.names = T)
  file_info = file.info(inFile) 
  load( inFile[which.max(file_info$ctime)] ) #only load the most recent!
  # fk05 being weird w loading in most recent, used this 
  #load("X:/Emma_Beretta/SoundscapesWebsite/products/sb/HMDdata_sb03_HourlySPL-gfs_2026-01-21.Rda")
  
  if( exists("outData") ) {
    gps = outData
    rm(outData)
  }
  
  st = as.Date( min(gps$UTC) )
  ed = as.Date( max(gps$UTC) )
  udays = length( unique(as.Date(gps$UTC)) )
  #cat("Input Data - ", site, " has ", udays, " unique days (", as.character(st), " to ",as.character(ed), ")\n")
 
  
  # FOR NEW OC02 DATA!!!
  #ALSO MB02
  # these sites have duplicate data (two deployments overlapped) so we need to average dB values 
  # between Jan 29 2024 and March 15 2024
  # should be good now sinc I saved non duplicate data to products, but keeping incase we need to fix this issue again
  
  # if (site == "oc02"){
  # 
  #   #gps row 12340 to 14561 are duplicates
  #   which(gps$UTC == "2024-01-29 00:00:00")
  #   which(gps$UTC == "2024-03-15 08:00:00")
  # 
  #   #pull out bad rows
  #   fix <- gps[12340:14561,]
  # 
  #   # Check how many times each `UTC` appears
  #   utc_counts <- gps %>%
  #     group_by(UTC) %>%
  #     summarize(count = n())
  # 
  #   occurrences_summary <- utc_counts %>%
  #     group_by(count) %>%
  #     summarize(num_utc_bins = n()) %>%  # How many UTC bins appear each number of times
  #     arrange(count)
  # 
  #   hi <- utc_counts %>% filter(count == 2)
  # 
  #   #hmmm more duplicates than I thought, average the whole dataset!
  #   #make sure output has same number of rows removed as number of rows of hi dataset above
  #   #takes a little...
  #    gps_avg <- gps %>%
  #     group_by(UTC) %>%
  #     summarize(
  #       Latitude = first(Latitude),
  #       Longitude = first(Longitude),
  #       software = first(software),
  #       yr = first(yr),
  #       mth = first(mth),
  #       site = first(site),
  #       windU = first(windU),
  #       windV = first(windV),
  #       precRate = first(precRate),
  #       matchLong = first(matchLong),
  #       matchLat = first(matchLat),
  #       matchTime = first(matchTime),
  #       windMag = first(windMag),
  #       across(starts_with("HMD"), mean, .names = "{.col}"),
  #       .groups = "drop" )
  # 
  #   gps_old <- gps
  #   gps <- gps_avg
  # 
  #   #save new data without duplicate rows
  #   outData = gps
  #   save(outData, file = paste0(outDirP, "HMDdata_", tolower(site), "_HourlySPL-gfs_", DC, ".Rda") )
  # 
  # }
  
  
  #removing HMD_20 from SS data so that it lines up with ONMS data
  # if(site == "sb03"){
  #   gps = gps[, -c(2:22)]
  # }
  # else if(siteInfo$`SanctSound Portal` != 'no'){
  #   gps = gps[, -2]
  # }
  
  #for GR01! Removing HMD_20 until we can fix data quality matrix to have 20 Hz ONMS data
  # if (site == "gr01"){
  # gps = gps[, -2]
  # }
   
  # #for SB03! forgot to remove HMD_0-19 during processing
  # if (site == "sb03"){
  # gps = gps[, -c(2:22)]
  # }
  
  
  #MB02: remove HMD_0-20
  if (site == "mb02"){
  gps = gps[, -c(2:22)]
  }
  
  #remove 2018 from all data for hi01 b/c only 1 day of recording
  # if(site == "hi01"){
  #   gps = gps %>%
  #     filter(yr != 2018)
  # }
  
  Fq = as.numeric( as.character( gsub("HMD_", "",  colnames(gps)[grep("HMD", colnames(gps))] ) ))

  
  # ##REMOVE sanctsound data ####
  # if (removess == 1){
  #   idxDD = which(endSS$site == site)
  #   if (length(idxDD) > 0) {
  #     gpsT = gps[gps$UTC > endSS$endSS[idxDD],] 
  #     gps = gpsT
  #   }
  #   
  # }
  
  ## calculate spectrum band levels in the TOLs ####
  # assumes all TOL data are in broadband measurements, NOT per/Hz
  # # this is needed to compare with the wind model results 
  # tol_columns = grep("TOL", colnames(gps))
  # gpsTOL = gps
  # 
  # for( cc in 1:length(tol_columns)) {
  #   toltmp = colnames(gps)[tol_columns[cc]]
  #   #find the bandwidth edges
  #   bw   = TOL_convert$Bandwidth[which(TOL_convert$Nominal == toltmp) ] 
  #   # cat("bandwidth for", toltmp, "is", bw, "\n" ) 
  #   dtmp = gps[,tol_columns[cc] ] 
  #   if ( length(bw) > 0 ){
  #     gps[,tol_columns[cc] ] = 10*log10 ( (10^(dtmp/10))/ bw )  
  #   } else {
  #     gps[,tol_columns[cc] ] = NA
  #   }
  # }
  #CHECK difference
  # gpsTOL$TOL_250 - gps$TOL_250 #should be the same offset for each frequency
  
  ## add SEASON ####
  seas = unique(season$Season)
  for( ss in 1:length(seas) ){
    moi = as.numeric(unlist(strsplit(as.character(season$Months[ss]), ","))) 
    gps$Season[gps$mth %in% moi] = season$Season[ss]
    rm(moi)}
  
  ##REMOVE years with low data ####
  # to make sure at least 15 day in a given year.
  gps$yr = year(gps$UTC)
  years_to_keep <- gps %>%
    count(yr) %>%
    filter(n >= removeShort) %>%
    pull(yr)
  
  #also remove years indicated by sanctuary managers to exclude on spreadsheet
  # years_to_remove <- siteInfo$`Years to Exclude `
  # 
  # if(is.na(years_to_remove) || years_to_remove %in% c("n/a", "unknown")){
  #   years_to_remove <- numeric(0)
  # } else {
  #   years_to_remove <- as.numeric(strsplit(years_to_remove, ",\\s*")[[1]])
  # }
  # 
  # #remove years to remove from years to keep if that applies
  # years_to_keep <- setdiff(years_to_keep, years_to_remove)
  # 
  # #apply years to keep
  # #save as new dataset because we only want to exclude data from Annual graphics, not seasonal or wind or FOI
  # gpsAG = gps %>%
  #   filter(yr %in% years_to_keep)
  # 
  # stAG = as.Date( min(gpsAG$UTC) )
  # edAG = as.Date( max(gpsAG$UTC) )
  # udaysAG = length( unique(as.Date(gpsAG$UTC)) )
  

  
  
  #Removing frequencies that have instrument issues that were not marked in data quality matrix
  #DIPS at ~3k Hz. They vary by site.

 if (site == "ch01"){
    
    #for seasonal 
    gps <- gps %>%
      mutate(across(num_range("HMD_", 2627:4300), ~ NA))
    
    #for annual
    # gpsAG <- gpsAG %>%
    #   mutate(across(num_range("HMD_", 2627:4300), ~ NA))
    
  } else if (site == "mb05"){
    
    #make those columns na
    gps <- gps %>%
      mutate(across(num_range("HMD_", 2701:4995), ~ NA))
    
    # gpsAG <- gpsAG %>%
    #   mutate(across(num_range("HMD_", 2701:4995), ~ NA))
    
  } else if (site %in% c("NRS01","NRS02","NRS03", "NRS04", "NRS05","NRS06","NRS07","NRS08","NRS09","NRS10","NRS11","NRS12","NRS13")){
      #(site == "NRS05"){
      gps <- gps %>%
       mutate(across(num_range("HMD_", 1249:1252), ~ NA))

     # gpsAG <- gpsAG %>%
      # mutate(across(num_range("HMD_", 1249:1252), ~ NA))

      
  } else if (site == "fgb01" | site == "fk08"){
    
    #make those columns na
    gps <- gps %>%
      mutate(across(num_range("HMD_", 2500:3900), ~ NA))
    
    
  } else if (site == "gr01" | site == "fk01"){
    
    #make those columns na
    gps <- gps %>%
      mutate(across(num_range("HMD_", 2800:4200), ~ NA))
    
    
  }else if (site == "fk05"){
    
    #make those columns na
    gps <- gps %>%
      mutate(across(num_range("HMD_", 2100:3900), ~ NA))
    
  } else if (site == "sb01" | site == "sb03"){
    
    #make those columns na
    gps <- gps %>%
      mutate(across(num_range("HMD_", 2900:4300), ~ NA))
    
  } 
    
  
  
  # use this code to manually find what frequencies to remove dip
  # dipS = mallDataS %>% filter(Quantile == "50%")
  # dipS = dipS %>% filter(Season == "Winter")
  # 
  # 
  # dip = mallData %>% filter(Quantile == "50%")
  # dip = dip %>% filter(Year == 2022)
  
  
  
  
  
  # CHECK: DATA SUMMARY ####
  cat(site, "Context Summary:\n", siteInfo$`Oceanographic category`,  "; season-",  unique(gps$Season), 
      "; times of interest-", nrow(TOIs), 
      "; frequencies of interest- ", nrow(FOIst), "\n",
      "Input Data - ", udays, " unique days (", as.character(st), " to ",as.character(ed), ")\n")
  
  
  
  #(1) EFFORT ALL DATA ####
  ## by month (days/ month-year) ####
  ## by season (hours/ season in each year) ####
  
  # summary <- gpsAG %>%
  #   mutate(
  #     year  = year(UTC),  # Extract Year
  #     month = format(UTC, "%m")  # Extract Month (numeric format)
  #   ) %>%
  #   count(year, month)  # Count occurrences (hours) in each year-month
  # summary$dy = round(summary$n/ 24)
  # 
  
  # season for HI sites includes December from the previous years- so make adjustment here,
  # if not HI this adds 0 so does nothing
  if ( substr(site, 1, 2) == "pm") {
    #shift october-december to the next year
    gps$yr[gps$mth == 12] = gps$yr[gps$mth == 12] + seasonShift
    gps$yr[gps$mth == 11] = gps$yr[gps$mth == 11] + seasonShift   #for PM sites, Eden wants to see with October and November counted as next year
    gps$yr[gps$mth == 10] = gps$yr[gps$mth == 10] + seasonShift   #for PM sites, Eden wants to see with October and November counted as next year
    
    # gpsAG$yr[gpsAG$mth == 12] = gpsAG$yr[gpsAG$mth == 12] + seasonShift
    # gpsAG$yr[gpsAG$mth == 11] = gpsAG$yr[gpsAG$mth == 11] + seasonShift   #for PM sites, Eden wants to see with October and November counted as next year
    # gpsAG$yr[gpsAG$mth == 10] = gpsAG$yr[gpsAG$mth == 10] + seasonShift   #for PM sites, Eden wants to see with October and November counted as next year
    # 
    
    # summary$year[summary$month == 12] =  summary$year[summary$month == 12] + seasonShift
    # summary$year[summary$month == 11] =  summary$year[summary$month == 11] + seasonShift   #for PM sites, Eden wants to see with October and November counted as next year
    # summary$year[summary$month == 10] =  summary$year[summary$month == 10] + seasonShift  #for PM sites, Eden wants to see with October and November counted as next year
    # 
    # #removing Sept 2022 for PM01, we only want Oct-Jul, usually no data recorded in Sept
    # summary <- summary[!(summary$year == 2022 & summary$month == "09"), ]
    
    #removing Sept 2022 for PM01, we only want Oct-Jul, usually no data recorded in Sept
    gps <- gps[!(gps$yr == 2022 & gps$mth == 9), ]
    
    # gpsAG <- gpsAG[!(gpsAG$yr == 2022 & gpsAG$mth == 9), ]
    
    # #order months to start with Oct for effort graph
    # summary$month <- factor(summary$month, levels = c("10", "11", "12", "01", "02", "03", "04", "05", "06", "07"))
    # month_nums <- as.numeric(as.character( sort(unique(summary$month)) ))
    # peakDays <- sum(summary$dy)
  } else if ( substr(site, 1, 2) == "hi") {
    #shift december to the next year
    gps$yr[gps$mth == 12] = gps$yr[gps$mth == 12] + seasonShift
    
    if (site == "hi01"){
      gps <- gps[!(gps$yr == 2018 & gps$mth == "11"), ]
    }
    
    # gpsAG$yr[gpsAG$mth == 12] = gpsAG$yr[gpsAG$mth == 12] + seasonShift
    
    # summary$year[summary$month == 12] =  summary$year[summary$month == 12] + seasonShift
    # 
    # #order months to start with Dec for effort graph
    # summary$month <- factor(summary$month, levels = c("11", "12", "01", "02", "03", "04", "05", "06", "07"))
    # month_nums <- as.numeric(as.character( sort(unique(summary$month)) ))
    # peakDays <- sum(summary$dy)
  }
  
  
  years_to_remove <- siteInfo$`Years to Exclude `
  
  if(is.na(years_to_remove) || years_to_remove %in% c("n/a", "unknown")){
    years_to_remove <- numeric(0)
  } else {
    years_to_remove <- as.numeric(strsplit(years_to_remove, ",\\s*")[[1]])
  }
  
  #remove years to remove from years to keep if that applies
  years_to_keep <- setdiff(years_to_keep, years_to_remove)
  
  #apply years to keep
  #save as new dataset because we only want to exclude data from Annual graphics, not seasonal or wind or FOI
  gpsAG = gps %>%
    filter(yr %in% years_to_keep)
  
  
  #removing dips that are specific to certain seasons and years
  if (site == "mb01"){
    
    #make those columns na
    gps <- gps %>%
      mutate(across(num_range("HMD_", 3115:4330), 
                    ~ if_else(Season != "Post-Upwelling", NA_real_, .)))
    
    gpsAG <- gpsAG %>%
      mutate(across(num_range("HMD_", 3115:4330), ~ NA))
    
  } else if (site == "mb02"){
    
    #for all seasons
    gps <- gps %>%
      mutate(across(num_range("HMD_", 3270:4330), ~ NA))
    
    #for 2023 and 2024 higher dip
    gpsAG <- gpsAG %>%
      mutate(across(num_range("HMD_", 3270:4330), 
                    ~ if_else(yr != 2022, NA_real_, .)))
    
    #for 2022 lower dip
    gpsAG <- gpsAG %>%
      mutate(across(num_range("HMD_", 1912:2567), 
                    ~ if_else(yr == 2022, NA_real_, .)))
    
  } else if (site == "ci04"){
    
    #for winter and post-upwelling lower dip
    gps <- gps %>%
      mutate(across(num_range("HMD_", 2175:2567), 
                    ~ if_else(Season != "Upwelling", NA_real_, .)))
    
    #for upwelling higher dip
    gps <- gps %>%
      mutate(across(num_range("HMD_", 3115:4330), 
                    ~ if_else(Season == "Upwelling", NA_real_, .)))
    
    #for 2023 and 2022 lower dip
    gpsAG <- gpsAG %>%
      mutate(across(num_range("HMD_", 2175:2567), 
                    ~ if_else(yr != 2024, NA_real_, .)))
    
    #for 2024 higher dip
    gpsAG <- gpsAG %>%
      mutate(across(num_range("HMD_", 3115:4330), 
                    ~ if_else(yr == 2024, NA_real_, .)))
  } 
  
  
  stAG = as.Date( min(gpsAG$UTC) )
  edAG = as.Date( max(gpsAG$UTC) )
  udaysAG = length( unique(as.Date(gpsAG$UTC)) )
  
  
  
  
  summary <- gpsAG %>%
    mutate(
      year  = year(UTC),  # Extract Year
      month = format(UTC, "%m")  # Extract Month (numeric format)
    ) %>%
    count(year, month)  # Count occurrences (hours) in each year-month
  summary$dy = round(summary$n/ 24)
  
  
  # season for HI sites includes December from the previous years- so make adjustment here,
  # if not HI this adds 0 so does nothing
  if ( substr(site, 1, 2) == "pm") {
    #shift october-december to the next year
    summary$year[summary$month == 12] =  summary$year[summary$month == 12] + seasonShift
    summary$year[summary$month == 11] =  summary$year[summary$month == 11] + seasonShift   #for PM sites, Eden wants to see with October and November counted as next year
    summary$year[summary$month == 10] =  summary$year[summary$month == 10] + seasonShift  #for PM sites, Eden wants to see with October and November counted as next year

    #removing Sept 2022 for PM01, we only want Oct-Jul, usually no data recorded in Sept
    summary <- summary[!(summary$year == 2022 & summary$month == "09"), ]
    
    
    #order months to start with Oct for effort graph
    summary$month <- factor(summary$month, levels = c("10", "11", "12", "01", "02", "03", "04", "05", "06", "07"))
    #month_nums <- as.numeric(as.character( sort(unique(summary$month)) ))
    peakDays <- sum(summary$dy)
  } else if ( substr(site, 1, 2) == "hi") {
    #shift december to the next year
    summary$year[summary$month == 12] =  summary$year[summary$month == 12] + seasonShift
   
    
    #order months to start with Dec for effort graph
    summary$month <- factor(summary$month, levels = c("11", "12", "01", "02", "03", "04", "05", "06", "07"))
    #month_nums <- as.numeric(as.character( sort(unique(summary$month)) ))
    peakDays <- sum(summary$dy)
  }
  
  month_nums <- as.numeric(as.character( sort(unique(summary$month)) ))
  
  
  if(substr(site, 1, 2) == "pm" | substr(site, 1, 2) == "hi"){
    legend_label = "Humpback\nYear*"
  } else {
    legend_label = "Year"
  }
  
  if(substr(site, 1, 2) == "pm"){
    effort_title = "monitoring effort by humpback year (all data)"
  } else {
    effort_title = "monitoring effort by year (all data)"
  }
  

  
  p1 = ggplot(summary, aes(x = month, y = dy, fill = as.factor(year))) +
    geom_col(position = "dodge", width = .4) +  # Use dodge to separate bars for each year within the same month
    #coord_flip() +
    labs(
      title = effort_title,
      subtitle = paste0(toupper(site), " has ", udaysAG, 
                        " unique days: ", as.character(stAG), " to ", as.character(edAG)),
      x = "",
      y = "Days",
      fill = legend_label,
      # Update your p1 labs call:
        #caption = "Data&nbsp;from&nbsp;months&nbsp;with&nbsp;effort&nbsp;below&nbsp;the&nbsp;red&nbsp;horizontal&nbsp;line&nbsp;are&nbsp;excluded&nbsp;from&nbsp;annual&nbsp;sound&nbsp;levels&nbsp;figure&nbsp;above"
      caption = "Data from months with effort below the red horizontal line are excluded from annual sound levels figure above"
    ) +
    scale_x_discrete(labels = month.abb[month_nums]) +  # Show month names instead of numbers
    #scale_fill_manual(values = rev(gray.colors(length(unique(summary$year))))) +  # Create grayscale colors
    scale_fill_manual(values = rev(colorRampPalette(c("darkblue", "lightblue"))(length(unique(summary$year))))) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0),
      axis.title.y = element_text(size = 14),
      axis.text.y = element_text(size = 14),
      axis.text.x = element_text(size = 14, hjust = 1, angle = 30),  
      plot.subtitle = element_text(size = 14),
      legend.text = element_text(size = 12),
      plot.caption = ggtext::element_markdown(hjust = 0, size = 11),
      legend.position = "right" 
    ) +
    #adding marker for cutoff threshold (months need more than 23 days of data to be kept in line graph)
    geom_hline(yintercept = siteInfo$MThreshold,    
               linetype = "dashed",
               color = "red",
               linewidth = .5)
  
  p1
  ##save figure ####
  ggsave(filename = paste0(outDirGe, "/plot_", toupper(site), "_HMDEffort.jpg"), plot = p1, width = 10, height = 4, dpi = 300)
  
  
  #remove any months that have less than siteInfo$MThreshold days of data from gps (ex: 23 days of data = 552 hours)
  #for PM01: 03/20, 04/20, 06/20, 07/21, 07/23, 06/24
  
  summary$month = as.character(summary$month)
  summary$month = as.numeric(summary$month)
  summaryX = summary %>% filter(n < (siteInfo$MThreshold*24)) %>% dplyr::select(year, month)
  
  
  gpsAGnew = anti_join(gpsAG, summaryX, by = c("yr" = "year",
                                           "mth" = "month"))
  gpsAGold = gpsAG
  gpsAG = gpsAGnew
  
  
  summary2 = gps %>%
    mutate(
      year  = yr,  # Extract Year
      month = mth  # Extract Month (numeric format)
    ) %>%
    count(year, Season)  # Count occurrences (hours) in each year-month
  
  summary2$dy = round(summary2$n/ 24)
  seasont = season %>% filter(Season %in% unique(summary2$Season) )
  
  
  ##reordering seasons in legend so they are chronological, not alphabetical
  # may need to make adjustments if this site doesnt have data from all seasons
  if  ( sidx == "biological") {
    summary2$Season <- factor(summary2$Season, levels = c("Early", "Peak", "Late", "Non"))
    seasont$Season <- factor(seasont$Season, levels = c("Early", "Peak", "Late", "Non"))
    seasont <- season[order(seasont$Season), ]
    
  } else if(site == "ch01"){
    summary2$Season <- factor(summary2$Season, levels = c("Post-Upwelling"))
    
  } else if (sidx == "upwelling") {
    summary2$Season <- factor(summary2$Season, levels = c("Upwelling", "Post-Upwelling", "Winter"))
    seasont$Season <- factor(seasont$Season, levels = c("Upwelling", "Post-Upwelling", "Winter"))
    seasont <- season[order(seasont$Season), ]
    
  } else if (site %in% c("fk07", "fgb01", "fk08")){
    summary2$Season <- factor(summary2$Season, levels = c("Spring", "Summer", "Fall"))
    
  } else if (site == "fk06"){
    summary2$Season <- factor(summary2$Season, levels = c("Winter", "Spring", "Summer")) 
    
  } else if ( sidx == "wssf") {
    summary2$Season <- factor(summary2$Season, levels = c("Winter", "Spring", "Summer", "Fall"))
    seasont$Season <- factor(seasont$Season, levels =  c("Winter", "Spring", "Summer", "Fall"))
    seasont <- season[order(seasont$Season), ]
    
  } else if ( length(sidx) == 0) {
    summary2$Season <- factor(summary2$Season, levels = c("Winter", "Spring", "Summer", "Fall"))
    seasont$Season <- factor(seasont$Season, levels =  c("Winter", "Spring", "Summer", "Fall"))
    seasont <- season[order(seasont$Season), ]
  }
  
  p2 = ggplot(summary2, aes(x = as.character(year), y = dy, fill = as.factor(Season))) +
    geom_col(position = "dodge", width = .3) +  # Use dodge to separate bars for each year within the same month
    #coord_flip()+ 
    labs(
      title = "monitoring effort by season (all data)",
      subtitle  = paste0(toupper(site), " has ", udays, 
                         " unique days: ", as.character(st), " to ", as.character(ed)), #, "\n", seasonLabel),
      x = "",      
      y = "Days",      
      fill = "Season") +
    scale_fill_manual(values = seasont$values) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0),
      axis.title.y = element_text(size = 14),
      axis.text.y = element_text(size = 14),
      axis.text.x = element_text(size = 14, hjust = 1, angle = 30),  
      plot.subtitle = element_text(size = 12),
      legend.text = element_text(size = 12),
      legend.position = "right" 
    )
  p2
  ##save figure ####
  ggsave(filename = paste0(outDirGe, "/plot_", toupper(site), "_HMDEffortSeason.jpg"), plot = p2, width = 10, height = 4, dpi = 300)
  
  
  
  
  #(2) WIND category ####
  gps$wind_category = NA
  gps <- gps %>%
    mutate(wind_category = case_when(
      is.na(windMag) ~ NA_character_,
      windMag < windL ~ "low",
      windMag >= windL & windV <= windH ~ "med",
      windMag > windH ~ "high"
    ))
  # Calculate the counts for wind each category
  category_counts = gps %>%
    count(wind_category) %>%
    mutate(label = paste(wind_category, ":", n))
  subtitle_text <- paste(category_counts$label, collapse = ", ")
  windInfo = windModel[tolower(windModel$si) == site3,]
  colnums = suppressWarnings(as.numeric(colnames(windInfo)))
 # widx = which(!is.na(colnums) & colnums == max(Fq))
 # windInfo = windInfo[,1:widx] #wind noise by frequency (columns) and speed (rows)
  
  #re-structure for ggplot
  mwindInfo = reshape2::melt(windInfo, id.vars = c("windSpeed"), measure.vars = colnames(windInfo)[4:ncol(windInfo)])
  mwindInfo$variable = as.numeric( as.character(mwindInfo$variable ))
 
   ## wind bar
  gps$wind_category <- factor(gps$wind_category, levels = c("low", "med", "high"))
 
   # Create the horizontal stacked bar plot with the counts of wind speed categories
  l = ggplot(gps, aes(x = "", fill = wind_category)) +
    geom_bar(stat = "count", position = position_stack(reverse = TRUE)) +  # Stacked bar chart
    coord_flip() +  # Flip the coordinates to make it horizontal
    facet_wrap(~yr, ncol = 1)+
    ggtitle("Percent time in different wind speed categories ") +  # Add the main title
    theme_minimal() +
    labs(x = NULL, y = NULL,
         subtitle = paste0("low <", windL, " | med ", windL,"-",windH, " | high >",windH, "m/s")) +  # Remove x-axis label
    theme(
      plot.title = element_text(hjust = 0, size = 16),  # Align the title to the left
      plot.subtitle = element_text(hjust = 0, size = 14),
      strip.text  = element_text(size = 14),
      axis.text.y = element_blank(),
      axis.text.x = element_blank(),  # Remove x-axis labels (now categories will appear below)
      axis.ticks.x = element_blank(),  # Remove x-axis ticks
      axis.title.x = element_blank(),  # Remove x-axis title
      legend.position = "bottom",  # Position the legend at the bottom
      legend.title = element_blank(),  # Optional: remove legend title
      legend.text = element_text(size = 12)  # Optional: adjust legend text size
    ) +
    #scale_x_discrete(labels = category_counts$wind_category) +  # Place the category labels under the plot
    scale_fill_manual(values = c("low" = "blue",  "med" = "orange", "high" = "red") )
  l
  #save figure ####
  ggsave(filename = paste0(outDirGe, "/plot_", toupper(site), "_HMDwindSpeed.jpg"), plot = l , width = 10, height = 12, dpi = 300)
  
  
  
  
  #(3) SEASONAL CONDITION PLOT ####
  caption_text = paste0(
    "<b>",toupper(site) , " </b> (", siteInfo$`Oceanographic category`, ")<br>",
    "<b>Vertical lines and grey shaded areas</b> indicate frequencies for sounds of interest in this soundscape<br>",
    "<b>Black lines</b> are modeled wind noise at this depth [", windLow, " m/s & ", windUpp, " m/s]<br>",
    "<b>Dotted sound level curve</b> is the median for all data<br>",
    "<b>Solid sound level curves and colored shaded areas</b> are the seasonal medians and 25th-75th percentiles for all data")
  
  
  # PERCENTILES for all the data ####
  # all data - NOTE recalculated below if only plotting peak
  hmd_columns = grep("HMD", colnames(gps))
  all_quantiles = apply(gps[, hmd_columns, drop = FALSE], 2, quantile, 
                        probs = c(0.99, 0.90, 0.75, 0.50, 0.25, 0.10, .01), na.rm = TRUE)
  
  # this re-formats the quantiles for plotting
  All = as.data.frame( all_quantiles )
  All$Quantile = rownames(All)
  All$Year = "all"
  hmd_columns = grep("HMD", colnames(All))
  mALL = reshape2::melt(All, id.vars = c("Quantile","Year"), measure.vars = hmd_columns)
  mALL$variable = as.numeric( as.character( gsub("HMD_", "", mALL$variable )))
  colnames(mALL) = c("Quantile", "Year", "Frequency" , "SoundLevel" )
 
   # by season
  hmd_columns = grep("HMD", colnames(gps))
  season_split = split(gps, gps$Season)
  season_quantiles = lapply(season_split, function(season_data) {
    apply(season_data[, hmd_columns, drop = FALSE], 2, quantile, 
          probs = c(0.99, 0.90, 0.75, 0.50, 0.25, 0.10, .01), na.rm = TRUE)   })
  
  # by year -- recalculated below if only plotting peak
  yr_split = split(gps, gps$yr) # Calculate quantiles for each year
  year_quantiles = lapply(yr_split, function(season_data) {
    apply(season_data[, hmd_columns, drop = FALSE], 2, quantile,  
          probs = c(0.99, 0.90, 0.75, 0.50, 0.25, 0.10, .01), na.rm = TRUE)   })
  
  # by year 1 
  yr1 = min( gps$yr )
  hmd_columns = grep("HMD", colnames(gps))
  yr1.df = gps[ gps$yr == yr1,]
  yr1_quantiles = apply(yr1.df[, hmd_columns, drop = FALSE], 2, quantile, 
                        probs = c(0.99, 0.90, 0.75, 0.50, 0.25, 0.10, .01), na.rm = TRUE)
  
  
  
 # tol_columns = grep("TOL", colnames(gps))
  
  seasonAll = NULL
  for (ii in 1: length(season_quantiles) ) {
    tmp = as.data.frame ( season_quantiles[ii] ) 
    colnames(tmp) = colnames(gps)[hmd_columns]
    tmp$Quantile  = rownames(tmp)
    tmp$Season    = names(season_quantiles)[ii]
    rownames(tmp) = NULL
    seasonAll = rbind(seasonAll,tmp)
  }
  hmd_columns = grep("HMD", colnames(seasonAll))
  
  ### format for plot ####
  mallData = reshape2::melt(seasonAll, id.vars = c("Quantile","Season"), measure.vars = hmd_columns)
  mallData$variable = as.numeric( as.character( gsub("HMD_", "", mallData$variable )))
  colnames(mallData) = c("Quantile", "Season", "Frequency" , "SoundLevel" )
  fqupper = max(as.numeric( as.character( mallData$Frequency) ))
  seasont = season %>% filter(Season %in% unique(mallData$Season) )
  
  #If upper limit of shaded FOI is greater than upper limit of data, scaale_x_log will remove shading
  #adjust any upper limit that goes over upper limit of data
  #FOIs$FQend[FOIs$FQend > fqupper] <- fqupper
  
  
  ##reordering seasons in legend so they are chronological, not alphabetical
  # may need to make adjustments if this site doesnt have data from all seasons
  if  ( sidx == "biological") {
    mallData$Season <- factor(
      mallData$Season,
      levels = c("Early", "Peak", "Late", "Non")
    )
  }else if (sidx == "upwelling") {
    mallData$Season <- factor(
      mallData$Season,
      levels = c("Upwelling", "Post-Upwelling", "Winter"))
    
  }else if ( sidx == "wssf" | length(sidx) == 0) {
    mallData$Season <- factor(
      mallData$Season,
      levels = c("Winter", "Spring", "Summer", "Fall"))
    
    #no winter in fk07 so had to use this
    #mallData$Season <- factor(
    # mallData$Season,
    #levels = c("Spring", "Summer", "Fall"))
  }
  
  #if NRS, scale y min lower so that FOI labels are visible 
  NRSLabelShift <- if (substr(site, 1, 3) == "NRS") 39 else NA
  
  mallDataS = mallData
  
  
  
  #making so shaded FOIs (ranges) are set dash instead of just dash line
  FOIsRange <- FOIs %>% filter(FQstart != FQend)
  FOIs <- FOIs %>% filter(FQstart == FQend)
  

  #some FOIs want labels on left side
  # CI01: put bocaccio label on left side of vert line
  if(site == "ci01"){
    FOIsL <- FOIs %>% filter(Label == "Bocaccio Rockfish Chorus")
    FOIs <- FOIs %>% filter(Label != "Bocaccio Rockfish Chorus")
    
    FOIsRangeL = FOIs[0, ]
    
  } else {
    FOIsL = FOIs[0, ]
    FOIsRangeL = FOIs[0, ]
  }
  
  
  
#for sites that we removed 3k hz dip for some seasons but not all, remove median line in that gap  
  if (site == "mb01"){
    
    mALL <- mALL %>%
      mutate(SoundLevel = ifelse(
        (Frequency >= 3115 & Frequency <= 4330) & (Quantile == "50%"),
        NA, 
        SoundLevel
      ))
    
  } else if (site == "ci04"){
    
    mALL <- mALL %>%
      mutate(SoundLevel = ifelse(
        (Frequency >= 2175 & Frequency <= 2567) & (Quantile == "50%"),
        NA, 
        SoundLevel
      ))
  } 
 
  
  #height of FOI shading and vert lines label will change based on the length of the label
  if (site == "mb02" | site == "ch01"){
    label_height = 38
  } else if (site == "fgb01"){
    label_height = 33
  } else if (site == "pm01" |site == "pm02"){
    label_height = 32
  } else {
    label_height = 35
  }
  
  
#plot  
  p = ggplot() +
    
    # Wind model values
    geom_line(data = mwindInfo[as.character(mwindInfo$windSpeed) == windUpp,], aes(x = variable, y = value), color = "black",  linewidth = 1) +
    geom_line(data = mwindInfo[as.character(mwindInfo$windSpeed) == windLow,], aes(x = variable, y = value), color = "black",  linewidth = 1) +
    scale_x_log10(labels = label_number(),limits = (c(10,fqupper)), guide = "axis_logticks") +  # Log scale for x-axis
    
    # Add vertical lines at FOIs, label on right side
    geom_vline(data = FOIs, aes(xintercept = FQstart, color = Label), linetype = "dashed", color = "black",linewidth = .5) +
    geom_text(data = FOIs, aes(x = FQstart, y = label_height, label = Label), angle = 90, vjust = 1, hjust = 0.45, size = 4) +

    # Add vertical lines at FOIs, label on left side, not common
    geom_vline(data = FOIsL, aes(xintercept = FQstart, color = Label), linetype = "dashed", color = "black",linewidth = .5) +
    geom_text(data = FOIsL, aes(x = FQstart, y = label_height, label = Label), angle = 90, vjust = 0, hjust = 0.5, size = 4) +
   
    # Add vertical set dash lines and grey shaded region at FOI ranges
  #  geom_vline(data = FOIsRange, aes(xintercept = FQstart, color = Label), linetype = "dashed", color = "grey50",linewidth = .5) +
  #  geom_vline(data = FOIsRange, aes(xintercept = FQend, color = Label), linetype = "dashed", color = "grey50",linewidth = .5) +
    geom_rect(data = FOIsRange, aes(xmin = FQstart, xmax = FQend, ymin = -Inf, ymax = Inf), 
              fill = "gray", alpha = 0.2)+  # Adjust alpha for transparency
    geom_text(data = FOIsRange, aes(x = FQstart, y = label_height, label = Label), color = "black", angle = 90, vjust = 1, hjust = 0.45, size = 4) +
   
    # Add vertical set dash lines and grey shaded region at FOI ranges, label on left
   # geom_vline(data = FOIsRangeL, aes(xintercept = FQstart, color = Label), linetype = "dashed", color = "grey50",linewidth = .5) +
  #  geom_vline(data = FOIsRangeL, aes(xintercept = FQend, color = Label), linetype = "dashed", color = "grey50",linewidth = .5) +
    geom_rect(data = FOIsRangeL, aes(xmin = FQstart, xmax = FQend, ymin = -Inf, ymax = Inf), 
              fill = "gray", alpha = 0.2)+  # Adjust alpha for transparency
    geom_text(data = FOIsRangeL, aes(x = FQstart, y = label_height, label = Label), color = "black", angle = 90, vjust = 0, hjust = 0.5, size = 4) +
    
    #shading (25-75%) HMD values
    geom_ribbon(data = mallDataS %>%
                  pivot_wider(names_from = Quantile, values_from = SoundLevel),
                aes(x = Frequency, ymin = `25%`, ymax = `75%`, fill = Season),
                alpha = 0.2) +  # Use alpha for transparency
    
    # Median (50%) HMD values
    geom_line(data = mallDataS[mallDataS$Quantile == "50%",], 
              aes(x = Frequency, y = SoundLevel, color = Season), linewidth = 2) +
    geom_line(data = mALL[mALL$Quantile == "50%",], aes(x = Frequency, y = SoundLevel), color = "black", linewidth = 1,
              linetype = "dotted")+ 
    
    # Set color and fill to match season
    scale_color_manual(values  = seasont$values ) +
    scale_fill_manual (values  = seasont$values ) +
    
    labs(
       subtitle = seasonLabel,
       caption  = caption_text,
       x = "Frequency Hz",
       y = expression(paste("Sound Levels (dB re 1 ", mu, " Pa"^2, "/Hz)" ) ) #dB re 1 uPa^2/Hz
    ) +
    # Additional aesthetics
    scale_y_continuous(limits = c(30, NA)) +  # use to manually scale y minimum so vert line labels are visible
    theme_minimal()+
    theme(legend.position = "right",
          plot.caption = ggtext::element_markdown(hjust = 0, size = 12),
          axis.title.x = element_text(size = 14),           
          axis.title.y = element_text(size = 14), 
          legend.text = element_text(size = 12),
          axis.text = element_text(size = 14),
          axis.ticks.length.x = unit(0.25, "cm"), 
          axis.ticks.x = element_line(color = "grey", linewidth = 0.3)
          , axis.line.x = element_line(color = "grey", linewidth = 0.3)           
    )
  
  p
  separator <- grid.rect(gp = gpar(fill = "black"), height = unit(2, "pt"), width = unit(1, "npc"))
  # arranged_plot = grid.arrange(p, separator, l, heights =c(4, 0.05, 0.8))
  arranged_plot = grid.arrange(p, separator, p2, heights =c(4, 0.1, 1))
  ## save figure ####
  ggsave(filename = paste0(outDirG, "/plot_", toupper(site), "_HMDSeasonalSPL.jpg"), plot = arranged_plot, width = 10, height = 12, dpi = 300)
  
  
  
  
  
  
  #(4) ANNUAL COMPARISION plots ####
  # truncates data to peak season for biological sites (Pacific Island Region Sanctuaries) - all plots after this are peak season (early, peak, late) only!!!
  subtitle_text <- if (sidx == "biological" & substr(site, 1, 2) == "hi"){
    #"*Each line represents that year's humpback season which includes Early (Dec of previous year-Jan), Peak (Feb-Mar), \nand Late (Apr-May) seasons." 
    "*Humpback Year includes data from Dec of previous year through May of the given year. Humpback season excludes data from\n the Non season (Jun-Nov)."
  } else if (sidx == "biological" & substr(site, 1, 2) == "pm"){
    "*Humpback Year includes data from Oct - Dec of previous year through Jul of the given year."
  } else NULL
 
  
  if (substr(site, 1, 2) == "hi"){ #only keep humpback season
    gpsAGAll = gpsAG
    my_subtitle = "(humpback season)"
    #####  COMMENTING OUT BC trying PM01 w/ Oct and Nov
    gpsAG = gpsAGold 
    gpsAG = gpsAG[gpsAG$Season %in% c("Early", "Peak","Late"), ] 
    #gps = gps[gps$Season %in% c("Peak"), ] was trying just Peak humpback for HIHWNMS sites
    unique( gpsAG$mth )
    #redo effort plot so not confusing
    summary <- gpsAG %>%
      mutate(
        year = year(UTC),  # Extract Year
        month = format(UTC, "%m")  # Extract Month (numeric format)
      ) %>%
      count(year, month)  # Count occurrences (hours) in each year-month
    summary$dy = round(summary$n/ 24)
    
    summary$year[summary$month == 12] =  summary$year[summary$month == 12] + seasonShift
    
    #summary$month <- factor(summary$month, levels = c("09", "10", "11", "12", "01", "02", "03", "04", "05", "06", "07"))
    #####for normal graphs 
    summary$month <- factor(summary$month, levels = c("12", "01", "02", "03", "04", "05"))
    month_nums <- as.numeric(as.character( sort(unique(summary$month)) ))
    peakDays <- sum(summary$dy)
    
    
    p1 = ggplot(summary, aes(x = month, y = dy, fill = as.factor(year))) +
      geom_col(position = "dodge", width = .4) +  # Use dodge to separate bars for each year within the same month
      #coord_flip() +
      labs(
        title = paste0( "monitoring effort by year ", my_subtitle) ,
        subtitle = paste0(toupper(site), " has ", udays, 
                          " unique days: ", as.character(st), " to ", as.character(ed)
                          , ", with ", peakDays, " unique days in humpback season"),
        x = "",
        y = "Days",
        fill = "Humpback\nYear*",
        #caption = "<span style='word-spacing: 10px;'>Data from months with effort below the red horizonal line are excluded from annual sound levels figure above"
        #caption = "Data&nbsp;&nbsp;&nbsp;&nbsp;from&nbsp;&nbsp;&nbsp;&nbsp;months&nbsp;&nbsp;&nbsp;&nbsp;with&nbsp;&nbsp;&nbsp;&nbsp;effort&nbsp;&nbsp;&nbsp;&nbsp;below&nbsp;&nbsp;&nbsp;&nbsp;the&nbsp;&nbsp;&nbsp;&nbsp;red&nbsp;&nbsp;&nbsp;&nbsp;horizontal&nbsp;&nbsp;&nbsp;&nbsp;line&nbsp;&nbsp;&nbsp;&nbsp;are&nbsp;&nbsp;&nbsp;&nbsp;excluded&nbsp;&nbsp;&nbsp;&nbsp;from&nbsp;&nbsp;&nbsp;&nbsp;annual&nbsp;&nbsp;&nbsp;&nbsp;sound&nbsp;&nbsp;&nbsp;&nbsp;levels&nbsp;&nbsp;&nbsp;&nbsp;figure&nbsp;&nbsp;&nbsp;&nbsp;above"
        caption = "Data from months with effort below the red horizontal line are excluded from annual sound levels figure above"
      ) +
      scale_x_discrete(labels = month.abb[ month_nums ]) +  # Show month names instead of numbers
      #scale_fill_manual(values = rev(gray.colors(length(unique(summary$year))))) +  # Create grayscale colors
      scale_fill_manual(values = rev(colorRampPalette(c("darkblue", "lightblue"))(length(unique(summary$year))))) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14, hjust = 1, angle = 30),  
        plot.subtitle = element_text(size = 12),
        legend.text = element_text(size = 12),
        plot.caption = ggtext::element_markdown(hjust = 0, size = 11),
        legend.position = "right" 
      )+
      #adding marker for cutoff threshold (months need more than 23 (default) or x days of data to be kept in line graph)
      geom_hline(yintercept = siteInfo$MThreshold,    
                 linetype = "dashed",
                 color = "red",
                 linewidth = .5)
    
    p1
    ## re-save effort figure ####
    ggsave(filename = paste0(outDirGe, "/plot_", toupper(site), "_HMDEffort.jpg"), plot = p1, width = 10, height = 4, dpi = 300)
  
    gpsAG = gpsAGAll
  } else {
    gpsAGAll = gpsAG
    my_subtitle = "all data" 
  }
  
 
  
  
  if (my_subtitle == "(humpback season)"){
    caption_text = paste0(
      "<b>",toupper(site) , " </b> (", siteInfo$`Oceanographic category`, ")<br>",
      "<b>Vertical lines and grey shaded areas</b> indicate frequencies for sounds of interest in this soundscape<br>",
      "<b>Black lines</b> are modeled wind noise at this depth [", windLow, " m/s & ", windUpp, " m/s]<br>",
      "<b>Dotted sound level curve</b> is the median for humpback season<br>",
      "<b>Solid sound level curves and blue shaded areas</b> are the annual medians and 25th-75th percentiles for humpback season") # for ", my_subtitle)
    
  } else {
    caption_text = paste0(
      "<b>",toupper(site) , " </b> (", siteInfo$`Oceanographic category`, ")<br>",
      "<b>Vertical lines and grey shaded areas</b> indicate frequencies for sounds of interest in this soundscape<br>",
      "<b>Black lines</b> are modeled wind noise at this depth [", windLow, " m/s & ", windUpp, " m/s]<br>",
      "<b>Dotted sound level curve</b> is the median for ",my_subtitle, "<br>",
      "<b>Solid sound level curves and blue shaded areas</b> are the annual medians and 25th-75th percentiles for ", my_subtitle) # for ", my_subtitle)
    
  }
  
  
  ## re-calculate percentiles for all the data ####
  # all data - mALL 
  hmd_columns = grep("HMD", colnames(gpsAG))
  all_quantiles = apply(gpsAG[, hmd_columns, drop = FALSE], 2, quantile, 
                        probs = c(0.99, 0.90, 0.75, 0.50, 0.25, 0.10, .01), na.rm = TRUE)
  All = as.data.frame( all_quantiles )
  All$Quantile = rownames(All)
  All$Year = "all"
  hmd_columns = grep("HMD", colnames(All))
  mALL = reshape2::melt(All, id.vars = c("Quantile","Year"), measure.vars = hmd_columns)
  mALL$variable = as.numeric( as.character( gsub("HMD_", "", mALL$variable )))
  colnames(mALL) = c("Quantile", "Year", "Frequency" , "SoundLevel" )
  # by year- mallData 
  hmd_columns = grep("HMD", colnames(gpsAG))
  
  yr_split = split(gpsAG, gpsAG$yr) # Calculate quantiles for each year
  year_quantiles = lapply(yr_split, function(season_data) {
    apply(season_data[, hmd_columns, drop = FALSE], 2, quantile,  
          probs = c(0.99, 0.90, 0.75, 0.50, 0.25, 0.10, .01), na.rm = TRUE)})
  
  hmd_columns = grep("HMD", colnames(gpsAG))
  yearAll = NULL
  for (ii in 1: length(year_quantiles) ) {
    tmp = as.data.frame ( year_quantiles[ii] ) 
    colnames(tmp) = colnames(gpsAG)[hmd_columns]
    tmp$Quantile = rownames(tmp)
    tmp$Year = names(year_quantiles)[ii]
    rownames(tmp) = NULL
    yearAll = rbind(yearAll,tmp)
  }
  hmd_columns = grep("HMD", colnames(yearAll))
  mallData = reshape2::melt(yearAll, id.vars = c("Quantile","Year"), measure.vars = hmd_columns)
  mallData$variable = as.numeric( as.character( gsub("HMD_", "", mallData$variable )))
  colnames(mallData) = c("Quantile", "Year", "Frequency" , "SoundLevel" )
  fqupper = max(as.numeric( as.character( mallData$Frequency) ))
  
  
  #if NRS, scale y min lower so that FOI labels are visible 
  NRSLabelShift <- if (substr(site, 1, 3) == "NRS") 39 else NA
  
  
  
  #for sites that we removed 3k hz dip for some seasons but not all, remove median line in that gap  
  if (site == "mb02"){
    
    #make those rows na
    mALL <- mALL %>%
      mutate(SoundLevel = ifelse(
        (Frequency >= 3115 & Frequency <= 4330) & (Quantile == "50%"),
        NA, 
        SoundLevel
      ))
    
  } else if (site == "ci04"){
  
  mALL <- mALL %>%
    mutate(SoundLevel = ifelse(
      (Frequency >= 2175 & Frequency <= 2567) & (Quantile == "50%"),
      NA, 
      SoundLevel
    ))
  
  }
  
  #saving value of the oldest year in the data set so that the shading around light blue can be darker 
  oldest_year <- min(as.character(mallData$Year))
  
  
  
#plot
  p = ggplot() +
    #wind model
    geom_line(data = mwindInfo[as.character(mwindInfo$windSpeed) == windUpp,], aes(x = variable, y = value), color = "black", linewidth = 1) +
    geom_line(data = mwindInfo[as.character(mwindInfo$windSpeed) == windLow,], aes(x = variable, y = value), color = "black", linewidth = 1) +
    scale_x_log10(labels = label_number(),limits = (c(10,fqupper)), guide = "axis_logticks") +  # Log scale for x-axis
    
    scale_color_manual(values = rev(colorRampPalette(c("darkblue", "lightblue"))(length(unique(summary$year))))) +
    scale_fill_manual(values =  rev(colorRampPalette(c("darkblue", "lightblue"))(length(unique(summary$year))))) +
    
    # Add vertical lines at FOIs, label on right side
    geom_vline(data = FOIs, aes(xintercept = FQstart, color = Label), linetype = "dashed", color = "black",linewidth = .5) +
    geom_text(data = FOIs, aes(x = FQstart, y = label_height, label = Label), angle = 90, vjust = 1, hjust = 0.45, size = 4) +
    geom_rect(data = FOIs, aes(xmin = FQstart, xmax = FQend, ymin = -Inf, ymax = Inf),
              fill = "gray", alpha = 0.2) +  # Adjust alpha for transparency
    
    # Add vertical lines at FOIs, label on left side
    geom_vline(data = FOIsL, aes(xintercept = FQstart, color = Label), linetype = "dashed", color = "black",linewidth = .5) +
    geom_text(data = FOIsL, aes(x = FQstart, y = label_height, label = Label), angle = 90, vjust = 0, hjust = 0.5, size = 4) +
    
    # Add vertical set dash lines and grey shaded region at FOI ranges
    #geom_vline(data = FOIsRange, aes(xintercept = FQstart, color = Label), linetype = "dashed", color = "red",linewidth = .5) +
    #geom_vline(data = FOIsRange, aes(xintercept = FQend, color = Label), linetype = "dashed", color = "red",linewidth = .5) +
    geom_rect(data = FOIsRange, aes(xmin = FQstart, xmax = FQend, ymin = -Inf, ymax = Inf), 
              fill = "gray", alpha = 0.2)+  # Adjust alpha for transparency
    geom_text(data = FOIsRange, aes(x = FQstart, y = label_height, label = Label), angle = 90, vjust = 1, hjust = 0.45, size = 4) +
    
    # Add vertical set dash lines and grey shaded region at FOI ranges, label on left
    #geom_vline(data = FOIsRangeL, aes(xintercept = FQstart, color = Label), linetype = "dashed", color = "red",linewidth = .5) +
    #geom_vline(data = FOIsRangeL, aes(xintercept = FQend, color = Label), linetype = "dotdash", color = "red",linewidth = .5) +
    geom_rect(data = FOIsRangeL, aes(xmin = FQstart, xmax = FQend, ymin = -Inf, ymax = Inf), 
              fill = "gray", alpha = 0.2)+  # Adjust alpha for transparency
    geom_text(data = FOIsRangeL, aes(x = FQstart, y = label_height, label = Label), angle = 90, vjust = 0, hjust = 0.5, size = 4) +
    
    # geom_ribbon(data = mallData %>% pivot_wider(names_from = Quantile, values_from = SoundLevel),
    #             aes(x = Frequency, ymin = `25%`, ymax = `75%`, fill = Year), alpha = 0.2) +
    
    #for the geom_ribbons below, if data only has one year (ch01 and fk08), comment out the first geom ribbon and change alpha of second from .3 to .1
    geom_ribbon(data = mallData %>%
                  filter(Year != oldest_year) %>%
                  pivot_wider(names_from = Quantile, values_from = SoundLevel),
                aes(x = Frequency, ymin = `25%`, ymax = `75%`, fill = Year),
                alpha = 0.1) +
    
    #for the oldest year, make the shading darker since it is hard to see at alpha = .1 for lightblue
    geom_ribbon(data = mallData %>% 
                  filter(Year == oldest_year) %>% 
                  pivot_wider(names_from = Quantile, values_from = SoundLevel),
                aes(x = Frequency, ymin = `25%`, ymax = `75%`, fill = Year), 
                alpha = 0.3) + # High alpha for visibility
    
    #median HMD values- each year
    geom_line(data = mallData[mallData$Quantile == "50%",], aes(x = Frequency, y = SoundLevel, color = Year), linewidth = 2) +
    
    #median HMD values- all data
    geom_line(data = mALL[mALL$Quantile == "50%",], aes(x = Frequency, y = SoundLevel), color = "black", linewidth = 1,
              linetype = "dotted") +
    scale_y_continuous(limits = c(30, NA)) +  # use to manually scale y minimum so vert line labels are visible
    
    # Additional aesthetics
    theme_minimal() +
    labs(
      #title = paste0(toupper(site), "(",siteInfo$`Oceanographic category`, ")"), 
      caption  = caption_text,
      color = legend_label,        #IF biological then change to Year*
      fill = legend_label,        #IF biological then change to Year*
      x = "Frequency Hz",
      y = expression(paste("Sound Levels (dB re 1 ", mu, " Pa"^2, "/Hz)" ) ),
      subtitle = subtitle_text) +
    theme(legend.position = "right",
          plot.caption = ggtext::element_markdown(hjust = 0, size = 12),
          axis.title.x = element_text(size = 14),           # X-axis label size
          axis.title.y = element_text(size = 14),           # Y-axis label size
          axis.text = element_text(size = 14),
          legend.text = element_text(size = 12),
          axis.ticks.length.x = unit(0.25, "cm"), 
          axis.ticks.x = element_line(color = "grey", linewidth = 0.3), 
          axis.line.x = element_line(color = "grey", linewidth = 0.3)    
    ) 
  p
  
  
  separator <- grid.rect(gp = gpar(fill = "black"), height = unit(2, "pt"), width = unit(1, "npc"))
  # arranged_plot = grid.arrange(p, separator, l, heights =c(4, 0.05, 0.8))
  pYear = grid.arrange(p, separator, p1, heights =c(4, 0.1, 1)) #make height of last graph larger when legend gets cut off  b/c of too many data years. default is 1
  
  ### save figure ####
  ggsave(filename = paste0(outDirG, "/plot_", toupper(site), "_HMDYearSPL.jpg"), plot = pYear, width = 10, height = 12, dpi = 300)
  
  
  
  
  
  
  
  
 
  
  #check to see why PM sites have strange shading - just occasional really loud sounds 
  # test = mallData %>% filter(Quantile %in% c("50%", "75%"))  %>%
  #   pivot_wider(
  #     id_cols = c(Year, Frequency),   # grouping by Year and Frequency
  #     names_from = Quantile,
  #     values_from = SoundLevel
  #   ) + 
  #   mutate (diff = mallData$"75%" - mallData$'50%')
  
  
  
  
  
  #INTERACTIVE PLOT
  #changes to make to ggplot above
  #still trying to figure out FOI vert lines and labels in plotly
  #move geom_ribbon to after geom_lines
  #make geom_rect y bounds: ymin = 35, ymax = 90
  #add fill = Year to geom_line 
  #add color = Year to geom_ribbon
  #add ", text = paste("Year:", Year)" to each year geom_line in aes
  #add "name = "Year", " to  scale_fill_manual and scale_color_manual
  #then run code below
   
  # mallData_recent <- mallData %>%
  #   filter(Year != oldest_year) %>%
  #   pivot_wider(names_from = Quantile, values_from = SoundLevel)
  # 
  # mallData_oldest <- mallData %>%
  #   filter(Year == oldest_year) %>%
  #   pivot_wider(names_from = Quantile, values_from = SoundLevel)
  # 
  # pInt = ggplot() +
  #   #wind model
  #   geom_line(data = mwindInfo[as.character(mwindInfo$windSpeed) == windUpp,], aes(x = variable, y = value), color = "black", linewidth = 1) +
  #   geom_line(data = mwindInfo[as.character(mwindInfo$windSpeed) == windLow,], aes(x = variable, y = value), color = "black", linewidth = 1) +
  #   scale_x_log10(labels = label_number(),limits = (c(10,fqupper)), guide = "axis_logticks") +  # Log scale for x-axis
  #   
  #   scale_color_manual(name = "Year", values = rev(colorRampPalette(c("darkblue", "lightblue"))(length(unique(summary$year))))) +
  #   scale_fill_manual(name = "Year", values =  rev(colorRampPalette(c("darkblue", "lightblue"))(length(unique(summary$year))))) +
  #   
  #   # Add vertical lines at FOIs, label on right side
  #   geom_vline(data = FOIs, aes(xintercept = FQstart, color = Label), linetype = "dashed", color = "black",linewidth = .5) +
  #   geom_text(data = FOIs, aes(x = FQstart, y = 40, label = Label), angle = 90, vjust = 1, hjust = 0.45, size = 4) +
  #   geom_rect(data = FOIs, aes(xmin = FQstart, xmax = FQend, ymin = 35, ymax = 90),
  #             fill = "gray", alpha = 0.2) +  # Adjust alpha for transparency
  #   
  #   # Add vertical lines at FOIs, label on left side
  #   geom_vline(data = FOIsL, aes(xintercept = FQstart, color = Label), linetype = "dashed", color = "black",linewidth = .5) +
  #   geom_text(data = FOIsL, aes(x = FQstart, y = 40, label = Label), angle = 90, vjust = 0, hjust = 0.5, size = 4) +
  #   
  #   # Add vertical set dash lines and grey shaded region at FOI ranges
  #   #geom_vline(data = FOIsRange, aes(xintercept = FQstart, color = Label), linetype = "dashed", color = "red",linewidth = .5) +
  #   #geom_vline(data = FOIsRange, aes(xintercept = FQend, color = Label), linetype = "dashed", color = "red",linewidth = .5) +
  #   geom_rect(data = FOIsRange, aes(xmin = FQstart, xmax = FQend, ymin = 35, ymax = 90), 
  #             fill = "gray", alpha = 0.2)+  # Adjust alpha for transparency
  #   geom_text(data = FOIsRange, aes(x = FQstart, y = 40, label = Label), angle = 90, vjust = 1, hjust = 0.45, size = 4) +
  #   
  #   # Add vertical set dash lines and grey shaded region at FOI ranges, label on left
  #   #geom_vline(data = FOIsRangeL, aes(xintercept = FQstart, color = Label), linetype = "dashed", color = "red",linewidth = .5) +
  #   #geom_vline(data = FOIsRangeL, aes(xintercept = FQend, color = Label), linetype = "dotdash", color = "red",linewidth = .5) +
  #   geom_rect(data = FOIsRangeL, aes(xmin = FQstart, xmax = FQend, ymin = 35, ymax = 90), 
  #             fill = "gray", alpha = 0.2)+  # Adjust alpha for transparency
  #   geom_text(data = FOIsRangeL, aes(x = FQstart, y = 40, label = Label), angle = 90, vjust = 0, hjust = 0.5, size = 4) +
  #   
  #   
  #   #median HMD values- each year
  #   geom_line(data = mallData[mallData$Quantile == "50%",], aes(x = Frequency, y = SoundLevel, color = Year, fill = Year, text = paste("Year:", Year)), linewidth = 2) +
  #   
  #   #median HMD values- all data
  #   geom_line(data = mALL[mALL$Quantile == "50%",], aes(x = Frequency, y = SoundLevel, fill = Year, text = paste("Year:", Year)), color = "black", linewidth = 1,
  #             linetype = "dotted") +
  #   
  #   
  #   # geom_ribbon(data = mallData %>% pivot_wider(names_from = Quantile, values_from = SoundLevel),
  #   #             aes(x = Frequency, ymin = `25%`, ymax = `75%`, fill = Year), alpha = 0.2) +
  #   
  #   #for the geom_ribbons below, if data only has one year (ch01 and fk08), comment out the first geom ribbon and change alpha of second from .3 to .1
  #   geom_ribbon(data = mallData %>%
  #                 filter(Year != oldest_year) %>%
  #                 pivot_wider(names_from = Quantile, values_from = SoundLevel),
  #               aes(x = Frequency, ymin = `25%`, ymax = `75%`, fill = Year, color = Year),
  #               alpha = 0.1) +
  #   
  #   #for the oldest year, make the shading darker since it is hard to see at alpha = .1 for lightblue
  #   geom_ribbon(data = mallData %>% 
  #                 filter(Year == oldest_year) %>% 
  #                 pivot_wider(names_from = Quantile, values_from = SoundLevel),
  #               aes(x = Frequency, ymin = `25%`, ymax = `75%`, fill = Year, color = Year), 
  #               alpha = 0.3) + # High alpha for visibility
  #   scale_y_continuous(limits = c(30, NA)) +  # use to manually scale y minimum so vert line labels are visible
  #   
  #   # Additional aesthetics
  #   theme_minimal() +
  #   labs(
  #     #title = paste0(toupper(site), "(",siteInfo$`Oceanographic category`, ")"), 
  #     caption  = caption_text,
  #     color = legend_label,        #IF biological then change to Year*
  #     fill = legend_label,        #IF biological then change to Year*
  #     x = "Frequency Hz",
  #     y = expression(paste("Sound Levels (dB re 1 ", mu, " Pa"^2, "/Hz)" ) ),
  #     subtitle = subtitle_text) +
  #   theme(legend.position = "right",
  #         plot.caption = ggtext::element_markdown(hjust = 0, size = 12),
  #         axis.title.x = element_text(size = 14),           # X-axis label size
  #         axis.title.y = element_text(size = 14),           # Y-axis label size
  #         axis.text = element_text(size = 14),
  #         legend.text = element_text(size = 12),
  #         axis.ticks.length.x = unit(0.25, "cm"), 
  #         axis.ticks.x = element_line(color = "grey", linewidth = 0.3), 
  #         axis.line.x = element_line(color = "grey", linewidth = 0.3)    
  #   ) 
  # pInt
  # 
  # #make plot interactive
  # interactive_plot <- ggplotly(pInt)
  # #, tooltip = "text"
  # 
  # #remove hover info over grey shading
  # interactive_plot$x$data[[14]]$hoverinfo <- "skip"
  # 
  # 
  # interactive_plot
  
  
  
  #  interactive_plot$x$layout$xaxis$type
  # interactive_plot$x$data[[18]]$textangle <- 90  
  
  #figure out which trace has the grey rectangles show "trace 13" when you hover
  # for (i in seq_along(interactive_plot$x$data)) {
  #  cat("TRACE", i, "\n")
  #print(interactive_plot$x$data[[i]][c("name","fill","mode","hoverinfo")])
  #}

  
  
  #(5) TIME SERIES- FOI ####
  # plot with error bars and median and hours above 75th percentile in title
  
  
  if(site %in% c("mb01", "ci01", "NRS03", "NRS04", "NRS05","NRS06","NRS09","NRS10","NRS11","NRS13")){
    gps = gpsAG
  }
  
  
  if ( nrow(FOIst) > 0 ) {
    for (tt in 1: nrow(FOIst) ){ # tt = 1       tt = 2
      
      #check to see if the FOI is broad band
      if (FOIst$FQstart [tt] == FOIst$FQend [tt] ){
        ftN = paste0("HMD_", FOIst$FQstart [tt]) #fqIn
        ft = FOIst$`Decidecade.(TOL)`[tt]
        cols_to_select = c("UTC", "yr", "windMag",ftN )
        gpsFQ = gps %>% select(all_of(cols_to_select))
        wspeeds = unique( (windModel$windSpeed) )
        gpsFQ$closest_windMag = wspeeds[pmax(1, findInterval(gpsFQ$windMag, wspeeds)+1)]
        
        #Thresholds from all data percentiles
        mALL$FrequencyName = paste0("HMD_", mALL$Frequency)
        thresholds = mALL[mALL$FrequencyName == ftN,]
        threshold_min  = thresholds$SoundLevel[thresholds$Quantile == "1%"]
        threshold_mid  = thresholds$SoundLevel[thresholds$Quantile == "50%"]
        threshold_up   = thresholds$SoundLevel[thresholds$Quantile  == "75%"]
        threshold_lo   = thresholds$SoundLevel[thresholds$Quantile == "25%"]
        threshold_max  = thresholds$SoundLevel[thresholds$Quantile == "99%"]
        
        q01 = thresholds$SoundLevel[thresholds$Quantile == "1%"]
        q10 = thresholds$SoundLevel[thresholds$Quantile == "10%"]
        q25 = thresholds$SoundLevel[thresholds$Quantile == "25%"]
        q50 = thresholds$SoundLevel[thresholds$Quantile == "50%"]
        q75 = thresholds$SoundLevel[thresholds$Quantile  == "75%"]
        q90 = thresholds$SoundLevel[thresholds$Quantile == "90%"]
        q99 = thresholds$SoundLevel[thresholds$Quantile == "99%"]
        
        gpsFQ$SpecBand = gpsFQ[ftN]
        
      }else { # if FOI is a range of frequencies
        
        
        ft = FOIst$`Decidecade.(TOL)`[tt]
        
        hmdc = ( grep("HMD", colnames(gps)) )
        hmdn =  as.numeric( gsub("HMD_", "", colnames(gps)[hmdc]) )
        idx = hmdc[( hmdn >= FOIst$FQstart[tt] & hmdn <= FOIst$FQend [tt] ) ]
        ftN = colnames(gps)[idx]
        cols_to_select = c("UTC", "yr", "windMag", ftN )
        gpsFQ = gps %>% select(all_of(cols_to_select))
        tmpBB = gpsFQ %>% select(all_of(ftN))
        
        # calculate a BB measurement for TOL FQ- unlog, sum, re-log
        gpsFQ$SpecBand = 10*log10 ( rowSums( 10^(tmpBB/10) ) ) 
        
        # thresholds for typical-- BB 
        # calculate a BB measurement for TOL FQ- unlog, sum, re-log
        mALL$FrequencyName = paste0("HMD_", mALL$Frequency)
        thresholds = mALL[mALL$FrequencyName %in% ftN, ]
        thresholds2 <- thresholds %>%
          group_by(Quantile) %>%
          summarise(
            summed_dB = 10 * log10(sum(10^(SoundLevel/10), na.rm = TRUE))
          )
        # all data thresholds 
        q01 = thresholds2$summed_dB[thresholds2$Quantile == "1%"]
        q10 = thresholds2$summed_dB[thresholds2$Quantile == "10%"]
        q25 = thresholds2$summed_dB[thresholds2$Quantile == "25%"]
        q50 = thresholds2$summed_dB[thresholds2$Quantile == "50%"]
        q75 = thresholds2$summed_dB[thresholds2$Quantile  == "75%"]
        q90 = thresholds2$summed_dB[thresholds2$Quantile == "90%"]
        q99 = thresholds2$summed_dB[thresholds2$Quantile == "99%"]
      }
      
      ### daily percentiles with hours above threshold- 75th percentile
      dailyFQ = gpsFQ %>%
        filter(!is.na(SpecBand)) %>% 
        mutate(Date = as.Date(UTC)) %>%
        group_by(Date) %>%
        summarise(
          HMD_25 = quantile(SpecBand, 0.25, na.rm = TRUE),
          HMD_50 = quantile(SpecBand, 0.50, na.rm = TRUE),
          HMD_75 = quantile(SpecBand, 0.75, na.rm = TRUE),
          windspeed = quantile(windMag, 0.50, na.rm = TRUE),
          count_above_threshold   = sum(SpecBand > q75, na.rm = TRUE),
          percent_above_threshold = sum(SpecBand > q75, na.rm = TRUE) / 
            sum(!is.na(SpecBand)) * 100
        )
      dailyFQ$yr = year(dailyFQ$Date)
      dailyFQ$Julian = yday(dailyFQ$Date)
      
      # Fill in missing days
      dailyFQ_complete = dailyFQ %>%
        group_by(yr) %>%
        complete(Julian = seq(min(Julian), max(Julian), by = 1)) %>% 
        arrange(yr, Julian) %>%
        mutate(Date = as.Date(Julian - 1, origin = paste0(yr, "-01-01")))
      names( dailyFQ_complete )
      
      # adjust for Hawaii year
      dailyFQ_complete$mth = month(dailyFQ_complete$Date)
      if(substring(site, 1, 2) == "hi") {
      dailyFQ_complete$yr[dailyFQ_complete$mth == 12] = dailyFQ_complete$yr[dailyFQ_complete$mth == 12] + seasonShift
      }
      
      if(substring(site, 1, 2) == "pm") {
        dailyFQ_complete$yr[dailyFQ_complete$mth == 12] = dailyFQ_complete$yr[dailyFQ_complete$mth == 12] + seasonShift
        dailyFQ_complete$yr[dailyFQ_complete$mth == 11] = dailyFQ_complete$yr[dailyFQ_complete$mth == 11] + seasonShift
        dailyFQ_complete$yr[dailyFQ_complete$mth == 10] = dailyFQ_complete$yr[dailyFQ_complete$mth == 10] + seasonShift
      }
      
      
      # make labels for graphics (changed below for HI)
      monthly_sequence = seq.Date(as.Date("2021-01-01"), as.Date("2021-12-01"), by = "month")
      month_names_seq  = format(monthly_sequence, "%b")  # Extracts full month names
      days_of_year_for_months = yday(monthly_sequence)
      
      ### annual status
      gpsFQ$yr = year(gpsFQ$UTC)
      gpsFQ$mth = month(gpsFQ$UTC )
      #adjust for Hawaii year
      if(substring(site, 1, 2) == "hi") {
      gpsFQ$yr[gpsFQ$mth == 12] = gpsFQ$yr[gpsFQ$mth == 12] + seasonShift
      }
      
      if(substring(site, 1, 2) == "pm") {
        gpsFQ$yr[gpsFQ$mth == 12] = gpsFQ$yr[gpsFQ$mth == 12] + seasonShift
        gpsFQ$yr[gpsFQ$mth == 11] = gpsFQ$yr[gpsFQ$mth == 11] + seasonShift
        gpsFQ$yr[gpsFQ$mth == 10] = gpsFQ$yr[gpsFQ$mth == 10] + seasonShift
      }
      
      
      yrFQ = gpsFQ %>% group_by(yr) %>%
        summarise(
          HMD_25 = quantile(SpecBand, 0.25, na.rm = TRUE),
          HMD_50 = quantile(SpecBand, 0.50, na.rm = TRUE),
          HMD_75 = quantile(SpecBand, 0.75, na.rm = TRUE) )
      
      #threshold bands with categories-- from all data
      # threshold_bands <- data.frame(
      #   category = c("Very Low", "Low", "Within Range", "High", "Very High"),
      #   xmin = c(q01,    q10,    q25,    q75,    q90),
      #   xmax = c(q10,     q25,    q75,    q90,    q99),
         fill = c("#6699CC", "#99CCFF", "#CCCCCC", "#FF9999", "#CC0000")
      # )
      
      # threshold_bands$category <- factor(
      #   threshold_bands$category,
      #   levels = c("Very Low", "Low", "Within Range", "High", "Very High")
      # )
      
      #threshold bands with categories-- from all data COMBINING Very Low and Low as well as High and Very High
      threshold_bands <- data.frame(
        category = c("Low", "Within Range", "High"),
        xmin = c(q01,  q25,    q75),
        xmax = c(q25,    q75,    q99),
       # fill = c( "#6699CC", "#66C2A5", "#9467BD")
       fill = c("#99CCFF",  "#8C8FD6", "#D4AF37")
      # "#8DA0CB",
       # fill = c("#99CCFF", "#377EB8", "#6A3D9A" )
      # fill = c("#4C78A8",  "#F2A541", "#8E6C8A")  # dusty purple
      # fill = c("#5B8DB8",   "#F2A541",  "#7A5195")  # refined purple
      )
      
      threshold_bands$category <- factor(
        threshold_bands$category,
        levels = c( "Low", "Within Range", "High")
      )
      yrFQ$yr = factor(yrFQ$yr, levels = rev(sort(unique(yrFQ$yr))))
      yrFQ <- na.omit(yrFQ)
      
      
      ### plot: annual threshold bars
      # pthrs = ggplot() +
      #   # Threshold bands (same for all facets)
      #   geom_rect(data = threshold_bands,
      #             aes(xmin = xmin, xmax = xmax, ymin = 0.4, ymax = 0.6, fill = category),
      #             color = "white") +
      #   # Marker line per year- median
      #   geom_vline(data = yrFQ, aes(xintercept = HMD_50), 
      #              linetype = "dashed", color = "black", linewidth = .5) +
      #   # Facet by year
      #   facet_wrap(~yr, ncol = 1) +  
      #   # Customize scales and colors
      #   scale_fill_manual(values = setNames(threshold_bands$fill, threshold_bands$category)) +
      #   # more formatting
      #   theme_void() +
      #   theme( legend.position = "none",
      #          plot.title = element_text(hjust = 0, size = 12),
      #          plot.caption = element_text(size = 12, hjust = 1),
      #          plot.subtitle = element_text(hjust = 0.5, size = 12),
      #          axis.title.x = element_text(size = 14),
      #          strip.text = element_text(size = 14) ) +
      #   labs(title  = "Vertical dotted line\nis the annual\nmedian sound level",
      #        strip.text = element_text(hjust = 0))
      # pthrs 
      
      
      ### plot: annual pie charts
      # if dailyFQ$TOL_50 is < threshold_bands$xmin[2] = blue pie slice
      # if dailyFQ$TOL_50 is > threshold_bands$xmin[2] and < threshold_bands$xmax[2] = grey pie slice
      # if dailyFQ$TOL_50 is > threshold_bands$xmax[2] = red pie slice
      
      # q25 = minimum threshold (a)
      # q75 = max threshold (b)
      
      # # with NA on pies, but there are only NA for between deployment start and end, not whole year
      # pies <- dailyFQ_complete %>%
      #   mutate(category = case_when(
      #     is.na(TOL_50)             ~ "NA",
      #     TOL_50 < q25               ~ "Low",
      #     TOL_50 >= q25 & TOL_50 <= q75 ~ "Within Range",
      #     TOL_50 > q75               ~ "High"
      #   )) %>%
      #   group_by(yr, category) %>%
      #   summarise(
      #     n_days = n(),
      #     .groups = "drop"
      #   )
      # 
      # 
      # pies$category <- factor(
      #   pies$category,
      #   levels = c("Low", "Within Range", "High", "NA")
      # )
      # 
      # 
      # pie <- ggplot(pies, aes(x = "", y = n_days, fill = category)) +
      #   geom_col(width = 1, color = "white") +
      #   coord_polar(theta = "y") +
      #   scale_fill_manual(values = setNames(threshold_bands$fill, threshold_bands$category)) +
      #   facet_wrap(~ yr, ncol = 1) +
      #   labs(
      #     title = "",
      #     fill = "",
      #     strip.text = element_text(hjust = 0)
      #   ) +
      #   theme_void() +
      #   theme( legend.position = "right",
      #          plot.title = element_text(hjust = 0.5, size = 16),
      #          strip.text = element_text(size = 14) ) 
      # 
      # pie
      # 
      # 
      # # without NA on pies 
      # pies <- dailyFQ %>%
      #   mutate(category = case_when(
      #     #is.na(TOL_50)             ~ "NA",
      #     TOL_50 < q25               ~ "Low",
      #     TOL_50 >= q25 & TOL_50 <= q75 ~ "Within Range",
      #     TOL_50 > q75               ~ "High"
      #   )) %>%
      #   group_by(yr, category) %>%
      #   summarise(
      #     n_days = n(),
      #     .groups = "drop"
      #   )
      # 
      # 
      # pies$category <- factor(
      #   pies$category,
      #   levels = c("Low", "Within Range", "High")
      # )
      # pies$yr <- factor(pies$yr, levels = sort(unique(pies$yr), decreasing = TRUE))
      # 
      # 
      # pie <- ggplot(pies, aes(x = "", y = prop, fill = category)) +
      #   geom_col(width = 1, color = "white")+
      #   coord_polar(theta = "y") +
      #   scale_fill_manual(values = setNames(threshold_bands$fill, threshold_bands$category)) +
      #   facet_wrap(~ yr, ncol = 1) +
      #   labs(
      #     title = "",
      #     fill = "",
      #     strip.text = element_text(hjust = 0)
      #   ) +
      #   theme_void() +
      #   theme( legend.position = "right",
      #          plot.title = element_text(hjust = 0.5, size = 16),
      #          strip.text = element_text(size = 14) )
      # 
      # pie
      # 
      # 
      
      
      #adjust for HI and PM
      dailyFQ$mth = month(dailyFQ$Date)
      
      if(substring(site, 1, 2) == "hi") {
      dailyFQ$yr[dailyFQ$mth == 12] = dailyFQ$yr[dailyFQ$mth == 12] + seasonShift
      }
      
      if(substring(site, 1, 2) == "pm") {
        dailyFQ$yr[dailyFQ$mth == 12] = dailyFQ$yr[dailyFQ$mth == 12] + seasonShift
        dailyFQ$yr[dailyFQ$mth == 11] = dailyFQ$yr[dailyFQ$mth == 11] + seasonShift
        dailyFQ$yr[dailyFQ$mth == 10] = dailyFQ$yr[dailyFQ$mth == 10] + seasonShift
      }
      
      
      
      # without NA on pies and as proportions
      # pies <- dailyFQ %>%
      #   mutate(status = case_when(
      #     #is.na(HMD_50)             ~ "NA",
      #     HMD_50 < q25  & HMD_50 >= q01  ~ "Low",
      #     HMD_50 >= q25 & HMD_50 <= q75 ~ "Within Range",
      #     HMD_50 > q75  & HMD_50 <= q99  ~ "High"
      #   )) %>%
      #   group_by(yr, status) %>%
      #   summarise(
      #     n_days = n(),
      #     .groups = "drop"
      #   )
      
      
      pies <- dailyFQ %>%
        mutate(status = case_when(
          HMD_50 < q25  & HMD_50 >= q01  ~ "Low",
          HMD_50 >= q25 & HMD_50 <= q75 ~ "Within Range",
          HMD_50 > q75  & HMD_50 <= q99  ~ "High"
        )) %>%
        # This line removes the rows where status didn't match the above rules
        filter(!is.na(status)) %>% 
        group_by(yr, status) %>%
        summarise(
          n_days = n(),
          .groups = "drop"
        )
      
      
      #getting effort
      piesEffort <- dailyFQ_complete %>%
        group_by(yr) %>%
        summarise(
          n_NA = sum(is.na(HMD_50) | HMD_50 < q01 | HMD_50 > q99, na.rm = TRUE),
          n_total = n(),
          n_effort = n_total - n_NA,
          .groups = "drop"
        )
      
      
      pies$yr <- as.character(pies$yr)
      piesEffort$yr <- as.character(piesEffort$yr)
      
      finalpies <- pies %>%
        left_join(piesEffort, by = "yr") %>%
        mutate(
          prop = n_days/n_effort
        ) %>%
        select(1:3, 6:7)%>%
        filter(!is.na(status))
      
      
      finalpies <- finalpies %>%
        mutate(status = if_else(status == "Within Range", "Typical", status))
      
      threshold_bands <- threshold_bands %>%
        mutate(category = if_else(category == "Within Range", "Typical", category))
      
      
      finalpies$status <- factor(
        finalpies$status,
        levels = c("Low", "Typical", "High")
      )
      
      finalpies$yr <- factor(finalpies$yr, levels = sort(unique(finalpies$yr), decreasing = TRUE))
      
      #reorder legend so that it matches graph (high to low top to bottom)
      finalpies$status <- factor(finalpies$status, levels = c("High", "Typical", "Low"))
      

      # Add a dummy row for sections if it doesn't exist in the data to maintain a full legend
      if (!"High" %in% finalpies$status) {
        dummy_row <- finalpies[1, ] # Copy the first row structure
        dummy_row$status <- "High"
        dummy_row$prop <- 0         # Zero proportion so it doesn't show in the pie
        finalpies <- rbind(finalpies, dummy_row)
      }
      if (!"Typical" %in% finalpies$status) {
        dummy_row <- finalpies[1, ] # Copy the first row structure
        dummy_row$status <- "Typical"
        dummy_row$prop <- 0         # Zero proportion so it doesn't show in the pie
        finalpies <- rbind(finalpies, dummy_row)
      }
      if (!"Low" %in% finalpies$status) {
        dummy_row <- finalpies[1, ] # Copy the first row structure
        dummy_row$status <- "Low"
        dummy_row$prop <- 0         # Zero proportion so it doesn't show in the pie
        finalpies <- rbind(finalpies, dummy_row)
      }
      
      
      pie <- ggplot(finalpies, aes(x = "", y = prop, fill = status)) +
        geom_col(width = 1, color = "white")+
        coord_polar(theta = "y") +
        scale_fill_manual(values = setNames(threshold_bands$fill, threshold_bands$category)) + 
        facet_wrap(~ yr, ncol = 1) +
        labs(
          title = "Proportion of annual\nrecording within each status",
          fill = paste0(FOIst$Label[tt], "\nStatus"),
          strip.text = element_text(hjust = 0)
        ) +
        theme_void() +
        theme( legend.position = "right",
               plot.title = element_text(hjust = 0.5, size = 12),
               strip.text = element_text(size = 14) )
      
      
      pie
      
    
      #adjust height variable to keep annual graph height the same regardless of years in graph
      #they liked height of graph w 7 years of data (hi01)
      
      num_years <- nrow(yrFQ)
      
      #plot_height <- (12/8)*num_years+1
      
      # old way to adjust graph height
      # ex: for graphs with one year of data change height of plot from 12 to 6
      # graph with 2 years: 10
      # graph with three of more years: 12
      if (num_years == 1) {
        #plot_height <- 6
        #plot_height <- 3.25
        plot_height <- 4.25
      } else if (num_years == 2) {
        plot_height <- 4.75
      } else if (num_years == 3) {
        plot_height <- 6
      } else if (num_years == 4) {
        plot_height <- 8
      } else if (num_years == 8) {
        plot_height <- 14
      } else {
        # This covers three or more years
        plot_height <- 12
      }
      
      if (ft == "125 Hz" ){
        
        ft = "125 Hz TOL"
        
      } else if(ft == "63 Hz"){
        
        ft = "63 Hz TOL"
        
      }
      
      
      ### plot: time series ####
      dailyFQ_complete$yr = factor(dailyFQ_complete$yr, levels = rev(sort(unique(dailyFQ_complete$yr))))
      
      if (sidx != "biological" | substring(site, 1, 3) == "NRS") {
        plg =  ggplot(dailyFQ_complete, aes(x = Julian, y = HMD_50, group = yr) ) +
          
          annotate("rect",
                   xmin = -Inf, xmax = Inf,
                   ymin = q25, ymax = q75,
                  # fill = "#CCCCCC"
                   fill =  "#8C8FD6",
                   , alpha = 0.5) +
          annotate("rect",
                   xmin = -Inf, xmax = Inf,
                   ymin = q75, ymax = q99 ,
                   fill = "#D4AF37" , alpha = 0.5) +
          annotate("rect",
                   xmin = -Inf, xmax = Inf,
                   ymin = q01, ymax = q25,
                   fill = "#99CCFF", alpha = 0.5) +
          ylim(q01, q99) +

          #geom_hline(aes(yintercept = q50),linetype = "dashed", color = "black",linewidth = .2) +
          geom_hline(data = yrFQ, aes(yintercept = HMD_50), 
                     linetype = "dashed", color = "black", linewidth = .5) +
    
          geom_text_npc(data = yrFQ, 
                    aes(npcx = 1, npcy = HMD_50, label = paste0("annual median = ", round(HMD_50, 1))),
                    inherit.aes = FALSE, # Ignores x = Julian from the main ggplot
                    vjust = -1,          # Moves it above the line
                    hjust = 1,           # Aligns it to the right
                    size = 3, 
                    color = "black") +
          coord_cartesian(clip = "off") + # Allows text to exist outside the 0-365 range
          
          geom_hline(aes(yintercept = q75),linetype = "dashed", color = "gray",linewidth = .2) +
          geom_hline(aes(yintercept = q25),linetype = "dashed", color = "gray",linewidth = .2) +
          
          geom_line() +
          facet_wrap(~yr, ncol = 1)+
          scale_x_continuous(limits = c(0,365), breaks = days_of_year_for_months, labels = month_names_seq) +
          labs(
            title    = paste0("Are sound levels at ", toupper(site)," typical for ", ft, ",\nan indicator of ", FOIst$Label[tt], "?" ) , 
            #subtitle =  paste0(toupper(site), " (",siteInfo$`Oceanographic category`, ")"), #toupper(site),
            caption  = "Typical conditions shown as purple area (25th and 75th percentiles of all the data)", 
            x = "",
            y = substitute(paste("Daily Median Sound Levels (dB re 1 ", mu, "Pa at ", f, ")"), list(f = ft) )
              #substitute(paste("Daily Median Sound Levels (dB re 1 ", mu, " Pa/Hz at ", f, " Hz)"), list(f = ft) )
            # expression(paste("Daily Median Sound Levels (dB re 1 ", mu, " Pa/Hz at,", fqInN, "Hz)" ) )
          ) +
          theme_minimal() +
          theme(legend.position = "none",
                plot.title = element_text(size = 16, face = "bold", hjust = 0),
                axis.text.x = element_text(size = 14, hjust = 1, angle = 30),
                axis.text.y = element_text(size = 14, hjust = 1),
                strip.text = element_text(hjust = 0, size = 12),
                plot.caption = element_text(size = 12, hjust = 0)) 
        
        ## save: plot 125 Hz time series with thresholds ####
        plg
      
        
        #with pies that show off effort, directly comparable across years. out of 365. na/off effort is blank part of pie
        # plg2 = plg + pthrs + pie + plot_layout(ncol = 3, widths = c(2, .7, 1))  #plg = grid.arrange(plg, pthrs, nrow = 1, widths = c(2, 1))
        # plg2
        
        #with pies that show off effort, directly comparable across years. out of 365. na/off effort is blank part of pie
        plg2 = plg + pie + plot_layout(ncol = 2, widths = c(2, 1))  #plg = grid.arrange(plg, pthrs, nrow = 1, widths = c(2, 1))
        plg2
        
        ft = str_remove(ft, " Hz TOL| Hz")
       
        ggsave(filename = paste0(outDirG, "plot_", toupper(site), "-", ft, "_HMDstatus.jpg"), plot = plg2, width = 10, height = plot_height, dpi = 300)
        
        
      } else if (substring(site, 1, 2) == "hi") {
        # alternative methods for plg for Hawaii sites
        monthly_sequence = seq.Date(as.Date("2020-12-01"), as.Date("2021-06-01"), by = "month")
        month_names_seq  = format(monthly_sequence, "%b")  # Extracts full month names
        
        days_of_year_for_months = c(-30, 1, 32, 60, 91, 121, 152)
        
        
        # Adjust Julian so December days are negative (for clean continuous x-axis)
        dailyFQ_modified = dailyFQ_complete %>%
          mutate(
            shifted_julian = ifelse(Julian >= 335, Julian - 365, Julian)
          )
        
        plg = ggplot(dailyFQ_modified, aes(x = shifted_julian, y = HMD_50, group = yr)) +
          annotate("rect",
                   xmin = -Inf, xmax = Inf,
                   ymin = q25, ymax = q75,
                   fill = "#8C8FD6", alpha = 0.5) +
          annotate("rect",
                   xmin = -Inf, xmax = Inf,
                   ymin = q75, ymax = q99 ,
                   fill = "#D4AF37", alpha = 0.5) +
          annotate("rect",
                   xmin = -Inf, xmax = Inf,
                   ymin = q01, ymax = q25,
                   fill = "#99CCFF", alpha = 0.5) +
          ylim(q01, q99) +
          
          geom_hline(data = yrFQ, aes(yintercept = HMD_50), 
                     linetype = "dashed", color = "black", linewidth = .5) +
          
          geom_text_npc(data = yrFQ, 
                        aes(npcx = 1, npcy = HMD_50, label = paste0("annual median = ", round(HMD_50, 1))),
                        inherit.aes = FALSE, # Ignores x = Julian from the main ggplot
                        vjust = -1,          # Moves it above the line
                        hjust = 1,           # Aligns it to the right
                        size = 3, 
                        color = "black"
                        # ,
                        # fontface = "bold"
                        ) +
          coord_cartesian(clip = "off") + # Allows text to exist outside the 0-365 range
          #geom_hline(aes(yintercept = q50),linetype = "dashed", color = "gray",linewidth = .2) +
          geom_hline(aes(yintercept = q75),linetype = "dashed", color = "gray",linewidth = .2) +
          geom_hline(aes(yintercept = q25),linetype = "dashed", color = "gray",linewidth = .2) +
          
          geom_line() +
          facet_wrap(~yr, ncol = 1)+
          scale_x_continuous(breaks = days_of_year_for_months, labels = month_names_seq,
                             limits = c(-40,150)) +
          labs(
            title    = paste0("Are sound levels at ", toupper(site)," typical for ", ft, ",\nan indicator of ", FOIst$Label[tt], "?" ) , 
            #subtitle =  paste0(toupper(site), " (",siteInfo$`Oceanographic category`, ")"), #toupper(site),
            caption  = paste0("Typical conditions shown as purple area (25th and 75th percentiles of all the data)"),
            x = "",
            y = substitute(paste("Daily Median Sound Levels (dB re 1 ", mu, "Pa at ", f, ")"), list(f = ft) )
            # expression(paste("Daily Median Sound Levels (dB re 1 ", mu, " Pa/Hz at,", fqInN, "Hz)" ) )
          ) +
          theme_minimal() +
          theme(legend.position = "none",
                plot.title = element_text(size = 16, face = "bold", hjust = 0),
                axis.text.x = element_text(size = 14, hjust = 1, angle = 30),
                axis.text.y = element_text(size = 14, hjust = 1),
                strip.text = element_text(hjust = 0, size = 12),
                plot.caption = element_text(size = 12, hjust = 0),
                plot.subtitle = element_text(size = 12))
        
        ## save: plot 125 Hz time series with thresholds ####
        plg
        
        # plg2 = plg + pthrs + pie + plot_layout(ncol = 3, widths = c(2, 1, 1)) #plg = grid.arrange(plg, pthrs, nrow = 1, widths = c(2, 1))
        # plg2
        
        plg2 = plg + pie + plot_layout(ncol = 2, widths = c(2, 1)) #plg = grid.arrange(plg, pthrs, nrow = 1, widths = c(2, 1))
        plg2
        
        ft = str_remove(ft, " Hz TOL| Hz")
        
        ggsave(filename = paste0(outDirG, "plot_", toupper(site), "-", ft, "_HMDstatus.jpg"), plot = plg2, width = 10, height = plot_height, dpi = 300)
        
      } else if (substring(site, 1, 2) == "pm") {
        # alternative methods for plg for Papahanamokuakea sites
        #CHANGED to 10 instead of 12 and 8 instead of 6 for PM sites trial w oct and nov
        monthly_sequence = seq.Date(as.Date("2020-10-01"), as.Date("2021-08-01"), by = "month")
        month_names_seq  = format(monthly_sequence, "%b")  # Extracts full month names
        
        #added -91, -60, 182, and 213 for PM adding Oct and Nov graphs
        days_of_year_for_months = c(-91, -60, -30, 1, 32, 60, 91, 121, 152, 182, 213)
        
        
        # Adjust Julian so December days are negative (for clean continuous x-axis)
        #I changed to 274 from 335 to include oct and nov
        dailyFQ_modified = dailyFQ_complete %>%
          mutate(
            shifted_julian = ifelse(Julian >= 274, Julian - 365, Julian)
          )
        
        plg = ggplot(dailyFQ_modified, aes(x = shifted_julian, y = HMD_50, group = yr)) +
          annotate("rect",
                   xmin = -Inf, xmax = Inf,
                   ymin = q25, ymax = q75,
                   fill = "#8C8FD6", alpha = 0.5) +
          annotate("rect",
                   xmin = -Inf, xmax = Inf,
                   ymin = q75, ymax = q99 ,
                   fill = "#D4AF37", alpha = 0.5) +
          annotate("rect",
                   xmin = -Inf, xmax = Inf,
                   ymin = q01, ymax = q25,
                   fill = "#99CCFF", alpha = 0.5) +
          ylim(q01, q99) +
          
          #geom_hline(aes(yintercept = q50),linetype = "dashed", color = "black",linewidth = .2) +
          geom_hline(data = yrFQ, aes(yintercept = HMD_50), 
                     linetype = "dashed", color = "black", linewidth = .5) +
          
          geom_text_npc(data = yrFQ, 
                        aes(npcx = 1, npcy = HMD_50, label = paste0("annual median = ", round(HMD_50, 1))),
                        inherit.aes = FALSE, # Ignores x = Julian from the main ggplot
                        vjust = -1,          # Moves it above the line
                        hjust = 1,           # Aligns it to the right
                        size = 3, 
                        color = "black") +
          coord_cartesian(clip = "off") + # Allows text to exist outside the 0-365 range
          geom_hline(aes(yintercept = q75),linetype = "dashed", color = "gray",linewidth = .2) +
          geom_hline(aes(yintercept = q25),linetype = "dashed", color = "gray",linewidth = .2) +
          
          geom_line() +
          facet_wrap(~yr, ncol = 1)+
          scale_x_continuous(breaks = days_of_year_for_months, labels = month_names_seq,
                             limits = c(-90,185)) +  #FOR PM01 w/ Oct and Nov
          labs(
            title    = paste0("Are sound levels at ", toupper(site)," typical for ", ft, ",\nan indicator of ", FOIst$Label[tt], "?" ) , 
            #subtitle =  paste0(toupper(site), " (",siteInfo$`Oceanographic category`, ")"), #toupper(site),
            caption  = paste0("Typical conditions shown as purple area (25th and 75th percentiles of all the data)"),
            x = "",
            y = substitute(paste("Daily Median Sound Levels (dB re 1 ", mu, "Pa at ", f, ")"), list(f = ft) )
            # expression(paste("Daily Median Sound Levels (dB re 1 ", mu, " Pa/Hz at,", fqInN, "Hz)" ) )
          ) +
          theme_minimal() +
          theme(legend.position = "none",
                plot.title = element_text(size = 16, face = "bold", hjust = 0),
                axis.text.x = element_text(size = 14, hjust = 1, angle = 30),
                axis.text.y = element_text(size = 14, hjust = 1),
                strip.text = element_text(hjust = 0, size = 12),
                plot.caption = element_text(size = 12, hjust = 0),
                plot.subtitle = element_text(size = 12))
        
        ## save: plot 125 Hz time series with thresholds ####
        plg
        
        plg2 = plg + pie + plot_layout(ncol = 2, widths = c(2, 1)) #plg = grid.arrange(plg, pthrs, nrow = 1, widths = c(2, 1))
        plg2
        
        ft = str_remove(ft, " Hz TOL| Hz")
        
        ggsave(filename = paste0(outDirG, "plot_", toupper(site), "-", ft, "_HMDstatus.jpg"), plot = plg2, width = 10, height = plot_height, dpi = 300)
        
      }
    }
  }
  
  
  
   
  # #(5) Wind Dominated in fqIn2 ####
  # # select only data for fqIn2
  # cols_to_select = c("UTC", "yr","windMag","wind_category",fqIn2)
  # gpsFQ = gps %>% select(all_of(cols_to_select))
  # gpsFQ$Day = as.Date( gpsFQ$UTC )
  # gpsFQ$mth = month(gpsFQ$UTC )
  # 
  # # what wind speed category is the measured value closest to?
  # wspeeds = unique( (windModel$windSpeed) )
  # gpsFQ$closest_windMag = wspeeds[pmax(1, findInterval(gpsFQ$windMag, wspeeds)+1)]
  # 
  # # what is the SPL values for that windspeed?
  # fqIdx = which( colnames( windInfo) == substr( fqIn2, 5,8)) #'500'
  # wsIdx = match(gpsFQ$closest_windMag, windInfo$windSpeed)
  # gpsFQ$WindModelfq = windInfo[wsIdx, fqIdx]
  # 
  # # what difference from the measured vs modeled?
  # tol_col = grep("TOL", colnames(gpsFQ))
  # gpsFQ$Exceed = gpsFQ[,tol_col] -  gpsFQ$WindModelfq 
  # 
  # # is the difference above or below 0?
  # gpsFQ$Windthres = "unk"
  # gpsFQ$Windthres[gpsFQ$Exceed <= ab2] = "below"
  # gpsFQ$Windthres[gpsFQ$Exceed > ab2]  = "above"
  # 
  # # what are the all data thresholds for this frequency (not used)
  # thresholds = mALL[mALL$FrequencyName == fqIn2,]
  # threshold_mid =  thresholds$SoundLevel[thresholds$Quantile == "50%"]
  # threshold_up  = thresholds$SoundLevel[thresholds$Quantile  == "75%"]
  # threshold_lo  = thresholds$SoundLevel[thresholds$Quantile == "25%"]
  # gpsFQ$SpecBand = gpsFQ[fqIn2]
  # 
  # dayNE = gpsFQ %>%
  #   mutate(Date = as.Date(UTC)) %>%
  #   group_by(yr, mth) %>%
  #   summarise(
  #     # Count of non-NA hours (remove NAs)
  #     hrs = sum(!is.na(Exceed ) ) ,  
  #     
  #     # hours for the year-day that are at or below wind speed estimate
  #     n_below = sum(Windthres == "below", na.rm = TRUE),
  #     n_above = sum(Windthres == "above", na.rm = TRUE),
  #     
  #     #percent_below = sum(Windthres == "below", na.rm = TRUE) / hrs * 100,
  #     #percent_above = sum(Windthres == "above", na.rm = TRUE)/ hrs * 100,
  #     
  #     percent_below = ifelse(hrs > 0, n_below / hrs * 100, NA_real_),
  #     percent_above = ifelse(hrs > 0, n_above / hrs * 100, NA_real_),
  #     
  #     # windspeed during the year-day
  #     windspeed = quantile(windMag, 0.50, na.rm = TRUE),
  #     .groups = "drop"
  #   )
  # # CHECK: dayNE$percent_below + dayNE$percent_above
  # 
  # dayNE = as.data.frame( dayNE )
  # 
  # #put most recent year at the top of the plot
  # dayNE$yr  = factor(dayNE$yr,  levels = rev(sort(unique(dayNE$yr)))) 
  # 
  # if (sidx == "biological" & substring(site, 1, 2) == "hi") {
  #   dayNE$mth = factor(dayNE$mth, levels = rev(c(12,1,2,3,4,5))) 
  # }else if (sidx == "biological" & substring(site, 1, 2) == "pm") {
  #   dayNE$mth = factor(dayNE$mth, levels = rev(c(10,11,12,1,2,3,4,5,6,7))) #ADDED 10, 11, 6, and 7 for PM Oct aand Nov graph
  # }else {
  #   #puts January at the top of the plot... maybe remove if confusing to go to previous year
  #   dayNE$mth = factor(dayNE$mth, levels = rev(c(1,2,3,4,5,6,7,8,9,10,11,12)))
  # }
  # 
  # # CHECK: is there a relationship between percent below and windspeed?
  # #plot(dayNE$percent_below, dayNE$windspeed)
  # 
  # windD = ggplot(dayNE, aes(x = factor(mth), y = as.numeric(percent_below), fill = factor( mth ) ) ) +
  #   geom_col(width = 1) +
  #   facet_wrap(~yr, ncol = 1) +
  #   coord_flip() +
  #   ylim(0,100) +
  #   scale_fill_viridis_d(option = "D") +
  #   labs(
  #     title = paste0("Habitat acoustically inactive (wind-driven at ", fqIn2name, ")"),
  #     subtitle  = paste0(toupper(site), " (",siteInfo$`Oceanographic category`, ")"),
  #     x = "",
  #     y = paste0("% of hours in a month wind noise dominate at ", fqIn2name, 
  #                "\n (when measured sound level are similar to predicted sound level at known wind speed)"),
  #     #color = "Year"  # Label for the color legend
  #   ) +
  #   theme_minimal()+
  #   theme(
  #     legend.position = "none",
  #     plot.title = element_text(size = 16, face = "bold", hjust = 0),
  #     strip.text  = element_text(size = 14),
  #     axis.title.x  = element_text(size = 14),
  #     axis.title.y  = element_text(size = 14),
  #     axis.text.x = element_text(size = 14, hjust = 1, angle = 30),
  #     axis.text.y = element_text(size = 14, hjust = 1) )
  # windD
  # ggsave(filename = paste0(outDirGe, "/plot_", toupper(site), "_WindDominatedwOctNovnoX.jpg"), plot = windD, width = 10, height = 12, dpi = 300)
  # 
  # x_scale = if (sidx == "biological" & substring(site, 1, 2) == "hi" ) {
  #   scale_x_discrete(
  #     breaks = c("12", "2", "4"),
  #     labels = c("Dec", "Feb", "Apr") 
  #   )
  # } else if (sidx == "biological" & substring(site, 1, 2) == "pm" ){
  #   scale_x_discrete(
  #     breaks = c("10", "12", "2", "4", "6"),
  #     labels = c("Oct", "Dec", "Feb", "Apr", "Jun")
  #   )
  # }else {
  #   scale_x_discrete(
  #     breaks = c("1", "3", "5", "7", "9", "11"),
  #     labels = c("Jan", "Mar", "May", "Jul", "Sep", "Nov")
  #   )
  # }
  # 
  # windB = ggplot(dayNE, aes(x = factor(mth), y = as.numeric(percent_above), fill = factor( mth ) ) ) +
  #   geom_col(width = 1) +
  #   facet_wrap(~yr, ncol = 1) +
  #   coord_flip() +
  #   ylim(0,100) +
  #   x_scale +
  #   labs(
  #     title = paste0("How often sources of interest are likey present (vocalizing species or vessels)"),
  #     subtitle  = paste0(toupper(site), " (",siteInfo$`Oceanographic category`, ")"),
  #     #caption = "Calculated as % hours when measured sound levels are above predicted level based on wind speed",
  #     x = "",
  #     y = paste0("% of hours above wind noise ", fqIn2name, "\n (calculated as % hours when measured sound levels are above predicted level based on wind speed)"),
  #     #color = "Year"  # Label for the color legend
  #   ) +
  #   theme_minimal()+
  #   theme(
  #     strip.text  = element_text(size = 14),
  #     plot.title   = element_text(size = 16, face = "bold", hjust = 0),
  #     plot.caption = element_text(size = 12, face = "italic", hjust = 0), 
  #     axis.text.x = element_text(size = 14, hjust = 1),
  #     axis.title.x  = element_text(size = 14),
  #     axis.title.y  = element_text(size = 12),
  #     axis.text.y   = element_text(size = 12, hjust = 1), 
  #     legend.position = "none")
  # windB
  # ggsave(filename = paste0(outDirG, "/plot_", toupper(site), "_AboveWindwOctNovnoX.jpg"), plot = windB, width = 10, height = 12, dpi = 300)
  
  
  
  # SAVE UPDATED DATA ####
  # names(gps)
  #save(gps, file = paste0(outDirP, "/data_", tolower(site), "_HourlySPL-gfs-season-spectrumlevel_", DC, ".Rda") )
  
}






