# COMPILE SOUNDSCAPE METRICS

# runs one site at a time 
# adds wind estimate from PAMscapes for any new data (takes time!)

# OUTPUTS: hourly Hybrid millidecade values with wind speed and list of files processed

#install.packages("rJava") make sure Java is installed for xlsx to work

# RUN this to make sure latest updates for PAMscapes
devtools::install_github('TaikiSan21/PAMscapes')


library(PAMscapes)
library(lubridate)
library(dplyr)
library(ggplot2)
library(reshape)
# library(rJava)
# library(xlsx)
library(openxlsx)
library(data.table)
library(devtools)



# SET UP PARAMS ####
rm(list=ls()) 
DC = Sys.Date()
site  = "sb03" 
site = tolower(site) 

#add for NRS
 # gcpF = "PMEL_HI"
 # prodName = "hi" 

# LOCAL DATA DIRECTORIES ####
#dirGCP = paste0( "/Users/quca3108/ONMS/", site,"/") # NCEI GCP min HMD netCDFs
#dirGCP = paste0( "C:/Users/emma.beretta/Documents/ONMS/", site,"/") # for NOAA computer
#dirGCP = paste0( "C:/Users/embe5980/ONMS/", site,"/") # for CIRES computer
dirGCP = paste0( "E:/onms/products/sound_level_metrics/", site,"/") # for GCP workstation
#dirGCP = paste0( "W:/DETECTOR_OUTPUT/PYTHON_SOUNDSCAPE_PYPAM/",gcpF,"/") #nmfs GCP HMD netCDFs


#SANCTSOUND DATA DIRECTORIES
#for when sanctsound data is in different directory than ONMS data: grnms, sbnms, hihwnms
#dirGCPSS = paste0( "M:/FATESD/PASSIVE_ACOUSTIC_DATA_ANALYSIS/SANCTSOUND_SBNMS/SB01") # for GCP workstation - SBNMS
#dirGCPSS = paste0("E:/sanctsound/products/sound_level_metrics/gr01") # for GCP workstation - SBNMS (same data as above directory but pulling from NCEI. aditional folder to get to hmd data so if using this directory will need to adjust code looking for file path)
#dirGCPSS = paste0( "X:/Emma_Beretta/HI01SanctSound") # for GCP workstation - HI01
#dirGCPSS = paste0("E:/onms/products/sound_level_metrics/mb05/onms_mb05_20220425_20220621_hmd") #to get MB05_01
dirGCPSS = paste0("E:/sanctsound/products/sound_level_metrics/", site,"/") 


# LOCAL CODE REPO DIRECTORIES ####
#outDir =  "/Users/quca3108/SoundscapesWebsite/"
#outDir =  "F:/CODE/GitHub/SoundscapesWebsite/" 
#outDir =  "C:/Users/emma.beretta/Documents/SoundscapesWebsite/" #for NOAA computer
#outDir =  "C:/Users/embe5980/SoundscapesWebsite/" #for CIRES computer
outDir =  "X:/Emma_Beretta/SoundscapesWebsite/" #for GCP workstation
#outDir =  "C:/Users/pam_user/Documents/GitHub/SoundscapesWebsite/" #Samara GCP WW

outDirC = paste0( outDir,"content/resources/") #context
outDirP = paste0( outDir,"products/", substr(tolower(site),start = 1, stop =2),"/" )#products
#outDirP = paste0( "Y:/soundscape_website_products/", substr(tolower(prodName),start = 1, stop =2),"/" ) #onms gcp folder #old NRS path paste0( outDir,"products/", substr(tolower(prodName),start = 1, stop =2),"/" )#NRS products
outDirG = paste0( outDir,"report/" ) #graphics


# ONMS Metadata ####
metaFile = paste0(outDirC,"ONMSSound_IndicatorCategories.xlsx")
lookup = as.data.frame ( openxlsx :: read.xlsx(metaFile, sheet  = "Summary") ) #xlsx::read.xlsx(metaFile, sheetName = "Summary")
colnames(lookup) = lookup[1, ]         # Set first row as column names
lookup = as.data.frame( lookup[-1, ] ) # Remove the first row
lookup = as.data.frame( lookup[!apply(lookup, 1, function(row) all(is.na(row))), ] )
siteInfo = lookup[lookup$`NCEI ID` == site,]
siteInfo = siteInfo[!is.na(siteInfo$`NCEI ID`), ]

cat("CHECK: Read in data for: ", 
    siteInfo$"NCEI ID\nfacilitates matching with metadata on gcp \ncheck gcp to see verify")


# GET list of files to process ####

## PyPAM soundscape (Sanctsound) FILES- NEFSC-GCP ####
# e.g. NEFSC_SBNMS_201811_SB03_20181112.nc
  inFilesPY = list.files(dirGCPSS, pattern = "_[0-9]{8}\\.nc$", recursive = T, full.names = T)
  tmp = sapply( strsplit(basename(inFilesPY), "[.]"), "[[", 1)
  if (length(tmp) != 0){
    dysPy = as.Date(sapply( strsplit(tmp, "_"), "[", 5),format = "%Y%m%d")
    cat("Found ", length(inFilesPY), "PyPAM files for ", site, "(", as.character( min(dysPy , na.rm = T) ), " to ", as.character(max(dysPy , na.rm = T)),
      "with", sum( duplicated(dysPy)), "duplicated days\n (if NA for date range fix line 59)\n")
  }

## ONMS Sound FILES- NCEI-GCP ####
# e.g. ONMS_HI01_20231201_8021.1.48000_20231201_DAILY_MILLIDEC_MinRes.nc

  #NEW Pypam processing results in files ending in .nc not MinRes.nc!
  #check for old .MinRes.nc and new .nc in the products folder
  #cant just take all .nc because folders w/ MinRes.nc also have netCDF.nc

  inFilesON = list.files(dirGCP, pattern = ".nc", recursive = T, full.names = T)

  # Filter out files that end with 'netCDF.nc'
  inFilesON = inFilesON[!grepl("netCDF\\.nc$", inFilesON)]
  
  #wierd xls coming through into nc list for some reason
  inFilesON = inFilesON[!grepl(".xls", inFilesON)]
  
  mantaFiles = inFilesON[grepl("MinRes\\.nc$", inFilesON)]  # Files ending with '_MinRes.nc'
  pypamFiles = inFilesON[!grepl("MinRes\\.nc$", inFilesON)]  # All other .nc files
  
  #you may need to change the number of the segment where the date is getting taken from he file name below
  dysON1 = as.Date(sapply( strsplit(basename(mantaFiles), "_"), "[[", 5), format = "%Y%m%d")
  #you may need to change the number of the segment where the date is getting taken from he file name below
  dysON2 = as.Date(sapply( strsplit(basename(pypamFiles), "_"), "[[", 4), format = "%Y%m%d")
  
  dysON = c(dysON1, dysON2)
  
  #for newer sites without manta data:
  #dysON =  dysON2
  
  # Output summary
  cat("Found ", length(inFilesON), "NCEI files for ", site, "(", as.character(min(dysON, na.rm = T)), " to ", as.character(max(dysON, na.rm = T)), 
      ") with", sum(duplicated(dysON)), "duplicated days\n")
  
  
  # use code below for MB01 fix. there was MB02 data in NCEI folder, only want MB01
  #inFilesON = list.files(dirGCP, pattern = "^ONMS_MB01.*MinRes\\.nc$", recursive = TRUE, full.names = TRUE )




## NMFS-GCP NRS sound files
#PMEL_CINMS_201410_NRS05_20141018.nc
if ( substr(site,start = 1, stop =3) == "nrs"){
inFilesPY = list.files(dirGCP, pattern = "_[0-9]{8}\\.nc$", recursive = T, full.names = T)
tmp = sapply( strsplit(basename(inFilesPY), "[.]"), "[[", 1)
if (length(tmp) != 0){
  dysPy = as.Date(sapply( strsplit(tmp, "_"), "[", 5),format = "%Y%m%d")
  cat("Found ", length(inFilesPY), "PyPAM files for ", site, "(", as.character( min(dysPy , na.rm = T) ), " to ", as.character(max(dysPy , na.rm = T)),
      "with", sum( duplicated(dysPy)), "duplicated days\n (if NA for date range fix line 59)\n")
}
inFiles = inFilesPY
}

#For SB03 since new data has different naming convention
# inFilesON = list.files(dirGCP, pattern = "MinRes.nc", recursive = T, full.names = T)
# 
# dirNew = paste0( "E:/onms/products/sound_level_metrics/", site,"/onms_sb03_20240524-20240930_hmd/data")
# inFilesNew = list.files(dirNew, pattern = ".nc", recursive = T, full.names = T)
# 
# dirNew2 = paste0( "E:/onms/products/sound_level_metrics/", site,"/onms_sb03_20240930-20250329_hmd/data")
# inFilesNew2 = list.files(dirNew2, pattern = ".nc", recursive = T, full.names = T)
# 
# inFilesON2 = c(inFilesNew, inFilesNew2)
# 
# dysON = as.Date(sapply( strsplit(basename(inFilesON), "_"), "[[", 5), format = "%Y%m%d")
# dysON2 = as.Date(sapply( strsplit(basename(inFilesON2), "_"), "[[", 4), format = "%Y%m%d")
# 
# dysON = c(dysON, dysON2)
# inFilesON = c(inFilesON, inFilesON2)
# 
# cat("Found ", length(inFilesON), "NCEI files for ", site, "(", as.character(min( dysON , na.rm = T)), " to ", as.character(max( dysON , na.rm = T)),") with",
#     sum( duplicated(dysON)), "duplicated days\n")



## COMBINE FILE LISTS ####
#check for duplicate days, remove MANTA
if (length(tmp) != 0){
  ixdR = which(dysON %in% dysPy)
  if ( length(ixdR) != 0 ){
    inFiles = c( inFilesPY, inFilesON[-ixdR] )
    ckFiles = as.data.frame(inFiles)
    dys = c(dysPy, dysON[-ixdR])
    cat("Found ", length(inFiles), " files for ", site, "with", sum( duplicated(dys)), "duplicated days\n")
  }else{
    inFiles = c( inFilesPY, inFilesON )
    ckFiles = as.data.frame(inFiles)
    dys = c(dysPy, dysON[-ixdR])
    cat("Found ", length(inFiles), " files for ", site, "with", sum( duplicated(dys)), "duplicated days\n")
  }
} else {
  inFiles = inFilesON
  dys = dysON
}



## CHECK FOR PROCESSED FILES ####
#updates list of files to process
pFile = list.files(path = (outDirP), pattern = paste0("HMDfilesProcesed_", site), full.names = T, recursive = T)
if ( length(pFile) > 0 ) {
  load(pFile)

  # are there any new files to process?
  inFilesN = inFiles[!basename(inFiles) %in% processedFiles]

  if ( length(inFilesN ) > 0 ) {

    # read in processed data to append results
    inFileP = list.files((outDir),
                         pattern = paste0("HMDdata_", site, "_HourlySPL-gfs_\\d{4}-\\d{2}-\\d{2}\\.Rda$"),
                         full.names = T, recursive = T)
    file_info = file.info(inFileP)
    load( inFileP[which.max(file_info$ctime)] )
    #outData = gps    #for some reason hi03,8, and 4 AND pm01 earlier outdata was saved as gps in products folder
    if( exists("outData") ) {
      processedData = outData
      rm(outData)
      #rm(gps) #fix for hi03,8, and 4 AND pm01
    }

    cat( "Processed data for ", site, ": ",
         as.character( as.Date( min( processedData$UTC))) , " to ",
         as.character( as.Date( max( processedData$UTC)) ),
         " Found ", length(inFilesN), "new files to process\n")

    # these are the files that will be processed!
    inFiles = inFilesN

  } else {
    stop("No new files to process... come back when you have more data")
  }

} else {
  cat("No processed files for", site, ", processing all new files")
  processedData = NULL
}


# PROCESS ONMS Sound FILES ####

#testing when SS data ends and ONMS data begins for when a site has both SS data and ONMS (SB01,03, GR01, and HI01)
#use this info to change if statement in processing loop so that it smootly continues to ONMS data after SS 
#if no SS data, ignore this step and make sure to remove HMD_20 column later on
# ncFile = inFiles[1291]
# test = loadSoundscapeData(ncFile)
# 
# ncFile = inFiles[1292]
# test2 = loadSoundscapeData(ncFile)

# faster method to save each day in list and rbind after, prevents R from crashing
cDatah = NULL
data_list <- list()

if (length(inFiles) > 0) { 
  for (f in 1: length(inFiles) ){ # 1245:1246 length(inFiles) # f = 1
    
    cat("Processing", f, "of", length(inFiles),basename(inFiles[f]), "\n")
    
    ncFile = inFiles[f]
    hmdData = loadSoundscapeData(ncFile, keepQuals = c(1,2)) #keeps quality 1 and 2 (unknown) - 2 needed for NMFS data not yet at NCEI
    
    # add software column
    if ( grepl("MinRes.nc", basename(inFiles[f]) ) ) {
      hmdData$software = "manta"
    } else {  
      hmdData$software = "pypam" }
    
    # remove platform column
    hmdData = hmdData[, setdiff(names(hmdData), "platform"), drop = FALSE]
    
    #bin to hourly median values
    cDatah_day = binSoundscapeData(hmdData, bin = "1hour", method = c("median") )
    
    #FOR ONMS gr01 data! onms has columns HMD_0-HMD_19 (all NA) but SS data starts at HMD_20
    #For SB03, remove HMD_0-HMD_19 from SS and ONMS data. There are values in those columns for SS dataset...
    # if (f >= 1292){
    #   cDatah_day = cDatah_day[, -c(2:22)]
    # } else{
    #   cDatah_day = cDatah_day[, -2]
    # }
    
      data_list[[f]] = cDatah_day
    
  } 
}

#combine list elements after processing each day seperately and saving into different list elements
cDatah <- rbindlist(data_list)

# the time binning seems to remove any "extra columns" so just the UTC and TOL bands for the output
# names(cDatah)
#ADD a few basic columns about the data
cDatah$yr  = year(cDatah$UTC)
cDatah$mth = month(cDatah$UTC)
cDatah$site = site


#rbindlist results in data.table, so change to data.frame so that matchGFS works
#THIS OR
cDatah = setDF(cDatah)

#THIS?
#cDatah = as.data.frame(cDatah)


# old way to process with rbind, will crash if >2000 days of data to process
# # PROCESS ONMS Sound FILES ####
# # binning after each day
# cDatah = NULL
# 
# if (length(inFiles) > 0) { 
#   for (f in 1: length(inFiles) ){ # 1245:1246 length(inFiles)
#     
#     cat("Processing", f, "of", length(inFiles),basename(inFiles[f]), "\n")
#     
#     ncFile = inFiles[f]
#     hmdData = loadSoundscapeData(ncFile) #only keeps quality 1 as default
#     
#     #tolData = createOctaveLevel(hmdData, type='tol')
#     #names( hmdData )
#     
#     # add software column
#     if ( grepl("MinRes.nc", basename(inFiles[f]) ) ) {
#       hmdData$software = "manta"
#     } else {  
#       hmdData$software = "pypam" }
#     
#     # combine data- check to make sure columns match
#     hmdData = hmdData[, setdiff(names(hmdData), "platform"), drop = FALSE]
#     
#     #remove_cols = setdiff(names(tolData), names(cData))
#     # if(f > 1) {
#     #   hmdData = hmdData[ , names(hmdData) %in% names(cData) ] 
#     # }
#     
#     #bin to hourly median values
#     cDatah_day = binSoundscapeData(hmdData, bin = "1hour", method = c("median") )
#     
#     #FOR ONMS gr01 data! onms has columns HMD_0-HMD_19 (all NA) but SS data starts at HMD_20
#     #For SB03, remove HMD_0-HMD_19 from SS and ONMS data. There are values in those columns for SS dataset...
#     #if (f >= 337){
#     #   cDatah_day = cDatah_day[, -c(2:22)]
#     # } else{
#     #   cDatah_day = cDatah_day[, -2]
#     # }
#     
#     if (is.null(cDatah)) {
#       cDatah = cDatah_day
#     } else {
#       cDatah = rbind(cDatah, cDatah_day)
#     }
#     
#   } 
#   
#   #had to use below line for CI01 because it wasnt keeping Lat/Long by default for some reason
#   #cDatah = binSoundscapeData(cData, bin = "1hour", method = c("median"), extraCols = c("Latitude", "Longitude"))
# }
# # the time binning seems to remove any "extra columns" so just the UTC and TOL bands for the output
# # names(cDatah)
# #ADD a few basic columns about the data
# cDatah$yr  = year(cDatah$UTC)
# cDatah$mth = month(cDatah$UTC)
# cDatah$site = site


# #(ALT GET WIND) 
# only if already ran previously but the SPL data were inaccurate!
# inWind = "F:/ONMS/SS_Manta/data_hi01_HourlySPL-gfs_2025-07-01.Rda"
# load( inWind[1] ) # names(outData)
# cols_to_keep = c("UTC",  "Latitude", "Longitude", "windU", "windV",
#                   "precRate", "matchLong", "matchLat",
#                   "matchTime", "windMag")
# if( exists("outData") ) {
#   gps = outData
#   rm(outData)
# }
# gps_subset = gps[, intersect(cols_to_keep, names(gps))]
# # # names(gps_subset) # names(cDatah)
# rm(gps)
# merged_data = merge(cDatah, gps_subset, by = "UTC", all.x = TRUE )
# gps = merged_data
# # # names(gps)


#GET WIND WITH CHUNKS ####
#Too many unique days to process all at once. Need to break cDatah into smaller datasets
#Split cDatah into chunks
unique_days <- sort(unique(as.Date(cDatah$UTC)))

# Split those days into ~100-day chunks
chunk_size_days <- 100
day_chunks <- split(unique_days, ceiling(seq_along(unique_days) / chunk_size_days))

# Split the full dataset by matching on those date chunks
data_chunks <- lapply(day_chunks, function(days) {
  filter(cDatah, as.Date(UTC) %in% days)
})

# Check result
length(data_chunks)             
sapply(data_chunks, nrow)   

gps_chunks <- list()  # store wind-matched results


#loop through chunks!
for (i in seq_along(data_chunks)) {
cat("Processing matchGFS for chunk", i, "of", length(data_chunks), "\n")
gps_chunks[[i]] <- matchGFS(data_chunks[[i]])
}


#add/remove lines for the number of chunks data was broken into
#run one line at a time, it will take a while
#can try for loop above but may crash if too many chunks
# gps_chunks[[1]] <- matchGFS(data_chunks[[1]])
 # gps_chunks[[2]] <- matchGFS(data_chunks[[2]])
 # gps_chunks[[3]] <- matchGFS(data_chunks[[3]])
 # gps_chunks[[4]] <- matchGFS(data_chunks[[4]])
# gps_chunks[[5]] <- matchGFS(data_chunks[[5]])
# gps_chunks[[6]] <- matchGFS(data_chunks[[6]])
# gps_chunks[[7]] <- matchGFS(data_chunks[[7]])
#gps_chunks[[8]] <- matchGFS(data_chunks[[8]])

#put chunks back together
gps <- dplyr::bind_rows(gps_chunks)



#SKIP IF YOU ALREADY GOT WIND USING CHUNKS
# GET WIND (without chunks, usually crashes because function cant handle large amounts of data)####
# when there arent too many days 
# if ( length(cDatah) > 0 ) {
#   
#   cat("ONMS data only ...") 
#   cat("This takes a bit ... maybe grab a coffee or go for walk") 
#   gps = matchGFS(cDatah)
#   
# }


#Fix for HI01
#two lat and long columns in processedData
#processedDataF <- processedData %>% select(-Latitude.x, -Longitude.x)
#processedDataOld <- processedData
#processedData <- processedDataF %>%
# rename(
#   Latitude = Latitude.y,
#   Longitude = Longitude.y
# )




# APPEND & SAVE NEW DATA FILES ####

if ( length(pFile) > 0 ){   #append old (processedData) and save out all processed data

  #remove any no matching headings
  data_mismatched = setdiff(colnames(gps), colnames(processedData))
  gps_clean = gps[, !colnames(gps) %in% data_mismatched] #new data with matching headings
  
  #re-order columns
  # setdiff(colnames(gps_clean), colnames(processedData))
  print(names(processedData))
  
  processedData <- processedData[, -c(2:21)]
  
  col_order = colnames(processedData)
  gps_clean1 = gps_clean[, col_order]
  names( processedData)
  names( gps_clean)

  # combine data
  outData = rbind(processedData, gps_clean)

  #track days added
  dys = length( unique( as.Date( outData$UTC) ) )
  dysA = dys - length( unique( as.Date( gps_clean$UTC) ) )

  # summary of data processed
  cat( "Output data for ", site, " has ", dys, "unique days: ",
       as.character( as.Date( min( outData$UTC))) , " to ",
       as.character( as.Date( max( outData$UTC)) ), "with ", dysA, " new days added")

  # writes new file with appended data
  save(outData, file = paste0(outDirP, "HMDdata_", tolower(site), "_HourlySPL-gfs_", DC, ".Rda") )

  # write out processed files
  processedFiles = c(processedFiles, basename(inFiles) )
  
  # writes over previous file
  save(processedFiles, file = paste0(outDirP, "HMDfilesProcesed_", tolower(site), "_HourlySPL.Rda") )

} else {  # save out all the newly processed data
  # summary of data processed
  dysA = length( unique( as.Date( gps$UTC) ) )
  cat( "Output data for ", site, " has ", dysA, "unique days: ",
       as.character( as.Date( min( gps$UTC))) , " to ",
       as.character( as.Date( max( gps$UTC)) ))
  # writes new file with data
  outData = gps
  save(outData, file = paste0(outDirP, "HMDdata_", tolower(site), "_HourlySPL-gfs_", DC, ".Rda") )
  # write out processed files
  processedFiles  = basename(inFiles)
  save(processedFiles, file = paste0(outDirP, "HMDfilesProcesed_", tolower(site), "_HourlySPL.Rda") )
}
