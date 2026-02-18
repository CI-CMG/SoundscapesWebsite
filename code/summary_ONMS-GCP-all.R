# PURPOSE -  query NCEI gcp passive acoustic archive to create gantt charts of data in the archive
# NOTES - use command line approach to access files, assumes open sites (do not need authentication step)
# INPUT - gcp directory for project- assumes metadata files are present
# OUTPUT - Rdat file to use in plotting

rm(list=ls()) 

# LOAD LIBRARIES ####
library(stringr)   
library(openxlsx)
library(jsonlite)
library(curl)
library(tidyverse)
library(ncdf4)
library(googlesheets4)
library(openxlsx)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
DC = Sys.Date()

# SET GCP DIRECTORY ####
# get directories from NCEI GCP data viewer: 
#https://console.cloud.google.com/storage/browser/noaa-passive-bioacoustic?inv=1&invt=Ab0oyg
typ = "audio"
gcpDirONMS  = "gs://noaa-passive-bioacoustic/onms/audio" #ONMS
gcpDirSS    = "gs://noaa-passive-bioacoustic/sanctsound/audio" #SANCTSOUND
gcpDirNRS   = "gs://noaa-passive-bioacoustic/nrs/audio" #SANCTSOUND
projectNONMS = "onms" # set this to deal with different metadata formats
projectNSS  = "sanctsound"# set this to deal with different metadata formats
projectNNRS = "nrs"# set this to deal with different metadata formats

#outDir =   "F:/CODE/GitHub/SoundscapesWebsite/"
outDir =   "C:/Users/embe5980/SoundscapesWebsite/"
outDirR =  paste0(outDir, "content/resources/") #save graphics
outDirP =  paste0(outDir, "products/onms/")     #products

# CONTEXT ####
# assign region abbreviations 
wc = c("oc","ci","mb","cb","fi") #west coast sanctuaries
ec = c("sb","gr","fk","fg") #east coast sanctuaries
pi = c("hi","pm","as") #greater pacific
gl = c("lo") # great lakes

inFile = paste0(outDirR, "/ONMSSound_IndicatorCategories.xlsx")
lookup = as.data.frame ( openxlsx :: read.xlsx(inFile) )
colnames(lookup) <- lookup[1, ]  # Set first row as column names
lookup <- as.data.frame( lookup[-1, ] ) # Remove the first row
colnames(lookup)[5] = "NCEI"

inFile = paste0(outDirP, "/early_nrs_datasets.xlsx")
NRSold = as.data.frame ( openxlsx :: read.xlsx(inFile) )
NRSold$Start_Date = as.Date(NRSold$Start_Date, origin = "1899-12-30")
NRSold$End_Date = as.Date(NRSold$End_Date, origin = "1899-12-30")

# LIST SUB DIRECTORIES ####
#these should be the "monitoring sites" you want to gather information about
command = "gsutil"
args =  c("ls", gcpDirONMS)
subdirsONMS = system2(command, args, stdout = TRUE, stderr = TRUE) 

command = "gsutil"
args =  c("ls", gcpDirSS)
subdirsSS = system2(command, args, stdout = TRUE, stderr = TRUE)  

command = "gsutil"
args =  c("ls", gcpDirNRS)
subdirsNRS = system2(command, args, stdout = TRUE, stderr = TRUE)  

subdirsALL = c(subdirsONMS, subdirsSS, subdirsNRS) 
subdirsALL <- subdirsALL[subdirsALL != "gs://noaa-passive-bioacoustic/onms/audio/fk08/"]
dirNames   = sapply(strsplit(basename( subdirsALL ), "/"), `[`, 1)
cat("Processing... ", projectNONMS, length(dirNames), "directories" )

## TEST one file ####
#noaa-passive-bioacoustic/onms/audio/as01/onms_as01_20230317/metadata
args = c("ls", "-r", subdirsALL[1])
sFiles = system2(command, args, stdout = TRUE, stderr = TRUE)  
json_files = grep("\\.json$", sFiles, value = TRUE) # metadata files
url = paste0("https://storage.googleapis.com/", gsub ("gs://", '', paste(json_files[1], collapse = "") ) )
# h = curl(url, "r")
# json_content = readLines(url)
# close(h)
# tmp = fromJSON(paste(url, collapse = ""))

tmp = fromJSON(url)
if ( length(tmp)  > 0 ) {
  cat("Successfully loaded file from GCP... contiune" )
} else {cat("File not loaded from GCP, check directories" ) }
 
# GET INFORMATION FROM METADATA FILES ####
# loads one file at a time from GCP, no saving to local machine 

## ONMS + SS ####
subdirsALL = c(subdirsONMS, subdirsSS) 
subdirsALL <- subdirsALL[subdirsALL != "gs://noaa-passive-bioacoustic/onms/audio/fk08/"]
output = NULL
dirNames   = sapply(strsplit(basename( subdirsALL ), "/"), `[`, 1)

for (s in 1:length(subdirsALL) ) { # s = 1
  
  # read in files
  args = c("ls", "-r", subdirsALL[s])
  sFiles = system2(command, args, stdout = TRUE, stderr = TRUE)  
  json_files = grep("\\.json$", sFiles, value = TRUE) #metadata files
  cat("Processing... ", dirNames[s], "[", s, " of ", length(subdirsALL),"]", "\n" )
  
  if ( length(grep(projectNONMS, subdirsALL[s]) ) > 0 ) { # check for format - onms
    
    for (jf in 1:length( json_files) ) {
      
      url = paste0("https://storage.googleapis.com/", gsub ("gs://", '', paste(json_files[jf], collapse = "") ) )
      # h = curl(url, "r")
      # json_content = readLines(url)
      # close(h)
      # tmp = fromJSON(paste(url, collapse = ""))
      tmp = fromJSON(url)
      
      name  = tmp$DATA_COLLECTION_NAME 
      instr = tmp$INSTRUMENT_TYPE
      start = as.Date( gsub("T"," ", tmp$DEPLOYMENT$AUDIO_START), format = "%Y-%m-%d")
      end   = as.Date( gsub("T"," ", tmp$DEPLOYMENT$AUDIO_END), format = "%Y-%m-%d")
      lat   = tmp$DEPLOYMENT$DEPLOY_LAT
      lon   = tmp$DEPLOYMENT$DEPLOY_LON
      
      #save to output data - each deployment
      output = rbind(output, c(subdirsALL[s], jf, name, instr, 
                               as.character(start), as.character(end), 
                               lat, lon) )
      
    }
    
    
  } else if ( length(grep(projectNSS, subdirsALL[s]) ) > 0 ) {  # check for format - sanctsound
    
    for (jf in 1:length( json_files) ) {
      
      url = paste0("https://storage.googleapis.com/", gsub ("gs://", '', paste(json_files[jf], collapse = "") ) )
      # h = curl(url, "r")
      # json_content = readLines(url)
      # close(h)
      # tmp = fromJSON(paste(url, collapse = ""))
      # 
      tmp = fromJSON(url)
      
      name  = tmp$DEPLOYMENT_NAME  
      instr = tmp$INSTRUMENT_NAME
      if ( length(instr ) == 0 ){
        instr = tmp$INSTRUMENT_TYPE
        start = as.Date( gsub("T"," ", tmp$DEPLOYMENT$AUDIO_START), format = "%Y-%m-%d")
        end   = as.Date( gsub("T"," ", tmp$DEPLOYMENT$AUDIO_END), format = "%Y-%m-%d")
        lat   = tmp$DEPLOYMENT$DEPLOY_LAT
        lon   = tmp$DEPLOYMENT$DEPLOY_LON
        
      } else {
        range = tmp$DATA_QUALITY$'1'$`Date Range`
        start = as.Date( strsplit(range, " to ")[[1]],format = "%Y-%m-%d" )[1]
        end   = as.Date( strsplit(range, " to ")[[1]],format = "%Y-%m-%d" )[2] 
        lon   = tmp$LOCATION$lon
        lat   = tmp$LOCATION$lat
        
      }
      
      #save to output data - each deployment
      output = rbind(output, c(subdirsALL[s], jf, name, instr, 
                               as.character(start), as.character(end), 
                               lat, lon) )
      
    }
  }
}
ouputTest = output # output = ouputTest

### FORMAT OUTPUT ####
output = as.data.frame(output)
colnames(output) = c("Path", "DeploymentNumber", "DeploymentName", "Instrument", "Start_Date", "End_Date","Lat","Lon")
output$Site = basename((output$Path))
output$Start_Date = as.Date(output$Start_Date, format = "%Y-%m-%d")
output$End_Date = as.Date(output$End_Date, format = "%Y-%m-%d")
### ADD INFO from context file ####
lookup_selected = lookup %>% select(NCEI, Region,`Common Name/Identifiers`,`Site Description/Driver for Monitoring Location Choice`)
output = left_join(output, lookup_selected, by = c("Site" = "NCEI"), relationship = "many-to-many")
output$Duration = difftime( output$End_Date, output$Start_Date,"days")
output$Project1 = sapply(strsplit(output$Path, "/"), `[`, 4)
output$Project [is.na(output$Region)]  = "SanctSound" 
output$Project [!is.na(output$Region)] = "ONMS-sound" 
names(output )

## NRS ####
outNRS = NULL
subdirsALL = subdirsNRS 
dirNames   = sapply(strsplit(basename( subdirsALL ), "/"), `[`, 1)
for (s in 1:length(subdirsALL) ) { # s=1
  
  ## read in files
  args = c("ls", "-r", subdirsALL[s])
  sFiles = ( system2(command, args, stdout = TRUE, stderr = TRUE)  )
  json_files = grep("\\.json$", sFiles, value = TRUE) #metadata files
  cat("Processing... ", dirNames[s], "[", s, " of ", length(subdirsALL),"]", length(sFiles), "\n" )
  
  for (jf in 1:length( json_files) ) { # jf = 1
    
    #path for this deployment metadata 
    path_parts <- unlist(strsplit(json_files[jf], "/"))
    deploymentPath <- paste(path_parts[1:(length(path_parts) - 2)], collapse = "/")
    
    url = paste0("https://storage.googleapis.com/", 
                 gsub ("gs://", '', paste(json_files[jf], collapse = "") ) )
    h = curl(url, "r")
    json_content = readLines(url)
    close(h)
    tmp = fromJSON(paste(url, collapse = ""))
    
    ## check which version
    if ( is.null( tmp$deployment$audio_start) ) { #not small caps
      
      name  = tmp$SITE 
      deploy = tmp$DEPLOYMENT_NAME
      instr = tmp$INSTRUMENT_TYPE
      start = as.Date( gsub("T"," ", tmp$DEPLOYMENT$AUDIO_START), format = "%Y-%m-%d")
      end   = as.Date( gsub("T"," ", tmp$DEPLOYMENT$AUDIO_END), format = "%Y-%m-%d")
      lat   = tmp$DEPLOYMENT$DEPLOY_LAT
      lon   = tmp$DEPLOYMENT$DEPLOY_LON
      
      #save to output data - each deployment
      outNRS = rbind(outNRS, c(deploymentPath, jf, name, deploy, instr, 
                               as.character(start), as.character(end), 
                               lat, lon) )
      
      
    } else if ( !is.null( tmp$deployment$audio_start) ) { #small caps
      
      name  = tmp$site 
      names(tmp)[names(tmp) == "INSTRUMENT_TYPE"] =  "instrument_type"
      instr = tmp$instrument_type
      deploy =  tmp$deployment_id
      start = as.Date( gsub("T"," ", tmp$deployment$audio_start), format = "%Y-%m-%d")
      end   = as.Date( gsub("T"," ", tmp$recover$audio_end), format = "%Y-%m-%d")
      lat   = tmp$deployment$lat
      lon   = tmp$deployment$lon
      
      #save to output data - each deployment
      outNRS = rbind(outNRS, c(deploymentPath, jf, name, deploy, instr, 
                               as.character(start), as.character(end), 
                               lat, lon) )
    } else {
      cat(deploymentPath, "- Not correct format", "\n" )
      
      outNRS = rbind(outNRS, c(deploymentPath, jf, NA, NA, 
                               NA, NA, 
                               NA, NA) )
      
    } 
  }
}
ouputTest2 = outNRS # outNRS = ouputTest2

#### FORMAT OUTPUT ####
# ouputTest2 = output # output = ouputTest
outNRS = as.data.frame(outNRS)
colnames(outNRS) = c("Path", "FileCount", "SiteName", "DeploymentName", "Instrument", "Start_Date", "End_Date","Lat","Lon")
outNRS$Site = paste0(basename((outNRS$Path)))
outNRS$Start_Date = as.Date(outNRS$Start_Date, format = "%Y-%m-%d")
outNRS$End_Date = as.Date(outNRS$End_Date, format = "%Y-%m-%d")
outNRS$Days = difftime( outNRS$End_Date, outNRS$Start_Date,"days")
outNRS$Instrument = "AUH"
# ADD OLD DEPLOYMENTS
outNRS = rbind(outNRS, NRSold )
outNRS$Site = paste0( tolower( projectNNRS ), "_", outNRS$SiteName ) 
outNRS$Site = gsub("_", "", outNRS$Site)

### ADD INFO from context file ####
lookup_selected = lookup %>% select(NCEI, Region,`Common Name/Identifiers`,`Site Description/Driver for Monitoring Location Choice`)
outNRS2 = left_join(outNRS, lookup_selected, by = c("Site" = "NCEI"), relationship = "many-to-many")
outNRS2$Duration = difftime( outNRS2$End_Date, outNRS2$Start_Date,"days")
outNRS2$Project1 = "NRS"
outNRS2$Project [is.na(outNRS2$Region)]  = "NRS" 
outNRS2$Project [!is.na(outNRS2$Region)] = "ONMS-sound" 
names(outNRS2 )

# COMBINE DATASETS ####
outputS  = subset(output,  select = c(Path, Site, DeploymentName, Instrument, Start_Date, End_Date, Lat,Lon,Duration, Project, Region, Project1 ))
outNRS2S = subset(outNRS2, select = c(Path, Site, DeploymentName, Instrument, Start_Date, End_Date, Lat,Lon,Duration, Project, Region, Project1 ))
outputC = rbind(outputS,  outNRS2S)
#outputC$Region

## SAVE ALL ####
save(outputC, file = paste0(outDirP, "/data_gantt_ONMS-SS-NRS_gantt_ALL", DC, ".Rda") ) #all data
## SAVE ONMS long-term only ####
outputONMS = outputC[!is.na(outputC$Region),]
save(outputONMS,      file = paste0(outDirP, "/data_gantt_ONMS-SS-NRS_gantt_", DC, ".Rda") )
write.csv(outputONMS, file = paste0(outDirP, "/data_gantt_ONMS-SS-NRS__gantt_", DC, ".csv") )


#option to load the file, if you already ran and just want to plot ####
#load(paste0(outDirP, "/data_gantt_ONMS-SS-NRS_gantt_2026-02-05.Rda") )

# GANTT CHART  ####
## COLOR ####
uColors = unique(outputONMS$Region) 
# nmfspalette::nmfs_palette("oceans")(10)
#[1] "#C6E6F0" "#8CCBE3" "#53B0D7" "#1F95CF" "#0072BB" "#004295" "#002B7B" "#002467" "#001D55" "#001743"
region_colors <- c(
  "Pacific Islands" = "#C6E6F0",  
  "West Coast"      = "#53B0D7",  
  "East Coast"      = "#004295", 
  "Gulf Coast"      = "#001743") 

uProject = unique(outputONMS$Project1) 
outputONMS$Project1[outputONMS$Project1 == "onms"] = "ONMS-Sound"
outputONMS$Project1[outputONMS$Project1 == "sanctsound"] = "SanctSound"
project_colors <- c(
  "SanctSound" = "#C6E6F0",  
  "ONMS-Sound" = "#53B0D7",
  "NRS" = "#004295") 

## geom_tile option ####
pTb = ggplot(outputONMS, aes(y = toupper(Site), x = Start_Date, xend = End_Date, fill = Project1 ) ) +
  geom_tile(aes(x = Start_Date, width = as.numeric(End_Date - Start_Date) ) , 
            color = "gray", height = 0.6) +  # Fill color by Instrument and outline in black
  scale_fill_manual(values = project_colors) +  # Use specific colors for instruments
  labs(x = "", y = "", title = "",
       caption = paste0("Audio data archived at NCEI as of ", format(Sys.Date(), "%B %d, %Y"))) +
  facet_wrap(~Region,scales = "free_y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 12),
        axis.text.y = element_text(angle = 0, size = 12),
        legend.position = "bottom", 
        legend.text = element_text(size = 14),
        legend.title = element_blank(),
        strip.text = element_text(size = 14),
        plot.caption = element_text(size = 14, hjust = 0.5),
        #panel.border = element_rect(color = "gray", fill = NA, size = .1),
        panel.spacing = unit(2, "cm") )
pTb
ggsave(filename = paste0(outDirR, "/gantt_ONMS-SS-NRS.jpg"), plot = pTb, width = 8, height = 6, dpi = 300)

# MAP DATA -- not working need to update data frame names ####
#reformat for per site- total recordings 
outputMap =  as.data.frame(
  outputONMS %>%
    group_by(Site) %>%
    summarise(
      total_days = sum(Duration, na.rm = TRUE),   # Summing total duration for each site
      min_start_date = min(Start_Date, na.rm = TRUE)  # Getting the minimum start date for each site
    )
)

outputMap$Site = as.character(outputMap$Site)
lookup_selected <- lookup %>% select(NCEI, Region, Latitude, Longitude)
outputMap = left_join(outputMap, lookup_selected, by = c("Site" = "NCEI"))
write.csv( outputMap, file = paste0(outDirP, "/map_ONMS_map_", DC, ".csv") )
outputMap2a = outputMap
# Assuming your data frame is named 'data'
# Convert the data frame to an sf object for mapping
outputMap2a$Latitude  = as.numeric(as.character( gsub("−", '-', outputMap2a$Latitude )) )
outputMap2a$Longitude = as.numeric(as.character( gsub("−", '-', outputMap2a$Longitude )) )
outputMap2a$TotalDays = as.numeric(as.character(outputMap2a$total_days))
data_sf <- st_as_sf(outputMap2a, coords = c("Longitude", "Latitude"), crs = 4326)
# Create the map
# Get world land data (in low resolution, change to 'medium' or 'large' if needed)
world <- ne_countries(scale = "medium", returnclass = "sf")
bbox <- st_bbox(data_sf)
# Create the map with land backgroun
p = ggplot() +
  geom_sf(data = world, fill = "gray80", color = "gray40") +   # Add land background
  geom_sf(data = data_sf, aes(color = Region, size = TotalDays)) +  # Color by Region, size by total days
  scale_color_manual(values = region_colors) +  # Use specific colors for instruments
  theme_minimal() +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]+5), 
           ylim = c(bbox["ymin"], bbox["ymax"]+5), 
           expand = FALSE) +  # Trim map to data points
  labs(title = "",
       size =  "Total Days",
       color = "Region",
       caption = paste0("Data available on NCEI-GCP (", typ, ") as of ", format(Sys.Date(), "%B %d, %Y") ) ) +
  scale_size_continuous(range = c(.5, 8))  # Adjust point size range
p
ggsave(filename = paste0(outDirR, "/map_ONMS.jpg"), plot = p, width = 8, height = 6, dpi = 300)
