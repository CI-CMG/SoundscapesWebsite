# How does your sanctuary compare?
# based on sanctsound data

rm(list=ls()) 

library(PAMscapes)
library(lubridate)
library(dplyr)
library(reshape)
library(ggplot2)
library(openxlsx)

dirSS  = "F:\\SanctSound" # SANCTSOUND
outDir = "F:\\CODE\\GitHub\\SoundscapesWebsite\\"
outDirR = paste0(outDir, "content\\resources\\") #save graphics
outDirP = paste0(outDir, "products\\onms\\")     #products

# LOAD ONMS Metadata ####
habitat = "coastal-shallow"
monthFocus = 7
yoi = 2019

metaFile = paste0(outDirR,"ONMSSound_IndicatorCategories.xlsx")
lookup = as.data.frame ( openxlsx::read.xlsx(metaFile, sheet  = "Summary") ) 
colnames(lookup) = lookup[1, ]         # Set first row as column names
lookup = as.data.frame( lookup[-1, ] ) # Remove the first row
lookup = as.data.frame( lookup[!apply(lookup, 1, function(row) all(is.na(row))), ] )
unique( (lookup$`Oceanographic category`) )
sites = lookup[lookup$`Oceanographic category` == habitat ,] # selects specific ecological setting, so comparable
siteInterest = sites[,5]

# FIND SanctSound files - ERDAP ####
# NOTE - might need to change these in some of the files 31_5 to 31.5 and UTC with : not _
inFiles = list.files(path = dirSS, pattern = "TOL_1h", full.names = T, recursive = T)
filesTOL = inFiles[grepl("TOL_1h", inFiles)] 
inFiles = filesTOL[!grepl("/analysis/", filesTOL)] 
inFiles = inFiles[!grepl("json", inFiles)] 
inFiles = inFiles[!grepl("1h.nc", inFiles)] 
inFiles = inFiles[!grepl("xml", inFiles)] 
inFiles = inFiles[sapply(inFiles, function(x) any(grepl(paste(toupper( siteInterest ), collapse = "|"), x)))]

# COMBINE DATA ####
sData = NULL
for (ii in 1:length(inFiles)) { # ii = 39
  tmpFile = inFiles[ii]
  typ = sapply( strsplit(basename(tmpFile), "[.]"), "[[",2)
  site = sapply( strsplit(basename(tmpFile), "[_]"), "[[",2)
  tmp = loadSoundscapeData( inFiles[ii], extension = typ)
  tmp$site = site
  tmp$yr = year(tmp$UTC) # unique( tmp$yr )
  tmp = tmp[tmp$yr == yoi, ]
  
  if (ii > 1) {
    col_mismatched = setdiff(colnames(tmp), colnames(sData))
    tmpc = tmp[, !colnames(tmp) %in% col_mismatched]
  } else {
    tmpc = tmp
  }

  if(nrow(tmpc) > 0 ) {
    cat( ii, inFiles[ii], "Start = ", as.character( as.Date( min(tmpc$UTC) ) ),"\n")
    sData = rbind(sData, tmpc)
    
  } else {
    cat( ii, inFiles[ii], "Not data in ", yoi, "\n")
    sData = rbind(sData, tmpc)
  }
  
}
sData$mth= month(sData$UTC)

# SELECT sites in same setting ####
sData = sData[tolower(sData$site) %in% siteInterest, ]

# SELECT representative month ####
month_counts = ( ( sData %>% count(mth) ))
moi = month_counts %>% filter(n == max(n))
sDataM  = sData[sData$mth == moi$mth,]
sDataMM = sData[sData$mth == monthFocus,] # unique(sDataMM$site)
cat("Most data for this month:", moi$mth, ", for these sites:", unique(sDataM$site) )
sDatat = sData[sData$mth == moi$mth,]
mthFocus =  moi$mth
# filter to this month... 
# if (siteFocus %in% unique(sDataM$site) == T ) {
#   sDatat = sData[sData$mth == moi$mth,]
#   mthFocus =  moi$mth
# }else { 
#   sDatat = sData[sData$mth == monthFocus,]
#   mthFocus = monthFocus }

# PERCENTILES for each site ####
tol_columns = grep("TOL", colnames(sDatat))
site_split = split(sDatat, sDatat$site) # Calculate quantiles for each site
season_quantiles = lapply(site_split, function(sDatat) {
  apply(sDatat[, tol_columns, drop = FALSE], 2, quantile, na.rm = TRUE)
})

seasonAll = NULL
for (ii in 1: length(season_quantiles) ) {
  tmp = as.data.frame ( season_quantiles[ii] ) 
  colnames(tmp) = colnames(sDatat)[tol_columns]
  tmp$Quantile = rownames(tmp)
  tmp$Season = names(season_quantiles)[ii]
  rownames(tmp) = NULL
  seasonAll = rbind(seasonAll,tmp)
}

# PLOT for each site ####
usites = unique(sDatat$site)

for (ss in 1:length(usites) ) {
  tol_columns = grep("TOL", colnames(seasonAll))
  mallData = melt(seasonAll, id.vars = c("Quantile","Season"), measure.vars = tol_columns)
  mallData$variable = as.numeric( as.character( gsub("TOL_", "", mallData$variable )))
  colnames(mallData) = c("Quantile", "Site", "Frequency" , "SoundLevel" )
  mallDataS = mallData[ mallData$Site == usites[ss], ]
  
  start_points = mallData %>%
    group_by(Site, Quantile) %>%
    slice_min(order_by = Frequency, n = 1)
  start_points = start_points[start_points$Quantile == "50%", ]
  
  
  p1 = ggplot() +
    #median TOL values
    geom_line(data = mallData[mallData$Quantile == "50%",], aes(x = Frequency, y = SoundLevel, color = Site), linewidth = 1) +
    geom_line(data = mallDataS[mallDataS$Quantile == "50%",], aes(x = Frequency, y = SoundLevel), color = "black", linewidth = 3) +
    scale_x_log10() +  
    theme_minimal() +
    theme(legend.position = "right",
          plot.title = element_text(size = 16, face = "bold", hjust = 0)) +  # This line removes the legend
    labs(
      title    = paste0("How does my sanctuary compare? "),
      subtitle = paste0( usites[ss], " compared to other ", habitat, " sites in ", month.abb[mthFocus],"-", yoi ),
      caption  = "Representative month from SanctSound Project", 
      x = "Frequency Hz",
      y = expression(paste("Sound Levels (dB re 1 ", mu, " Pa 1/3 octave bands)" ) )
    ) +
    geom_text(data = start_points, aes(x = Frequency, y = SoundLevel, label = Site), 
              vjust = 0, hjust = 1, size = 3, color = "black")
  
  p1
  
  ggsave(filename = paste0(outDirR, "plot_", toupper(usites[ss]), "_Compare.jpg"), plot = p1, width = 8, height = 6, dpi = 300)
  
}




