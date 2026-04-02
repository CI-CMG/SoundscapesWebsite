#Model for Wind-generated Ocean Noise
# https://github.com/jahildebrand/WindNoise/blob/main/OceanNoiseModel.m
#J Hildebrand July 2021
# An empirical model for wind-generated ocean noise
# The Journal of the Acoustical Society of America 149, 4516 (2021); https://doi.org/10.1121/10.0005430
#John A. Hildebrand, Kaitlin E. Frasier, Simone Baumann-Pickering, and Sean M. Wiggins 

#modifed to run in R- Megan McKenna Jan 2025 and read in depths for ONMS sites

rm(list=ls()) 
library(ggplot2)
library(reshape)
library(plotly)
library(xlsx)

# GET DEPTHS OF INTEREST...
# depth = 200 # as.numeric(readline(prompt = "Receiver Depth: ")) # user input for depth
outDir = "~/GitHub/SoundscapesWebsite/" #"F:\\CODE\\GitHub\\SoundscapeScenes\\NCEI summary\\"

outDirG  =  paste0(outDir,"content/resources/") #where save graphics
outDirC  =  paste0(outDir,"context/") #where to get context
#metaFile = paste0(outDir, "F:\\CODE\\GitHub\\SoundscapeScenes\\NCEI summary\\ONMSSound_IndicatorCategories.xlsx")
metaFile = paste0(outDirG,"ONMSSound_IndicatorCategories.xlsx")
#lookup = as.data.frame ( read.xlsx(metaFile, sheetIndex = 1) )
lookup = as.data.frame ( openxlsx :: read.xlsx(metaFile, sheet  = "Summary") )

colnames(lookup) = lookup[1, ]         # Set first row as column names
lookup = as.data.frame( lookup[-1, ] ) # Remove the first row
lookup = as.data.frame( lookup[!apply(lookup, 1, function(row) all(is.na(row))), ] )
depthIn = as.data.frame( cbind( paste0(lookup[,3],lookup[,4]), as.numeric( as.character(gsub("~","",lookup[,23])) ) ) )
colnames(depthIn) = c("site","depth")
depthIn$depth = as.numeric( as.character( depthIn$depth ))

# WIND NOISE MODEL FOR specified depths
windModel = NULL
c = 0
for (depth in depthIn$depth ) {
  c=c+1
  si =  depthIn$site[c]
  cat("processing... ", si, " at depth of", depth,"\n")
  # Parameters ####
  a = 2.8          # (dB) depth dependence
  b = 600          # (m) 1/e for depth dependence
  aof = 0          # parameter aof
  bof = 100        # parameter bof
  cof = 12         # parameter cof
  
  mfacl = 1000     # parameter mfacl
  mfacf = 150      # parameter mfacf
  mfaca = 3        # parameter mfaca
  
  nfacl = 1000     # parameter nfacl
  nfacld = 400     # nfac linear depth
  
  nfacf = c(201, 101, 62, 41) # nfacf = freq for non-linear
  
  # Define the custom color scale ####
  custom_colors = c(
    rgb(0.3686, 0.3098, 0.6353),
    rgb(0.1961, 0.5333, 0.7412),
    rgb(0.3020, 0.7451, 0.9333),
    rgb(0.4667, 0.6745, 0.1882),
    rgb(0.4196, 0.8196, 0.1569),
    rgb(0.8314, 0.8314, 0.2667),
    rgb(0.9686, 0.7725, 0.2235),
    rgb(0.9490, 0.5255, 0.1059),
    rgb(0.9569, 0.4275, 0.2627),
    rgb(0.8353, 0.2431, 0.3098),
    rgb(0.6196, 0.0039, 0.2588),
    rgb(0.4980, 0.8706, 0.2353)
  )
  
  
  off = 50 + a*exp(-depth/b);
  
  #wind speed in m/s
  ms = c( 1, 2.5, 4.5, 6.7, 9.4, 12.3, 15.5, 19, 22.6, 26.5, 30.5)
  lms = log10(ms)  # Log10 of wind speed
  ss = c(0.5, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)   # Sea state
  beau = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)  # Beaufort scale
  
  #frequencies 
  fm = seq(0.01, 0.1, by = 0.01)  # 10 Hz steps from 10 Hz to 100 Hz
  #fm = seq(0.01, 0.435, by = 0.001)  # 1 Hz steps from 10 Hz to 435 Hz
  f  = seq(0.1, 160, by = 0.1)    # 100 Hz steps from 100 Hz to 100 kHz
  #f  = seq(0.435, 160, by = 0.01)    # 10 Hz steps from 435 Hz to 16 kHz
  fp = c(fm, f)                   # Combine for plotting
  fWi = c(100, 500, 1000, 5000, 10000, 20000, 30000, 100000, 160000)
  
  # MODEL for Knudson curves 10 Hz - 100 Hz ####
  #wind less than 1 m/s - nlm(iw,iifm) = ofac + off+6.4 + (n*20* log10(ms(iw))) + 4 *m* log10(fm(iifm)) ;
  #wind 1 - 2.5 m/s     - nlm(iw,iifm) = ofac + off+1.5 + (n*20* log10(ms(iw))) + 4 *m* log10(fm(iifm)) ;
  #wind 2.5 - 4.5 m/s   - nlm(iw,iifm) = ofac + off-0.5 + (n*20* log10(ms(iw))) + 4 *m* log10(fm(iifm)) ;
  #wind 4.5 - 6.7 m/s   - nlm(iw,iifm) = ofac + off+3.2 + (n *22* log10(ms(iw))) +4 *m* log10(fm(iifm)) ; %m=1
  #wind 6.7 - 9.4 m/s   - nlm(iw,iifm) = ofac + off+1.83 + (n *23* log10(ms(iw))) + 4 *m* log10(fm(iifm)) ; %m=1
  #wind 9.4 - 12.3 m/s  - nlm(iw,iifm) = ofac + off+2.4 + (n *23* log10(ms(iw))) + 4 *m* log10(fm(iifm)) ; %m=1
  #wind > 12.3 than 12 m/s   - nlm(iw,iifm) = ofac + off+2.18 + (n *23* log10(ms(iw))) + 4 *m* log10(fm(iifm)) ; %m=1
  #wind > 15.5 than 12 m/s   - nlm(iw,iifm) = ofac + off+2.27 + (n *23* log10(ms(iw))) + 4 *m* log10(fm(iifm)) ; %m=1
  #wind > 19.0 than 12 m/s   - nlm(iw,iifm) = ofac + off+2.54 + (n *23* log10(ms(iw))) + 4 *m* log10(fm(iifm)) ; %m=1
  #wind > 22.6 than 12 m/s   - nlm(iw,iifm) = ofac + off+2.72 + (n *23* log10(ms(iw))) + 4 *m* log10(fm(iifm)) ; %m=1
  #wind> 26.5 than 12 m/s    - nlm(iw,iifm) = ofac + off+2.8 + (n *23* log10(ms(iw))) + 4 *m* log10(fm(iifm)) ; %m=1
  
  nlm = matrix(0,  nrow = length(ms), ncol = length(fm)+4)     # Noise levels + columns with offset values for checking
  colnames(nlm) = c(as.character(fm*1000), "offset-2", "offset-n", "offset-adj", "windSpeed")
  rownames(nlm) = as.character(ms)
  
  off2 = c(6.4, 1.5, -0.5,3.2, 1.83, 2.4, 2.18, 2.27, 2.54, 2.72, 2.8 )
  offn = c(20,20,20,22,23,23,23,23,23,23,23)
  n = c(0.5,1,1,1.25,1.25,1.25,1.25,1.25,1.25,1.25,1.25)
  m = -1
  offadj = c(off-8, off-8,off-8,(off-8)-10,(off-8)-10,(off-8)-10,(off-8)-10,(off-8)-10,(off-8)-10,(off-8)-10,(off-8)-10)
  
  for ( iw in 1:length(ms) ) {
    n2 = n[(iw)] 
    for (iifm in 1:length(fm))
    {
      ofac = exp(-depth/bof)*aof*(cof-iifm/10)/cof # depth and frequency
      
      nlm[iw,iifm] = ofac + (offadj[iw] + off2[iw]) + ( n2*offn[iw]*log10(ms[iw]) ) + (4*m*log10(fm[iifm]))
      
      nlm[iw,13] = offadj[iw]
      nlm[iw,11] = off2[iw]
      nlm[iw,12] = offn[iw]
      nlm[iw,14] = ms[iw]
    }
    
  }
  nlm # NOTE- output matches matlab output
  off = (off-8)-10
  
  # MODEL for Knudson curves 100 Hz - 100 kHz ####
  #steps of 100 Hz
  # Noise levels for 100 Hz to 100 kHz
  # more frequency dependant parameters at higher frequencies
  nl = matrix(0, nrow = length(ms), ncol = length(f))  # Noise levels for 100 Hz to 100 kHz
  colnames(nl) = c(as.character(f*1000))
  rownames(nl) = as.character(ms)
  mfac = rep(0, length(f))  # Scaling factors
  
  ## winds < 1m/s ####
  n = 0.5
  m = 1
  off = off +18
  iw = 1  
  cat ("Processing winds ", ms[iw],"\n")
  ### 100-400 Hz ####
  iif = 1:4 
  ofac = exp(-depth/bof)*aof*(cof-iif)/cof; # depth and frequency
  nl[iw,iif] =  ofac + off+5.4 + (n*20* log10(ms[iw])) + 3 *(m - mfac[iif])* log10(f[iif]) 
  ### 500-900 Hz ####
  iif = 5:9 
  ofac = exp(-depth/bof)*aof*(cof-iif)/cof
  nl[iw,iif] = ofac + off+0.3 + (n*20* log10(ms[iw])) -  10.5*(m - mfac[iif])* log10(f[iif])
  # nl[iw,iif]
  ### 1000-4000 Hz  ####
  for (iif in 10:40 ){
    if( iif <= cof){
      ofac = exp(-depth/bof)*aof*(cof-iif)/cof
    }
    nl[iw,iif] = ofac + off+0.1 + (n*20* log10(ms[iw])) - 10.4 *(m - mfac[iif])* log10(f[iif]) 
  }
  # nl[iw,iif]
  ### 4100-10000 Hz  ####
  iif = 41:100
  nl[iw,iif] = off+2 + (n*20* log10(ms[iw])) - 13.5 *(m - mfac[iif])* log10(f[iif]) 
  # nl[iw,iif]
  ### > 10000 Hz  ####
  for (iif in 101:length(f)) { 
    if(depth < 1000){ 
      depm = depth
    }else{ 
      depm = 1000 }
    # ?####
    # changes the value from 0...
    mfac[iif] = (1 - depm/mfacl)* mfaca * m * (mfacf - iif)/length(f)
    if( iif > 400){
      mfac[iif] = mfac[400]
    }else if (iif < mfacf){
      mfac[iif] = 0   }
    
    nl[iw,iif] = off+2 + (n*20* log10(ms[iw])) - 13.5*(m - mfac[iif])* log10(f[iif])
  }
  # nl[iw,101:length(f)]
  
  ## winds 1 - 2.5 m/s ####
  n = 1
  m = 1 
  iw = 2
  cat ("Processing winds ", ms[iw],"\n")
  ### 100-400 Hz ####
  iif = 1:4 
  ofac = exp(-depth/bof)*aof*(cof-iif)/cof
  nl[iw,iif] = ofac + off-0.14 + (n*20* log10(ms[iw])) + 2.3 *(m - mfac[iif])* log10(f[iif])
  nl[iw,iif] 
  ### 500-900 Hz ####
  iif = 5:9
  ofac = exp(-depth/bof)*aof*(cof-iif)/cof;
  nl[iw,iif] =  ofac + off-4.7 + (n*20* log10(ms[iw])) - 10.7 *(m - mfac[iif])* log10(f[iif])
  # nl(iw,iif) = ofac + off-4.7 + (n*20* log10(ms(iw))) - 10.7*(m - mfac(iif))* log10(f(iif))
  nl[iw,iif] 
  ### 1000-4000 Hz  ####
  for (iif in 10:40 ){
    if( iif <= cof){
      ofac = exp(-depth/bof)*aof*(cof-iif)/cof
    }
    nl[iw,iif] = ofac +off-5 + (n*20* log10(ms[iw])) - 10.2 *(m - mfac[iif])* log10(f[iif]) 
  }
  nl[iw,10:40] 
  ### 4100 - 10000 Hz  ####
  iif = 41:100 
  nl[iw,iif] = off-3 + (n*20* log10(ms[iw])) - 13.5 *(m - mfac[iif])* log10(f[iif])
  nl[iw,iif] 
  ### > 10000 Hz ####
  iif = 101:length(f) 
  nl[iw,iif] = off-3 + (n*20* log10(ms[iw])) - 13.5 *(m - mfac[iif])* log10(f[iif])
  
  #CHECK- nl[ nl[iw,] == 0] 
  
  ## winds 2.5 - 4.5 m/s ####
  n = 1
  m = 1 
  iw = 3
  cat ("Processing winds ", ms[iw],"\n")
  ### 100-400 Hz ####
  iif = 1:4
  ofac = exp(-depth/bof)*aof*(cof-iif)/cof
  nl[iw,iif] = ofac + off-2.81 + (n*20* log10(ms[iw])) + 1.6 *(m - mfac[iif])* log10(f[iif])
  nl[iw,iif]
  ### 500-900 Hz ####
  iif = 5:9 
  ofac = exp(-depth/bof)*aof*(cof-iif)/cof
  #nl(iw,iif) = ofac + off-6.8 + (n*20* log10(ms(iw))) - 10*(m - mfac(iif))* log10(f(iif)) 
  nl[iw,iif] = ofac + off-6.8 + (n*20* log10(ms[iw])) - 10 *(m - mfac[iif])* log10(f[iif])
  nl[iw,iif] 
  ### 1000-4000 Hz   ####
  for (iif in 10:40 ){
    if( iif <= cof){
      ofac = exp(-depth/bof)*aof*(cof-iif)/cof
    }
    nl[iw,iif] = ofac +off-7 + (n*20* log10(ms[iw])) - 8.7 *(m - mfac[iif])* log10(f[iif]) 
  }
  nl[iw,10:40 ]
  ### 4100- 10000 Hz  ####
  iif = 41:100 
  nl[iw,iif] = off-4 + (n*20* log10(ms[iw])) - 13.5 *(m - mfac[iif])* log10(f[iif]) 
  nl[iw,iif]
  ### > 10000 Hz ####
  iif = 101:length(f) 
  nl[iw,iif] = off-4 + (n*20* log10(ms[iw])) - 13.5 *(m - mfac[iif])* log10(f[iif]) 
  nl[iw,iif]
  
  nl[ nl[iw,] == 0] 
  
  ## winds 4.5 - 6.7 m/s ####
  n = 1.25
  m = 1 
  off = off - 10
  iw = 4
  cat ("Processing winds ", ms[iw],"\n")
  ### 100-400 Hz ####
  iif = 1:4 
  ofac = exp(-depth/bof)*aof*(cof-iif)/cof
  nl[iw,iif] = ofac + off+0.18 + (n *22* log10(ms[iw])) + 0.9 *(m - mfac[iif])* log10(f[iif]) 
  nl[iw,iif]
  ### 500-900 Hz ####
  iif = 5:9 
  ofac = exp(-depth/bof)*aof*(cof-iif)/cof
  nl[iw,iif] = ofac + off-0.1 + (n *20* log10(ms[iw])) -  7*(m - mfac[iif])* log10(f[iif]) 
  nl[iw,iif]
  ### 1000 - 4000 Hz ####
  for (iif in 10:40 ){
    if( iif <= cof){
      ofac = exp(-depth/bof)*aof*(cof-iif)/cof
    }
    nl[iw,iif] = ofac +off-0.4 + (n*20* log10(ms[iw])) - 10.2 *(m - mfac[iif])* log10(f[iif]) 
  }
  nl[iw,10:40]
  ### 4100 - 10000 Hz ####
  n= 1
  iif = 41:200 #4100 Hz - 10000 Hz
  nl[iw,iif] = off+5.75 + (n *20* log10(ms[iw])) - 13.5 *(m - mfac[iif])* log10(f[iif]) 
  #nl(iw,iif) = off+5.75 + (n *20* log10(ms(iw))) - 13.5 *(m - mfac(iif))* log10(f(iif))
  nl[iw,iif]
  #### > 10000 Hz ####
  iif = 201:length(f) #> 10000 Hz
  nfac = (depth/nfacl+ 0.04*exp(-depth/nfacld))*0.3*n*(iif-nfacf[1])/length(f)
  nl[iw,iif] = off+5.7 + ((n - nfac) *20* log10(ms[iw])) - 13.5 *(m - mfac[iif])* log10(f[iif]) 
  # nl(iw,iif) = off+5.7 + ((n - nfac) *20* log10(ms(iw))) - 13.5 *(m - mfac(iif))* log10(f(iif))
  nl[iw,iif]
  nl[ nl[iw,] == 0] 
  
  ## winds 6.7 - 9.4 m/s ####
  n = 1.25
  m = 1 
  iw = 5
  cat ("Processing winds ", ms[iw],"\n")
  #### 100-400 Hz ####
  iif = 1:4 
  ofac = exp(-depth/bof)*aof*(cof-iif)/cof
  nl[iw,iif] = ofac + off-1.9 + (n *23* log10(ms[iw])) + 0.2 *(m - mfac[iif])* log10(f[iif]) 
  nl[iw,iif]
  #### 500-900 Hz ####
  iif = 5:9 
  ofac = exp(-depth/bof)*aof*(cof-iif)/cof
  nl[iw,iif] = ofac + off+0 + (n *20* log10(ms[iw])) - 6 *(m - mfac[iif])* log10(f[iif])
  nl[iw,iif]
  #### 1000 Hz - 4000 Hz ####
  for (iif in 10:40 ){
    if( iif <= cof){
      ofac = exp(-depth/bof)*aof*(cof-iif)/cof
    }
    nl[iw,iif] =ofac +off-0.3 + (n*20* log10(ms[iw])) - 12.5 *(m - mfac[iif])* log10(f[iif]) 
  }
  nl[iw, 10:40]
  ### 4100- 20000 Hz ####
  iif = 41:200 
  n = 1
  nl[iw,iif] = off+5.5 + (n *20* log10(ms[iw])) - 14 *(m - mfac[iif])* log10(f[iif]) 
  nl[iw,iif] 
  ### > 10000 Hz ####
  iif = 201:length(f) 
  nfac = (depth/nfacl+ 0.08*exp(-depth/nfacld))*0.3*n*(iif-nfacf[1])/length(f)
  nl[iw,iif] = off+5.5 + ((n - nfac) *20* log10(ms[iw])) - 14 *(m - mfac[iif])* log10(f[iif]) 
  nl[iw,iif] 
  nl[ nl[iw,] == 0] 
  
  ## winds 9.4 - 12.3 m/s ####
  n = 1.25
  m = 1 
  iw = 6
  cat ("Processing winds ", ms[iw],"\n")
  ### 100-400 Hz ####
  iif = 1:4 
  ofac = exp(-depth/bof)*aof*(cof-iif)/cof
  nl[iw,iif]  = ofac + off-2.10 + (n *23* log10(ms[iw])) - 0.5 *(m - mfac[iif])* log10(f[iif]) 
  nl[iw,iif] 
  ### 500-900 Hz ####
  iif = 5:9 
  ofac = exp(-depth/bof)*aof*(cof-iif)/cof
  nl[iw,iif] = ofac + off-0.12 + (n *20* log10(ms[iw])) - 6 *(m - mfac[iif])* log10(f[iif]) 
  nl[iw,iif]
  ### 1000 - 4000 Hz  ####
  for (iif in 10:40 ){
    if( iif <= cof){
      ofac = exp(-depth/bof)*aof*(cof-iif)/cof
    }
    nl[iw,iif] = ofac +off-0.3 + (n *20* log10(ms[iw])) - 13.7 *(m - mfac[iif])* log10(f[iif]) 
  }
  nl[iw,10:40] 
  ### 4100 - 10000 Hz ####
  iif = 41:100
  n = 1
  nl[iw,iif] = off+5.6 + (n *20* log10(ms[iw])) - 14.5 *(m - mfac[iif])* log10(f[iif]) 
  nl[iw,iif]
  ### > 10000 Hz ####
  iif = 101:length(f) #> 10000 Hz
  nfac = (depth/nfacl+ 0.17*exp(-depth/nfacld))*0.7*n*(iif-nfacf[2])/length(f)
  nl[iw,iif] = off+5.65 + ((n - nfac) *20* log10(ms[iw])) - 14.5 *(m - mfac[iif])* log10(f[iif]) 
  nl[iw,iif]
  nl[ nl[iw,] == 0] 
  
  ## winds 15.5  ####
  n = 1.25
  m = 1 
  iw = 7
  cat ("Processing winds ", ms[iw],"\n")
  ### 100-400 Hz ####
  iif = 1:4 
  ofac = exp(-depth/bof)*aof*(cof-iif)/cof;
  nl[iw,iif] = ofac + off-3.02 + (n *23* log10(ms[iw])) - 1.2 *(m - mfac[iif])* log10(f[iif])
  nl[iw,iif]
  ### 500-900 Hz ####
  iif = 5:9 #500-900 Hz
  ofac = exp(-depth/bof)*aof*(cof-iif)/cof
  nl[iw,iif] = ofac + off-.3 + (n *20* log10(ms[iw])) - 5 *(m - mfac[iif])* log10(f[iif])
  nl[iw,iif]
  ### 1000 Hz - 4000 Hz ####
  for (iif in 10:40 ){
    if( iif <= cof){
      ofac = exp(-depth/bof)*aof*(cof-iif)/cof
    }
    nl[iw,iif] = ofac + off-.3 + (n *20* log10(ms[iw])) - 14.5 *(m - mfac[iif])* log10(f[iif]) 
  }
  nl[iw,10:40]
  ### 4100 - 6100 Hz ####
  iif = 41:61 
  n = 1
  nl[iw,iif] = off+5.75 + (n *20* log10(ms[iw])) - 14.5 *(m - mfac[iif])* log10(f[iif]) 
  nl[iw,iif]
  ### > 6100 Hz ####
  for (iif in 62:length(f) ) {
    nfac[iif] = (depth/nfacl+ 0.24*exp(-depth/nfacld))*1.2*n*(iif-nfacf[3])/(length(f))
    nl[iw,iif] = off+5.85 + ((n - nfac[iif]) *20* log10(ms[iw])) - 14.5 *(m - mfac[iif])* log10(f[iif]) 
  }
  nl[iw,62:length(f) ]
  nl[ nl[iw,] == 0] 
  
  ## winds 19 m/s ####
  n = 1.25
  m = 1 
  iw = 8
  cat ("Processing winds ", ms[iw],"\n")
  ### 100-400 Hz ####
  iif = 1:4 
  ofac = exp(-depth/bof)*aof*(cof-iif)/cof
  nl[iw,iif] = ofac + off-3.54 + (n *23* log10(ms[iw])) - 1.9 *(m - mfac[iif])* log10(f[iif]) 
  nl[iw,iif]
  ### 500-900 Hz ####
  iif = 5:9 
  ofac = exp(-depth/bof)*aof*(cof-iif)/cof
  nl[iw,iif] = ofac +off-.3 + (n *20* log10(ms[iw])) - 6 *(m - mfac[iif])* log10(f[iif]) 
  nl[iw,iif]
  ### 1000- 4000 Hz ####
  for (iif in 10:40 ){
    if( iif <= cof){
      ofac = exp(-depth/bof)*aof*(cof-iif)/cof
    }
    nl[iw,iif] = ofac +off-.3 + (n *20* log10(ms[iw])) - 14.5 *(m - mfac[iif])* log10(f[iif]) 
  }
  nl[iw,10:40]
  ### 4100- 10000 Hz ####
  n = 1.0
  for (iif in 41:length(f)){
    nfac[iif] = (0.6*exp(-depth/nfacld) + depth/nfacl)*1.8*n*(iif-nfacf[(4)])/(length(f))
    nl[iw,iif] = off+6.1 + ((n - nfac[(iif)]) *20* log10(ms[(iw)])) - 14.5 *(m - mfac[(iif)])* log10(f[(iif)])
  }
  nl[iw,41:length(f)]
  ### >10000 Hz ####
  #  no wind dependence > 10 kHz for > 15 m/s
  nl[ nl[iw,] == 0] 
  
  ## winds 22.6  ####
  n = 1.25
  m = 1 
  iw = 9
  cat ("Processing winds ", ms[iw],"\n")
  ### 100-400 Hz ####
  iif = 1:4 
  ofac = exp(-depth/bof)*aof*(cof-iif)/cof
  nl[iw,iif] = ofac + off-4.06 + (n *23* log10(ms[(iw)])) - 2.6 *(m - mfac[(iif)])* log10(f[(iif)])
  nl[iw,iif]
  ### 500-900 Hz ####
  iif = 5:9 
  ofac = exp(-depth/bof)*aof*(cof-iif)/cof
  nl[iw,iif] = ofac + off-.3 + (n *20* log10(ms[(iw)])) - 6*(m - mfac[(iif)])* log10(f[(iif)]) 
  nl[iw,iif]
  ### 1000- 4000 Hz ####
  for (iif in 10:40 ){
    if( iif <= cof){
      ofac = exp(-depth/bof)*aof*(cof-iif)/cof
    }
    nl[iw,iif] = ofac + off-.3 + (n *20* log10(ms[(iw)])) - 14.5 *(m - mfac[iif])* log10(f[iif]) 
  }
  nl[iw,10:40]
  ### 4100- 10000 Hz ####
  n = 1
  for (iif in 41:length(f)){
    nfac[iif] = (depth/nfacl+ 1*exp(-depth/nfacld))*2.2*n*(iif-nfacf[(4)])/(length(f))
    nl[iw,iif] = off+6.4 + ((n - nfac[(iif)]) *20* log10(ms[(iw)])) - 14.5 *(m - mfac[(iif)])* log10(f[(iif)])
  }
  nl[iw,41:length(f)]
  ### >10000 Hz ####
  #  no wind dependence > 10 kHz for > 15 m/s
  nl[ nl[iw,] == 0] 
  
  ## winds 26.5  ####
  n = 1.25
  m = 1 
  iw = 10
  cat ("Processing winds ", ms[iw],"\n")
  ### 100-400 Hz ####
  iif = 1:4 
  ofac = exp(-depth/bof)*aof*(cof-iif)/cof
  nl[iw,iif]  = ofac + off-4.58 + (n *23* log10(ms[(iw)])) - 3.3 *(m - mfac[(iif)])* log10(f[(iif)]) 
  nl[iw,iif] 
  ### 500-900 Hz ####
  iif = 5:9 
  ofac = exp(-depth/bof)*aof*(cof-iif)/cof
  nl[iw,iif]  = ofac + off-.3 + (n *20* log10(ms[(iw)])) - 6*(m - mfac[(iif)])* log10(f[(iif)]) 
  nl[iw,iif] 
  ### 1000- 4000 Hz ####
  for (iif in 10:40 ){
    if( iif <= cof){
      ofac = exp(-depth/bof)*aof*(cof-iif)/cof
    }
    nl[iw,iif] = ofac + off-.3 + (n *20* log10(ms[(iw)])) - 14.5 *(m - mfac[iif])* log10(f[iif]) 
  }
  nl[iw,10:40]
  ### 4100- 10000 Hz ####
  n = 1
  for ( iif in 41:length(f) ){
    nfac[iif] = (depth/nfacl+ 1.6*exp(-depth/nfacld))*2.5*n*(iif-nfacf[4])/(length(f))
    nl[iw,iif] = off+6.85 + ((n - nfac[(iif)]) *20* log10(ms[(iw)])) - 14.5 *(m - mfac[(iif)])* log10(f[(iif)])
  }
  nl[iw,41:length(f)]
  ### >10000 Hz ####
  #  no wind dependence > 10 kHz for > 15 m/s
  nl[ nl[iw,] == 0] 
  
  ## winds >30.5  ####
  n = 1.25
  m = 1 
  iw = 11 
  cat ("Processing winds ", ms[iw],"\n")
  ### 100-400 Hz ####
  iif = 1:4 
  ofac = exp(-depth/bof)*aof*(cof-iif)/cof
  nl[iw,iif] = ofac + off-5.2 + (n *23* log10(ms[(iw)]))  - 4 *(m - mfac[(iif)])* log10(f[(iif)])
  nl[iw,iif]
  ### 500-900 Hz ####
  iif = 5:9 
  ofac = exp(-depth/bof)*aof*(cof-iif)/cof
  nl[iw,iif] = ofac + off-.3 + (n *20* log10(ms[(iw)])) - 6*(m - mfac[(iif)])* log10(f[(iif)])
  nl[iw,iif]
  ### 1000-4000 Hz ####
  for (iif in 10:40 ){
    if( iif <= cof){
      ofac = exp(-depth/bof)*aof*(cof-iif)/cof
    }
    nl[iw,iif] = ofac + off-.3 + (n *20* log10(ms[(iw)])) - 14.5 *(m - mfac[iif])* log10(f[iif]) 
  }
  nl[iw,10:40]
  ### 4100- 10000 Hz ####
  n = 1
  for ( iif in 41:length(f) ){
    nfac[iif] = (depth/nfacl+ 1.8*exp(-depth/nfacld))*2.7*n*(iif-nfacf[(4)])/(length(f))
    nl[iw,iif] = off+7.15 + ((n - nfac[(iif)]) *20* log10(ms[(iw)])) - 14.5 * (m - mfac[(iif)])* log10(f[(iif)])
  }
  nl[iw,41:length(f)]
  ### >10000 Hz ####
  #  no wind dependence > 10 kHz for > 15 m/s
  nl[ nl[iw,] == 0] 
  
  # RESULTS ####
  NL = cbind( cbind(si, depth, as.data.frame( nlm[,c(14, 1:9)] )) , as.data.frame(nl) )
  windModel = rbind(windModel, NL)
  save(windModel, file = paste0(outDirC, "WindModel_ONMS-", length(depthIn$site), "sites_.Rda") )
}

# PLOTS ####
#plot spectra for SPL vs windspeed for each depth as new graphic- need to melt to columns: depth, FQ, SPL, WS
colnames(windModel)
fq_columns = colnames(windModel)[3:length(windModel)]
windModelM = melt(windModel, id.vars = c("depth","windSpeed"), measure.vars = fq_columns)
colnames(windModelM)
unique( windModelM$variable )

ggplot(windModelM, aes(as.numeric(as.character(variable)),(value),color=as.factor(windSpeed ) ))+
  geom_point()+
  ylim(25,90)+
  scale_x_log10()+
  theme_minimal()+
  facet_wrap(~depth)

plot_ly(
  data = windModelM,
  x = ~as.numeric(as.character(variable)),
  y = ~value,
  color = ~as.factor(windSpeed),
  symbol = ~as.factor(depth),
  type = 'scatter',
  mode = 'markers',
  hoverinfo = 'text',
  text = ~paste(
    "Variable:", variable,
    "<br>Value:", round(value, 2),
    "<br>Wind Speed:", windSpeed,
    "<br>Depth:", depth
  )
) %>%
  layout(
    yaxis = list(title = "Value", range = c(25, 90)),
    xaxis = list(title = "Variable (Log Scale)", type = "log"),
    title = "Interactive Plot of Wind Model",
    legend = list(title = list(text = "Wind Speed"))
  )
