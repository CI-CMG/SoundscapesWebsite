# SPL Annual and Seasonal graphs to interactive using plotly
# By: Emma Beretta, ONMS Sound Team August 25th, 2026
# run this script after running plot_ONMS-conditions_HMD.R, so all data will already be loaded into the environment

library(htmlwidgets)
library(htmltools)
library(plotly)



# create shaded regions for FOIs on both annual and seasonal graph
polygon_data <- FOIsRange %>%
  mutate(id = row_number()) %>%  
  uncount(4) %>%               
  group_by(id) %>%
  mutate(
    corner = row_number(),
    # Map out the 4 corners of each rectangle clockwise
    x = case_when(
      corner == 1 ~ FQstart,    # Bottom-Left
      corner == 2 ~ FQstart,    # Top-Left
      corner == 3 ~ FQend,      # Top-Right
      corner == 4 ~ FQend       # Bottom-Right
    ),
    y = case_when(
      corner == 1 ~ 27,         # Bottom boundaries matching scale min
      corner == 2 ~ y_max,         # Top boundaries matching scale max
      corner == 3 ~ y_max,         
      corner == 4 ~ 27          
    )
  ) %>%
  ungroup()


polygon_data <- FOIsRange %>%
  mutate(width = log10(FQend) - log10(FQstart)) %>%   # width in log space, since your x axis is log
  arrange(desc(width)) %>%                            # widest first (bottom), narrowest last (top)
  mutate(id = row_number()) %>%
  uncount(4) %>%
  group_by(id) %>%
  mutate(
    corner = row_number(),
    x = case_when(
      corner == 1 ~ FQstart,
      corner == 2 ~ FQstart,
      corner == 3 ~ FQend,
      corner == 4 ~ FQend
    ),
    y = case_when(
      corner == 1 ~ 27,
      corner == 2 ~ y_max,
      corner == 3 ~ y_max,
      corner == 4 ~ 27
    )
  ) %>%
  ungroup()


#ANNUAL LINE GRAPH

#dealing with data gaps for plotly
ribbonData <- mallData %>% mutate(is_na = is.na(`SoundLevel`) ,
         gap = is_na != lag(is_na, default = first(is_na)),
         segment = cumsum(gap)) %>%
  ungroup()

if (site == 'fk08'){
  segment1 = 0
  segment2 = 2
} else if (site == 'cinms_b'){
  segment1 = 0
  }else {
  segment1 = 1
  segment2 = 3
}



# Compute gaps per-year on the pivoted data (so both 25% and 75% missing-ness count)
ribbonData <- mallData %>%
  filter(Quantile %in% c("25%", "75%")) %>%
  pivot_wider(names_from = Quantile, values_from = SoundLevel) %>%
  group_by(Year) %>%
  arrange(Frequency, .by_group = TRUE) %>%
  mutate(
    is_na   = is.na(`25%`) | is.na(`75%`),
    gap     = is_na != lag(is_na, default = first(is_na)),
    segment = cumsum(gap)
  ) %>%
  ungroup() %>%
  filter(!is_na)          # drop the NA runs entirely, keep only real data segments



#make it so hover appears along whole black dashed line

if (nrow(FOIs) > 0 || nrow(FOIsL) > 0){
vline_data <- bind_rows(FOIs, FOIsL) %>%
  mutate(id = row_number()) %>%
  rowwise() %>%
  mutate(pts = list(data.frame(
    x = FQstart,
    y = seq(27, y_max, length.out = 100)
  ))) %>%
  ungroup() %>%
  select(id, Label, FQstart, pts) %>%
  unnest(pts)
} else{
  #vline_data = data.frame()
  
  vline_data <- tibble(
    id = integer(),
    Label = character(),
    x = numeric(),   # change to as.Date(character()) or as.POSIXct(character()) if FQstart is a date/time
    y = numeric()
  )
}



if (site == "wea_hua"){

hover_vlines <- polygon_data %>%
  distinct(id, Label, FQstart, FQend) %>%
  filter(grepl("fin|blue", Label, ignore.case = TRUE)) %>%
  mutate(minf = FQstart, maxf = FQend,
         FQmid = sqrt(FQstart * FQend)) %>%          # halfway on the log axis
  pivot_longer(c(FQstart, FQmid, FQend), names_to = "edge", values_to = "x") %>%
  mutate(line_id = paste(id, edge)) %>%
  rowwise() %>%
  mutate(pts = list(data.frame(y = seq(27, y_max, length.out = 100)))) %>%
  ungroup() %>%
  unnest(pts)

}

# create annual graph

pl = ggplot() +
  #wind model
  geom_line(data = mwindInfo[as.character(mwindInfo$windSpeed) == windUpp,], 
            aes(x = variable, y = value, 
                text = "Modeled Max Wind Noise"), color = "black", linewidth = .5) +
  
  geom_line(data = mwindInfo[as.character(mwindInfo$windSpeed) == windLow,], 
            aes(x = variable, y = value, 
                text = "Modeled Min Wind Noise"), color = "black", linewidth = .5) +
 
  geom_polygon(data = polygon_data,
               aes(x = x, y = y, group = id,  
                   text = paste0(Label, "<br>Min Freq: ", round(FQstart, 1) , " Hz<br>Max Freq: ", round(FQend, 1), " Hz")), # 
               fill = "gray",
               alpha = 0.2,
               inherit.aes = FALSE) 

  if (nrow(vline_data) >0 ){
  pl = pl + geom_line(data = vline_data,
            aes(x = x, y = y, group = id,
                text = paste0(Label, "<br>Freq: ", round(FQstart, 1), " Hz")),
            color = "black", linetype = "dashed", linewidth = 0.5,
            inherit.aes = FALSE)
    } 

  pl = pl +
  
  scale_x_log10(labels = label_number(),limits = (c(10,fqupper)), guide = "axis_logticks") +  # Log scale for x-axis
  #scale_x_continuous(limits = c(10, fqupper)) +
  
  
  scale_color_manual(values = rev(colorRampPalette(c("darkblue", "lightblue"))(length(unique(summary$year))))) +
  scale_fill_manual(values =  rev(colorRampPalette(c("darkblue", "lightblue"))(length(unique(summary$year))))) +
  
  
  #median HMD values- each year
  geom_line(data = mallData[mallData$Quantile == "50%",],
            aes(x = Frequency, y = SoundLevel, color = Year, fill = Year, group = Year,
                text = paste0("Year: ", Year, "<br>Freq: ", trimws(format(Frequency, big.mark = ",")), " Hz<br>Sound Level: ", round(SoundLevel,1), " dB") ),
            linewidth = 1.25) +
  
  geom_ribbon(
    data = ribbonData %>% filter(Year == oldest_year),
    aes(x = Frequency, ymin = `25%`, ymax = `75%`,
        fill = Year, color = Year, group = interaction(Year, segment)),
    alpha = 0.3, show.legend = FALSE
  ) +
  geom_ribbon(
    data = ribbonData %>% filter(Year != oldest_year),
    aes(x = Frequency, ymin = `25%`, ymax = `75%`,
        fill = Year, color = Year, group = interaction(Year, segment)),
    alpha = 0.1, show.legend = FALSE
  ) + 
  
  
  #median HMD values- all data
  geom_line(data = mALL[mALL$Quantile == "50%",],
            aes(x = Frequency, y = SoundLevel, group = Quantile,
                text = paste0("Median across all years<br>Freq: ", trimws(format(Frequency, big.mark = ",")), " Hz<br>Sound Level: ", round(SoundLevel,1), " dB") ),
            color = "black", linewidth = 1,
            linetype = "dotted") 
  
  #for the oldest year, make the shading darker since it is hard to see at alpha = .1 for lightblue
  # geom_ribbon(data = ribbonData %>% 
  #               filter(Year == oldest_year & segment == segment1)%>%
  #               pivot_wider(names_from = Quantile, values_from = SoundLevel),
  #             #%>%
  #               #filter(!is.na(`25%`) & !is.na(`75%`)),
  #             aes(x = Frequency, ymin = `25%`, ymax = `75%`, fill = Year, color = Year), 
  #             alpha = 0.3, 
  #             show.legend = FALSE) + # High alpha for visibility
  # 
  # #for the geom_ribbons below, if data only has one year (ch01 and fk08), comment out the first geom ribbon and change alpha of second from .3 to .1
  # geom_ribbon(data = ribbonData %>%
  #               filter(Year != oldest_year & segment == segment1) %>%
  #               pivot_wider(names_from = Quantile, values_from = SoundLevel),
  #             # %>%
  #               #filter(!is.na(`25%`) & !is.na(`75%`)),
  #               aes(x = Frequency, ymin = `25%`, ymax = `75%`, fill = Year, color = Year),
  #               alpha = 0.1, 
  #             show.legend = FALSE) 
  
  #only sites with a data gap need the following ribbons
  # if (segment2 %in% ribbonData$segment){
  # 
  # #for the oldest year, make the shading darker since it is hard to see at alpha = .1 for lightblue
  # pl <- pl + geom_ribbon(data = ribbonData %>% 
  #               filter(Year == oldest_year & segment == segment2)%>%
  #               pivot_wider(names_from = Quantile, values_from = SoundLevel),
  #             #%>%
  #             #filter(!is.na(`25%`) & !is.na(`75%`)),
  #             aes(x = Frequency, ymin = `25%`, ymax = `75%`, fill = Year, color = Year), 
  #             alpha = 0.3, 
  #             show.legend = FALSE) + 
  # 
  # #for the geom_ribbons below, if data only has one year (ch01 and fk08), comment out the first geom ribbon and change alpha of second from .3 to .1
  # geom_ribbon(data = ribbonData %>%
  #               filter(Year != oldest_year & segment == segment2) %>%
  #               pivot_wider(names_from = Quantile, values_from = SoundLevel),
  #             # %>%
  #             #filter(!is.na(`25%`) & !is.na(`75%`)),
  #             aes(x = Frequency, ymin = `25%`, ymax = `75%`, fill = Year, color = Year),
  #             alpha = 0.1, 
  #             show.legend = FALSE) 
  # 
  # }
  
  pl <- pl +
    
  scale_y_continuous(limits = c(27, NA),          
                     breaks = seq(30, y_max, by = 10)) + 
  
  # Additional aesthetics
  theme_minimal() +
  labs(
    title = header_text1, 
    #caption  = caption_text1,
    color = legend_label,        #IF biological then change to Year*
    fill = legend_label,        #IF biological then change to Year*
    x = "Frequency (Hz)",
    y = "Sound Levels (dB re 1 &#956; Pa<sup>2</sup>/Hz)",
    subtitle = subtitle_text) +
  theme(legend.position = "right",
        plot.caption = ggtext::element_markdown(hjust = 0, size = 12),
        plot.title = ggtext::element_markdown(hjust = 0, size = 14),
        plot.subtitle = ggtext::element_markdown(hjust = 0, size = 12),
        axis.title.x = element_text(size = 12),           # X-axis label size
        axis.title.y = element_text(size = 12),           # Y-axis label size
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 9),
        axis.ticks.length.x = unit(0.25, "cm"), 
        axis.ticks.x = element_line(color = "grey", linewidth = 0.3), 
        axis.line.x = element_line(color = "grey", linewidth = 0.3)    
  ) 
  
  
  if (nrow(hover_vlines) > 0) {
    pl <- pl + geom_line(data = hover_vlines,
                         aes(x = x, y = y, group = line_id,
                             text = paste0(Label, "<br>Min Freq: ", round(minf, 1),
                                           " Hz<br>Max Freq: ", round(maxf, 1), " Hz")),
                         color = "transparent", linewidth = 3, inherit.aes = FALSE)
  }

pl



# trying to make logarithmic tick marks, doesnt work rn
# my_custom_x_positions <- c(
#   10, 20, 30, 40, 50, 60, 70, 80, 90, 
#   100, 200, 300, 400, 500, 600, 700, 800, 900, 
#   1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 
#   10000, 20000, 30000
# )
# 
# log_tick_positions <- log10(my_custom_x_positions)
# 
# # 2. Match every single position with a text label (32 items total)
# my_custom_x_labels <- c(
#   "10", "", "", "", "", "", "", "", "", 
#   "100", "", "", "", "", "", "", "", "", 
#   "1000", "", "", "", "", "", "", "", "", 
#   "10000", "", ""
# )

if (site == "cinms_b" | site == "as01"){
  label_height = 40
} else if (site %in% c("ci04", "pm01", "pm02") ){
  label_height = 37
} else if (site %in% c( "cox01", "cox03", "ns02", "ns08", "wea_hua", "as10") ){
  label_height = 38
}

# for label text in the middle of the shading box
foi_labels <- lapply(1:nrow(FOIsRange), function(i) {
  list(
    # Find the horizontal midpoint in log-space for the text to sit perfectly center
    x = (log10(FOIsRange$FQstart[i]) + log10(FOIsRange$FQend[i])) / 2,

    y = label_height,

    xref = "x",
    yref = "y",

    text = FOIsRange$Label[i], # Pulls your text label string dynamically

    textangle = -90,           # -90 reads cleanly from bottom-to-top (or use 90)

    showarrow = FALSE,
    xanchor = "center",        # Centers the text horizontal anchor point
    yanchor = "middle",        # Centers the text vertical anchor point

    font = list(
      size = 13,
      color = "black",         # Match your layout aesthetic
      family = "sans-serif"
    )
  )
})


# for label text on the left side of the shading box
# foi_labels <- lapply(1:nrow(FOIsRange), function(i) {
#   list(
#     x = log10(FOIsRange$FQstart[i]),
#     
#     y = label_height, 
#     
#     xref = "x",
#     yref = "y",
#     
#     text = FOIsRange$Label[i],
#     
#     textangle = -90,
#     
#     showarrow = FALSE,
#     xanchor = "left",          
#     yanchor = "middle",
#     
#     font = list(
#       size = 13, 
#       color = "black",
#       family = "sans-serif"
#     )
#   )
# })

# to add labels to black dashed FOI lines
# foi_vline_labels <- lapply(1:nrow(bind_rows(FOIs, FOIsL)), function(i) {
#   d <- bind_rows(FOIs, FOIsL)
#   list(
#     x = log10(d$FQstart[i]),
#     y = label_height,
#     xref = "x", yref = "y",
#     text = d$Label[i],
#     textangle = -90,
#     showarrow = FALSE,
#     xanchor = "center",
#     yanchor = "middle",
#     font = list(size = 13, color = "black", family = "sans-serif")
#   )
# })




#giving extra room at top of graph for a subtitle at certain NMS
if (substr(site3, 1, 2) == "hi" | substr(site3, 1, 2) == "pm"){
  t = 90
}else {
  t = 60
}




#make annual graph interactive
pl_interactive <- ggplotly(pl, tooltip = "text", height = 600, width = 600) %>%  
  
  #style(hoverinfo = "none", traces = c(1, 2, 3))  %>%
  
  layout(

    title = list(
      text = paste0( header_text1, "<br>", "<span style='font-size:11.5px'>", subtitle_text, "</span>"),
      x = 0,          
      xanchor = "left",
      font = list(size = 16)
    ),
    
    #trying log x axis
    # xaxis = list(
    #   type      = "linear",
    #   autorange = FALSE,
    #   range     = c(log10(10), log10(fqupper)),
    #   tickvals  = log_tick_positions,
    #   ticktext  = my_custom_x_labels,
    #   ticks     = "outside",
    #   ticklen   = 6,
    #   tickcolor = "grey",
    #   showgrid  = FALSE
    # ),
    
    autosize = TRUE,
    
    yaxis = list(
      autorange = FALSE,       
      range     = c(27, y_max),   
      ticks     = "outside",
      tickvals = c(30, 40, 50, 60, 70, 80, 90, 100, 110)
    ),
    
    legend = list(
      orientation    = "v",       
      x              = 1.02,      
      y              = 0.5,        
      # font.weight = 
       # xanchor        = "left",     
      yanchor        = "middle"   
      # entrywidth     = 100,       
      # entrywidthmode = "pixels"   
    ),

    # shapes = foi_shapes,
    annotations = foi_labels,
    
    #make hover closer to lines
    hovermode = "closest",
    hoverdistance = 4,
  
    
    # Increase the bottom margin (b) to ensure there is room for the caption text
    margin = list(b = 50, l = 50, r = 50, t = t)
    
  )%>%
  plotly::config(modeBarButtonsToRemove = list('toImage', 'select2d', 'lasso2d'))

# Display the interactive plot
pl_interactive



as.character(month_nums)

month_nums <- as.character(sort(unique(summary$month)))
month_nums2 <- as.numeric(as.character(sort(unique(summary$month))))

summary$month <- factor(summary$month,
                        levels = month_nums,
                        labels = month.abb[month_nums2])

#effort graph interactive

# summaryt <- gpsAG %>%
#   mutate(
#     year  = year(UTC),  # Extract Year
#     month = format(UTC, "%m")  
#   ) %>%
#   count(year, month) 
# summaryt$dy = round(summaryt$n/ 24)
# 
# 


p1 = ggplot(summary, aes(x = month, y = dy, fill = as.factor(year),
                         # custom hover layout:
                         text = paste0("Year: ", year, "<br>Days: ", dy))) +
  geom_col(position = "dodge", width = .4) + 
  labs(
    title = paste0("<b>", effort_title, "</b><br>",
                   "<span style='font-size: 13px; font-weight: normal; color: black;'>",
                   toupper(site), " has ", udaysAG, " unique days: ", 
                   as.character(stAG), " to ", as.character(edAG), "</span>"),
    x = "",
    y = "Days",
    fill = legend_label,
    #caption = "Data from months with effort below the red horizontal line are excluded from annual sound levels figure above"
  ) +
  scale_x_discrete(drop = FALSE) + 
  scale_fill_manual(values = rev(colorRampPalette(c("darkblue", "lightblue"))(length(unique(summary$year))))) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 13, face = "bold", hjust = 0),
    axis.title.y = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    axis.text.x = element_text(size = 11, hjust = 1, angle = 30),  
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 9),
    legend.position = "right" 
  ) +
  # geom_line(data = data.frame(
  #   x = c(0.5, length(month_nums) + 0.5), 
  #   y = rep(siteInfo$MThreshold, length(month_nums))),
  #   aes(x = x, y = y, 
  #       text = paste0("Min # of Days Threshold = ", siteInfo$MThreshold, " Days")), 
  #   color = "red", 
  #   linetype = "dashed", 
  #   linewidth = 0.5,
  #   inherit.aes = FALSE)
  geom_line(data = data.frame(
    x = seq(0.5, length(month_nums) + 0.5, length.out = 100),
    y = siteInfo$MThreshold),
    aes(x = x, y = y, 
        text = paste0("Min # of Days Threshold = ", siteInfo$MThreshold, " Days")), 
    color = "red", 
    linetype = "dashed", 
    linewidth = 0.5,
    inherit.aes = FALSE)


p1


#adjusting heigh of graph based on number of years displayed
if (length(years_to_keep) > 3){
    height_int = 240
  
} else if (length(years_to_keep) < 3){
    height_int = 200
  
}else if (length(years_to_keep) == 3){
  height_int = 215
  
} 
# else if (length(years_to_keep) < 4){
#   height_int = 280
#   
# }


p1_interactive <- ggplotly(p1, tooltip = c("text", "group"), height = 215, width = 600) %>% 
  layout(
    autosize = TRUE,
    
    margin = list(t = 50,          # Tucks the title tightly above the bars
                  b = 40,          # Leaves just enough room for the angled month text (Jan, Feb...)
                  l = 50,          # Aligns perfectly with the top plot's left axis
                  r = 50), # Ensure room for your caption at the bottom
    
    legend = list(
      x = 1.02,
      xanchor = "left",
      y = 1.25,          # push the top of the legend up into the title area
      yanchor = "top",
      font = list(size = 10),
      tracegroupgap = 0  # removes extra spacing between entries
    ),
    
    annotations = list(
      x = 0, y = -0.4, 
      text = "Data from months with effort below the red horizontal line are excluded from annual sound levels figure above", 
      showarrow = FALSE, 
      xref = 'paper', yref = 'paper', 
      xanchor = 'left', yanchor = 'top',
      font = list(size = 9.5)
    )
  )%>%
  plotly::config(modeBarButtonsToRemove = list('toImage', 'select2d', 'lasso2d'))

# View the final result
p1_interactive





#combine effort and line graphs


combined_layout <- browsable(
  div(
    style = "position: relative; display: flex; flex-direction: column; gap: 10px; 
             font-family: sans-serif; padding: 10px 40px 10px 10px; width: 640px;
             border: 1px solid #d3d3d3; border-radius: 4px; box-sizing: border-box;",
    
    # top plot
    div(style = "height: 600px; width: 600px;", pl_interactive),
    
    # caption
    p(HTML(caption_text2), style = "font-size: 11px; color: black; margin: 0; padding-left: 5px; line-height: 1.4;"),
    
    # divider
    tags$hr(style = "width: 600px; border: none; border-top: 1.5px solid black; margin: 5px 0;"),
    
    # bottom plot
    div(style = "height: 215px; width: 600px;", p1_interactive),
    
    # watermark
    div(
      "© 2026 soundscapemonitoring.us",
      style = "position: absolute; top: 50%; right: 9px; transform: translateY(-50%);
               padding: 6px 4px;
               font-size: 11px; color: rgba(0,0,0,0.4); 
               writing-mode: vertical-rl; 
               text-orientation: sideways;
               pointer-events: none;"
    )
  )
)

combined_layout


#save html file to Dev contents folder
outDir = "X:/Emma_Beretta/SoundscapesWebsiteDev/" #for GCP workstation remote desktop Emma
#outDir   =  "C:/Users/embe5980/SoundscapesWebsiteDev/" #local


outDirG  =  paste0(outDir,"content/resources/") #where save graphics
outDirGe =  paste0(outDir,"content/resources/extra") #where extra save graphics
outDirC  =  paste0(outDir,"context/") #where to get context


# Save the entire HTML layout bundle natively
htmltools::save_html(combined_layout, paste0(outDirG, "/plot_", toupper(site), "_interactiveAnnualSPL.html"))







#SEASONAL GRAPH





# Compute gaps per-year on the pivoted data (so both 25% and 75% missing-ness count)
ribbonDataS <- mallDataS %>%
  filter(Quantile %in% c("25%", "75%")) %>%
  pivot_wider(names_from = Quantile, values_from = SoundLevel) %>%
  group_by(Season) %>%
  arrange(Frequency, .by_group = TRUE) %>%
  mutate(
    is_na   = is.na(`25%`) | is.na(`75%`),
    gap     = is_na != lag(is_na, default = first(is_na)),
    segment = cumsum(gap)
  ) %>%
  ungroup() %>%
  filter(!is_na)          # drop the NA runs entirely, keep only real data segments



#adjusting ribbons to deal with gaps in data, plotly couldnt automatically handle them
# ribbonDataS <- mallDataS %>% mutate(is_na = is.na(`SoundLevel`) ,
#                                   gap = is_na != lag(is_na, default = first(is_na)),
#                                   # unique segment ID every time a change happens from data to no data or back
#                                   segment = cumsum(gap)) %>%
#                                   ungroup()


# original code for seasonal graph
# p = ggplot() +
#   
#   # Wind model values
#   geom_line(data = mwindInfo[as.character(mwindInfo$windSpeed) == windUpp,], aes(x = variable, y = value), color = "black",  linewidth = 1) +
#   geom_line(data = mwindInfo[as.character(mwindInfo$windSpeed) == windLow,], aes(x = variable, y = value), color = "black",  linewidth = 1) +
#   scale_x_log10(labels = label_number(),limits = (c(10,fqupper)), guide = "axis_logticks") +  # Log scale for x-axis
#   
#   # Add vertical lines at FOIs, label on right side
#   geom_vline(data = FOIs, aes(xintercept = FQstart, color = Label), linetype = "dashed", color = "black",linewidth = .5) +
#   geom_text(data = FOIs, aes(x = FQstart, y = label_height, label = Label), angle = 90, vjust = 1, hjust = 0.45, size = 4) +
#   
#   # Add vertical lines at FOIs, label on left side, not common
#   geom_vline(data = FOIsL, aes(xintercept = FQstart, color = Label), linetype = "dashed", color = "black",linewidth = .5) +
#   geom_text(data = FOIsL, aes(x = FQstart, y = label_height, label = Label), angle = 90, vjust = 0, hjust = 0.5, size = 4) +
#   
#   # Add vertical set dash lines and grey shaded region at FOI ranges
#   #  geom_vline(data = FOIsRange, aes(xintercept = FQstart, color = Label), linetype = "dashed", color = "grey50",linewidth = .5) +
#   #  geom_vline(data = FOIsRange, aes(xintercept = FQend, color = Label), linetype = "dashed", color = "grey50",linewidth = .5) +
#   geom_rect(data = FOIsRange, aes(xmin = FQstart, xmax = FQend, ymin = -Inf, ymax = Inf), 
#             fill = "gray", alpha = 0.2)+  # Adjust alpha for transparency
#   geom_text(data = FOIsRange, aes(x = FQstart, y = label_height, label = Label), color = "black", angle = 90, vjust = 1, hjust = 0.45, size = 4) +
#   
#   # Add vertical set dash lines and grey shaded region at FOI ranges, label on left
#   # geom_vline(data = FOIsRangeL, aes(xintercept = FQstart, color = Label), linetype = "dashed", color = "grey50",linewidth = .5) +
#   #  geom_vline(data = FOIsRangeL, aes(xintercept = FQend, color = Label), linetype = "dashed", color = "grey50",linewidth = .5) +
#   geom_rect(data = FOIsRangeL, aes(xmin = FQstart, xmax = FQend, ymin = -Inf, ymax = Inf), 
#             fill = "gray", alpha = 0.2)+  # Adjust alpha for transparency
#   geom_text(data = FOIsRangeL, aes(x = FQstart, y = label_height, label = Label), color = "black", angle = 90, vjust = 0, hjust = 0.5, size = 4) +
#   
#   #shading (25-75%) HMD values
#   geom_ribbon(data = mallDataS %>%
#                 pivot_wider(names_from = Quantile, values_from = SoundLevel),
#               aes(x = Frequency, ymin = `25%`, ymax = `75%`, fill = Season),
#               alpha = 0.2) +  # Use alpha for transparency
#   
#   # Median (50%) HMD values
#   geom_line(data = mallDataS[mallDataS$Quantile == "50%",], 
#             aes(x = Frequency, y = SoundLevel, color = Season), linewidth = 2) +
#   
#   geom_line(data = mALL[mALL$Quantile == "50%",], aes(x = Frequency, y = SoundLevel), color = "black", linewidth = 1,
#             linetype = "dotted")+ 
#   
#   # Set color and fill to match season
#   scale_color_manual(values  = seasont$values ) +
#   scale_fill_manual (values  = seasont$values ) +
#   
#   labs(
#     subtitle = seasonLabel,
#     caption  = caption_text,
#     x = "Frequency (Hz)",
#     y = expression(paste("Sound Levels (dB re 1 ", mu, " Pa"^2, "/Hz)" ) ) #dB re 1 uPa^2/Hz
#   ) +
#   # Additional aesthetics
#   scale_y_continuous(limits = c(30, NA)) +  # use to manually scale y minimum so vert line labels are visible
#   theme_minimal()+
#   theme(legend.position = "right",
#         plot.caption = ggtext::element_markdown(hjust = 0, size = 12),
#         axis.title.x = element_text(size = 14),           
#         axis.title.y = element_text(size = 14), 
#         legend.text = element_text(size = 12),
#         axis.text = element_text(size = 14),
#         axis.ticks.length.x = unit(0.25, "cm"), 
#         axis.ticks.x = element_line(color = "grey", linewidth = 0.3)
#         , axis.line.x = element_line(color = "grey", linewidth = 0.3)           
#   )
# 
# p
# 


# v2 graph with lines above shading but non matching legend to effort legend

# p = ggplot() +
#   #wind model
#   geom_line(data = mwindInfo[as.character(mwindInfo$windSpeed) == windUpp,], 
#             aes(x = variable, y = value, 
#                 text = "Modeled Max Wind Noise"), color = "black", linewidth = 1) +
#   
#   geom_line(data = mwindInfo[as.character(mwindInfo$windSpeed) == windLow,], 
#             aes(x = variable, y = value, 
#                 text = "Modeled Min Wind Noise"), color = "black", linewidth = 1) +
#   
#   geom_polygon(data = polygon_data,
#                aes(x = x, y = y, group = id,  
#                    text = paste0("Min Freq: ", round(FQstart, 1) , " Hz<br>Max Freq: ", round(FQend, 1), " Hz")), # 
#                fill = "gray",
#                alpha = 0.2,
#                inherit.aes = FALSE) +
#   
#   # geom_line(data = polygon_data,
#   #           aes(x = x, 
#   #               y = label_height,    # <-- Hard-coded to your precise label text elevation
#   #               group = id,
#   #               text = paste0("Start FQ: ", round(FQstart, 2) , " Hz<br>End FQ: ", round(FQend, 2), " Hz")),   # <-- Plotly maps the tooltip directly here!
#   #           color = "transparent",   # <-- Makes the tracking line invisible
#   #           linewidth = 5,           # <-- Generates a wide invisible target area for the mouse
#   #           inherit.aes = FALSE) +
#   # 
#   scale_x_log10(labels = label_number(),limits = (c(10,fqupper)), guide = "axis_logticks") +  # Log scale for x-axis
#   #scale_x_continuous(limits = c(10, fqupper)) +
#   
#   
#   
#   # geom_ribbon(data = mallDataS %>%
#   #               pivot_wider(names_from = Quantile, values_from = SoundLevel),
#   #             aes(x = Frequency, ymin = `25%`, ymax = `75%`, fill = Season),
#   #             alpha = 0.2) + 
#   
#   #for the oldest year, make the shading darker since it is hard to see at alpha = .1 for lightblue
#   geom_ribbon(data = ribbonDataS %>% 
#                 filter(segment == segment1)%>%
#                 pivot_wider(names_from = Quantile, values_from = SoundLevel),
#               #%>%
#               #filter(!is.na(`25%`) & !is.na(`75%`)),
#               aes(x = Frequency, ymin = `25%`, ymax = `75%`, fill = Season), 
#               alpha = 0.3, 
#               show.legend = TRUE) 
# 
# #only sites with a data gap need the following ribbons
# if (segment2 %in% ribbonData$segment){
#   
#   #for the oldest year, make the shading darker since it is hard to see at alpha = .1 for lightblue
#   p <- p + geom_ribbon(data = ribbonDataS %>% 
#                            filter( segment == segment2)%>%
#                            pivot_wider(names_from = Quantile, values_from = SoundLevel),
#                          #%>%
#                          #filter(!is.na(`25%`) & !is.na(`75%`)),
#                          aes(x = Frequency, ymin = `25%`, ymax = `75%`, fill = Season), 
#                          alpha = 0.3, 
#                          show.legend = FALSE) 
#   
# }
# 
# p <- p +
#   
#   #median HMD values- each season
#   geom_line(data = mallDataS[mallDataS$Quantile == "50%",], 
#             aes(x = Frequency, y = SoundLevel, color = Season, group = Season,
#                 text = paste0("Season: ", Season, "<br>Freq: ", trimws(format(Frequency, big.mark = ","))," Hz<br>Sound Level: ", round(SoundLevel,1), " dB")), 
#             linewidth = 2,
#             key_glyph = draw_key_rect) +
#   
#   
#   #median HMD all seasons
#   geom_line(data = mALL[mALL$Quantile == "50%",], aes(x = Frequency, y = SoundLevel, group = Quantile,
#                                                       text = paste0("Median across all years<br>Freq: ", trimws(format(Frequency, big.mark = ",")), " Hz<br>Sound Level: ", round(SoundLevel,1), " dB")), color = "black", linewidth = 1,
#             linetype = "dotted")+ 
#   
#   
#   # Set color and fill to match season
#   scale_color_manual(name = legend_label2, values = seasont$values) +
#   scale_fill_manual(name = legend_label2, values = seasont$values) +
#   
#   scale_y_continuous(limits = c(27, NA),          
#                      breaks = seq(30, y_max, by = 10)) + 
#   
#   # Additional aesthetics
#   theme_minimal() +
#   labs(
#     #subtitle = seasonLabel,
#     title = header_text1,
#     #caption  = caption_text,
#     x = "Frequency (Hz)",
#     y = "Sound Levels (dB re 1 &#956; Pa<sup>2</sup>/Hz)" #dB re 1 uPa^2/Hz
#   )  +
#   theme(legend.position = "right",
#         plot.caption = ggtext::element_markdown(hjust = 0, size = 12),
#         plot.title = ggtext::element_markdown(hjust = 0, size = 14),
#         axis.title.x = element_text(size = 14),           # X-axis label size
#         axis.title.y = element_text(size = 14),           # Y-axis label size
#         axis.text = element_text(size = 14),
#         legend.text = element_text(size = 12),
#         axis.ticks.length.x = unit(0.25, "cm"), 
#         axis.ticks.x = element_line(color = "grey", linewidth = 0.3), 
#         axis.line.x = element_line(color = "grey", linewidth = 0.3)    
#   ) 
# 
# 
# p
# 
# 
# 
# 
# p_interactive <- ggplotly(p, tooltip = "text", height = 800, width = 800) %>%  
#   
#   #style(hoverinfo = "none", traces = c(1, 2, 3))  %>%
#   
#   layout(
#     
#     
#     autosize = TRUE,
#     
#     yaxis = list(
#       autorange = FALSE,       # Prevents Plotly from adding its own padding
#       range     = c(27, y_max),   # Hard-locks the frame exactly to your polygon edges
#       ticks     = "outside",
#       tickvals = c(30, 40, 50, 60, 70, 80, 90, 100)
#     ),
#     
#     legend = list(
#       orientation    = "v",        # Keeps items stacked as a vertical column
#       x              = 1.02,       # Leaves it just past the right axis line
#       y              = 0.5,        # <--- Position coordinate set precisely at 50% height
#       # font.weight = 
#       # xanchor        = "left",     
#       yanchor        = "middle"   # <--- Locks the center of the legend block to that 50% mark
#       # entrywidth     = 100,       
#       # entrywidthmode = "pixels"   
#     ),
#     
#     # shapes = foi_shapes,
#     annotations = foi_labels,
#     
#     # Increase the bottom margin (b) to ensure there is room for the caption text
#     margin = list(b = 50, l = 50, r = 50, t = 50)
#     
#     
#   )%>%
#   
#   # CLEAN UP THE PLOTLY LEGEND TEXT LABELS
#   style(
#     # This searches the layout strings for "(SeasonName,1)" patterns and strips them to just "SeasonName"
#     style = list(), 
#     # Use a loop over all generated traces to clean up the names dynamically
#     traces = seq_along(.$x$data)
#   )
# 
# # Explicitly map cleanly across the list of labels using a map function
# for (i in seq_along(p_interactive$x$data)) {
#   # Clean up formatting like "(Winter,1)" to "Winter"
#   if (!is.null(p_interactive$x$data[[i]]$name)) {
#     p_interactive$x$data[[i]]$name <- gsub("\\(([^,]+),[^)]+\\)", "\\1", p_interactive$x$data[[i]]$name)
#   }
# }
# 
# # Display the interactive plot
# p_interactive
# 
# 
# 
# 



#shading on top of lines but matching legend! version that we will display


pv2 = ggplot() +
  #wind model
  geom_line(data = mwindInfo[as.character(mwindInfo$windSpeed) == windUpp,], 
            aes(x = variable, y = value, 
                text = "Modeled Max Wind Noise"), color = "black", linewidth = .5) +
  
  geom_line(data = mwindInfo[as.character(mwindInfo$windSpeed) == windLow,], 
            aes(x = variable, y = value, 
                text = "Modeled Min Wind Noise"), color = "black", linewidth = .5) +
  
  geom_polygon(data = polygon_data,
               aes(x = x, y = y, group = id,  
                   text = paste0(Label, "<br>Min Freq: ", round(FQstart, 1) , " Hz<br>Max Freq: ", round(FQend, 1), " Hz")), # 
               fill = "gray",
               alpha = 0.2,
               inherit.aes = FALSE) 

  if (nrow(vline_data) > 0 ){
  pv2 = pv2 + geom_line(data = vline_data,
                      aes(x = x, y = y, group = id,
                          text = paste0(Label, "<br>Freq: ", round(FQstart, 1), " Hz")),
                      color = "black", linetype = "dashed", linewidth = 0.5,
                      inherit.aes = FALSE) 
    } 

pv2 = pv2 +
  
  
  scale_x_log10(labels = label_number(),limits = (c(10,fqupper)), guide = "axis_logticks") +  # Log scale for x-axis
  
  
  #median HMD values- each season
  geom_line(data = mallDataS[mallDataS$Quantile == "50%",], 
            aes(x = Frequency, y = SoundLevel, color = Season, group = Season, fill = Season,
                text = paste0("Season: ", Season, "<br>Freq: ", trimws(format(Frequency, big.mark = ",")), " Hz<br>Sound Level: ", round(SoundLevel,1), " dB")), 
            linewidth = 1.25,
            key_glyph = draw_key_rect) +
  
  
  #scale_x_continuous(limits = c(10, fqupper)) +
  
  # geom_ribbon(data = mallDataS %>%
  #               pivot_wider(names_from = Quantile, values_from = SoundLevel),
  #             aes(x = Frequency, ymin = `25%`, ymax = `75%`, fill = Season),
  #             alpha = 0.2) + 
  
  
  
  # ribbons for each season, broken into contiguous non-NA segments automatically
  geom_ribbon(data = ribbonDataS,
              aes(x = Frequency, ymin = `25%`, ymax = `75%`,
                  fill = Season, color = Season, group = interaction(Season, segment)),
              alpha = 0.3,
              show.legend = TRUE)+


  #median HMD all seasons
  geom_line(data = mALL[mALL$Quantile == "50%",], aes(x = Frequency, y = SoundLevel, group = Quantile,
                                                    text = paste0("Median across all years<br>Freq: ", trimws(format(Frequency, big.mark = ",")), " Hz<br>Sound Level: ", round(SoundLevel,1), " dB")), 
            color = "black", linewidth = 1,
          linetype = "dotted")
  
  #for the oldest year, make the shading darker since it is hard to see at alpha = .1 for lightblue
  # geom_ribbon(data = ribbonDataS %>% 
  #               filter(segment == segment1)%>%
  #               pivot_wider(names_from = Quantile, values_from = SoundLevel),
  #             #%>%
  #             #filter(!is.na(`25%`) & !is.na(`75%`)),
  #             aes(x = Frequency, ymin = `25%`, ymax = `75%`, fill = Season, color = Season), 
  #             alpha = 0.3, 
  #             show.legend = TRUE) 

#only sites with a data gap need the following ribbons
# if (segment2 %in% ribbonData$segment){
#   
#   #for the oldest year, make the shading darker since it is hard to see at alpha = .1 for lightblue
#   pv2 <- pv2 + geom_ribbon(data = ribbonDataS %>% 
#                          filter( segment == segment2)%>%
#                          pivot_wider(names_from = Quantile, values_from = SoundLevel),
#                        #%>%
#                        #filter(!is.na(`25%`) & !is.na(`75%`)),
#                        aes(x = Frequency, ymin = `25%`, ymax = `75%`, fill = Season, color = Season), 
#                        alpha = 0.3, 
#                        show.legend = FALSE) 
#   
# }

pv2 <- pv2 +
  
  scale_color_manual(name = legend_label2, values = seasont$values) +
  scale_fill_manual(name = legend_label2, values = seasont$values) +
  
  scale_y_continuous(limits = c(27, NA),          
                     breaks = seq(30, y_max, by = 10)) + 
  
  # Additional aesthetics
  theme_minimal() +
  labs(
    title = header_text1,
    #caption  = caption_text,
    x = "Frequency (Hz)",
    y = "Sound Levels (dB re 1 &#956; Pa<sup>2</sup>/Hz)" #dB re 1 uPa^2/Hz
  )  +
  theme(legend.position = "right",
        plot.caption = ggtext::element_markdown(hjust = 0, size = 12),
        plot.title = ggtext::element_markdown(hjust = 0, size = 12),
        plot.subtitle = ggtext::element_markdown(hjust = 0, size = 12),
        axis.title.x = element_text(size = 12),           # X-axis label size
        axis.title.y = element_text(size = 12),           # Y-axis label size
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 9),
        axis.ticks.length.x = unit(0.25, "cm"), 
        axis.ticks.x = element_line(color = "grey", linewidth = 0.3), 
        axis.ticks.y = element_line(color = "grey", linewidth = 0.3), 
        axis.line.x = element_line(color = "grey", linewidth = 0.3)    
  ) 


if (nrow(hover_vlines) > 0) {
  pv2 <- pv2 + geom_line(data = hover_vlines,
                       aes(x = x, y = y, group = line_id,
                           text = paste0(Label, "<br>Min Freq: ", round(minf, 1),
                                         " Hz<br>Max Freq: ", round(maxf, 1), " Hz")),
                       color = "transparent", linewidth = 3, inherit.aes = FALSE)
}

pv2



#making SPL graph interactive

pv2_interactive <- ggplotly(pv2, tooltip = "text", height = 600, width = 600) %>%  
  
  layout(
    
    
    autosize = TRUE,
    
    yaxis = list(
      autorange = FALSE,      
      range     = c(27, y_max),  
      ticks     = "outside",
      tickvals = c(30, 40, 50, 60, 70, 80, 90, 100)
    ),
    
    legend = list(
      orientation    = "v",       
      x              = 1.02,      
      y              = 0.5,        
      # font.weight = 
      # xanchor        = "left",     
      yanchor        = "middle"  
      # entrywidth     = 100,       
      # entrywidthmode = "pixels"   
    ),
    
    # shapes = foi_shapes,
    annotations = foi_labels,
    
    hovermode = "closest",
    hoverdistance = 4,   
    
    # Increase the bottom margin (b) to ensure there is room for the caption text
    margin = list(b = 50, l = 50, r = 50, t = 50)
    
    
  ) %>% config(modeBarButtonsToRemove = list('toImage', 'select2d', 'lasso2d'))

# Display the interactive plot
pv2_interactive




#making effort graph interactive
 
# summary <- gpsAG %>%
#   mutate(
#     year  = year(UTC),  # Extract Year
#     month = format(UTC, "%m")  # Extract Month (numeric format)
#   ) %>%
#   count(year, month)  # Count occurrences (hours) in each year-month
# summary$dy = round(summary$n/ 24)



p2 = ggplot(summary2, aes(x = as.character(year), y = dy, fill = as.factor(Season),
                          # Define your custom hover layout here:
                          text = paste0("Season: ", Season, "<br>Days: ", dy))) +
  geom_col(position = "dodge", width = .3) +  # Use dodge to separate bars for each year within the same month
  #coord_flip()+ 
  labs(
    title = paste0("<b>monitoring effort by season (all data)</b><br>",
                   "<span style='font-size: 13px; font-weight: normal; color: black;'>",
                   toupper(site), " has ", udays, 
                   " unique days: ", as.character(st), " to ", as.character(ed), "</span>"),
    x = "",      
    y = "Days",      
    fill = legend_label2) +
  scale_fill_manual(values = seasont$values) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 13, face = "bold", hjust = 0),
    axis.title.y = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    axis.text.x = element_text(size = 11, hjust = 1, angle = 30), 
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 9),
    legend.position = "right" 
  )

p2




#adjusting heigh of graph based on number of years displayed
if (length(seas) > 3){
  height_int2 = 260
  
} else if (length(seas) < 3){
  height_int2 = 200
  
}else if (length(seas) == 3){
  height_int2 = 215
  
}


# 
# # change height = based on how many seasons are in this sites dataset
# p2_interactive <- ggplotly(p2, tooltip = c("text", "group"), height = height_int2, width = 600) %>% 
#   layout(
#     autosize = TRUE,
#     
#     margin = list(t = 50,          # Tucks the title tightly above the bars
#                   b = 20,          # Leaves just enough room for the angled month text (Jan, Feb...)
#                   l = 50,          # Aligns perfectly with the top plot's left axis
#                   r = 40) # Ensure room for your caption at the bottom
#     
#     # legend = list(
#     #   font = list(size = 13) # to make legend slightly shorter, less spacing between years didnt work
#     #   #, groupclick = "toggleitem"
#     #   ),
#     
#   )%>% config(modeBarButtonsToRemove = list('toImage', 'select2d', 'lasso2d'))
# 
# p2_interactive


p2_interactive <- ggplotly(p2, tooltip = c("text", "group"), height = 215, width = 600) %>% 
  layout(
    autosize = TRUE,
    margin = list(t = 50, b = 20, l = 50, r = 40),
    legend = list(
      x = 1.02,
      xanchor = "left",
      y = 1.25,          # push the top of the legend up into the title area
      yanchor = "top",
      font = list(size = 9),
      tracegroupgap = 0  # removes extra spacing between entries
    )
  ) %>% 
  config(modeBarButtonsToRemove = list('toImage', 'select2d', 'lasso2d'))
p2_interactive


#add watermark and grey border
combined_layout2 <- browsable(
  div(
    style = "position: relative; display: flex; flex-direction: column; gap: 10px; 
             font-family: sans-serif; padding: 10px 20px 10px 10px; width: 640px;
             border: 1px solid #d3d3d3; border-radius: 4px; box-sizing: border-box;",
    
    # top plot
    div(style = "height: 600px; width: 600px;", pv2_interactive),
    
    # caption
    p(HTML(caption_text2), style = "font-size: 11px; color: black; margin: 0; padding-left: 5px; line-height: 1.4;"),
    
    # divider
    tags$hr(style = "width: 600px; border: none; border-top: 1.5px solid black; margin: 5px 0;"),
    
    # bottom plot
    div(style = "height: 215px; width: 600px;", p2_interactive),
    
    # watermark
    div(
      "© 2026 soundscapemonitoring.us",
      style = "position: absolute; top: 50%; right: 9px; transform: translateY(-50%);
               padding: 6px 4px;
               font-size: 11px; color: rgba(0,0,0,0.4); 
               writing-mode: vertical-rl; 
               text-orientation: sideways;
               pointer-events: none;"
    )
  )
)

combined_layout2


#save html file to Dev contents folder
outDir = "X:/Emma_Beretta/SoundscapesWebsiteDev/" #for GCP workstation remote desktop Emma
#outDir   =  "C:/Users/embe5980/SoundscapesWebsiteDev/" #local


outDirG  =  paste0(outDir,"content/resources/") #where save graphics
outDirGe =  paste0(outDir,"content/resources/extra") #where extra save graphics
outDirC  =  paste0(outDir,"context/") #where to get context


# Save the entire HTML layout bundle natively
htmltools::save_html(combined_layout2, paste0(outDirG, "/plot_", toupper(site), "_interactiveSeasonalSPL.html"))



