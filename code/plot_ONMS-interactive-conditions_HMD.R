#use this script after running plot_ONMS-conditions_HMD.R
library(plotly)

# 1. Build the plot as you did, but add a 'text' aesthetic for the hover tooltip
pInt <- ggplot() +
  # Wind models (keep as is)
  geom_line(data = mwindInfo[as.character(mwindInfo$windSpeed) == windUpp,], aes(x = variable, y = value), color = "black", linewidth = 1) +
  geom_line(data = mwindInfo[as.character(mwindInfo$windSpeed) == windLow,], aes(x = variable, y = value), color = "black", linewidth = 1) +
  
  # Frequency Ranges (Shaded Regions)
  geom_rect(data = FOIs, 
            aes(xmin = FQstart, xmax = FQend), 
            ymin = 30, ymax = 100, # Use actual numbers instead of Inf for plotly stability
            fill = "gray", alpha = 0.2,
            inherit.aes = FALSE) + # This prevents it from looking for 'Year' or 'text' in FOIsgeom_rect(data = FOIsRange, aes(xmin = FQstart, xmax = FQend, ymin = -Inf, ymax = Inf), fill = "gray", alpha = 0.2) +
  
  # Ribbons (25%-75% quantiles)
  geom_ribbon(data = mallData %>% filter(Year != oldest_year) %>% pivot_wider(names_from = Quantile, values_from = SoundLevel),
              aes(x = Frequency, ymin = `25%`, ymax = `75%`, fill = Year), alpha = 0.1) +
  geom_ribbon(data = mallData %>% filter(Year == oldest_year) %>% pivot_wider(names_from = Quantile, values_from = SoundLevel),
              aes(x = Frequency, ymin = `25%`, ymax = `75%`, fill = Year), alpha = 0.3) +
  
  # MAIN DATA LINE (Adding the 'text' aesthetic here for the tooltip)
  geom_line(data = mallData[mallData$Quantile == "50%",], 
            aes(x = Frequency, y = SoundLevel, color = Year, 
                text = paste("Year:", Year, "<br>Freq:", Frequency, "Hz<br>Level:", round(SoundLevel, 1), "dB")), 
            linewidth = 1.5) +
  
  # Median of all data
  geom_line(data = mALL[mALL$Quantile == "50%",], aes(x = Frequency, y = SoundLevel), 
            color = "black", linewidth = 0.8, linetype = "dotted") +
  
  # Scales & Themes
  scale_x_log10(labels = scales::label_number(), limits = c(10, fqupper)) +
  scale_y_continuous(limits = c(30, NA)) +
  scale_color_manual(values = rev(colorRampPalette(c("darkblue", "lightblue"))(length(unique(mallData$Year))))) +
  scale_fill_manual(values = rev(colorRampPalette(c("darkblue", "lightblue"))(length(unique(mallData$Year))))) +
  theme_minimal() +
  labs(x = "Frequency (Hz)", y = "dB re 1 uPa^2/Hz", color = legend_label, fill = legend_label)

# 2. Convert to plotly
# 'tooltip = "text"' tells plotly to ONLY show what we defined in the paste() function above
p_interactive <- ggplotly(pInt, tooltip = "text") %>% 
  layout(legend = list(orientation = "v", x = 1, y = 0.5))

# Display
p_interactive

















#INTERACTIVE PLOT
#changes to make to ggplot above
#still trying to figure out FOI vert lines and labels in plotly
#move geom_ribbon to after geom_lines
#make geom_rect y bounds: ymin = 35, ymax = 90
#add fill = Year to geom_line 
#add color = Year to geom_ribbon
#add "name = "Year", " to  scale_fill_manual and scale_color_manual
#then run code below

mallData_recent <- mallData %>%
  filter(Year != oldest_year) %>%
  pivot_wider(names_from = Quantile, values_from = SoundLevel)

mallData_oldest <- mallData %>%
  filter(Year == oldest_year) %>%
  pivot_wider(names_from = Quantile, values_from = SoundLevel)

pInt  = ggplot() +
  #wind model
  geom_line(data = mwindInfo[as.character(mwindInfo$windSpeed) == windUpp,], aes(x = variable, y = value), color = "black", linewidth = 1) +
  geom_line(data = mwindInfo[as.character(mwindInfo$windSpeed) == windLow,], aes(x = variable, y = value), color = "black", linewidth = 1) +
  scale_x_log10(labels = label_number(),limits = (c(10,fqupper)), guide = "axis_logticks") +  # Log scale for x-axis
  
  scale_color_manual(name = "Year", values = rev(colorRampPalette(c("darkblue", "lightblue"))(length(unique(summary$year))))) +
  scale_fill_manual(name = "Year", values =  rev(colorRampPalette(c("darkblue", "lightblue"))(length(unique(summary$year))))) +
  # 
  # # Add vertical lines at FOIs, label on right side
  # geom_vline(data = FOIs, aes(xintercept = FQstart, color = Label), linetype = "dashed", color = "black",linewidth = .5, inherit.aes = FALSE) +
  # geom_text(data = FOIs, aes(x = FQstart, y = label_height, label = Label), angle = 90, vjust = 1, hjust = 0.45, size = 4, inherit.aes = FALSE) +
  # geom_rect(data = FOIs, aes(xmin = FQstart, xmax = FQend, ymin = 30, ymax = 90),
  #           fill = "gray", alpha = 0.2, inherit.aes = FALSE) +  # Adjust alpha for transparency
  # 
  # # Add vertical lines at FOIs, label on left side
  # geom_vline(data = FOIsL, aes(xintercept = FQstart, color = Label), linetype = "dashed", color = "black",linewidth = .5, inherit.aes = FALSE) +
  # geom_text(data = FOIsL, aes(x = FQstart, y = label_height, label = Label), angle = 90, vjust = 0, hjust = 0.5, size = 4, inherit.aes = FALSE) +
  # 
  # # Add vertical set dash lines and grey shaded region at FOI ranges
  # #geom_vline(data = FOIsRange, aes(xintercept = FQstart, color = Label), linetype = "dashed", color = "red",linewidth = .5) +
  # #geom_vline(data = FOIsRange, aes(xintercept = FQend, color = Label), linetype = "dashed", color = "red",linewidth = .5) +
  # geom_rect(data = FOIsRange, aes(xmin = FQstart, xmax = FQend, ymin = 30, ymax = 90), 
  #           fill = "gray", alpha = 0.2, inherit.aes = FALSE)+  # Adjust alpha for transparency
  # geom_text(data = FOIsRange, aes(x = FQstart, y = label_height, label = Label), angle = 90, vjust = 1, hjust = 0.45, size = 4, inherit.aes = FALSE) +
  # 
  # # Add vertical set dash lines and grey shaded region at FOI ranges, label on left
  # #geom_vline(data = FOIsRangeL, aes(xintercept = FQstart, color = Label), linetype = "dashed", color = "red",linewidth = .5) +
  # #geom_vline(data = FOIsRangeL, aes(xintercept = FQend, color = Label), linetype = "dotdash", color = "red",linewidth = .5) +
  # geom_rect(data = FOIsRangeL, aes(xmin = FQstart, xmax = FQend, ymin = 30, ymax = 90), 
  #           fill = "gray", alpha = 0.2, inherit.aes = FALSE)+  # Adjust alpha for transparency
  # geom_text(data = FOIsRangeL, aes(x = FQstart, y = label_height, label = Label), angle = 90, vjust = 0, hjust = 0.5, size = 4, inherit.aes = FALSE) +
  # 
  # # geom_ribbon(data = mallData %>% pivot_wider(names_from = Quantile, values_from = SoundLevel),
  # #             aes(x = Frequency, ymin = `25%`, ymax = `75%`, fill = Year), alpha = 0.2) +
  # 
 
  #median HMD values- each year
  geom_line(data = mallData[mallData$Quantile == "50%",], 
            aes(x = Frequency, y = SoundLevel, color = Year, fill = Year, 
                text = paste("Year: ", Year)), linewidth = 2) +
  
  #median HMD values- all data
  geom_line(data = mALL[mALL$Quantile == "50%",], 
            aes(x = Frequency, y = SoundLevel), color = "black", linewidth = 1,
            linetype = "dotted") +
  scale_y_continuous(limits = c(30, NA)) +  # use to manually scale y minimum so vert line labels are visible
  #for the geom_ribbons below, if data only has one year (ch01 and fk08), comment out the first geom ribbon and change alpha of second from .3 to .1
 
   geom_ribbon(data = mallData %>%
                filter(Year != oldest_year) %>%
                pivot_wider(names_from = Quantile, values_from = SoundLevel),
              aes(x = Frequency, ymin = `25%`, ymax = `75%`, 
                  fill = Year, color = Year, text = paste("Year: ", Year)),
              alpha = 0.1) +
  
  #for the oldest year, make the shading darker since it is hard to see at alpha = .1 for lightblue
  geom_ribbon(data = mallData %>% 
                filter(Year == oldest_year) %>% 
                pivot_wider(names_from = Quantile, values_from = SoundLevel),
              aes(x = Frequency, ymin = `25%`, ymax = `75%`, 
                  fill = Year, color = Year, text = paste("Year: ", Year)), 
              alpha = 0.3) + # High alpha for visibility
  
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

pInt

#make plot interactive
interactive_plot <- ggplotly(pInt)

p_interactive <- ggplotly(pInt, tooltip = "text")

#remove hover info over grey shading
interactive_plot$x$data[[14]]$hoverinfo <- "skip"


interactive_plot



#  interactive_plot$x$layout$xaxis$type
# interactive_plot$x$data[[18]]$textangle <- 90  

#figure out which trace has the grey rectangles show "trace 13" when you hover
# for (i in seq_along(interactive_plot$x$data)) {
#  cat("TRACE", i, "\n")
#print(interactive_plot$x$data[[i]][c("name","fill","mode","hoverinfo")])
#}