source("src/wgi_calculations.R")


########################
#PACKAGES
########################
library(tidyverse)
library(dplyr)
library(ggrepel)
library(ggtext)
library(ggplot2)
library(hrbrthemes)
library(viridis)


########################
#LOAD DATA
########################
df_mac <- d_calcs %>% 
  # Subset variables
  select(year, countryname, score_wgi_scale) %>% 
  # If there is more than one record per year/country, use the mean 
  group_by(countryname, year) %>% 
  summarize(wgi = mean(score_wgi_scale))%>%
  ungroup()
 
usa_corruption <- df_mac[df_mac$countryname == "United States" ,]


###########################
#PLOT
###########################
plt <- ggplot(
  # The ggplot object has associated the data for the highlighted countries
  usa_corruption, aes(year, wgi , group = countryname))+
  # Label Irak war 
  annotate(geom = "rect", xmin = 2003, xmax = 2011, 
           ymin = -Inf, ymax = Inf,
           alpha = 0.3, fill = "#7ad151") +
  # Label G.Bush reelection 
  geom_vline(xintercept = 2004, color = "#22a884", size = 1.5)+
  # Subprimes Crisis
  annotate(geom = "rect", xmin = 2007, xmax = 2010, 
           ymin = -Inf, ymax = Inf, 
           alpha = 0.3, fill = "#d175b8") +
  # Label 9/11
  geom_vline(xintercept = 2001, color = "#65cb5e", size = 1.5)+
  # D. Trum Election 
  geom_vline(xintercept = 2016, color= "#414487", size = 1.5)+
  
  # Labels 
  geom_label(aes(2007, 1.38), label = "Irak War",
             show.legend = FALSE, color = "#5ec962", size = 4 )+
  geom_label(aes(2004, 1.48), label = "G.Bush's Reelection",
             show.legend = FALSE, color = "#22a884", size = 4 )+
  geom_label(aes(2008.5, 1.48), label = "Subprimes Crisis",
             show.legend = FALSE, color = "#d175b8", size = 4 )+
  geom_label(aes(2001, 1.55), label = "9/11 Terrorist attack",
             show.legend = FALSE, color = "#65cb5e", size = 4  )+
  geom_label(aes(2016, 1.55), label = "D.Trump's Election",
             show.legend = FALSE, color = "#414487", size = 4)+ 
  theme_ipsum(axis_title_just = c(2), axis_title_size = 14 ,
              plot_title_size = 18,plot_title_face = "bold",
              axis_text_size = 14,subtitle_face = "italic",subtitle_size  = 12) +
  theme(plot.title = element_markdown(lineheight = 1.1),
    legend.text = element_text(size= 6),
        legend.title = element_text(size = 8),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 10, angle = 90),
        axis.text.y = element_text(size = 10))+
  geom_line(color = alpha("#AA4466",0.7), size = 2) +
  scale_x_continuous(breaks = round(seq(min(usa_corruption$year),
                                        max(usa_corruption$year), 
                                        by = 1))) +
  scale_y_continuous(breaks = round(seq(min(usa_corruption$wgi)-0.1,
                                        max(usa_corruption$wgi)+0.1, 
                                        by = 0.2),
                                    digits = 2),
                    limits = c(round(min(usa_corruption$wgi)-0.1, digits = 2),
                               round(max(usa_corruption$wgi)+0.1, digits = 2)), 
                    expand = c(0, 0))+
  geom_label_repel(aes(label = countryname),
                   fill = alpha("#AA4466",0.7),
                   data = usa_corruption %>% filter(year == "2021"),
                   color = 'white',
                   nudge_x = 1.5,
                   nudge_y = 0.02,
                   size = 4)+
  labs(title = "Evolution of <span style='color:#AA4466;'>Worldwide Governance Indicators (WGI)</span> in the US from 1996 to 2021",
       subtitle = "The WGI is a mean of 6 indicators which represents the corruption indicator of a country.\nOn top of that, we added major geopolitical incidents that occured from 1996 to 2021")
plt

