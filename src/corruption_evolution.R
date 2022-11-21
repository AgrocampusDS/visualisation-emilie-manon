source("src/wgi_calculations.R")

head(d_calcs)

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

# Also define the group of countries that are going to be highlighted
highlights <- c("Russia")
n <- length(highlights)


df_mac_indexed <- df_mac %>% 
  # Keep countries that have a record for 2008, the index year.
  group_by(countryname) %>%
  # Compute the `price_index`
  mutate(
    # Create 'group', used to color the lines.
    group = if_else(countryname %in% highlights, countryname, "other"),
    group = as.factor(group)
  ) %>% 
  mutate(
    group = fct_relevel(group, "other", after = Inf)
  ) %>% 
  ungroup()


###########################
#PLOT
###########################
usa_corruption <- df_mac_indexed[df_mac_indexed$countryname == "United States" ,]
plt <- ggplot(
  # The ggplot object has associated the data for the highlighted countries
  usa_corruption, 
  aes(year, wgi , group = countryname))+
  geom_line(color = "#21918c") +
  scale_x_continuous(breaks = round(seq(min(usa_corruption$year),
                                        max(usa_corruption$year), 
                                        by = 1))) +
  geom_label(aes(label = countryname),
             color = "#21918c",
             data = usa_corruption %>% filter(year == "2021"),
             nudge_x = 0.35,
             size = 4) +
  layer(data = usa_corruption %>% filter(year >= 2001 & year <=2006),
        geom = "area", 
        mapping = aes(x = year, y = wgi),
        stat = "identity",
        position = "identity",
        params = list(fill = "#addc30", alpha = 0.5)) +
  geom_vline(xintercept = 2001, color = "#5ec962", size = 1.5)+
  geom_label(aes(2001, 2), label = "Terrorist attack\n9/11",
             show.legend = FALSE, color = "#5ec962" )+  
  geom_vline(xintercept = 2016, color= "#440154", size = 1.5)+
  geom_label(aes(2016, 2), label = "Trump president",
             show.legend = FALSE, color = "#440154" )+ 
  theme_ipsum(axis_title_just = c(2), axis_title_size = 8 , plot_title_size = 10 
              , plot_title_face = "plain",axis_text_size = 6) +
  theme(legend.text = element_text(size= 6),legend.title = element_text(size = 8))

plt


######################
#ADD TITLES
######################
plt <- plt + 
  labs(
    title = "Corruption indicator evolution in the United States between 1996 en 2020", 
    subtitle = "The <i>wgi</i> visualizes the evolution of..."  )
plt