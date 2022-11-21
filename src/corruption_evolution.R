source("src/wgi_calculations.R")

head(d_calcs)

########################
#PACKAGES
########################
library(tidyverse)
library(dplyr)
library(ggrepel)
library(ggtext)

library(showtext)
font_add_google("Lato")
showtext_auto()

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

#########################
#THEME
#########################
# This theme extends the 'theme_minimal' that comes with ggplot2.
# The "Lato" font is used as the base font. This is similar
# to the original font in Cedric's work, Avenir Next Condensed.
theme_set(theme_minimal(base_family = "Lato"))


theme_update(
  # Remove title for both x and y axes
  axis.title = element_blank(),
  # Axes labels are grey
  axis.text = element_text(color = "grey40"),
  # The size of the axes labels are different for x and y.
  axis.text.x = element_text(size = 20, margin = margin(t = 5)),
  axis.text.y = element_text(size = 17, margin = margin(r = 5)),
  # Also, the ticks have a very light grey color
  axis.ticks = element_line(color = "grey91", size = .5),
  # The length of the axis ticks is increased.
  axis.ticks.length.x = unit(1.3, "lines"),
  axis.ticks.length.y = unit(.7, "lines"),
  # Remove the grid lines that come with ggplot2 plots by default
  panel.grid = element_blank(),
  # Customize margin values (top, right, bottom, left)
  plot.margin = margin(20, 40, 20, 40),
  # Use a light grey color for the background of both the plot and the panel
  plot.background = element_rect(fill = "grey98", color = "grey98"),
  panel.background = element_rect(fill = "grey98", color = "grey98"),
  # Customize title appearence
  plot.title = element_text(
    color = "grey10", 
    size = 28, 
    face = "bold",
    margin = margin(t = 15)
  ),
  # Customize subtitle appearence
  plot.subtitle = element_markdown(
    color = "grey30", 
    size = 16,
    lineheight = 1.35,
    margin = margin(t = 15, b = 40)
  ),
  # Title and caption are going to be aligned
  plot.title.position = "plot",
  plot.caption.position = "plot",
  plot.caption = element_text(
    color = "grey30", 
    size = 13,
    lineheight = 1.2, 
    hjust = 0,
    margin = margin(t = 40) # Large margin on the top of the caption.
  ),
  # Remove legend
  legend.position = "none"
)


###########################
#PLOT
###########################
plt <- ggplot(
  # The ggplot object has associated the data for the highlighted countries
  df_mac_indexed[df_mac_indexed$countryname == "United States" ,], 
  aes(year, wgi , group = countryname))+
  geom_line(color = "red") +
  geom_label(aes(label = countryname),
             color = "red",
             data = df_mac_indexed[df_mac_indexed$countryname == "United States" ,] %>% filter(year == "2021"),
             nudge_x = 0.35,
             size = 4) 
plt


######################
#ADD TITLES
######################
plt <- plt + 
  labs(
    title = "Corruption indicator evolution in the United States between 1996 en 2020", 
    subtitle = "The <i>wgi</i> visualizes the evolution of..."  )
plt