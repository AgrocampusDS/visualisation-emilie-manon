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
highlights <- c("France", "Russia", "United Stats", "China", "Afghanistan")
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
  df_mac_indexed, 
  aes(year, wgi , group = countryname)
) + 
  # Geometric annotations that play the role of grid lines
  geom_vline(
    xintercept = seq(1995, 2021, by = 1),
    color = "grey91", 
    size = .6
  ) +
  geom_segment(
    data = tibble(y = seq(-3, 3, by = 1), x1 = 1995, x2 = 2021),
    aes(x = x1, xend = x2, y = y, yend = y),
    inherit.aes = FALSE,
    color = "grey91",
    size = .6
  ) +
  geom_segment(
    data = tibble(y = 0, x1 = 1995, x2 = 2020),
    aes(x = x1, xend = x2, y = y, yend = y),
    inherit.aes = FALSE,
    color = "grey60",
    size = .8
  ) +
  ## Lines for the non-highlighted countries
  geom_line(
    data = df_mac_indexed %>% filter(group == "other"),
    color = "grey",
    size = .6,
    alpha = .5
  ) +
  ## Lines for the highlighted countries.
  # It's important to put them after the grey lines
  # so the colored ones are on top
  geom_line(
    aes(color = group),
    size = .9
  )
plt


######################
#ADD LABELS
######################
plt <- plt +
  geom_text_repel(
    aes(color = group),
    family = "Lato",
    fontface = "bold",
    size = 8,
    direction = "y",
    xlim = c(2020.8, NA),
    hjust = 0,
    segment.size = .7,
    segment.alpha = .5,
    segment.linetype = "dotted",
    box.padding = .4,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20
  ) +
  ## coordinate system + scales
  coord_cartesian(
    clip = "off",
    ylim = c(-4, 3)
  ) +
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(2000, 2023.5), 
    breaks = seq(2000, 2020, by = 5)
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = seq(-4, 3, by = 1),
    labels = glue::glue("{format(seq(-4, 3, by = 1), nsmall = 2)}$")
  )
plt 



######################
#ADD TITLES
######################
plt <- plt + 
  scale_color_manual(
    values = c(rcartocolor::carto_pal(n = n, name = "Bold")[1:n-1], "grey50")
  ) +
  labs(
    title = "Compared to the financial crisis in 2008, how much more or less do you have to pay for a Big Mac today?",
    subtitle = "The <i>index chart</i> visualizes the price changes (in USD) of a Big Mac based on a 2008 as index year. The <b>Big Mac Index</b> is published by The Economist as an informal way to provide a test of the<br>extent to which market exchange rates result in goods costing the same in different countries. It <i>seeks to make exchange-rate theory a bit more digestible</i> and takes its name from the Big Mac,<br>a hamburger sold at McDonald's restaurants.",
    caption = "Visualization by Cédric Scherer  •  Data by The Economist  •  The index chart shows the 27 countries that provide Big mac prices for all years from 2000 to 2020. In case a country was reported twice per year, the mean value was visualized."
  )
plt