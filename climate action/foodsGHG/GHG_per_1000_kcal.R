# Climate Action
# Diets carbon consumption
# Source: Our World in Data

library(jsonlite)
library(tidyverse)
library(showtext)

# Project directory
setwd("~/Docs/Proyectos/DataViz-ClimateAction/")

# Fetch the data
# GHG per 1000 kcal
df1 = read.csv("https://ourworldindata.org/grapher/ghg-kcal-poore.csv?v=1&csvType=full&useColumnShortNames=false")
# Fetch the metadata
metadata1 = fromJSON("https://ourworldindata.org/grapher/ghg-kcal-poore.metadata.json?v=1&csvType=full&useColumnShortNames=false")

# GHG per kg of product
df2 = read.csv("https://ourworldindata.org/explorers/food-footprints.csv?v=1&csvType=full&useColumnShortNames=false&Commodity+or+Specific+Food+Product=Commodity&Environmental+Impact=Carbon+footprint&Kilogram+%2F+Protein+%2F+Calories=Per+kilogram&By+stage+of+supply+chain=false&hideControls=false")

# Fetch the metadata
metadata2 = fromJSON("https://ourworldindata.org/explorers/food-footprints.metadata.json?v=1&csvType=full&useColumnShortNames=true&Commodity+or+Specific+Food+Product=Commodity&Environmental+Impact=Carbon+footprint&Kilogram+%2F+Protein+%2F+Calories=Per+kilogram&By+stage+of+supply+chain=false&hideControls=false")

df = merge(df1, df2, by = c("Entity", "Year"))

# DESC ----
# Carbon dioxide equivalents (CO₂eq)
# Carbon dioxide is the most important greenhouse gas, but not the only one. 
# To capture all greenhouse gas emissions, researchers express them in “carbon 
# dioxide equivalents” (CO₂eq). This takes all greenhouse gases into account, 
# not just CO₂.
# 
# To express all greenhouse gases in carbon dioxide equivalents (CO₂eq), 
# each one is weighted by its global warming potential (GWP) value. 
# GWP measures the amount of warming a gas creates compared to CO₂. CO₂ is given 
# a GWP value of one. If a gas had a GWP of 10 then one kilogram of that gas 
# would generate ten times the warming effect as one kilogram of CO₂.
# Carbon dioxide equivalents are calculated for each gas by multiplying the 
# mass of emissions of a specific greenhouse gas by its GWP factor.
# This warming can be stated over different timescales. To calculate CO₂eq 
# over 100 years, we’d multiply each gas by its GWP over a 100-year timescale 
# (GWP100). 
# 
# Total greenhouse gas emissions – measured in CO₂eq – are then calculated by 
# summing each gas’ CO₂eq value.

# Citation: 
# Data source: Poore and Nemecek (2018)
# OurWorldinData.org/environmental-impacts-of-food | CC BY 4.0


# THEME ----
## Font 
font_add_google("Noto sans", "Noto Sans") # Noto Sans
# Activate showtext to use in plots
showtext_auto()
showtext_opts(dpi = 300)


# DATA PREP ----
# add colors
df = df %>% 
  mutate(
    color = case_when(
      Entity == "Coffee" ~ "#bf9026",
      Entity == "Beef (beef herd)" ~ "#bf7226",
      Entity == "Tomatoes" ~ "#bf3826",
      !(Entity %in% c("Coffee", "Beef (beef herd)")) ~ "#1a6576"
    )
  )

# filter out some foods
dff = df %>% 
  filter(
    !(Entity %in% c(
      "Brassicas", 
      "Cassava", 
      "Cane Sugar", 
      "Groundnuts", 
      "Palm Oil",
      "Beet Sugar",
      "Rapeseed Oil",
      "Sunflower Oil",
      "Barley"
      )
      )
  )


# PLOTS ----
# parameters for saving images
u = "cm"
d = 300

# GHG per 1000 kcal ----
## TEXTS ----
t = "Greenhouse gas emissions\nper 1000 kilocalories"
st = "Emissions in kg of carbon dioxide-equivalents.\n"
ct = paste0(
  "Data source: Poore and Nemecek (2018) DOI: 10.1126/science.aaq0216",
  "\n",
  "Adapted from OurWorldinData.org/environmental-impacts-of-food | CC BY 4.0"
)
xt = NULL
yt = NULL

# reorder data
dff = dff %>% 
  mutate(
    Entity = fct_reorder(Entity, Greenhouse.gas.emissions.per.1000kcal)
  )

## PLOT ----
ggplot(data = dff) + 
  geom_col(
    aes(
      y = Entity, 
      x = Greenhouse.gas.emissions.per.1000kcal,
      fill = color
      ),
    width = 0.8
    ) +
  geom_text(
    aes(
      y = Entity, 
      x = Greenhouse.gas.emissions.per.1000kcal + 0.2,
      label = round(Greenhouse.gas.emissions.per.1000kcal, 1),
      color = color,
      ),
    hjust = 0,
    size = 1.75
    ) + 
  geom_curve(
    aes(
      x = 39, 
      xend = 52,
      y = 20,
      yend = 27.5
      ),
    color = "#bf9026",
    curvature = 0.5,
    size = 0.25,
    arrow = arrow(type = "closed", length = unit(1, "mm"))
  ) +
  geom_label(
    aes(y = 20, x = 35),
    label = "Wait... \nWhat ???",
    fill = "#bf9026",
    color = "#EBEBEB",
    size = 3,
    hjust = 0.5
  ) +
  scale_fill_identity(guide = "none") +
  scale_color_identity(guide = "none") + 
  labs(y = yt, x = xt, title = t, subtitle = st, caption = ct) + 
  theme_void() + 
  theme(
    text = element_text(family = "Noto sans"),
    axis.text.y = element_text(hjust = 1, size = rel(0.5)),
    axis.text.x = element_blank(),
    plot.caption.position = "panel",
    plot.caption = element_text(hjust = 0, size = rel(0.5), color = "gray40"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray40"),
    plot.background = element_rect(fill = "#EBEBEB")
  )


ggsave(
  plot = last_plot(),
  filename = "food-GHG-1000kcal.png",
  height = 13.50,
  width = 10.80,
  units = u,
  dpi = d
)


# GHG per kg of product
## TEXTS ----
t = "Greenhouse gas emissions\nper kilogram of food product"
st = "Emissions in kg of carbon dioxide-equivalents.\n"
ct = paste0(
  "Data source: Poore and Nemecek (2018) DOI: 10.1126/science.aaq0216",
  "\n",
  "Adapted from OurWorldinData.org/environmental-impacts-of-food | CC BY 4.0"
)
xt = NULL
yt = NULL

# reorder data
dff = dff %>% 
  mutate(
    Entity = fct_reorder(Entity, Greenhouse.gas.emissions.per.kilogram)
  )

## PLOT ----
ggplot(data = dff) + 
  geom_col(
    aes(
      y = Entity, 
      x = Greenhouse.gas.emissions.per.kilogram,
      fill = color
    ),
    width = 0.8
  ) +
  geom_text(
    aes(
      y = Entity, 
      x = Greenhouse.gas.emissions.per.kilogram + 0.2,
      label = round(Greenhouse.gas.emissions.per.kilogram, 1),
      color = color,
    ),
    hjust = 0,
    size = 1.75
  ) + 
  geom_curve(
    aes(
      x = 68, 
      xend = 34,
      y = 20,
      yend = 24
    ),
    color = "#bf9026",
    curvature = 0.5,
    size = 0.25,
    arrow = arrow(type = "closed", length = unit(1, "mm"))
  ) +
  geom_label(
    aes(y = 20, x = 68),
    label = " Well... \nNot that bad",
    fill = "#bf9026",
    color = "#EBEBEB",
    size = 3,
    hjust = 0.5
  ) +
  scale_fill_identity(guide = "none") +
  scale_color_identity(guide = "none") + 
  labs(y = yt, x = xt, title = t, subtitle = st, caption = ct) + 
  theme_void() + 
  theme(
    text = element_text(family = "Noto sans"),
    axis.text.y = element_text(hjust = 1, size = rel(0.5)),
    axis.text.x = element_blank(),
    plot.caption.position = "panel",
    plot.caption = element_text(hjust = 0, size = rel(0.5), color = "gray40"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray40"),
    plot.background = element_rect(fill = "#EBEBEB")
  )

ggsave(
  plot = last_plot(),
  filename = "food-GHG-kg-product.png",
  height = 13.50,
  width = 10.80,
  units = u,
  dpi = d
)
