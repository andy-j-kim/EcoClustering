##### Cross Country Figures for EC Manuscript

library(ggplot2)
library(dplyr)
library(tidyverse)
library(forcats)
#library(rgdal)
library(sf)
library(sp)
library(spData)
library(spDataLarge)
#library(raster)

data <- read.csv("~/Desktop/EcoClustering/metadata/crosscountryfull.csv")

table(data$description)

### Figure 0: Histogram of ASW Values for the top cluster
data %>% dplyr::filter(cluster == 1) %>% 
  group_by(country) %>% summarize(asw_mean = mean(asw)) %>%
  ggplot(aes(asw_mean)) +
  geom_histogram(color = "#000000", fill = "#A8BAC4", bins = 10) +
  theme_minimal() + 
  labs(x = "Average Silhouette Width of Top Cluster", y = "Count") + 
  geom_vline(aes(xintercept = mean(asw_mean)), color = "#000000", linewidth = 1.25) +
  geom_vline(aes(xintercept = mean(asw_mean) + sd(asw_mean)), color = "#000000", linewidth = 1, linetype = "dashed") +
  geom_vline(aes(xintercept = mean(asw_mean) - sd(asw_mean)), color = "#000000", linewidth = 1, linetype = "dashed")

### Figure 1: Bar chart of each variable count
### https://r-graph-gallery.com/web-horizontal-barplot-with-labels-the-economist.html
dat <- data %>% dplyr::filter(cluster == 1) %>% 
  group_by(description) %>% summarize(count = n()) %>% 
  arrange(count) %>% top_n(10) %>% arrange(desc(count))

colnames(dat) <- c("name","count")

BLUE <- "#076fa2"
RED <- "#E3120B"
BLACK <- "#202020"
GREY <- "grey50"
FONT <- "Times New Roman"

plt <- ggplot(dat) +
  geom_col(aes(count, y = reorder(name,count)), fill = GREY, width = 0.6) 

plt <- plt + 
  scale_x_continuous(
    limits = c(0, 27),
    breaks = seq(0, 60, by = 3), 
    expand = c(0, 0), # The horizontal axis does not extend to either side
    position = "top"  # Labels are located on the top
  ) +
  
  # The vertical axis only extends upwards 
  scale_y_discrete(expand = expansion(add = c(0, 0.5))) +
  theme(
    # Set background color to white
    panel.background = element_rect(fill = "white"),
    # Set the color and the width of the grid lines for the horizontal axis
    panel.grid.major.x = element_line(color = "#A8BAC4", linewidth = 0.3),
    # Remove tick marks by setting their length to 0
    axis.ticks.length = unit(0, "mm"),
    # Remove the title for both axes
    axis.title = element_blank(),
    # Only left line of the vertical axis is painted in black
    axis.line.y.left = element_line(color = "grey50"),
    # Remove labels from the vertical axis
    axis.text.y = element_blank(),
    # But customize labels for the horizontal axis
    axis.text.x = element_text(family = FONT, size = 16)
  )

plt <- plt + 
  shadowtext::geom_shadowtext(
    data = subset(dat, count < 6),
    aes(count, y = name, label = name),
    hjust = 0,
    nudge_x = 0.3,
    colour = GREY,
    bg.colour = "white",
    bg.r = 0.2,
    family = FONT,
    size = 6
  ) + 
  geom_text(
    data = subset(dat, count >= 6),
    aes(0, y = name, label = name),
    hjust = 0,
    nudge_x = 0.3,
    colour = "white",
    family = FONT,
    size = 6
  ) + 
  labs(title = "The 11 assets which appear most frequently in the top clusters")

plt

save(plt, file = "~/Desktop/EcoClustering/metadata/figures/fig1.pdf")

##### Figure 2: Bar plot of most common 10 variables in Top Three Clusters
### https://r-graph-gallery.com/web-horizontal-barplot-with-labels-the-economist.html
dat <- data %>% 
  group_by(description) %>% summarize(count = n()) %>% 
  arrange(count) %>% top_n(10) %>% arrange(desc(count))

colnames(dat) <- c("name","count")

BLUE <- "#076fa2"
RED <- "#E3120B"
BLACK <- "#202020"
GREY <- "grey50"
FONT <- "Times New Roman"

plt <- ggplot(dat) +
  geom_col(aes(count, y = reorder(name,count)), fill = GREY, width = 0.6) 

plt <- plt + 
  scale_x_continuous(
    limits = c(0, 65),
    breaks = seq(0, 65, by = 5), 
    expand = c(0, 0), # The horizontal axis does not extend to either side
    position = "top"  # Labels are located on the top
  ) +
  
  # The vertical axis only extends upwards 
  scale_y_discrete(expand = expansion(add = c(0, 0.5))) +
  theme(
    # Set background color to white
    panel.background = element_rect(fill = "white"),
    # Set the color and the width of the grid lines for the horizontal axis
    panel.grid.major.x = element_line(color = "#A8BAC4", linewidth = 0.3),
    # Remove tick marks by setting their length to 0
    axis.ticks.length = unit(0, "mm"),
    # Remove the title for both axes
    axis.title = element_blank(),
    # Only left line of the vertical axis is painted in black
    axis.line.y.left = element_line(color = "grey50"),
    # Remove labels from the vertical axis
    axis.text.y = element_blank(),
    # But customize labels for the horizontal axis
    axis.text.x = element_text(family = FONT, size = 16)
  )

plt <- plt + 
  shadowtext::geom_shadowtext(
    data = subset(dat, count < 14),
    aes(count, y = name, label = name),
    hjust = 0,
    nudge_x = 0.3,
    colour = GREY,
    bg.colour = "white",
    bg.r = 0.2,
    family = FONT,
    size = 6
  ) + 
  geom_text(
    data = subset(dat, count >= 14),
    aes(0, y = name, label = name),
    hjust = 0,
    nudge_x = 0.3,
    colour = "white",
    family = FONT,
    size = 6
  ) + 
  labs(title = "The 10 assets which appear most frequently in the top three clusters")

plt

save(plt, file = "~/Desktop/EcoClustering/metadata/figures/fig2.pdf")

#### Figure 3 + 4 stratified barchart by median of rural-ness, and gini coefficient

###rural: 1 if rural pop greater than median, 0 otherwise
data$rural <- ifelse(data$percent_rural >= median(data$percent_rural), "High Rural", "Low Rural")

###gini_eq: 1 if gini less than median (more equality), 0 otherwise 
data$gini_eq <- ifelse(data$gini < median(data$gini), "Low Gini (Less Inequality)", "High Gini (More Inquality)")

###gdp_high: 1 if gdp greater than median (more country wealth), 0 otherwise
data$gdp_high <- ifelse(data$gdp >= median(data$gdp), 1, 0)

###gdp__per_capita_high: 1 if gdp per capita greater than median (more individual wealth), 0 otherwise
data$gdp_per_capita_high <- ifelse(data$gdp_per_capita >= median(data$gdp_per_capita), 1, 0)

dat <- data %>% dplyr::filter(cluster == 1) %>% 
  group_by(rural,description) %>% summarize(count = n()) %>% 
  arrange(count) %>% top_n(5) %>% arrange(desc(count))

colnames(dat) <- c("rural","name","count")

BLUE <- "#076fa2"
RED <- "#E3120B"
BLACK <- "#202020"
GREY <- "grey50"
FONT <- "Times New Roman"

plt <- ggplot(dat) +
  geom_col(aes(count, y = reorder(name,count)), fill = GREY, width = 0.6) + 
  facet_wrap(~rural)

plt <- plt + 
  scale_x_continuous(
    limits = c(0, 16),
    breaks = seq(0, 14, by = 2), 
    expand = c(0, 0), # The horizontal axis does not extend to either side
    position = "bottom"  # Labels are located on the top
  ) +
  
  # The vertical axis only extends upwards 
  scale_y_discrete(expand = expansion(add = c(0, 0.5))) +
  theme(
    # Set background color to white
    panel.background = element_rect(fill = "white"),
    # Set the color and the width of the grid lines for the horizontal axis
    panel.grid.major.x = element_line(color = "#A8BAC4", linewidth = 0.3),
    # Remove tick marks by setting their length to 0
    axis.ticks.length = unit(0, "mm"),
    # Remove the title for both axes
    axis.title = element_blank(),
    # Only left line of the vertical axis is painted in black
    axis.line.y.left = element_line(color = "grey50"),
    # Remove labels from the vertical axis
    #axis.text.y = element_blank(),
    # But customize labels for the horizontal axis
    axis.text.x = element_text(family = FONT, size = 16)
  ) #+ labs(title = "The 5-6 assets which appear most frequently in the top clusters for gdp_per_cap_high (=1) vs nonrurual areas")

plt

save(plt, file = "~/Desktop/EcoClustering/metadata/figures/fig3.pdf")

#### Figure 4 stratified barchart by median of Gini coefficient

dat <- data %>% dplyr::filter(cluster == 1) %>% 
  group_by(gini_eq,description) %>% summarize(count = n()) %>% 
  arrange(count) %>% top_n(5) %>% arrange(desc(count))

colnames(dat) <- c("gini_eq","name","count")

BLUE <- "#076fa2"
RED <- "#E3120B"
BLACK <- "#202020"
GREY <- "grey50"
FONT <- "Times New Roman"

plt <- ggplot(dat) +
  geom_col(aes(count, y = reorder(name,count)), fill = GREY, width = 0.6) + 
  facet_wrap(~gini_eq)

plt <- plt + 
  scale_x_continuous(
    limits = c(0, 16),
    breaks = seq(0, 14, by = 2), 
    expand = c(0, 0), # The horizontal axis does not extend to either side
    position = "bottom"  # Labels are located on the top
  ) +
  
  # The vertical axis only extends upwards 
  scale_y_discrete(expand = expansion(add = c(0, 0.5))) +
  theme(
    # Set background color to white
    panel.background = element_rect(fill = "white"),
    # Set the color and the width of the grid lines for the horizontal axis
    panel.grid.major.x = element_line(color = "#A8BAC4", linewidth = 0.3),
    # Remove tick marks by setting their length to 0
    axis.ticks.length = unit(0, "mm"),
    # Remove the title for both axes
    axis.title = element_blank(),
    # Only left line of the vertical axis is painted in black
    axis.line.y.left = element_line(color = "grey50"),
    # Remove labels from the vertical axis
    #axis.text.y = element_blank(),
    # But customize labels for the horizontal axis
    axis.text.x = element_text(family = FONT, size = 16)
  ) #+ labs(title = "The 5-6 assets which appear most frequently in the top clusters for gdp_per_cap_high (=1) vs nonrurual areas")

plt

save(plt, file = "~/Desktop/EcoClustering/metadata/figures/fig4.pdf")

##### TO BE MODIFIED/BRAINSTORMED AFTER THIS POINT

##### Figure 5a: Pie chart by asset category for all 33 countries

dat <- data %>% filter(cluster == 1) %>% 
  group_by(asset_category) %>%
  summarize(count = n()) 

dat$asset <- c("Structural","Electric",
               "Appliance/Furniture",
               "Agricultural/Environmental","Other")

# Compute the position of labels
dat <- dat %>% 
  mutate(prop = count / sum(dat$count) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop ) 

dat$label <- scales::percent(dat$prop/100)

# Basic piechart
ggplot(dat, aes(x="", y=prop, fill=asset)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none") +
  geom_text(aes(y = ypos, label = asset), color = "white", size=6) +
  scale_fill_brewer(palette="Set1")

ggplot(data=dat)+
  geom_bar(aes(x="", y=prop, fill=asset), stat="identity", width = 1)+
  coord_polar("y", start=0)+
  theme_void() +
  labs(title = "Pie Chart of Asset Category Distribution among the Top Clusters")
#geom_text(aes(x=1, y = ypos, label=label))

##### Figure 5: Africa Map,  https://gld.gu.se/media/1702/usingrformapmaking_notes.pdf

asset_data <- data %>% group_by(country,asset_category) %>%
  tally() %>%
  pivot_wider(names_from = asset_category, values_from = n) %>%
  mutate(across(everything(), .fns=~replace_na(.,0)))

meta_data <- data %>% group_by(country) %>%
  summarize(year = mean(year),
            asw = mean(asw),
            percent_rural = mean(percent_rural),
            gini = mean(gini),
            gdp = mean(gdp),
            gdp_per_capita = mean(gdp_per_capita))

africa = world[world$region_un == "Africa",]
africa$country <- africa$name_long

merge_africa <- merge(africa, meta_data, by = "country")

africa.plot = ggplot() +
  geom_sf(data = merge_africa, aes(fill = asw)) + 
  labs(title = "Top Clustering ASW by SSA Country")
africa.plot

africa.plot = ggplot() +
  geom_sf(data = merge_africa, aes(fill = gini)) + 
  labs(title = "Gini Coefficients by SSA Country")
africa.plot

#### Stratified pie charts (stratifying by rural/gdp/gini)
#### Stratified bar charts of the top assets chosen (specific assets not categories)
#### link for making some pie charts: 
#### https://stackoverflow.com/questions/40088819/r-ggplot-pie-chart-with-facet-wrap

##### Maybe something with a spineplot: 

plot(factor(data$description)~factor(data$asset_category))


###########################
######## ARCHIVED #########
###########################
