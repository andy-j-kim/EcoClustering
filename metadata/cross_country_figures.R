##### Cross Country Figures for EC Manuscript

library(ggplot2)
library(tidyverse)
library(forcats)
library(rgdal)
library(sf)
library(sp)
library(spData)
library(spDataLarge)
library(raster)

data <- read.csv("~/Downloads/crosscountry.csv")

### Figure 0: Histogram of ASW Values for the top cluster
data %>% filter(cluster == 1) %>% 
  group_by(country) %>% summarize(asw_mean = mean(asw)) %>%
  ggplot(aes(asw_mean)) +
  geom_histogram(color = "#000000", fill = "#A8BAC4", bins = 10) +
  theme_minimal() + 
  labs(x = "Average Silhouette Width of Top Cluster", y = "Count") + 
  geom_vline(aes(xintercept = mean(asw_mean)), color = "#000000", linewidth = 1.25) +
  geom_vline(aes(xintercept = mean(asw_mean) + sd(asw_mean)), color = "#000000", linewidth = 1, linetype = "dashed") +
  geom_vline(aes(xintercept = mean(asw_mean) - sd(asw_mean)), color = "#000000", linewidth = 1, linetype = "dashed")

### Figure 2: Bar chart of each variable count
### https://r-graph-gallery.com/web-horizontal-barplot-with-labels-the-economist.html
dat <- cross %>% filter(cluster == 1) %>% 
  group_by(description) %>% summarize(count = n()) %>% 
  arrange(count) %>% top_n(10) %>% arrange(desc(count))

colnames(dat) <- c("name","count")

BLUE <- "#076fa2"
RED <- "#E3120B"
BLACK <- "#202020"
GREY <- "grey50"
FONT <- "Times New Roman"

plt <- ggplot(dat) +
  geom_col(aes(count, y = reorder(name,count)), fill = BLACK, width = 0.6) 

plt <- plt + 
  scale_x_continuous(
    limits = c(0, 15),
    breaks = seq(0, 14, by = 2), 
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
    axis.line.y.left = element_line(color = "black"),
    # Remove labels from the vertical axis
    axis.text.y = element_blank(),
    # But customize labels for the horizontal axis
    axis.text.x = element_text(family = FONT, size = 16)
  )

plt <- plt + 
  geom_shadowtext(
    data = subset(dat, count < 4),
    aes(count, y = name, label = name),
    hjust = 0,
    nudge_x = 0.3,
    colour = BLACK,
    bg.colour = "white",
    bg.r = 0.2,
    family = FONT,
    size = 6
  ) + 
  geom_text(
    data = subset(dat, count >= 4),
    aes(0, y = name, label = name),
    hjust = 0,
    nudge_x = 0.3,
    colour = "white",
    family = FONT,
    size = 6
  )

plt

##### Figure 2: Bar plot of most common 10 variables in Top Three Clusters
##### Modify the code above after is is finalized

##### Figure 3a: Pie chart by asset category for all 33 countries

dat <- cross %>% filter(cluster == 1) %>% 
  group_by(asset_category) %>%
  summarize(count = n()) 

dat$asset <- c("Structural","Electric","Appliance/Furniture")

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
  theme_void() #+
#geom_text(aes(x=1, y = ypos, label=label))

##### Figure 3b: Asset categories chosen by EC algorithm in the top three clustes for each country
##### Idea: then compare these to the metadata 

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

##### Figure 4: Africa Map,  https://gld.gu.se/media/1702/usingrformapmaking_notes.pdf

africa = world[world$region_un == "Africa"]


##### Maybe something with a spineplot: 

plot(factor(data$description)~factor(data$asset_category))


###########################
######## ARCHIVED #########
###########################

##### Figure 1: Bar plot of most common 10 variables in Top Cluster

top_num = 10

data %>% 
  filter(cluster == 1) %>%
  count(description) %>%
  mutate(description = fct_reorder(description,n)) %>%
  top_n(top_num,n) %>%
  ggplot(aes(x = n, y = description)) + 
  geom_col() + 
  scale_fill_manual(values = c("#3db5ff", "#0099f9")) +
  labs(x = "Counts",
       y = element_blank(),
       title = "Title")  + 
  theme_minimal() + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) + 
  geom_text(aes(label = description), position = position_stack(vjust = 0.5), size = 3, col = "#ffffff")

