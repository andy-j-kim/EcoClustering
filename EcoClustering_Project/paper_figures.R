##### Cross Country Figures for EC Manuscript

library(ggplot2)
library(dplyr)
library(tidyverse)
library(forcats)
#library(rgdal)
library(sf)
library(sp)
library(spData)
library(cowplot)
#library(spDataLarge)
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
# Filter and process data, then replace specific values in the 'name' column
dat <- data %>%
  dplyr::filter(cluster == 1) %>%
  group_by(description) %>%
  summarize(count = n()) %>%
  arrange(count) %>%
  top_n(10) %>%
  arrange(desc(count))

# Rename columns and values in the 'name' column
colnames(dat) <- c("name", "count")
dat$name <- recode(dat$name, "roof" = "type of roofing", "floor" = "type of flooring")

# Set colors and font
BLUE <- "#004488"
DARK_GREY <- "#333333"
LIGHT_GREY <- "#b0b0b0"
FONT <- "Times New Roman"

plt <- ggplot(dat) +
  geom_col(aes(count, y = reorder(name, count)), fill = DARK_GREY, width = 0.6) +
  scale_x_continuous(
    limits = c(0, max(dat$count) + 2),
    breaks = seq(0, max(dat$count) + 2, by = 2),
    expand = c(0, 0),
    position = "top"
  ) +
  scale_y_discrete(expand = expansion(add = c(0, 0.5))) +
  theme_minimal(base_family = FONT) +
  theme(
    panel.grid.major.x = element_line(color = LIGHT_GREY, size = 0.2),
    panel.grid.minor = element_blank(),
    axis.ticks.length = unit(0, "mm"),
    axis.title = element_blank(),
    axis.line.y.left = element_line(color = DARK_GREY, size = 0.5),
    axis.text.y = element_blank(),
    axis.text.x = element_text(family = FONT, size = 14, color = DARK_GREY),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, color = DARK_GREY)
  ) +
  labs(title = "Most Frequent Assets in Top ASW Result")

# Add labels inside the bars with contrasting colors
plt <- plt +
  geom_text(
    aes(count, y = reorder(name, count), label = name),
    hjust = ifelse(dat$count < (max(dat$count) / 3), 0, 1),
    nudge_x = ifelse(dat$count < (max(dat$count) / 3), 0.3, -0.3),
    color = ifelse(dat$count < (max(dat$count) / 3), DARK_GREY, "white"),
    family = FONT,
    size = 5
  )

plt


save(plt, file = "~/Desktop/EcoClustering/metadata/figures/fig1.pdf")

#### Figure 3 stratified barchart by median of rural-ness, and gini coefficient

###rural: 1 if rural pop greater than median, 0 otherwise
data$rural <- ifelse(data$percent_rural >= median(data$percent_rural), "More Rural (>57%)", "More Urban")

###gini_eq: 1 if gini less than median (more equality), 0 otherwise
data$gini_eq <- ifelse(data$gini < median(data$gini), "Low Gini (Less Inequality)", "High Gini (More Inquality)")

###gdp_high: 1 if gdp greater than median (more country wealth), 0 otherwise
data$gdp_high <- ifelse(data$gdp >= median(data$gdp), 1, 0)

###gdp__per_capita_high: 1 if gdp per capita greater than median (more individual wealth), 0 otherwise
data$gdp_per_capita_high <- ifelse(data$gdp_per_capita >= median(data$gdp_per_capita), 1, 0)

# Process data and format it for plotting
dat <- data %>%
  dplyr::filter(cluster == 1) %>%
  group_by(rural, description) %>%
  summarize(count = n()) %>%
  arrange(count) %>%
  top_n(5) %>%
  arrange(desc(count))

colnames(dat) <- c("rural", "name", "count")

# Set color and font preferences
BLUE <- "#004488"
DARK_GREY <- "#333333"
LIGHT_GREY <- "#A8BAC4"
FONT <- "Times New Roman"

# Create the plot with facets for 'rural' and improved aesthetics
plt <- ggplot(dat) +
  geom_col(aes(count, y = reorder(name, count)), fill = DARK_GREY, width = 0.6) +
  facet_wrap(~rural) +
  scale_x_continuous(
    limits = c(0, 16),
    breaks = seq(0, 16, by = 2),
    expand = c(0.02, 0.02), # Add slight padding to avoid crowding at edges
    position = "bottom"
  ) +
  scale_y_discrete(expand = expansion(add = c(0, 0.5))) +
  theme_minimal(base_family = FONT) +
  theme(
    panel.grid.major.x = element_line(color = LIGHT_GREY, linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    panel.spacing = unit(1, "lines"), # Increase space between facets
    axis.ticks.length = unit(0, "mm"),
    axis.title = element_blank(),
    axis.line.y.left = element_line(color = DARK_GREY, size = 0.5),
    axis.text.y = element_blank(), # Remove text from the y-axis
    axis.text.x = element_text(family = FONT, size = 14, color = DARK_GREY),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, color = DARK_GREY),
    strip.text = element_text(size = 14, face = "bold", color = DARK_GREY)
  ) +
  labs(title = "Top Assets by Rural vs Urban Areas")

# Add labels inside bars for readability
plt <- plt +
  geom_text(
    aes(count, y = reorder(name, count), label = name),
    hjust = ifelse(dat$count < 8, 0, 1), # Dynamic position based on count threshold
    nudge_x = ifelse(dat$count < 8, 0.3, -0.3), # Adjust label positioning
    color = ifelse(dat$count < 8, DARK_GREY, "white"), # Text color based on count
    family = FONT,
    size = 5
  )

plt

ggsave(
  filename = "~/Desktop/EcoClustering/metadata/figures/rural_assets.pdf", # Filename and format
  plot = plt,                       # The plot object to save
  width = 8,                        # Width in inches
  height = 6,                       # Height in inches
  dpi = 300,                         # DPI for high resolution
  device = cario_pdf
)

save(plt, file = "~/Desktop/EcoClustering/metadata/figures/fig3.pdf")

##### Figure 1: Africa Map,  https://gld.gu.se/media/1702/usingrformapmaking_notes.pdf

# Process asset and metadata
asset_data <- data %>%
  group_by(country, asset_category) %>%
  tally() %>%
  pivot_wider(names_from = asset_category, values_from = n) %>%
  mutate(across(everything(), ~replace_na(., 0)))

meta_data <- data %>%
  group_by(country) %>%
  summarize(
    year = mean(year),
    asw = mean(asw),
    percent_rural = mean(percent_rural),
    gini = mean(gini),
    gdp = mean(gdp),
    gdp_per_capita = mean(gdp_per_capita)
  )

# Filter Africa data and merge with metadata
africa <- world[world$region_un == "Africa", ]
africa$country <- africa$name_long
merge_africa <- merge(africa, meta_data, by = "country", all.x = TRUE) # Keep all African countries, even without data

# Set font and color scale
FONT <- "Times New Roman"
LOW_COLOR <- "#D73027" # Red for low Gini
HIGH_COLOR <- "#1A9850" # Green for high Gini
NA_COLOR <- "grey80"    # Gray color for countries without data

# Create the base map plot without the legend
africa.plot <- ggplot(data = merge_africa) +
  geom_sf(aes(fill = gini), color = "grey30", size = 0.2) + # Grey borders for countries
  scale_fill_gradient(
    low = LOW_COLOR,
    high = HIGH_COLOR,
    name = "Gini Index",
    na.value = NA_COLOR
  ) +
  labs(title = "Inequality Distribution in Sub-Saharan Africa") +
  theme_minimal(base_family = FONT) +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(
      family = FONT, size = 18, face = "bold", hjust = 0.5, color = "#333333"
    ),
    legend.position = "none", # Remove default legend
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )

# Extract the legend as a separate grob
legend <- get_legend(
  ggplot(data = merge_africa) +
    geom_sf(aes(fill = gini), color = "grey30", size = 0.2) +
    scale_fill_gradient(
      low = LOW_COLOR,
      high = HIGH_COLOR,
      name = "Gini Index",
      na.value = NA_COLOR,
      guide = guide_colorbar(
        barwidth = 0.5,
        barheight = 6,
        title.position = "top",
        title.hjust = 0.5
      )
    ) +
    theme_void() +
    theme(legend.position = "right")
)

# Combine the plot and the legend in the southwest corner
final_plot <- ggdraw() +
  draw_plot(africa.plot) +
  draw_grob(legend, x = 0.15, y = 0.1, width = 0.2, height = 0.4) # Position in southwest corner

final_plot

###########################
######## ARCHIVED #########
###########################

##### Maybe something with a spineplot:

plot(factor(data$description)~factor(data$asset_category))

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