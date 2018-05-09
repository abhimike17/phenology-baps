library(tidyverse)
library(ggplot2)
library(ggridges)
library(jsonlite)
library(viridis)

# transform the BIS emitted JSON into something ggplot2 can work with
yell <- read_json("YellowstoneNP-1981-2016-processed-numbers.json", simplifyDataFrame = TRUE, simplifyVector = TRUE, flatten = TRUE)
yelldf <- as_tibble(yell)
yellt <- gather(yelldf, Year, DOY)

# produce a histogram for all years
ggplot(yellt, aes(DOY)) +
  geom_histogram(binwidth = 1, color = "grey", fill = "lightblue") +
  ggtitle("DRAFT: Histogram of Spring Index, Yellowstone National Park (1981 to 2016)") +
  geom_vline(aes(xintercept=mean(DOY, na.rm=T)), color = "red", linetype = "dotted", size = 0.5) +
  geom_vline(aes(xintercept = min(DOY, na.rm=T)), color = "green", linetype = "dotted", size = 0.5) +
  geom_vline(aes(xintercept = max(DOY, na.rm=T)), color = "green", linetype = "dotted", size = 0.5)

# density trace for each year
ggplot(yellt, aes(DOY)) +
  geom_density(fill = "lightblue") +
  geom_vline(aes(xintercept = mean(DOY, na.rm=T)), color = "red", linetype = "dotted", size = 0.5) +
  facet_wrap(~ Year, ncol = 10)

# boxplot for each year
ggplot(yellt, aes(y = DOY, x = Year, group = Year)) +
  geom_boxplot() +
  geom_hline(aes(yintercept = median(DOY, na.rm=T)), color = "blue", linetype = "dotted", size = 0.5) +
  ggtitle("DRAFT: Boxplot of Spring Index, Yellowstone National Park (1981 to 2016)")

# ridgeline plot for each year
ggplot(yellt, aes(x = DOY, y = Year, group = Year)) +
  geom_density_ridges(scale = 4, fill = "lightblue") +
  theme_minimal() + ggtitle("DRAFT: Ridgeline Plot of Spring Index, Yellowstone National Park (1981 to 2016)")

# ridgeplot with gradient for each year
ggplot(yellt, aes(x = DOY, y = Year, group = Year, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, gradient_lwd = 1.0, from = 80, to = 180) +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_continuous(expand = c(0.01, 0)) +
  scale_fill_viridis(name = "Day of\nYear", option = "D", direction = -1) +
  labs(title = 'DRAFT: Spring Index, Yellowstone National Park',
       subtitle = 'Annual Spring Index by Year for the Period 1981 to 2016\nModel Results from the USA National Phenology Network',
       y = 'Year',
       x = 'Spring Index (Day of Year)',
       caption = "(model results retrieved 2018-01-26)") +
  theme_ridges(font_size = 12, grid = TRUE) +
  geom_vline(aes(xintercept = mean(DOY, na.rm=T)), color = "red", linetype = "dotted", size = 0.5) +
  geom_vline(aes(xintercept = min(DOY, na.rm=T)), color = "green", linetype = "dotted", size = 0.5) +
  geom_vline(aes(xintercept = max(DOY, na.rm=T)), color = "green", linetype = "dotted", size = 0.5) 
