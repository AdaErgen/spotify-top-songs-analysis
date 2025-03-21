if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("conflicted", quietly = TRUE)) install.packages("conflicted")
if (!requireNamespace("magrittr", quietly = TRUE)) install.packages("magrittr")

library(tidyverse)
library(ggplot2)
library(conflicted)
library(magrittr)

conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")



spotify_data <- read.csv("universal_top_spotify_songs.csv")
spotify_data <- spotify_data[1:100, ]



spotify_data$snapshot_date <- as.Date(spotify_data$snapshot_date)


spotify_data <- spotify_data[!is.na(spotify_data$popularity) & !is.na(spotify_data$danceability) & !is.na(spotify_data$duration_ms), ]


print("Data Summary:")
print(summary(spotify_data))


# Hypothesis 1: Relationship between popularity and danceability
# Correlation test
cor_test_dance <- cor.test(spotify_data$danceability, spotify_data$popularity, method = "pearson")
print(cor_test_dance)

# Scatter plot
dance_plot <- ggplot(spotify_data, aes(x = danceability, y = popularity)) +
  geom_point(alpha = 0.5, color = "#FFADAD") +
  geom_smooth(method = "lm", se = FALSE, color = "#BDB2FF") +
  labs(title = "Relationship between Danceability and Popularity",
       x = "Danceability",
       y = "Popularity") +
  theme_minimal()
print(dance_plot)

# Hypothesis 2: Relationship between popularity and energy
# Correlation test
cor_test_energy <- cor.test(spotify_data$energy, spotify_data$popularity, method = "pearson")
print(cor_test_energy)

# Scatter plot
energy_plot <- ggplot(spotify_data, aes(x = energy, y = popularity)) +
  geom_point(alpha = 0.5, color = "#80B1D3") +
  geom_smooth(method = "lm", se = FALSE, color = "#FDB462") +
  labs(title = "Relationship between Energy and Popularity",
       x = "Energy",
       y = "Popularity") +
  theme_minimal()
print(energy_plot)


# Hypothesis 3: Longer songs have lower popularity
# Correlation test
cor_test_duration <- cor.test(spotify_data$duration_ms, spotify_data$popularity, method = "pearson")
print(cor_test_duration)

# Categorizing song durations
spotify_data$duration_category <- cut(spotify_data$duration_ms,
                                      breaks = c(0, 180000, 240000, Inf),
                                      labels = c("Short", "Medium", "Long"))

# ANOVA test
anova_duration <- aov(popularity ~ duration_category, data = spotify_data)
summary(anova_duration)

# Boxplot of popularity by duration category
duration_plot <- ggplot(spotify_data, aes(x = duration_category, y = popularity, fill = duration_category)) +
  geom_boxplot() +
  labs(title = "Song Durations and Popularity",
       x = "Duration Category",
       y = "Popularity") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal()
print(duration_plot)

# Checking for NA values after filtering
print("Checking for NA values:")
print(sapply(spotify_data, function(x) sum(is.na(x))))

# Verifying data structure
print("Data Structure:")
print(str(spotify_data))



ggsave("danceability_vs_popularity.png", plot = dance_plot, width = 8, height = 6)
ggsave("energy_vs_popularity.png", plot = energy_plot, width = 8, height = 6)
ggsave("duration_vs_popularity.png", plot = duration_plot, width = 8, height = 6)









