# MSc Dissertation
# Elizabeth Fitzpatrick
# Environmental Characteristics Associated with
# Whimbrel Stopover Habitat Selection in the Lower Derwent Valley
#
# Script used for:
# - Data processing
# - Summary statistics
# - Spearman correlations
# - Linear regressions
# - Wilcoxon tests
# - Figure production

# LOAD PACKAGES
library(tidyverse)
library(readxl)
library(janitor)
library(vegan)
library(lme4)
library(ggrepel)

# INSTALLING & SORTING THE DATA

file_path <- "Field_data.xlsx"

field_data <- read_excel(file_path, sheet = "fields") %>% clean_names()
pit_data   <- read_excel(file_path, sheet = "pits") %>% clean_names()

str(field_data)
str(pit_data)

# GENERATING PIT MEANS
pit_summary <- pit_data %>%
  group_by(field_id) %>%
  summarise(
    moisture_mean = mean(soil_moisture, na.rm = TRUE),
    worms_mean = mean(earthworms, na.rm = TRUE),
    leather_mean = mean(leatherjackets, na.rm = TRUE),
    penetrometer_mean = mean(penetrometer, na.rm = TRUE),
    sward_mean = mean(sward_height, na.rm = TRUE),
    vess_mean = mean(vess_score, na.rm = TRUE)
  )

# MERGING PIT & FIELD DATA
df <- field_data %>%
  left_join(pit_summary, by = "field_id")

# Rename response for convenience
df <- df %>%
  rename(gps_points = x2024_gps_points)

# Ensure use category is a factor
df$whimbrel_use <- as.factor(df$whimbrel_use)

# Merging prey data - "prey total" = the mean of the earthworms and leatherjackets combined
df <- df %>%
  
  mutate(
    prey_total = worms_mean + leather_mean
  )

# SPEARMAN'S CORRELATION TESTS
#correlations
cor.test(df$clay, df$gps_points, method = "spearman")
cor.test(df$moisture_mean, df$gps_points, method = "spearman")
cor.test(df$penetrometer_mean, df$gps_points, method = "spearman")
cor.test(df$prey_total, df$gps_points, method = "spearman")
cor.test(df$sward_mean, df$gps_points, method = "spearman")
cor.test(df$boundary_score_average, df$gps_points, method = "spearman")

# REGRESSION TESTS
summary(lm(gps_points ~ clay, data = df))
summary(lm(gps_points ~ moisture_mean, data = df))
summary(lm(gps_points ~ penetrometer_mean, data = df))

#2024 versus historic comparisons
table(df$whimbrel_use)

#correlations between variables - i.e. mechanism
cor.test(df$clay, df$moisture_mean, method = "spearman")
cor.test(df$moisture_mean, df$penetrometer_mean, method = "spearman")
cor.test(df$clay, df$penetrometer_mean, method = "spearman")

# WILCOXON TESTS - 2024 versus historic
wilcox.test(clay ~ whimbrel_use, data = df)
wilcox.test(moisture_mean ~ whimbrel_use, data = df)
wilcox.test(penetrometer_mean ~ whimbrel_use, data = df)
wilcox.test(prey_total ~ whimbrel_use, data = df)
wilcox.test(boundary_score_average ~ whimbrel_use, data = df)
wilcox.test(sward_mean ~ whimbrel_use, data = df)
wilcox.test(org_matter ~ whimbrel_use, data = df)

# FORMATTING OF PLOTS
library(ggrepel)

# Consistent colours
use_cols <- c(
  "historic" = "#0072B2",
  "2024"     = "#E69F00"
)

# Global theme
theme_set(
  theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(
        size = 0.2,
        colour = "grey85"
      ),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(colour = "black")
    )
)

# SCATTER PLOTS
# MOISTURE

moisture_plot <- ggplot(
  df,
  aes(
    x = moisture_mean,
    y = gps_points,
    label = field_id,
    colour = whimbrel_use
  )
) +
  
  geom_point(size = 3.5) +
  
  geom_smooth(
    method = "lm",
    se = FALSE,
    colour = "black",
    linewidth = 0.9
  ) +
  
  geom_text_repel(
    size = 3,
    colour = "black",
    show.legend = FALSE
  ) +
  
  scale_colour_manual(values = use_cols) +
  
  labs(
    x = "Mean soil moisture (1–10 index)",
    y = "Whimbrel GPS use"
  ) +
  
  theme(
    legend.position = "none"
  )

moisture_plot

# SAVE PLOT
ggsave(
  "moisture_vs_gps.png",
  plot = moisture_plot,
  width = 6,
  height = 4,
  dpi = 300
)

# CLAY
clay_plot <- ggplot(
  df,
  aes(
    x = clay,
    y = gps_points,
    label = field_id,
    colour = whimbrel_use
  )
) +
  
  geom_point(size = 3.5) +
  
  geom_smooth(
    method = "lm",
    se = FALSE,
    colour = "black",
    linewidth = 0.9
  ) +
  
  geom_text_repel(
    size = 3,
    colour = "black",
    show.legend = FALSE
  ) +
  
  scale_colour_manual(values = use_cols) +
  
  labs(
    x = "Clay (%)",
    y = "Whimbrel GPS use"
  ) +
  
  theme(
    legend.position = "none"
  )

clay_plot

# SAVE PLOT
ggsave(
  "clay_vs_gps.png",
  plot = clay_plot,
  width = 6,
  height = 4,
  dpi = 300
)

# SOIL RESISTANCE
pen_plot <- ggplot(
  df,
  aes(
    x = penetrometer_mean,
    y = gps_points,
    label = field_id,
    colour = whimbrel_use
  )
) +
  
  geom_point(size = 3.5) +
  
  geom_smooth(
    method = "lm",
    se = FALSE,
    colour = "black",
    linewidth = 0.9
  ) +
  
  geom_text_repel(
    size = 3,
    colour = "black",
    show.legend = FALSE
  ) +
  
  scale_colour_manual(values = use_cols) +
  
  labs(
    x = "Mean soil resistance (psi)",
    y = "Whimbrel GPS use"
  ) +
  
  theme(
    legend.position = "none"
  )

pen_plot

# SAVE PLOT
ggsave(
  "pen_vs_gps.png",
  plot = pen_plot,
  width = 6,
  height = 4,
  dpi = 300
)

# MECHANISM PLOT
mechanism_plot <- ggplot(
  df,
  aes(
    x = moisture_mean,
    y = penetrometer_mean,
    label = field_id,
    colour = whimbrel_use
  )
) +
  
  geom_point(size = 3.5) +
  
  geom_smooth(
    method = "lm",
    se = FALSE,
    colour = "black",
    linewidth = 0.9
  ) +
  
  geom_text_repel(
    size = 3,
    colour = "black",
    show.legend = FALSE
  ) +
  
  scale_colour_manual(values = use_cols) +
  
  labs(
    x = "Mean soil moisture (1–10 index)",
    y = "Mean soil resistance (psi)"
  ) +
  
  theme(
    legend.position = "none"
  )

mechanism_plot

# SAVE PLOT
ggsave(
  "mechanism_plot.png",
  plot = mechanism_plot,
  width = 6,
  height = 4,
  dpi = 300
)

#JITTER PLOTS

# assign colours to fields
field_cols <- c(
  "F01" = "#E69F00",
  "F02" = "#E69F00",
  "F03" = "#E69F00",
  "F04" = "#E69F00",
  
  "F05" = "#0072B2",
  "F06" = "#0072B2",
  "F07" = "#0072B2",
  "F08" = "#0072B2"
)

# moisture jitter
moisture_jitter <- ggplot(
  pit_data,
  aes(
    x = field_id,
    y = soil_moisture,
    colour = field_id
  )
) +
  
  geom_jitter(
    width = 0.15,
    size = 2.8,
    alpha = 0.85
  ) +
  
  stat_summary(
    fun = mean,
    geom = "point",
    colour = "black",
    shape = 18,
    size = 3.5
  ) +
  
  scale_colour_manual(values = field_cols) +
  
  labs(
    x = "Field ID",
    y = "Soil moisture (1–10 index)"
  ) +
  
  theme(
    legend.position = "none"
  )

moisture_jitter

# SAVE PLOT
ggsave(
  "moisture_jitter.png",
  plot = moisture_jitter,
  width = 6,
  height = 4,
  dpi = 300
)

# PENETROMETER
pen_jitter <- ggplot(
  pit_data,
  aes(
    x = field_id,
    y = penetrometer,
    colour = field_id
  )
) +
  
  geom_jitter(
    width = 0.15,
    size = 2.8,
    alpha = 0.85
  ) +
  
  stat_summary(
    fun = mean,
    geom = "point",
    colour = "black",
    shape = 18,
    size = 3.5
  ) +
  
  scale_colour_manual(values = field_cols) +
  
  labs(
    x = "Field ID",
    y = "Soil resistance (psi)"
  ) +
  
  theme(
    legend.position = "none"
  )

pen_jitter

# SAVE PLOT
ggsave(
  "pen_jitter.png",
  plot = pen_jitter,
  width = 6,
  height = 4,
  dpi = 300
)

# sward height jitter
sward_jitter <- ggplot(
  pit_data,
  aes(
    x = field_id,
    y = sward_height,
    colour = field_id
  )
) +
  
  geom_jitter(
    width = 0.15,
    size = 2.8,
    alpha = 0.85
  ) +
  
  stat_summary(
    fun = mean,
    geom = "point",
    colour = "black",
    shape = 18,
    size = 3.5
  ) +
  
  scale_colour_manual(values = field_cols) +
  
  labs(
    x = "Field ID",
    y = "Sward height (cm)"
  ) +
  
  theme(
    legend.position = "none"
  )

sward_jitter

# SAVE PLOT
ggsave(
  "sward_jitter.png",
  plot = sward_jitter,
  width = 6,
  height = 4,
  dpi = 300
)

#prey jitter
pit_data <- pit_data %>%
  mutate(prey_total = earthworms + leatherjackets)

prey_jitter <- ggplot(
  pit_data,
  aes(
    x = field_id,
    y = prey_total,
    colour = field_id
  )
) +
  
  geom_jitter(
    width = 0.15,
    size = 2.8,
    alpha = 0.85
  ) +
  
  stat_summary(
    fun = mean,
    geom = "point",
    colour = "black",
    shape = 18,
    size = 3.5
  ) +
  
  scale_colour_manual(values = field_cols) +
  
  labs(
    x = "Field ID",
    y = "Prey abundance"
  ) +
  
  theme(
    legend.position = "none"
  )

prey_jitter

# SAVE PLOT
ggsave(
  "prey_jitter.png",
  plot = prey_jitter,
  width = 6,
  height = 4,
  dpi = 300
)
