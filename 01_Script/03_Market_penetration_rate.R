#Input: Cleaned dataframe
#Output: Market penetration rate barplot
#Load library
library(here)
library(tidyverse)
library(readxl)
library(ggimage)
library(scales)
library(magick)
library(grid)
library(ggtext)
library(ggforce)
library(patchwork)
#Load data
load(here::here("03_Df_output", "cleaned_data.RData"))
rm(aware_long_complete)
excel_path <- here::here("02_Input", "Code_to_label.xlsx") # Replace with your actual file path

# Get sheet names
sheet_names <- excel_sheets(excel_path)

# Read all sheets into a named list
survey_labels <- map(setNames(sheet_names, sheet_names), 
                     ~read_excel(excel_path, sheet = .x))
# If you need to convert from tibble to dataframe (not always necessary)
survey_labels <- map(survey_labels, as.data.frame)
#Load function
round_excel <- function(x, digits = 0) {
  posneg <- sign(x)
  z <- abs(x) * 10^digits
  z <- z + 0.5
  z <- trunc(z)
  z <- z / 10^digits
  z * posneg
}
#Select the relevant variable (QUEST, C1, C15, C16)
rate <- survey %>%
  filter(A1 == 1) %>%
  select(QUEST, C1)
#Prepare the data for plotting
C1_plot <- rate %>%
  group_by(C1) %>%
  summarise(
    number = n()) %>%
  mutate(
    total_respondent = n_distinct(rate$QUEST),
    percentage = number / total_respondent * 100 ,
    perc_label = round_excel(percentage, digits = 0)
  )
C15_plot <- C15_long %>%
  group_by(C15) %>%
  summarise(
    number = n()) %>%
  mutate(
    total_respondent = n_distinct(rate$QUEST),
    percentage = number / total_respondent * 100 ,
    perc_label = round_excel(percentage, digits = 0)
  )
C16_plot <- C16_long %>%
  group_by(C16) %>%
  summarise(
    number = n()) %>%
  mutate(
    total_respondent = n_distinct(rate$QUEST),
    percentage = number / total_respondent * 100 ,
    perc_label = round_excel(percentage, digits = 0)
  )
#Labeling code for plot
C1_plot <- C1_plot %>%
  left_join(survey_labels[["C1"]], by = c("C1" = "code")) %>%
  mutate( C1 = factor(C1,
                      levels = c("1", "2", "3", "4"),
                      ordered = TRUE)) %>%
  relocate(label, .after = C1) %>%
  mutate(label = factor(label, levels = label[order(C1)], ordered = TRUE))
C15_plot <- C15_plot %>%
  left_join(survey_labels[["C15"]], by = c("C15" = "code")) %>%
  mutate(C15 = factor(C15,
                      levels = c("1", "2", "3", "4", "95", "99"),
                      ordered = TRUE),
         # Create a custom order: "Are using deodorant" last, others by percentage
         custom_order = ifelse(
           label == "Are using deodorant", 
           Inf,  # Always last
           -percentage  # Others sorted by percentage (descending)
         )) %>%
  relocate(label, .after = C15) %>%
  mutate(label = factor(label, levels = label[order(C15)], ordered = TRUE))
C16_plot <- C16_plot %>%
  left_join(survey_labels[["C16"]], by = c("C16" = "code")) %>%
  mutate(C15 = factor(C16,
                      levels = c("1", "2", "3", "4", "99"),
                      ordered = TRUE),
         # Create a custom order: "Are using deodorant" last, others by percentage
         custom_order = ifelse(
           label == "Are using deodorant", 
           Inf,  # Always last
           -percentage  # Others sorted by percentage (descending)
         ))%>%
  relocate(label, .after = C16) %>%
  mutate(label = factor(label, levels = label[order(C16)], ordered = TRUE))
# Prepare the data with proper angles
C1_plot <- C1_plot %>%
  mutate(exploded = ifelse(label == "Have never used deodorant", 0.2, 0))
C1_plot <- C1_plot %>%
  arrange(desc(label)) %>%
  mutate(
    # Calculate cumulative percentages for angles
    cum_percentage = cumsum(percentage),
    start_angle = 2 * pi * lag(cum_percentage, default = 0) / 100,
    end_angle = 2 * pi * cum_percentage / 100,
    mid_angle = (start_angle + end_angle) / 2,
    # Explosion distance
    explosion = ifelse(label == "Have never used deodorant", 0.2, 0),
    # Calculate new center positions for exploded slice
    x0 = explosion * sin(mid_angle),
    y0 = explosion * cos(mid_angle)
  )
#Plot
p <- ggplot(C1_plot) +
  geom_arc_bar(
    aes(
      x0 = x0, 
      y0 = y0,
      r0 = 0,
      r = 1,
      start = start_angle,
      end = end_angle,
      fill = label
    ),
    color = "white",
    linewidth = 0.5
  ) +
  coord_fixed() +
  # Adjust text positions - use smaller multiplier
  geom_text(
    aes(
      x = (0.8 + explosion) * sin(mid_angle),  # Reduced from 1.3 to 0.8
      y = (0.8 + explosion) * cos(mid_angle),  # Reduced from 1.3 to 0.8
      label = paste0(perc_label, "%")
    ),
    color = "white",  # Back to white now that we can see it
    size = 4,
    fontface = "bold",
    inherit.aes = FALSE
  ) +
  scale_fill_manual(values = c(
    "Using only deodorant" = "#6BF20A",
    "Usually deodorant but also using other product" = "#4FC3F7",
    "Have used deodorant before but not often" = "#1976D2", 
    "Have never used deodorant" = "#FF5722"
  )) +
  labs(
    title = "Market penetration rate",
    fill = "Deodorant usage"
  ) +
  theme_void() +
  theme(
    legend.position = "left" ,
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.caption = element_text(
      size = 12,
      hjust = 0.5,
      vjust = 1,
      face = "italic",
      margin = margin(t = 10)
    )
  )
print(p)
#Plot 2
p2 <- C15_plot %>%
  ggplot(aes( x= reorder(label, -custom_order), y = percentage,
              fill = label == "Are using deodorant"))+
  coord_flip()+
  geom_bar(stat = "identity" , show.legend = FALSE)+
  geom_text(
    data = C15_plot %>% filter(percentage > 5),
    aes(y = percentage / 2 ,
      label = paste0(perc_label, "%")),
    color = "white"
    )+
  geom_text(
    data = C15_plot %>% filter (percentage < 5),
    aes(y = percentage + 2,
      label = paste0(perc_label, "%")),
    color = "black",
  )+
  scale_fill_manual(
    values = c("FALSE" = "#FF5722", "TRUE" = "gray"),
    guide = "none"  # Hide the legend since it's not meaningful
  ) +
  labs(
    title = "Non-User Analysis: Key Adoption Barriers",
    fill = "Barrier Categories"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text.y = element_text(face = "bold", colour = "black"),
    plot.caption = element_text(
      size = 12,
      hjust = 0.5,
      vjust = 1,
      face = "italic"
    )
  )
print(p2)
p3 <- C16_plot %>%
  ggplot(aes( x= reorder(label, -custom_order), y = percentage,
              fill = label == "Are using deodorant"))+
  geom_bar(stat = "identity" , show.legend = FALSE)+
  coord_flip()+
  geom_text(
    data = C16_plot %>% filter(percentage > 5),
    aes(y = percentage / 2 ,
        label = paste0(perc_label, "%")),
    color = "white"
  )+
  geom_text(
    data = C16_plot %>% filter (percentage < 5),
    aes(y = percentage + 1,
        label = paste0(perc_label, "%")),
    color = "black",
  )+
  scale_fill_manual(
    values = c("FALSE" = "#FF5722", "TRUE" = "gray"),
    guide = "none"  # Hide the legend since it's not meaningful
  ) +
  labs(
    title = "Market Substitutes: Deodorant Alternatives"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text.y = element_text(face = "bold", colour = "black"),
    plot.caption = element_text(
      size = 12,
      hjust = 0.5,
      vjust = 1,
      face = "italic"
    )
  )
print(p3)
#patching p to p2, then p to p3
arrow_plot <- ggplot() +
  annotate("segment", x = 0, xend = 1, y = 0.5, yend = 0.5,
           arrow = arrow(type = "closed", length = unit(0.3, "inches")),
           color = "#FF5722", size = 2) +
  theme_void() +
  theme(plot.margin = margin(0, 10, 0, 10))
print(arrow_plot)
# Create the first combination: p1 and p2 side by side
combo1 <- p + arrow_plot + p2 + 
  plot_layout(widths = c(1,0.2,1)) + # Equal widths
  plot_annotation(
    caption = "Base: General Public n = 600 NB: Multiple choice on barrier so total may exceed 100%",
    theme = theme(
      plot.caption = element_text(
        size = 12,
        hjust = 0.5,
        vjust = 1,
        face = "italic"
      )
    )
  )
print(combo1)

# Create the second combination: p1 and p3 side by side
combo2 <- p + arrow_plot+ p3 + 
  plot_layout(widths = c(1,0.2, 1)) +
  plot_annotation(
    caption = "Base: General Public n = 600 NB: Multiple choice on alternatives so total may exceed 100%",
    theme = theme(
      plot.caption = element_text(
        size = 12,
        hjust = 0.5,
        vjust = 1,
        face = "italic"
      )
    )
  )

print(combo2)

ggsave(
  here::here("04_Graphic_output", "market_penetration_to_barriers.png"),
  plot = combo1, 
  width = 16,    # Wider to accommodate 2 plots + arrow
  height = 8,    # Slightly taller for better proportions
  dpi = 600
)

ggsave(
  here::here("04_Graphic_output", "market_penetration_to_balternative.png"),
  plot = combo2, 
  width = 16,    # Wider to accommodate 2 plots + arrow
  height = 8,    # Slightly taller for better proportions
  dpi = 600
)
