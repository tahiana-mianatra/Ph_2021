#Input: cleaned dataframe
#Output : barplot for average score
#Load library
library(here)
library(tidyverse)
library(readxl)
library(ggimage)
library(scales)
library(magick)
library(grid)
library(ggtext)

#Load data
load(here::here("03_Df_output", "cleaned_data.RData"))
rm(aware_long_complete, C15_long, C16_long)
#Load function
round_excel <- function(x, digits = 0) {
  posneg <- sign(x)
  z <- abs(x) * 10^digits
  z <- z + 0.5
  z <- trunc(z)
  z <- z / 10^digits
  z * posneg
}
shift_trans = function(d = 0) {
  scales::trans_new("shift", transform = function(x) x - d, inverse = function(x) x + d)
}
#Select all the note of satisfaction
note <- survey %>%
  select(QUEST, starts_with("D1") )
#Build the dataframe that we will ggplot
mean_score <- note %>%
  summarise(across(
    -QUEST, ~ round_excel(mean(., na.rm = TRUE), digits = 1))) %>%
  pivot_longer(
    everything(), 
    names_to = "brand", 
    values_to = "mean"
  )
#sd
sd_score <- note %>%
  summarise(across(
    -QUEST, ~ round_excel(sd(., na.rm = TRUE), digits = 1))) %>%
  pivot_longer(
    everything(), 
    names_to = "brand", 
    values_to = "sd"
  )
# Calculate sample sizes correctly
n_score <- note %>%
  summarise(across(
    -QUEST, ~ sum(!is.na(.)))) %>%  # Count non-NA values
  pivot_longer(
    everything(), 
    names_to = "brand", 
    values_to = "size"
  )
#Welch's t-test because the sample is different for each note from another
#The question is: Is D1A good or bad compare to each other brand
pval_score <- note %>%
  pivot_longer(-QUEST, names_to = "brand", values_to = "score") %>%
  group_by(brand) %>%
  summarise(
    pvalue = if (unique(brand) == "D1A") NA_real_ else {
      x <- score[!is.na(score)]
      y <- note$D1A[!is.na(note$D1A)]
      if (length(x) > 1 && length(y) > 1) {
        t.test(x, y, var.equal = FALSE)$p.value
      } else {
        NA_real_
      }
    },
    .groups = "drop"
  )

comparison_score <- mean_score %>%
  left_join(pval_score, by = "brand") %>%
  mutate(
    comparison = case_when(
      is.na(pvalue) ~ NA_character_,
      mean > mean[brand == "D1A"] & pvalue < 0.05 ~ "superior",
      mean < mean[brand == "D1A"] & pvalue < 0.05 ~ "inferior",
      TRUE ~ "no_difference"
    )
  ) %>%
  select(brand, comparison)

score <- mean_score %>%
  left_join(sd_score, by = "brand") %>%
  left_join(n_score, by = "brand") %>%
  left_join(pval_score, by = "brand") %>%
  left_join(comparison_score, by = "brand")
rm(mean_score, sd_score,n_score, pval_score, comparison_score)
#Renaming the Brand
Brand_names <- c(
  D1A = "Brand1",
  D1B = "Brand2",
  D1C = "Brand3",
  D1D= "Brand4",
  D1E = "Brand5"
)
#Changing label names and adding label brand n = on the df
score <- score %>%
  mutate(
    brand = factor(Brand_names[brand], levels = Brand_names),
    plot_label = paste(brand, "n =", size*3) 
  )
#Plot
p <- score %>%
  ggplot(aes(x = brand, y = mean, fill = brand)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(
    data = score %>% filter(comparison == "no_difference" | is.na(comparison)),
    aes(label = mean),
    vjust = -0.5
  ) +
  geom_label(
    data = score %>% filter(comparison == "inferior"),
    aes(label = mean),
    vjust = -0.5,
    color = "red",
    fill = "white"
  ) +
  labs( caption = "Base: Respondents who have rated each brand", title = "Satisfaction note") +
  geom_hline(yintercept = 6.8, linetype = "solid", color = "red", linewidth = 1) +
  scale_y_continuous(limits = c(0, 10),
                     breaks = c(0, 6.8, 10),
                     trans = shift_trans(6.8)) +
  scale_x_discrete(labels = score$plot_label) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    panel.grid = element_blank(),
    axis.text.x = element_text(face = "bold", colour = "black"),
    axis.text.y = element_blank(),
    plot.caption = element_text(
      size = 12,                    # Larger font size
      hjust = 0.5,                  # Center alignment (0=left, 0.5=center, 1=right)
      vjust = 1,                    # Vertical position
      face = "italic",              # Optional: italic style
      margin = margin(t = 10)       # Add space above caption
    ),
    legend.position = "none"
  )

print(p)
ggsave(here::here("04_Graphic_output", "satisfaction.png"),
       plot = p, width = 10, height = 6, dpi = 600)
