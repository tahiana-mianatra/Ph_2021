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
rm("aware_long_complete")
#Load function
round_excel <- function(x, digits = 0) {
  posneg <- sign(x)
  z <- abs(x) * 10^digits
  z <- z + 0.5
  z <- trunc(z)
  z <- z / 10^digits
  z * posneg
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
sd_score <- note %>%
  summarise(across(
    -QUEST, ~ round_excel(sd(., na.rm = TRUE), digits = 1))) %>%
  pivot_longer(
    everything(), 
    names_to = "brand", 
    values_to = "sd"
  )

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

