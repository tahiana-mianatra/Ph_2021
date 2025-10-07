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
