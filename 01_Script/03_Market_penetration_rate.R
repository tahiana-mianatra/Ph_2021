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
#Load data
load(here::here("03_Df_output", "cleaned_data.RData"))
rm("aware_long_complete")
#Select the relevant variable (QUEST, C1, C15, C16)
rate <- survey %>%
  filter(A1 == 1) %>%
  select(QUEST, C1, C15, C16)
#Prepare the data for plotting

#Plot