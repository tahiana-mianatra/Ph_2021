#Input: cleaned dataframe
#Output : Awareness per profile barplot
#Load library
library(here)
library(tidyverse)
library(readxl)
library(ggimage)
library(scales)
library(magick)
library(grid)
library(ggtext)

load(here::here("03_Df_output", "cleaned_data.RData"))
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
