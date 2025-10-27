#Input cleaned dataframe
#Output: multiple flipped_barplot (A4B,C13) for the test
#Load library
library(here)
library(readxl)
library(tidyverse)
library(openxlsx)
#load data
load(here::here("03_Df_output", "cleaned_data.RData"))
rm(aware_long_complete, C15_long, C16_long,C11_long)
excel_path <- here::here("02_Input", "code_to_label.xlsx")
barplot_var <- read_excel(here::here("02_Input", "graphic_control.xlsx"), sheet = "barplot")
#Load function
round_excel <- function(x, digits = 0) {
  posneg <- sign(x)
  z <- abs(x) * 10^digits
  z <- z + 0.5
  z <- trunc(z)
  z <- z / 10^digits
  z * posneg
}