#Input cleaned dataframe
#Output: multiple barplot (C12,C14) for the test
#Load library
library(here)
library(readxl)
library(tidyverse)
library(openxlsx)
#load data
load(here::here("03_Df_output", "cleaned_data.RData"))
rm(aware_long_complete, C15_long, C16_long,C11_long)

