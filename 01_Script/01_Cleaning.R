#Input : Df from the result of the survey
#Output : Cleaned df ready for analysis
#Load library
#Portfolio: Satisfaction (D1A), Using the product vs substitute(C1), Top of mind profil(B1xA3xA4BxA4E), 
library(readxl)
library(openxlsx)
library(tidyverse)
library(here)
#Load data
gross_data<- read_excel(here::here("02_Input", "Base_VF.xlsx"))
#Step 1: removing 11 from D1
survey <- gross_data %>%
  mutate(across(
    .cols = D1A ,
    .fns = ~ replace(.x, .x == 11, NA)                
  ))
  
#Step 2 : preparing brand awareness long format
aware <- survey %>%
  select(QUEST, )