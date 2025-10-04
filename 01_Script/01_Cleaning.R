#Input : Df from the result of the survey
#Output : Cleaned df ready for analysis
#Load library
#Portfolio: Satisfaction (D1), Using the product vs substitute(C1), Top of mind profil(B1xA3xA4BxA4E), 
library(readxl)
library(openxlsx)
library(tidyverse)
library(here)
#Load data
gross_data<- read_excel(here::here("02_Input", "Base_VF.xlsx"))
#