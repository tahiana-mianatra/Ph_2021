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
#Step 2: removing 99 from B1A; B1B
survey <- survey %>%
  mutate(across(
    .cols = c(B1A, B1B) ,
    .fns = ~replace(.x , x == 99, NA)
  ))
#Step 3 : preparing brand awareness long format
aware <- survey %>%
  select(QUEST,B1A, B1B)
tom <- aware %>%
  filter(!is.na(B1A) & B1A != "") %>%
  transmute(QUEST, brand = B1A , type = "Top of Mind" )

#Spontaneous
spont <- aware %>%
  filter(!is.na(B1B)) %>%
  separate_rows(B1B, sep = "-") %>%
  filter(!is.na(B1B) & B1B =="") %>%
  transmute(QUEST, brand = B1B, type = "Spontaneous" ) %>%
  mutate(
    brand = as.numeric(brand)
  )
 #Assisted awareness
assisted <- survey %>%
  select(QUEST, starts_with("B2_")) %>%
  pivot_longer(
    cols = starts_with("B2_"),
    names_to = "brand_code" ,
    values_to = "awareness"
  ) %>%
  filter( awareness == 1) %>%
  mutate(
    brand = as.numeric(str_remove(brand_code, "B2_")),
    type = "Assisted"
  ) %>%
  select(QUEST, brand, type)
  
  