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
    .cols = starts_with("D1"),
    .fns = ~ replace(.x, .x == 11, NA)                
  ))
#Step 2: removing 99 from B1A; B1B
survey <- survey %>%
  mutate(across(
    .cols = c(B1A, B1B) ,
    .fns = ~replace(.x , .x == 99, NA)
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
  mutate(across(
    .cols = B1B ,
    .fns = ~replace(.x , .x == 99, NA)
  )) %>%
  filter(!is.na(B1B) & B1B !="") %>%
  transmute(QUEST, brand = B1B, type = "Spontaneous" ) %>%
  mutate(
    brand = as.numeric(brand)
  )
#Check for duplicate within spontaneous (should be no data)
dup_spont <- aware %>%
  filter(!is.na(B1B)) %>%
  separate_rows(B1B, sep = "-") %>%
  count(QUEST, B1B, name = "n") %>%
  filter(n > 1) %>%
  rename(brand = B1B)
#Check for duplicate for TOM and spontaneous (should be no data)
tom_spont_conflict <- aware %>%
  filter(!is.na(B1B)) %>%
  separate_rows(B1B, sep = "-") %>%
  inner_join(
    aware %>% select(QUEST, TOM = B1A),
    by = "QUEST"
  ) %>%
  filter(B1B == TOM) %>%
  select(QUEST, brand = TOM)
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
  
#Combining all the awareness
aware_long_complete <- bind_rows(tom, spont, assisted) %>%
  distinct
#Step 4 Recoding C15, C16, N/A and mc
reason_no <- survey %>%
  filter(A1 == 1) %>%
  select(QUEST, C15, C16) %>%
  mutate(across(
    .cols = c(C15, C16),
    .fns = ~ replace(.x, is.na(.x), 99)                
  ))
C15_long <- reason_no %>%
  select(QUEST, C15) %>%
  filter(!is.na(C15)) %>%
  separate_rows(C15, sep = "-") %>%
  filter(!is.na(C15) & C15 !="") %>%
  mutate(
    C15 = as.numeric(C15)
  )
C16_long <- reason_no %>%
  select(QUEST, C16) %>%
  filter(!is.na(C16)) %>%
  separate_rows(C16, sep = "-") %>%
  filter(!is.na(C16) & C16 !="") %>%
  mutate(
    C16 = as.numeric(C16)
  )
#Writing the data
save(survey, aware_long_complete, C15_long, C16_long,
     file = here::here("03_Df_output", "cleaned_data.RData"))

