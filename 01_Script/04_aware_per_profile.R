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
rm(C15_long, C16_long)
excel_path <- here::here("02_Input", "Code_to_label.xlsx") # Replace with your actual file path

# Get sheet names
sheet_names <- excel_sheets(excel_path)

# Read all sheets into a named list
survey_labels <- map(setNames(sheet_names, sheet_names), 
                     ~read_excel(excel_path, sheet = .x))
# If you need to convert from tibble to dataframe (not always necessary)
survey_labels <- map(survey_labels, as.data.frame)
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

#Profil variable A3(Age), A4E (Education), A4B (CSP)
profile <- survey %>%
  filter( A1 == 1) %>%
  select(QUEST, A3B, A4E, A4B)
#Creating age_group
profile <- profile %>%
  mutate(age_group = factor(case_when(
    A3B >= 18 & A3B <= 24 ~ "18-24",
    A3B >= 25 & A3B <= 34 ~ "25-34", 
    A3B >= 35 & A3B <= 44 ~ "35-44",
    A3B >= 45 & A3B <= 59 ~ "45-59",
    A3B >= 60 ~ "60+",
    TRUE ~ NA_character_
  ), levels = c("18-24", "25-34", "35-44", "45-59", "60+")))

#creating Socioprofessional_group
profile <- profile %>%
  mutate(CSP = factor(case_when(
    A4B %in% c(1,2) ~ "Upper-middle class professionals",
    A4B %in% c(3,4,5,6,10) ~ "Middle-income occupations", 
    A4B %in% c(7,8,9) ~ "Low-skilled occupations",
    A4B == 11 ~ "Economically inactive"
  ), levels = c("Upper-middle class professionals", 
                "Middle-income occupations", 
                "Low-skilled occupations", 
                "Economically inactive")))
#Labeling A4E
profile <- profile %>%
  left_join(survey_labels[["A4E"]], by = c("A4E" = "code")) %>%
  mutate( A4E = factor(A4E,
                      levels = c("1", "2", "3", "4", "5", "6"),
                      ordered = TRUE)) %>%
  rename(educ = label) %>%
  mutate(educ = factor(educ, 
                       levels = c("Master's degree or higher", 
                                  "Bachelor's degree", 
                                  "Upper secondary", 
                                  "Lower secondary", 
                                  "Primary or no education"), 
                       ordered = TRUE))
# First, identify which QUEST IDs have brand 1 as Top of Mind
tom_1_respondents <- aware_long_complete %>%
  filter(brand == 1 & type == "Top of Mind") %>%
  distinct(QUEST) %>%
  pull(QUEST)

# Then create the binary variable in profile
profile <- profile %>%
  mutate(TOM_1 = if_else(QUEST %in% tom_1_respondents, 1, 0))
rm(tom_1_respondents)

#Summarize table
summary_df <- bind_rows(
  # Overall
  profile %>%
    summarise(group = "Overall", 
              number = sum(TOM_1 == 1),
              total = n(),
              percentage = number/total * 100),
  
  # By age group
  profile %>%
    group_by(group = age_group) %>%
    summarise(number = sum(TOM_1 == 1),
              total = n(),
              percentage = number/total * 100) %>%
    ungroup(),
  
  # By education
  profile %>%
    group_by(group = educ) %>%
    summarise(number = sum(TOM_1 == 1),
              total = n(),
              percentage = number/total * 100) %>%
    ungroup(),
  
  # By CSP
  profile %>%
    group_by(group = CSP) %>%
    summarise(number = sum(TOM_1 == 1),
              total = n(),
              percentage = number/total * 100) %>%
    ungroup()
)

