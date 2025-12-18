library(readxl)
library(openxlsx)
library(tidyverse)
library(here)
#Load data
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
#Summarize
normal <- survey %>% 
  filter(A1 == 1) %>% 
  select(QUEST,A4B, A4E)

excel_path <- here::here("02_Input", "Code_to_label.xlsx")
# Get sheet names
sheet_names <- excel_sheets(excel_path)

# Read all sheets into a named list
survey_labels <- map(setNames(sheet_names, sheet_names), 
                     ~read_excel(excel_path, sheet = .x))  
  
common_vars <- intersect(names(normal), names(survey_labels))
complete_coded <- normal
for (var in common_vars) {
  # Get the code-to-label mapping for the current variable
  code_mapping <- survey_labels[[var]] %>%
    select(code, label) %>%   # Note: Now Code is first (the key for joining)
    arrange(code)
  
  # Join to replace codes with French labels
  complete_coded <- complete_coded %>%
    left_join(code_mapping, by = setNames("code", var)) %>%
    mutate(
      !!var := factor(label, 
                      levels = unique(code_mapping$label),  # Preserve Excel order
                      ordered = TRUE)  # Makes it an ordered factor
    ) %>%
    select(-label)  # Drop the temporary label column
}

#Summary
job <- complete_coded %>% 
  group_by(A4B) %>% 
  summarise(
    count = n_distinct(QUEST)
  ) %>% 
  mutate(
    prop = paste0(round_excel(count *100 / sum(count)), "%")  
  )
 
degree <- complete_coded %>% 
  group_by(A4E) %>% 
  summarise(
    count = n_distinct(QUEST)
  )  %>% 
  mutate(
    prop = paste0(round_excel(count *100 / sum(count)), "%") 
  )
