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
rm(aware_long_complete)
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
#Select the relevant variable (QUEST, C1, C15, C16)
rate <- survey %>%
  filter(A1 == 1) %>%
  select(QUEST, C1)
#Prepare the data for plotting
C1_plot <- rate %>%
  group_by(C1) %>%
  summarise(
    number = n()) %>%
  mutate(
    total_respondent = n_distinct(rate$QUEST),
    percentage = number / total_respondent * 100 ,
    perc_label = round_excel(percentage, digits = 0)
  )
C15_plot <- C15_long %>%
  group_by(C15) %>%
  summarise(
    number = n()) %>%
  mutate(
    total_respondent = n_distinct(rate$QUEST),
    percentage = number / total_respondent * 100 ,
    perc_label = round_excel(percentage, digits = 0)
  )
C16_plot <- C16_long %>%
  group_by(C16) %>%
  summarise(
    number = n()) %>%
  mutate(
    total_respondent = n_distinct(rate$QUEST),
    percentage = number / total_respondent * 100 ,
    perc_label = round_excel(percentage, digits = 0)
  )
#Labeling code for plot
C1_plot <- C1_plot %>%
  left_join(survey_labels[["C1"]], by = c("C1" = "code")) %>%
  mutate( C1 = factor(C1,
                      levels = c("1", "2", "3", "4"),
                      ordered = TRUE)) %>%
  relocate(label, .after = C1) %>%
  mutate(label = factor(label, levels = label[order(C1)], ordered = TRUE))
#Plot
p <- C1_plot %>%
  ggplot(aes( x = "", y = percentage, fill = label))+
  geom_bar( stat = "identity", width = 1)+
  coord_polar("y")+
  geom_text(
    aes(label = paste(perc_label,"%", sep="")),
        position = position_stack(vjust = 0.5),
        color = "white",
        size = 4
  )+
  scale_fill_manual(values = c(
    "Using only deodorant" = "#24A10B",
    "Usually deodorant but also using other product" = "#4FC3F7",
    "Have used deodorant before but not often" = "#1976D2",
    "Have never used deodorant" = "#FF5722"
  ))+
  labs(
    caption = "Base: General Public n = 600",
    title = "Market penetration rate") +
  theme_void()+
  theme(plot.title = element_text(hjust = 0.5,
                                  size = 14,
                                  face = "bold"),
        plot.caption = element_text(
          size = 12,                    # Larger font size
          hjust = 0.5,                  # Center alignment (0=left, 0.5=center, 1=right)
          vjust = 1,                    # Vertical position
          face = "italic",              # Optional: italic style
          margin = margin(t = 10)       # Add space above caption
        ))
print(p)

