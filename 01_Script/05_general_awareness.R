#Input: cleaned dataframe; aware_long
#Out: Brand awareness stacked barplot
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
rm(C15_long, C16_long)
code_to_label <- read_excel(here::here("02_Input", "Code_to_label.xlsx"), sheet = "Brand_code")
#Load function
round_excel <- function(x, digits = 0) {
  posneg <- sign(x)
  z <- abs(x) * 10^digits
  z <- z + 0.5
  z <- trunc(z)
  z <- z / 10^digits
  z * posneg
}
#Just replace aware_long_complete because it is too long
aware_df <-aware_long_complete
rm(aware_long_complete)
to_get_total <- survey %>%
  filter(A1 == 1)
#Arrange the data
brand_see <- aware_df %>%
  # First, get the total distinct respondents
  mutate(total = n_distinct(to_get_total$QUEST)) %>% #better get the total from other df because not everyone can quote brand
  # Then count by type and brand
  count(type, brand, total, name = "n") %>%
  arrange(brand, type) %>%
  relocate(total, .after = last_col()) %>% #relocate the total column at the end
  mutate(
    percentage = n / total * 100,  # Calculate percentage
    perc_text = round_excel(percentage, 1)  # Excel-style rounded text
  )


#Coding the brand_see  
brand_see <- brand_see %>%
  left_join(code_to_label, by = c("brand" = "code")) %>%  # Correct join syntax
  mutate(brand = label) %>%  # Overwrite brand with Label (not Label = brand)
  select(-label)  # Drop the Label column since we don't need it anymore
#Intergrate Total on brand_awareness
# Calculate actual reach (unique respondents per brand, regardless of type)
brand_reach <- aware_df %>%
  group_by(brand) %>%
  summarise(
    reach = n_distinct(QUEST),  # Actual unique people who mentioned this brand
    total_percentage = reach / n_distinct(to_get_total$QUEST) * 100,
    total_perc_text = round_excel(total_percentage, 1)
  ) %>%
  left_join(code_to_label, by = c("brand" = "code")) %>%
  mutate(brand = label) %>%
  select(-label)

brand_see <- brand_see %>%
  left_join(brand_reach %>%
              select(brand, total_percentage, total_perc_text),
            by = "brand")
#Arranging the order of type of awareness
brand_see <- brand_see %>%
  mutate(type = factor(type,
                       levels = c("Assisted","Spontaneous", "Top of Mind"),
                       labels = c("Assisted", "Spontaneous", "Top of Mind"),  # Add labels parameter
                       ordered = TRUE)) %>%
  mutate(brand = factor(brand,
                        levels = c("brand 1", "brand 2", "brand 3", "brand 4",
                                   "brand 5", "Other"),
                        ordered = TRUE))
#Plotting
p <- ggplot(brand_see, aes( x =brand, y = percentage, fill = type ))+
  geom_col(position = "stack")+
  geom_text(
    data = brand_see %>% 
      filter(perc_text > 1) %>%
      mutate(
        text_color = ifelse(type == "Assisted", "black", "white")
      ),
    aes(
      label = paste0(perc_text, "%"),
      color = I(text_color)
    ),
    position = position_stack(vjust = 0.5),  # Use the calculated vjust
    size = 4,
    show.legend = FALSE
  ) +
  # Add triangle markers for total
  geom_point(
    data = brand_see,
    aes(x = brand, y = total_percentage, shape = "Global awareness", color = "Global awareness"),
    size = 3,
    inherit.aes = FALSE
  ) +
  
  # add these (put them after your scale_fill_manual)
  scale_shape_manual(name = "Awareness", values = c("Global awareness" = 17)) +
  scale_color_manual(name = "Awareness", values = c("Global awareness" = "green")) +  # shape 17 = triangle
  scale_fill_manual(values = c(
    "Assisted" = "#FFFF9C",   # deep red
    "Spontaneous"   = "#5BC2D9",   # professional amber
    "Top of Mind"  = "#005AB5"    # dark teal green
  ))+
  geom_label(data = brand_see %>% distinct(brand, total_perc_text, .keep_all =TRUE),
             aes(x = brand, y = total_perc_text +5,
                 label = paste0(total_perc_text, "%")),
             inherit.aes = FALSE)+
  labs(title = "Brand Awareness")+
  guides(
    fill = guide_legend(title = NULL),
    shape = guide_legend(title = NULL),
    color = guide_legend(title = NULL)
  )+
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    panel.grid = element_blank(),
    axis.text.x = element_text(face = "bold", colour = "black"),
    axis.text.y = element_blank() # Fixed: Added closing parenthesis
  )

print(p)  
