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
library(broom) #for clean result on prop.test
#Load data
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
    A3B >= 18 & A3B <= 24 ~ "18-24 years old",
    A3B >= 25 & A3B <= 34 ~ "25-34 years old", 
    A3B >= 35 & A3B <= 44 ~ "35-44 years old",
    A3B >= 45 & A3B <= 59 ~ "45-59 years old",
    A3B >= 60 ~ "60 years +",
    TRUE ~ NA_character_
  ), levels = c("18-24 years old", "25-34 years old", "35-44 years old", "45-59 years old", "60 years +")))

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
  mutate (A4E = ifelse(A4E == 7, 5, A4E)) %>%
  left_join(survey_labels[["A4E"]], by = c("A4E" = "code")) %>%
  mutate( A4E = factor(A4E,
                      levels = c("1", "2", "3", "4", "5"),
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

#setting for the test
overall_tom <- sum(profile$TOM_1)
overall_total <- nrow(profile)
age_group_data <- profile %>% filter(age_group == "18-24 years old")
age_group_tom <- sum(age_group_data$TOM_1)
age_group_total <- nrow(age_group_data)
prop.test(x = c(age_group_tom, overall_tom), 
          n = c(age_group_total, overall_total))

# Function to test each subgroup against overall
test_subgroup <- function(subgroup_name, subgroup_value) {
  if (subgroup_name == "group_var") return(NULL)  # Skip if it's the column name
  
  subgroup_data <- profile %>% filter(!!sym(subgroup_name) == subgroup_value)
  subgroup_tom <- sum(subgroup_data$TOM_1)
  subgroup_total <- nrow(subgroup_data)
  
  if (subgroup_total > 0) {
    test_result <- prop.test(x = c(subgroup_tom, overall_tom), 
                             n = c(subgroup_total, overall_total))
    return(tidy(test_result) %>% mutate(subgroup = subgroup_value, variable = subgroup_name))
  }
  return(NULL)
}

# Run tests for all subgroups
Age_results <- map_dfr(unique(profile$age_group), 
                       ~test_subgroup("age_group", .x))

Educ_result <- map_dfr(unique(profile$educ), 
                       ~test_subgroup("educ", .x))

CSP_result <- map_dfr(unique(profile$CSP), 
                      ~test_subgroup("CSP", .x))
test_result <- Age_results %>%
  select(subgroup, p.value)
test_result2 <- Educ_result %>%
  select(subgroup, p.value)
test_result3 <- CSP_result %>%
  select(subgroup, p.value)
#mutate to character to allow bind_rows because of ordered label earlier
test_result_final <- bind_rows(
  test_result %>% mutate(subgroup = as.character(subgroup)),
  test_result2 %>% mutate(subgroup = as.character(subgroup)),
  test_result3 %>% mutate(subgroup = as.character(subgroup))
)

summary_df <- summary_df %>%
  left_join(test_result_final, by = c("group" = "subgroup"))

# First calculate overall percentage
overall_percentage <- (overall_tom / overall_total) * 100

summary_df <- summary_df %>%
  mutate(
    p.value = as.numeric(p.value),
    significance = case_when(
      p.value < 0.05 & percentage > overall_percentage ~ "signi more",
      p.value < 0.05 & percentage < overall_percentage ~ "signi less", 
      p.value >= 0.05 ~ "none",
      TRUE ~ "check"  # for any edge cases
    ),
    perc_label = round_excel(percentage),
    plot_label = paste(group, "n=",round_excel(total*600/202), sep=" " )
  )
summary_df <- summary_df %>%
  mutate(profile_type = case_when(
    group == "Overall" ~ "Overall",
    group %in% unique(profile$age_group) ~ "Age Group",
    group %in% unique(profile$educ) ~ "Education", 
    group %in% unique(profile$CSP) ~ "Socio-professional",
    TRUE ~ "Other"
  ))

#plot
p <- summary_df %>%
  ggplot(aes( x = group, y = percentage)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  #Header
  geom_text(data = . %>% filter(profile_type != "Overall") %>%
              group_by(profile_type) %>%
              slice(1) %>%  # First row of each group
              mutate(label_x = -2),  # Position to the left of y-axis
            aes(x = group, y = -5, label = profile_type),
            fontface = "bold", size = 4, hjust = 0.5, angle = 90) +  # Vertical text
  geom_text( data = summary_df %>% filter(percentage >= overall_percentage),
            aes( y = percentage +5 ,
                 label = paste(perc_label, "%", sep="")),
                 color = "black") +
  geom_text( data = summary_df %>% filter(percentage < overall_percentage),
             aes(y = percentage -5 ,
                 label = paste(perc_label, "%", sep="")),
                 color = "black") +
  scale_x_discrete(limits = rev(unique(summary_df$group)), #it expect unique
                   labels = rev(summary_df$plot_label)) +
  scale_y_continuous(limits = c(-5, 100),
                     breaks = c(0, overall_percentage, 100),
                     labels = c(paste0("0", "%"),
                                paste0(round_excel(overall_percentage), "%"),
                                paste0("100", "%")),
                     trans = shift_trans(overall_percentage)) +
  labs( caption = "Base: General Public n = 600",
        title = "Distribution of Brand 1 Top of Mind Awareness by Profile") +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    panel.grid.major.y = element_blank(), #It will be on the center of the bar
    panel.grid.major.x = element_blank(),  # Keep horizontal
    panel.grid = element_blank(),
    axis.text.x = element_text(face = "bold", colour = "black"),
    axis.text.y = element_text(face = "bold", colour = "black"),
    plot.caption = element_text(
      size = 12,                    # Larger font size
      hjust = 0.5,                  # Center alignment (0=left, 0.5=center, 1=right)
      vjust = 1,                    # Vertical position
      face = "italic",              # Optional: italic style
      margin = margin(t = 10)       # Add space above caption
    ),
    legend.position = "none"
  )
print(p)

#Working table like plot
p <- summary_df %>%
  ggplot(aes(x = group, y = percentage)) +
  
  # Rectangle framing each bar + label
  geom_rect(
    data = summary_df %>%
      mutate(row_num = as.numeric(factor(group, levels = rev(unique(group))))) %>%
      group_by(profile_type) %>%
      summarise(
        xmin = min(row_num) - 0.5,
        xmax = max(row_num) + 0.5,
        ymin = -35,
        ymax = 100
      ),
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax
    ),
    fill = NA,
    color = "gray30",
    linewidth = 0.8,
    inherit.aes = FALSE
  )+
  # Bars
  geom_bar(stat = "identity",
           aes(fill = case_when(
             significance == "signi more" ~ "signi more",
             significance == "signi less" ~ "signi less",
             TRUE ~ "none"
           ))) +
  coord_flip() +
  geom_text(data = . %>% filter(profile_type != "Overall") %>%
              group_by(profile_type) %>%
              slice(1) %>%  # First row of each group
              mutate(label_x = -1),  # Position to the left of y-axis
            aes(x = group, y = -40, label = profile_type),
            fontface = "bold", size = 4, hjust = 1, angle = 90) +  # Vertical text
  
  # Text labels for percentages (above or below bars)
  geom_text(
    data = summary_df %>% filter(percentage >= overall_percentage),
    aes(y = percentage + 5, label = paste0(perc_label, "%")),
    color = "black"
  ) +
  geom_text(
    data = summary_df %>% filter(percentage < overall_percentage),
    aes(y = percentage - 5, label = paste0(perc_label, "%")),
    color = "black"
  ) +
  
  # Custom labels INSIDE the rectangles (replacing axis labels)
  geom_text(
    data = summary_df %>%
      mutate(row_num = as.numeric(factor(group, levels = rev(unique(group))))),
    aes(
      x = group,
      y = -3,                # position inside left side of rectangle
      label = plot_label
    ),
    hjust = 1,               # right align label text
    color = "black",
    fontface = "bold",
    size = 4
  ) +
  scale_fill_manual(
    values = c(
      "signi more" = "blue",
      "signi less" = "red",
      "none" = "grey80" # this will be overridden by default fill if not specified
    ),
    guide = "none"
  ) +
  # Axes
  scale_x_discrete(
    limits = rev(unique(summary_df$group)),
    labels = NULL  # remove external axis labels
  ) +
  scale_y_continuous(
    limits = c(-40, 100),
    breaks = c(0, overall_percentage, 100),
    labels = c(
      paste0("0", "%"),
      paste0(round_excel(overall_percentage), "%"),
      paste0("100", "%")
    ),
    trans = shift_trans(overall_percentage)
  ) +
  
  # Titles and captions
  labs(
    caption = "Base: General Public n = 600",
    title = "Distribution of Brand 1 Top of Mind Awareness by Profile"
  ) +
  
  # Clean theme
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text.x = element_text(face = "bold", colour = "black"),
    axis.text.y = element_blank(),   # hide original y labels
    panel.grid = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(
      size = 12,
      hjust = 0.5,
      vjust = 1,
      face = "italic",
      margin = margin(t = 10)
    ),
    legend.position = "none"
  )

print(p)
ggsave(here::here("04_Graphic_output", "brandxprofile.png"),
       plot = p, width = 15, height = 8, dpi = 600)
