#Input cleaned dataframe
#Output: multiple flipped_barplot (A4B,C13) for the test
#Load library
library(here)
library(readxl)
library(tidyverse)
library(openxlsx)
#load data
load(here::here("03_Df_output", "cleaned_data.RData"))
rm(aware_long_complete, C15_long, C16_long,C11_long)
excel_path <- here::here("02_Input", "code_to_label.xlsx")
flipped_barplot_var <- read_excel(here::here("02_Input", "graphic_control.xlsx"), sheet = "flipped_barplot")
#Load function
round_excel <- function(x, digits = 0) {
  posneg <- sign(x)
  z <- abs(x) * 10^digits
  z <- z + 0.5
  z <- trunc(z)
  z <- z / 10^digits
  z * posneg
}
univariable_flipped_barplots <- function(
    data, 
    metadata, 
    output_dir = here::here("04_Graphic_output", "Univariable_flipped_barplot"),
    file_prefix = "flipped_barplot"
) {
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Set working directory temporarily
  old_wd <- setwd(output_dir)
  on.exit(setwd(old_wd))  # Restore WD on exit
  
  # Find common variables
  common_vars <- intersect(names(data), metadata$variable)
  
  if (length(common_vars) == 0) {
    warning("No common variables found between data and metadata")
    return(invisible(NULL))
  }
  
  walk(common_vars, ~ {
    tryCatch({
      # Get metadata for the current variable
      var_meta <- metadata %>% filter(variable == .x)
      
      if (nrow(var_meta) == 0) {
        warning("Skipping ", .x, ": no metadata found.")
        return()
      }
      
      # Calculate percentages
      var_percentages <- data %>%
        count(!!sym(.x)) %>%
        mutate(percentage = n / sum(n) * 100)
      
      # Skip if all values are NA
      if (all(is.na(var_percentages[[.x]]))) {
        warning("Skipping ", .x, ": all values are NA")
        return()
      }
      
      # Generate plot
      p <- ggplot(var_percentages, aes(x = reorder(!!sym(.x), percentage), 
                                       y = percentage)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        coord_flip() +
        geom_text(
          data = var_percentages %>% 
            mutate(
              text_color = ifelse(percentage < 5, "red", "white"),
              text_hjust = ifelse(percentage < 5, -0.1, 1.1),
              text_size = ifelse(percentage < 5, 4.5, 4)
            ),
          aes(
            label = sprintf("%.1f%%", percentage),
            color = text_color,
            hjust = text_hjust
          ),
          size = 4,
          show.legend = FALSE
        ) +
        scale_color_identity() +
        scale_y_continuous(limits = c(0, 100),
                           breaks = NULL,
                           expand = c(0, 0)) +
        labs(
          x = "",
          y = "",
          title = var_meta$title,
          caption = var_meta$captions
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          plot.caption = element_text(
            size = 12,
            hjust = 0.5,
            vjust = 1,
            face = "italic"),
          panel.grid = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_text(face = "bold", colour = "black"),
          legend.position = "none"
        )
      
      # Save plot
      ggsave(
        filename = paste0(file_prefix, "_", .x, ".png"),
        plot = p,
        width = 10,
        height = 6,
        dpi = 300
      )
      
    }, error = function(e) {
      warning("Failed to create plot for ", .x, ": ", e$message)
    })
  })
  
  message("Created ", length(common_vars), " flipped bar plots in ", output_dir)
}

#Step 1: select all the relevant variable and use label

flipped_barplot_df <- survey %>%
  select(QUEST, intersect(names(survey), flipped_barplot_var$variable))

#labellling
# Get sheet names
sheet_names <- excel_sheets(excel_path)

# Read all sheets into a named list
survey_labels <- map(setNames(sheet_names, sheet_names), 
                     ~read_excel(excel_path, sheet = .x))
common_vars <- intersect(names(flipped_barplot_df), names(survey_labels))
complete_coded <- flipped_barplot_df
for (var in common_vars) {
  # Get the code-to-label mapping for the current variable
  code_mapping <- survey_labels[[var]] %>%
    select(code, label)  # Note: Now Code is first (the key for joining)
  
  # Join to replace codes with French labels
  complete_coded <- complete_coded %>%
    left_join(code_mapping, by = setNames("code", var)) %>%  # Match codes to labels
    mutate(!!var := label) %>%  # Overwrite code column with labels
    select(-label)  # Drop the temporary French label column
}

#step 2 run the function
univariable_flipped_barplots(complete_coded, flipped_barplot_var)
