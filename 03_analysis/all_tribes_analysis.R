# Load the necessary libraries
library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(gtable)
library(knitr)
library(kableExtra)
library(webshot)


# Read the CSV data into a dataframe
tribal_data <- read_csv("./osf_files/tribal_dispossession_analysis_all.csv")

# View the first few rows of the dataframe
head(tribal_data)

# Calculate the summary statistics for each variable
summary_stats <- tribal_data %>%
  summarise(across(where(is.numeric), list(
    mean = ~ mean(.x, na.rm = TRUE),
    sd = ~ sd(.x, na.rm = TRUE),
    min = ~ min(.x, na.rm = TRUE),
    pctl25 = ~ quantile(.x, 0.25, na.rm = TRUE),
    pctl75 = ~ quantile(.x, 0.75, na.rm = TRUE),
    max = ~ max(.x, na.rm = TRUE)
  )))

# Directory where you want to save the files
output_dir <- "plots_tables"

# Create the directory if it does not exist
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Export the summary table to a CSV file
write_csv(summary_stats, file.path(output_dir, "summary_stats.csv"))

# # Instead of pack_rows, just use the column_spec for styling
# formatted_table <- kable(summary_stats, format = "latex", booktabs = TRUE) %>%
#   kable_styling(latex_options = c("striped", "hold_position")) %>%
#   column_spec(1, bold = TRUE, border_right = TRUE)  # Apply bold and border to the first column


# # Save the table as a PDF
# save_kable(formatted_table, file = file.path(output_dir, "summary_table.pdf"))


# Function to create density plots for each variable
create_density_plot <- function(data, var_name) {
  var <- sym(var_name)
  ggplot(data, aes(x = !!var)) +
    geom_density(fill = "blue", alpha = 0.5) +
    geom_vline(aes(xintercept = mean(!!var, na.rm = TRUE)),
      color = "red", linetype = "dashed", linewidth = 1
    ) + # Changed size to linewidth
    labs(title = paste("Density Plot for", var_name))
}


# List of all numeric variables for density plots
numeric_vars <- names(select_if(tribal_data, is.numeric))

# Create and export density plots for all numeric variables
plots_list <- list()
for (var in numeric_vars) {
  plot <- create_density_plot(tribal_data, var)
  plots_list[[var]] <- plot
  ggsave(file.path(output_dir, paste0("density_plot_", var, ".png")), plot, width = 10, height = 8, units = "in")
}

# If you want to arrange all plots in a single image, adjust the number of rows and columns as needed
combined_plot <- do.call(grid.arrange, c(plots_list, ncol = 3))
ggsave(file.path(output_dir, "combined_density_plots.png"), combined_plot, width = 30, height = 24, units = "in")