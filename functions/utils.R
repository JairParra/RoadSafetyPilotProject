###############################################################################
##  utils.R
##   This script contains utility functions used in the project.
## @author: Hair Albeiro Parra Barrera
## @date: 2020-11-29
###############################################################################

################
### 0. Setup ###
################

# required libraries 
library("here")

# Custom scripts  
source(here("functions", "clean_data.R"))

############################
### 1. Utility Functions ###
############################

# Function to safely get the description from varnames_dict
f_get_description <- function(variable, varnames_dict) {
  if (is.null(varnames_dict[[variable]])) {
    return(NA)
  } else {
    return(varnames_dict[[variable]])
  }
}

# Function to compute different correlation measures
f_compute_correlations <- function(df, target_var, filtered_num_vars = filtered_num_vars) {
  require("dplyr")
  
  # Selecting only the correct numerical variables
  numeric_dat <- df %>%
    select(all_of(filtered_num_vars))
  
  # Standardize the numerical data
  std_dat <- f_standardize_data(numeric_dat)
  
  # Convert the target variable to a matrix with the same number of rows
  target <- matrix(df[[target_var]], nrow = nrow(std_dat), ncol = 1)
  
  # Compute correlations
  results <- sapply(std_dat, function(x) {
    c(pearson = cor(x, target, method = "pearson", use = "complete.obs"),
      spearman = cor(x, target, method = "spearman", use = "complete.obs"),
      kendall = cor(x, target, method = "kendall", use = "complete.obs"))
  })
  return(as.data.frame(t(results)))
}