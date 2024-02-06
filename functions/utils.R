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


# Function to load the variable descriptions as dictionary into the env 
f_load_varnames <- function(excel_path) {
  
  # Load necessary library
  require(readxl)
  require(here)
  
  # Read the Excel file
  varnames <- read_excel(excel_path)
  
  # Create the dictionary
  varnames_dict <- setNames(as.list(varnames$DESCRIPTION), varnames$NOM)
  names(varnames_dict) <- gsub("[^[:alnum:]_]+", "", names(varnames_dict)) # Ensure clean list names
  
  # Return a list containing both objects
  return(list(varnames = varnames, varnames_dict = varnames_dict))
}


# Function to convert the factor variables to dummies
# 
# Args: 
#  df: A data frame.
#  all_vars: A vector of all variables to include in the final dataframe.
#
# Returns: 
#  A data frame wher eall factor variables have been converted to dummy variables. 
f_convert_to_dummies <- function(df, all_vars) {
  require("fastDummies")
  
  # Filter the dataframe to only include variables in 'all_vars'
  df_filtered <- df[, all_vars, drop = FALSE]
  
  # Create dummy variables using fastDummies for the filtered dataframe
  df_filtered <- dummy_cols(df_filtered, remove_first_dummy = TRUE, remove_selected_columns = TRUE)
  
  # Merge the dummies back with the original dataframe
  # This step ensures that any variables not in 'all_vars' are retained in the final dataframe
  df <- cbind(df[, !names(df) %in% all_vars], df_filtered)
  
  # Remove this weird column if it exists
  if ("df[, !names(df) %in% all_vars]" %in% names(df)) {
    df <- df[, !names(df) %in% "df[, !names(df) %in% all_vars]"]
  }
  
  return(df)
}


# Function to compute different correlation measures
f_compute_correlations <- function(df, target_var, filtered_num_vars = filtered_num_vars) {
  require("dplyr")
  
  # Selecting only the correct numerical variables
  numeric_dat <- df %>%
    dplyr::select(all_of(filtered_num_vars))
  
  # Standardize the numerical data
  std_dat <- f_standardize_data(numeric_dat, auto=TRUE)
  
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
