################################################################################
# install_packages.R
# 
# @author: Hair Albeiro Parra Barrera
# 
# This R script is designed to load specified packages and install them if they 
# are not already installed. 
################################################################################

####################
### 1. Functions ###
####################

# Load required packages
f_load_packages <- function() {
  
  # List of packages to automatically install
  proj_packages <- c(
    "sf", 
    "here", 
    "DMwR2", 
    "tidyr", 
    "dplyr", 
    "haven",
    "tibble", 
    "readxl", 
    "corrplot", 
    "ggplot2", 
    "data.table", 
    "sqldf", 
    "car", 
    "zoo", 
    "rockchalk", 
    "glmnet",
    "lubridate", 
    "elasticnet", 
    "factoextra", 
    "randomForest", 
    "fastDummies"
  )
  
  for (package in proj_packages) {
    # Check if the package is already installed
    if (!requireNamespace(package, quietly = TRUE)) {
      # If not installed, install the package with dependencies
      install.packages(package, dependencies = TRUE)
    }
    # Load the package
    library(package, character.only = TRUE)
  }
}
