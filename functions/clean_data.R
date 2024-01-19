###############################################################################
## clean_data.R 
##  Performs some basic data processing including NA cleaning, removing irrelevant columns, 
##  cleaning the borough names and other basic transformations.
## 
## @author: Hair Albeiro Parra Barrera
## @date: 2020-11-29
###############################################################################

############################
### 1. Utility Functions ###
############################

# Function to impute missing dates using KNN
# 
# Args: 
#  df: A data frame.
#
# Returns: 
#  A data frame with imputed dates.
f_knn_date_imputation <-function(df){
  
  # Load necessary libraries
  require(lubridate)
  require(DMwR2)
  
  # Convert valid date strings to Dates and assign NA to invalid dates
  dat$date_ <- sapply(df$date_, function(x) {
    if (x != "" && nchar(x) == 10) {  # Check if the string is not empty and has a length of 10 (dd/mm/yyyy)
      return(dmy(x))
    } else {
      return(NA)
    }
  }, USE.NAMES = FALSE)
  
  # Convert the result to a Date class (sapply might return a list otherwise)
  df$date_ <- as.Date(df$date_)
  
  # Convert dates to numeric (number of days since a reference date)
  reference_date <- as.Date("1900-01-01")
  df$days_since_ref <- ifelse(!is.na(df$date_), as.numeric(difftime(df$date_, reference_date, units = "days")), NA)
  
  # Create a new data frame for KNN imputation
  # Include 'days_since_ref' and other relevant numeric columns
  imputation_data <- df[, c("days_since_ref","acc", num_vars)]
  
  # Perform KNN imputation on the new data frame
  set.seed(123)  # for reproducibility
  imputation_data <- knnImputation(imputation_data, k = 5)  # k is the number of neighbors
  
  # Update the 'date_' column in the original data frame
  df$date_ <- reference_date + imputation_data[, "days_since_ref"]
  
  # Optionally, remove the temporary numeric date column
  df$days_since_ref <- NULL
  
  return(df)
}


f_clean_data <- function(df) {
  
  # Correct names mapping
  correct_names <- list(
    "C¶te-des-Neiges-Notre-Dame-de-Graces" = "Côte-des-Neiges-Notre-Dame-de-Grâce",
    "Ville-Marie" = "Ville-Marie",
    "Verdun" = "Verdun",
    "Mercier-Hochelaga-Maisonneuve" = "Mercier-Hochelaga-Maisonneuve",
    "Rosemont-La-Petite-Patrie" = "Rosemont-La Petite-Patrie",
    "Anjou" = "Anjou",
    "Lasalle" = "LaSalle",
    "Plateau-Mont-Royal" = "Le Plateau-Mont-Royal",
    "Westmount" = "Westmount",
    "Saint-Laurent" = "Saint-Laurent",
    "Villeray-Saint-Michel-Parc-Extension" = "Villeray-Saint-Michel-Parc-Extension",
    "Pointe-aux-Trembles-RiviÞres-des-Prairies" = "Pointe-aux-Trembles-Rivières-des-Prairies",
    "MontrÚal-Nord" = "Montréal-Nord",
    "Ahuntsic-Cartierville" = "Ahuntsic-Cartierville",
    "MontrÚal-Est" = "Montréal-Est",
    "Pierrefonds-Roxboro" = "Pierrefonds-Roxboro",
    "St-LÚonard" = "Saint-Léonard",
    "Outremont" = "Outremont",
    "Lachine" = "Lachine",
    "Beaconsfield" = "Beaconsfield",
    "Hampstead" = "Hampstead",
    "Dollard-des-Ormeaux" = "Dollard-Des Ormeaux",
    "Dorval" = "Dorval",
    "C¶te-Saint-Luc" = "Côte-Saint-Luc",
    "Mont-Royal" = "Mont-Royal",
    "?le-Bizard-Sainte-GeneviÞve" = "Île-Bizard-Sainte-Geneviève",
    "Kirkland" = "Kirkland"
  )
  
  # Function to correct borough names
  correct_borough_name <- function(name) {
    if (name %in% names(correct_names)) {
      return(correct_names[[name]])
    } else {
      return(name)
    }
  }
  
  # 1. Remove specified columns
  df <- df[ , !(names(df) %in% c("street_1", "street_2", "X", "X.1"))]
  
  # 2. Convert 'borough' to a categorical variable and correct names
  df$borough <- as.factor(sapply(df$borough, correct_borough_name))
  
  # 3. Convert binary and categorical variables to factors
  categ_vars <- c("all_pedest", 
                   "median", "green_stra", "half_phase", "any_ped_pr", 
                   "ped_countd", "lt_protect", "lt_restric", "lt_prot_re",
                   "any_exclus", "curb_exten", "all_red_an", "new_half_r")
  for (var in categ_vars) {
    df[[var]] <- as.factor(df[[var]])
  }
  
  # `parking` needs to be treated separatedly 
  df$parking <- as.factor(ifelse(df$parking == 0.0, 0, ifelse(df$parking == 0.5, 1, 2)))

  
  # 4. Convert ordinal variables to ordered factors (if any)
  # Example: df$ordinal_var <- factor(df$ordinal_var, order = TRUE, levels = c("Low", "Medium", "High"))
  
  # 5. Ensure numerical variables are of type numeric
  numeric_vars <- c("int_no", "fi", "fli", "fri", "fti", "cli",
                    "cri", "cti", "acc", "ln_pi", "ln_fi", "ln_fli",
                    "ln_fri", "ln_fti", "ln_cli", "ln_cri", "ln_cti",
                    "tot_crossw", "number_of_", "avg_crossw", "tot_road_w", 
                    "north_veh", "north_ped", "east_veh", "east_ped",
                    "south_veh", "south_ped", "west_veh", "west_ped", 
                    "total_lane", "of_exclusi", "commercial",
                    "distdt", "ln_distdt", "traffic_10000", "ped_100")
  df[numeric_vars] <- lapply(df[numeric_vars], as.numeric)
  
  # 6. Inpute all rows with NAs for 'ln_distdt' with 0
  df$ln_distdt[is.na(df$ln_distdt)] <- 0
  
  # 7. Perform KNNM imputation for 'date_'
  df <- f_knn_date_imputation(df)
  
  # 6. Produce statistics on the number of NAs per column and print
  na_counts <- sapply(df, function(x) sum(is.na(x)))
  total_rows <- nrow(df)
  na_percentages <- na_counts / total_rows * 100
  na_stats <- data.frame(Count = na_counts, Percentage = na_percentages)
  na_stats_with_na <- na_stats[na_counts > 0, ]
  print(na_stats_with_na)

  return(df)
}


# Function to standardize numerical variables
# 
# Args:
#   data: A data frame.
#   reverse: If TRUE, reverses the standardization. Defaults to FALSE.
#   auto: If TRUE, automatically standardizes all numeric columns. Defaults to FALSE.
#   cols: A vector of column names to standardize. Defaults to NULL.
#
# Returns:
#   A data frame with standardized columns.
f_standardize_data <- function(data, reverse = FALSE, auto = FALSE, cols = NULL) {
  if (reverse) {
    if (is.null(cols)) {
      stop("Please specify the columns to reverse standardize.")
    }
    # Reverse standardization for specified columns
    data %>% 
      mutate_at(vars(all_of(cols)), ~ . / attr(., "scaled:scale"))
  } else {
    if (auto) {
      # Standardize all numeric columns
      data %>% 
        mutate_if(is.numeric, scale)
    } else {
      if (is.null(cols)) {
        stop("Please specify the columns to standardize or set auto = TRUE.")
      }
      # Standardize specified columns
      data %>% 
        mutate_at(vars(all_of(cols)), scale)
    }
  }
}


