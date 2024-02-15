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

# Function to standardize numerical variables
# 
# Args:
#   data: A data frame.
#   reverse: If TRUE, reverses the standardization. Defaults to FALSE.
#   auto: If TRUE, automatically standardizes all numeric columns. Defaults to FALSE.
#   cols: A vector of column names to standardize. Defaults to NULL.
#   exclude: A vector of column names to exclude from standardization. Defaults to NULL.
#
# Returns:
#   A data frame with standardized columns.
f_standardize_data <- function(data, reverse = FALSE, auto = FALSE, cols = NULL, exclude=NULL) { 
  # if exclude is not null, remove the columns from the list of columns to standardize
  if(!is.null(exclude)){
    cols = cols[!cols %in% exclude]
  }
  
  if (reverse) {
    if (is.null(cols)) {
      stop("Please specify the columns to reverse standardize.")
    }
    # Reverse standardization for specified columns
    data %>% 
      mutate_at(vars(all_of(cols)), ~ . / attr(., "scaled:scale"))
  } else {
    if (auto) {
      # Standardize all numeric columns, excluding specified ones
      numeric_cols <- sapply(data, is.numeric) & !names(data) %in% exclude
      data %>% 
        mutate_at(vars(names(which(numeric_cols))), scale)
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



# Function to impute missing dates using KNN
# 
# Args: 
#  df: A data frame.
#  num_vars: list of numerical variables (columns) to consider in the dataframe
#
# Returns: 
#  A data frame with imputed dates.
f_knn_date_imputation <-function(df, num_vars){
  
  # Load necessary libraries
  require("lubridate")
  require("DMwR2")
  
  # Create a temporary copy of the data 
  temp_df <- df
  
  # Convert valid date strings to Dates and assign NA to invalid dates
  temp_df$date_ <- sapply(temp_df$date_, function(x) {
    if (x != "" && nchar(x) == 10) {  # Check if the string is not empty and has a length of 10 (dd/mm/yyyy)
      return(dmy(x))
    } else {
      return(NA)
    }
  }, USE.NAMES = FALSE)
  
  # Convert the result to a Date class (sapply might return a list otherwise)
  temp_df$date_ <- as.Date(temp_df$date_)
  
  # Convert dates to numeric (number of days since a reference date)
  reference_date <- as.Date("1900-01-01")
  temp_df$days_since_ref <- ifelse(!is.na(temp_df$date_), 
                                   as.numeric(difftime(temp_df$date_, reference_date, units = "days")), NA)
  
  # Standardize the numerical variables in the temporary copy
  temp_df <- f_standardize_data(temp_df, cols = num_vars)
  
  # Create a new data frame for KNN imputation
  # Include 'days_since_ref' and other relevant numeric columns
  imputation_data <- temp_df[, c("days_since_ref","acc", num_vars)]
  
  # Perform KNN imputation on the new data frame
  set.seed(123)  # for reproducibility
  imputation_data <- knnImputation(imputation_data, k = 5)  # k is the number of neighbors
  
  # Update the 'date_' column in the original data frame
  df$date_ <- reference_date + imputation_data[, "days_since_ref"]
  
  # # Optionally, remove the temporary numeric date column
  # df$days_since_ref <- NULL
  
  return(df)
}


# Function to extract time variables from dates and remove date_.
# 
# Args: 
#  df: A data frame.
#
# Returns: 
#  A data frame with new variables and without date_.
f_extract_time = function(df){
  
  df$month = factor(format(df$date_, "%m"))
  df$year <- as.numeric(format(df$date_, "%Y"))
  df$dow <- factor(weekdays(df$date_))
  
  df = subset(df, select=-c(date_))
  
  return(df)
}



# Function to group boroughs with fewer than n observations
# 
# Args: 
#  df: A data frame.
#
# Returns: 
#  A data frame with new variables and without date_.
f_group_borough = function(df, n){
  
  levels <- table(df$borough)
  
  # Identify levels with fewer than n observations
  levels_to_group <- names(levels[levels < n])
  
  # Create a new factor with 'Other' level for levels with fewer than n observations
  df$borough_grouped <- fct_collapse(df$borough, Other = levels_to_group)
  
  return(df)
}


# Function to expand the data frame with polynomial features. 
# 
# Args: 
#  df: A data frame.
#
# Returns: 
#  A data frame with new polynomial features.
expand_polynomial_features <- function(df) {
  # Traffic Flow Variables
  df$pi_squared <- df$pi^2
  df$fi_squared <- df$fi^2
  
  # Distance from Downtown
  df$distdt_squared <- df$distdt^2
  df$distdt_cubed <- df$distdt^3
  
  # Crosswalk and Road Widths
  df$tot_crossw_squared <- df$tot_crossw^2
  df$avg_crossw_squared <- df$avg_crossw^2
  df$tot_road_w_squared <- df$tot_road_w^2
  
  # Turning Flows
  df$fli_squared <- df$fli^2
  df$fri_squared <- df$fri^2
  df$fti_squared <- df$fti^2
  
  return(df)
}


# Function to clean the data, including NA cleaning, removing irrelevant columns,
# 
# Args: 
#   group_boroughs: If TRUE, group boroughs with fewer than 50 observations.
#   drop_borough: If TRUE, drop the original 'borough' column.
#   drop_year: If TRUE, drop the 'year' column.
#   numerical_categories: If TRUE, convert positive integer variables to factors.
#   standarize: If TRUE, standardize the numerical variables.
# 
# Returns: 
f_clean_data <- function(df, 
                         group_boroughs=TRUE, 
                         drop_borough=FALSE,
                         drop_year=FALSE, 
                         numerical_categories=FALSE, 
                         standarize=FALSE
                         ) {
  
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
  
  # 0. Extract the intersection ids into a vector
  int_no <- df$int_no
  
  # 1. Remove specified columns
  df <- df[ , !(names(df) %in% c("street_1", "street_2", "X", "X.1",
                                 "int_no", 'rue_1', 'rue_2', 
                                 "traffic_10000", "ped_100"  # drop repeated variables
                                 ))]
  
  # 2. Convert 'borough' to a categorical variable and correct names
  df$borough <- as.factor(sapply(df$borough, correct_borough_name))
  
  # 3. Convert binary and categorical variables to factors
  categ_vars <- c("all_pedest", 
                   "median", "green_stra", "half_phase", "any_ped_pr", 
                   "ped_countd", "lt_protect", "lt_restric", "lt_prot_re",
                   "any_exclus", "curb_exten", "all_red_an", "new_half_r"
                  )
  for (var in categ_vars) {
    df[[var]] <- as.factor(df[[var]])
  }
  
  # `parking` needs to be treated separatedly 
  df$parking <- as.factor(ifelse(df$parking == 0.0, 0, ifelse(df$parking == 0.5, 1, 2)))
  
  # 4. Convert ordinal variables to ordered factors (if any)
  # Example: df$ordinal_var <- factor(df$ordinal_var, order = TRUE, levels = c("Low", "Medium", "High"))
  
  # 5. Ensure numerical variables are of type numeric
  numeric_vars <- c("fi", "fli", "fri", "fti", "cli",
                    "cri", "cti", "acc", "ln_pi", "ln_fi", "ln_fli",
                    "ln_fri", "ln_fti", "ln_cli", "ln_cri", "ln_cti",
                    "tot_crossw", "number_of_", "avg_crossw", "tot_road_w", 
                    "south_veh", "south_ped", "west_veh", "west_ped", 
                    "total_lane", "of_exclusi", "commercial",
                    "distdt", "ln_distdt"
                    # "traffic_10000", "ped_100" # repeated columns
                    )
  df[numeric_vars] <- lapply(df[numeric_vars], as.numeric)
  
  # 6. Impute all rows with NAs for 'ln_distdt' with 0
  df$ln_distdt[is.na(df$ln_distdt)] <- 0
  
  # 7. Create an indicator variable for NA's
  df$missing_date_ind = factor(ifelse(df$date_ == "",1,0))
  
  # 8. Perform KNN imputation for 'date_'
  df <- f_knn_date_imputation(df, numeric_vars)
  
  # 9. Extract time variables
  df = f_extract_time(df)
  
  # 10. Group boroughs
  if(group_boroughs){
    # Group boroughs with fewer than 50 observations
    df =  f_group_borough(df, 50)
    
    # drop the old borough columns
    if(drop_borough){
      df$borough = NULL
    }
  }

  # 11. Rename 'x' to 'latitude' and 'y' to 'longitude'
  colnames(df)[colnames(df) %in% c("x", "y")] <- c("latitude", "longitude")
  
  # 12. Optionally, drop additional columns like year 
  if(drop_year){
    df$year = NULL
  }
  
  # 13. Convert positive integer variables to factors (as better representation)
  if(numerical_categories){
    
    # specify numeric covariates to convert to factors
    categ_vars <- c(
      "number_of_", "total_lane", "commercial"
    )
    
    # convert each of the variables as factors.
    for (var in categ_vars) {
      if(var %in% names(df)){
        df[[var]] <- as.factor(df[[var]])
      }
    }
  }
  
  # 14. Standarize the numerical variables
  if(standarize){
    df <- f_standardize_data(df, auto = TRUE, exclude = c("int_no", "acc", "borough_grouped"))
  }
  
  # 15. Re-add the int_no from the original dataframe as a column 
  df$int_no = int_no
  
  # 16. Incorporate polynomial features 
  df <- expand_polynomial_features(df)
  
  return(df)
}









