###############################################################################
## clean_data.R 
##  Performs some basic data processing including NA cleaning, removing irrelevant columns, 
##  cleaning the borough names and other basic transformations.
## 
## @author: Hair Albeiro Parra Barrera
## @date: 2020-11-29
###############################################################################

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
  df <- df[ , !(names(df) %in% c("rue_1", "rue_2", "street_1", "street_2", "X", "X.1"))]
  
  # 2. Convert 'borough' to a categorical variable and correct names
  df$borough <- as.factor(sapply(df$borough, correct_borough_name))
  
  # 3. Produce statistics on the number of NAs per column and print
  na_counts <- sapply(df, function(x) sum(is.na(x)))
  total_rows <- nrow(df)
  na_percentages <- na_counts / total_rows * 100
  na_stats <- data.frame(Count = na_counts, Percentage = na_percentages)
  na_stats_with_na <- na_stats[na_counts > 0, ]
  print(na_stats_with_na)
  
  # 4. Drop all rows with NAs
  df <- df[complete.cases(df), ]
  
  return(df)
}
