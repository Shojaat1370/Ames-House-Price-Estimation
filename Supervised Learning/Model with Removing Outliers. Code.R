
# ## Pre-Processing and Algorithm analysis on "Ames House Price" dataset  #####

# Second Scenario :  Analsis with handling outliers 




########################## -------------- PreProcessing ------------- ###################### 


# /////////////   1.    Importing the dataset and description of dataset ///////

data <- read.csv("C:/00 Transferable Folders/01 Unimi- Uni Milan/03 Third Trimester/02 Statistics for ML. Prof. Salini/Project/House Price/train.csv", stringsAsFactors = F)

# Checking the dimension of the dataset
dim <- dim(data)
dim

# Examining the "structure" of the data
str(data)


# //////////////////      2.   Drop columns that aren’t useful.  //////////////////

# here first lables are stored and then remove. 
data_Ids <- data.frame(data$Id)

# removing ID column
data$Id <- NULL


# //////////////////     3.     Dropping Target variable.  //////////////////

# #removing the SalePrice column from dataset but keeping SalePrice column as new separate column:
SalePrice <- data$SalePrice
data$SalePrice<- NULL
dim(data)


# //////////////////     4.   Analyzing types of data and Manipulating.  //////////////////

# --------------- 4.1. Originally a categorical data ------ 

# since 'MSSubClass' is originally a categorical data, so we need to convert it to character
# then in Categorical Part we will do lable encoding operation on it.

library(car)
library(dplyr)

data <- data %>% mutate(MSSubClass=recode(MSSubClass, `20` = "SC20", `30` = "SC30", `40` = "SC40", `45` = "SC45", 
                                          `50` = "SC50", `60` = "SC60", `70` = "SC70", `75` = "SC75", 
                                          `80` = "SC80", `85` = "SC85", `90` = "SC90", `120` = "SC120", 
                                          `150` = "SC150", `160` = "SC160", `180` = "SC180", '190' = "SC190" ))



data$MSSubClass


# --------------- 4.2. Feature combination ------ 

# Combinations of some features that have same meaning to avoid dimension issue:

#    4.2.1.    Creating Total number of bathrooms (TotalBath) by Combinations of four features 

data["TotalBath"] <- data["BsmtFullBath"] + (0.5 * data["BsmtHalfBath"]) + data["FullBath"] + (0.5 * data["HalfBath"])
dim(data)

# deleting above used features from dataset
data <- data[, !(names(data) %in% c("BsmtFullBath", "BsmtHalfBath","FullBath", "HalfBath"))]
dim(data)


#    4.2.2.   Creating Total square feet of porch area (AllPorchSF) by Combinations of four following features:

# OpenPorchSF: Open porch area in square feet
# EnclosedPorch: Enclosed porch area in square feet
# X3SsnPorch: Three season porch area in square feet
# ScreenPorch: Screen porch area in square feet

data["AllPorchSF"] <- data["OpenPorchSF"] + data["EnclosedPorch"] + data["X3SsnPorch"] + data["ScreenPorch"]
data["AllPorchSF"] 
dim(data)
# deleting above used features from dataset
data <- data[, !(names(data) %in% c("OpenPorchSF", "EnclosedPorch","X3SsnPorch", "ScreenPorch"))]
dim(data)


# --------------- 4.3. Discovering numerical value which originally categorical ------ 

# In dataset exist some features that have numerical values, but actually they are categorical data,
#  so first, before any operation on numerical data we need to separate them from numerical data and
#  create a sub-dataset for them (original_cat_dataset), then in next steps when we already
# operate Encoding on categorical data, we will add this sub-dataset to categorical sub-dataset.

# these features are:
# TotalBath     # new created features. total number of Bath
# OverallQual   # Overall material and finish quality
# OverallCond   # Overall condition rating
# BedroomAbvGr  # Number of bedrooms above basement level
# TotRmsAbvGrd  # Total rooms above grade
# Fireplaces    # Number of fireplaces
# GarageCars    # Size of garage in car capacity. number of cars that can park
# MoSold        # month sold
# KitchenAbvGr  # Number of Kitchens above grade

# creating sub-dataset for these features:
original_cat_dataset <- data[, c("TotalBath", "OverallQual", "OverallCond", "BedroomAbvGr", "TotRmsAbvGrd", "Fireplaces", "GarageCars", "MoSold", "KitchenAbvGr")]
dim(original_cat_dataset)
# deleting these features from original dataset (data) then creating new dataset without them. "data2"
data <- data[, !(names(data) %in% c("TotalBath", "OverallQual", "OverallCond", "BedroomAbvGr", "TotRmsAbvGrd", "Fireplaces", "GarageCars", "MoSold", "KitchenAbvGr"))]
dim(data)



# ////////////////////    5.    Separating Numerical and Categorical features  ///////

# Here we get name of the features that have 'numeric' values and 'categorical' values, and create
# sub-dataset for each of them

#   Determining numerical features
num_features <- which(sapply(data, is.numeric))
num_features

#   Determining categorical features
cat_features <- which(sapply(data, is.character))
cat_features

#   we save column names in a variable for both
num_fea_name <- names(num_features)
num_fea_name

cat_fea_name <- names(cat_features)
cat_fea_name

# creating sub-dataset for "numerical" features
numeric_data <- data[, num_fea_name]

# creating sub-dataset for "categorical" features
categorical_data <- data[, cat_fea_name]

# checking dimension of both
dim(categorical_data)
dim(numeric_data)




# /////////////////////////      6.    Missing data handling -    /////////////////////////

# We have Two strategy: 1. imputing  2. removing missing values

# ------------       6. 1.    Not exist data ---------

# Having quick review I found In this dataset some missing data are not missing, although their values
# are 'NA' but this means amount is zero. For example for FireplaceQu (Fireplace quality)
# 'NA' means 'No Fireplace' which we need to put them to unknown or put 0 by imputing manually.
# then adding them to categorical data

# These features are: GarageFinish, GarageQual, GarageCond, BsmtQual, BsmtCond

# ---  First Step: Replacing 'NA' values in these columns with "None" - then next stem encoding them

categorical_data$GarageFinish[is.na(categorical_data$GarageFinish)] <- "None"

categorical_data$GarageQual[is.na(categorical_data$GarageQual)] <- "None"

categorical_data$GarageCond[is.na(categorical_data$GarageCond)] <- "None"

categorical_data$BsmtQual[is.na(categorical_data$BsmtQual)] <- "None"

categorical_data$BsmtCond[is.na(categorical_data$BsmtCond)] <- "None"


# ---  Second Step: Replacing 'None' values with 0 - and encoding other categories too.
categorical_data$GarageFinish

categorical_data$GarageFinish <- recode(categorical_data$GarageFinish,
                                        'None'  = 0,
                                        'Unf'   = 1,
                                        'RFn'   = 2,
                                        'Fin'   = 3 )

# Checking is feature Encoded or not:
categorical_data$GarageFinish

categorical_data$GarageQual <- recode(categorical_data$GarageQual,
                                      "None" = 0,
                                      "Po"   = 1,
                                      "Fa"   = 2,
                                      "TA"   = 3,
                                      "Gd"   = 4,
                                      "Ex"   = 5)


categorical_data$GarageCond <- recode(categorical_data$GarageCond,
                                      "None" = 0,
                                      "Po"   = 1,
                                      "Fa"   = 2,
                                      "TA"   = 3,
                                      "Gd"   = 4,
                                      "Ex"   = 5)


categorical_data$BsmtQual <- recode(categorical_data$BsmtQual,
                                    "None" = 0,
                                    "Po"   = 1,
                                    "Fa"   = 2,
                                    "TA"   = 3,
                                    "Gd"   = 4,
                                    "Ex"   = 5)


categorical_data$BsmtCond <- recode(categorical_data$BsmtCond,
                                    "None" = 0,
                                    "Po"   = 1,
                                    "Fa"   = 2,
                                    "TA"   = 3,
                                    "Gd"   = 4,
                                    "Ex"   = 5)



#  ------   Third Step:    Deleting above used features:

# deleting these features from catogorical sub-dataset, then adding to original_cat_dataset which is 
# sub-datset that contains features with integer data while originally they have categorical type. 
# creating sub-dataset for these variable in order to combine with last "original_cat_dataset":

# creating new original_cat_dataset2 for these features to add to previous 'original_cat_dataset':
original_cat_dataset2 <- categorical_data[, c("GarageFinish", "GarageQual", "GarageCond", "BsmtCond", "BsmtQual")]
original_cat_dataset2

# Adding above new sub-dataset to the previous 'original_cat_dataset' sub-dataset:
original_cat_dataset <- cbind(original_cat_dataset, original_cat_dataset2)
dim(original_cat_dataset)

# then deleting these features from categorical sub-dataset to avoid duplication:
categorical_data <- categorical_data[, !(names(categorical_data) %in% c("GarageFinish", "GarageQual", "GarageCond", "BsmtCond", "BsmtQual"))]
dim(categorical_data)


# ------------       6. 2.    Removing strategy     ---------

# Finding missing data and removing data which has more than 50 percent missing values

#  threshold percentage
threshold <- 50

# Calculating the percentage of missing values for each feature in both sub-datasets 
missing_percent_num <- colMeans(is.na(numeric_data)) * 100
missing_percent_num

missing_percent_cat <- colMeans(is.na(categorical_data)) * 100
missing_percent_cat

# Identify the features with more than the threshold percentage of missing values
# for both numerical and categorical sub-datasets:
num_features_to_remove <- names(missing_percent_num[missing_percent_num > threshold])
num_features_to_remove

cat_features_to_remove <- names(missing_percent_cat[missing_percent_cat > threshold])
cat_features_to_remove

# Removing the identified features from the sub-datasets
numeric_data <- numeric_data[, !(names(numeric_data) %in% num_features_to_remove)]
categorical_data <- categorical_data[, !(names(categorical_data) %in% cat_features_to_remove)]

dim(categorical_data)
dim(numeric_data)

# ------------------ 6.3. Third step :   Imputing missing values -------------

# imputing value of features with Less than 50 percent missing value

# 'Mean' imputation for 'numerical' data 
for (col in colnames(numeric_data)) {
  if (any(is.na(numeric_data[, col]))) {
    col_mean <- mean(numeric_data[, col], na.rm = TRUE)
    numeric_data[is.na(numeric_data[, col]), col] <- col_mean
  }
}


# 'Mode' imputation for 'categorical' data
for (col in colnames(categorical_data)) {
  if (any(is.na(categorical_data[, col]))) {
    col_mode <- names(table(categorical_data[, col]))[which.max(table(categorical_data[, col]))]
    categorical_data[is.na(categorical_data[, col]), col] <- col_mode
  }
}


# Checking for any remaining rows with null values
# for numerical data
has_null <- any(rowSums(is.na(numeric_data)) > 0)

if (has_null) {
  print("There are still rows with null values.")
} else {
  print("All rows are free of null values.")
}

# for categorical data
has_null <- any(rowSums(is.na(categorical_data)) > 0)

if (has_null) {
  print("There are still rows with null values.")
} else {
  print("All rows are free of null values.")
}



# /////////////////  Operation on numerical data /////////////////////


#  ////////////////////   7. Outlier detection for numerical data //////////////////////// 

# ---------      7.1.   First Step:   Finding Outliers by using Z score    -----------

# Writing a function to detect outliers in each features:
set.seed(1)

find_outlier <- function(data) {
  # empty vector to accumulate outlier
  outlier_point <- c()
  
  # Setting upper and lower limit to 3 standard deviations distance from Mean of data
  data_std <- sd(data)
  data_mean <- mean(data)
  outlier_cut_off <- data_std * 3
  
  lower_limit <- data_mean - outlier_cut_off
  upper_limit <- data_mean + outlier_cut_off
  
  # Detecting outliers
  for (outlier in data) {
    if (outlier > upper_limit || outlier < lower_limit) {
      outlier_point <- c(outlier_point, outlier)
    }
  }
  
  return(outlier_point)
}

name_numerical_features <- names(numeric_data)
name_numerical_features

# finding outliers for some important features
find_outlier(numeric_data$LotFrontage)
length(find_outlier(numeric_data$LotFrontage))

find_outlier(numeric_data$LotArea)
length(find_outlier(numeric_data$LotArea))

find_outlier(numeric_data$GarageArea)
length(find_outlier(numeric_data$GarageArea))

find_outlier(numeric_data$GrLivArea)
length(find_outlier(numeric_data$GrLivArea))

find_outlier(numeric_data$X1stFlrSF)
length(find_outlier(numeric_data$X1stFlrSF))

find_outlier(numeric_data$LowQualFinSF)
length(find_outlier(numeric_data$LowQualFinSF))

find_outlier(numeric_data$PoolArea)
length(find_outlier(numeric_data$PoolArea))


# ---------     7.2 Second Step: Concatenating all datasets -----------

# --- 7.2.1. First: 

# concatinating the SalePrice and Numeric_data Subset in order to plot and remove outliers
dim(numeric_data)
numeric_data <- cbind(numeric_data, SalePrice)
dim(numeric_data)


# --- 7.2.2. Second: 

# concatenating the 'categorical_data', 'numeric_data' and 'original_cat_dataset' sub-datasets, because
# when we delete one value from numerical data, it deletes it's row in dataset, so
# we need to merge all subsets to delete this row from all of them.

# we need to retrieve the names of the features (columns), because we will use these names to
# separate all_data dataset into different numeric and categorical sub-dataset:
feature_names_OCD <- colnames(original_cat_dataset)
feature_names_OCD

feature_names_CD <- colnames(categorical_data)
feature_names_CD

feature_names_ND <- colnames(numeric_data)
feature_names_ND


#  So we concatenate all datasets together.
all_data <- cbind(original_cat_dataset, categorical_data, numeric_data)


dim(all_data)



# ---------        7.3.   Third Step: Plotting and Handling Outliers  -----------

library(ggplot2) 

# Ploting the "Scatter Plot" for "LotFrontage", also Adding a line using the lm method (linear regression)
options(repr.plot.width=5, repr.plot.height=4)
ggplot(all_data, aes(x = all_data$LotFrontage , y = all_data$SalePrice)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  
  labs(x = "LotFrontage", y = "SalePrice") +
  ggtitle("Scatter Plot: LotFrontage vs. SalePrice")

# Deleting outliers from dataset based on displaying plot and choosing outliers range
all_data <- all_data[all_data$LotFrontage <= 200, ]
dim(all_data)

# Ploting the "Scatter Plot" for "LotFrontage", 'After' deleting ouliers
options(repr.plot.width=5, repr.plot.height=4)
ggplot(all_data, aes(x = all_data$LotFrontage , y = all_data$SalePrice)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  
  labs(x = "LotFrontage", y = "SalePrice") +
  ggtitle("Scatter Plot: LotFrontage vs. SalePrice")



# Plot the scatter plot for "LotArea"
options(repr.plot.width=5, repr.plot.height=4)
ggplot(all_data, aes(x = all_data$LotArea , y = all_data$SalePrice)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  
  labs(x = "LotArea", y = "SalePrice") +
  ggtitle("Scatter Plot: LotArea vs. SalePrice")

# Deleting outliers from dataset, then ploting again with above code
all_data <- all_data[all_data$LotArea <= 50000, ]
dim(all_data)



# Plot the scatter plot for "GarageArea"
options(repr.plot.width=5, repr.plot.height=4)
ggplot(all_data, aes(x = all_data$GarageArea , y = all_data$SalePrice)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  
  labs(x = "GarageArea", y = "SalePrice") +
  ggtitle("Scatter Plot: GarageArea vs. SalePrice")

# Deleting outliers from dataset, then ploting again
all_data <- all_data[all_data$GarageArea <= 1200, ]
dim(all_data)



# Plot the scatter plot for "GrLivArea"
options(repr.plot.width=5, repr.plot.height=4)
ggplot(all_data, aes(x = all_data$GrLivArea , y = all_data$SalePrice)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  
  labs(x = "GrLivArea", y = "SalePrice") +
  ggtitle("Scatter Plot: GrLivArea vs. SalePrice")

# Deleting outliers from dataset, then ploting again
all_data <- all_data[all_data$GrLivArea <= 3500, ]
dim(all_data)


#  -------------  7.4.   Forth Step:  Split dataset to numeric and categorical -----------

# Create a last datasets (categorical_data, numeric_data, ...) based on selected feature names
original_cat_dataset <- all_data[, feature_names_OCD]
dim(original_cat_dataset)

categorical_data <- all_data[, feature_names_CD]
dim(categorical_data)

numeric_data <- all_data[, feature_names_ND]
dim(numeric_data)  

# here numeric_data contains the 'SalePrice' feature, so we need to separate this feature before 
# any operation on numerical data, because for comparing with last models we keep scale of SalePrice

SalePrice <- numeric_data$SalePrice
length(SalePrice)

numeric_data <- numeric_data[, !(names(numeric_data) %in% c("SalePrice"))]
dim(numeric_data)



#  ////////////////////    8.     Data transformation (Log)     //////////////////////// 

# Data transformation - converting data into a different representation (e.g. log transformation)

#  Detecting the skewness of each numerical feature 
library(dplyr)
library(magrittr)
library(MASS)
library(e1071)

# Since if the skewness is between -0.5 to +0.5 then we can say data is fairly symmetrical. So  
# this analysis considers Threshold equal to 0.50.

# log transform skewed numeric features
numerical_feat <- names(numeric_data)[!sapply(numeric_data, is.factor)]

skewed_feats <- sapply(numeric_data[numerical_feat], function(x) skewness(x[!is.na(x)]))
skewed_feats_name <- names(skewed_feats[skewed_feats > 0.50])
skewed_feats_name

# 'log' operation on skewed features to reduce skewness
numeric_data[skewed_feats_name] <- lapply(numeric_data[skewed_feats_name], function(x) log1p(x))

numeric_data$LotFrontage



#  //////////////////   9.    Scaling 'all' numerical data //////////////////////// 

# Scaling transforms the value of all features to a specific range. and in some case normalize data

# --------  9.1    First : Checking 'Normality' of features by  plot ------

#  QQ plot and Histogram to assess normality of some features
qqnorm(numeric_data$LotFrontage)
qqline(numeric_data$LotFrontage, col = "red")

hist(numeric_data$LotFrontage, breaks = 'FD', main = "Distribution of LotFrontage", xlab = "LotFrontage Values")


qqnorm(numeric_data$LotArea)
qqline(numeric_data$LotArea, col = "red")

hist(numeric_data$LotArea, breaks = 'FD', main = "Distribution of LotArea", xlab = "LotArea Values")

qqnorm(numeric_data$GarageArea)
qqline(numeric_data$GarageArea, col = "red")

hist(numeric_data$GarageArea, breaks = 'FD', main = "Distribution of GarageArea", xlab = "LotFrontage GarageArea")


# --------  9.2    Second : Checking 'Normality' of features by function ------

# Vector of numerical feature names
numerical_feat2 <- names(numeric_data)
numerical_feat2

# creating an empty dataframe to store the results of for loop:
normality_table <- data.frame(Feature = character(),
                              Statistic = numeric(),
                              P_value = numeric(),
                              stringsAsFactors = FALSE)

# Testing Shapiro-Wilk to test normality of each feature
for (feat in numerical_feat2) {
  result <- shapiro.test(numeric_data[[feat]])
  
  # Storing results in dataframe
  normality_table <- rbind(normality_table, data.frame(Feature = feat,
                                                       Statistic = result$statistic,
                                                       P_value = result$p.value,
                                                       stringsAsFactors = FALSE))
}

# Printing the result:
print(normality_table)



# -----------     9.2    Handling Non-Normal data by Scaling:      ----------

# Here we used the "scale()" function, which standardizes the values. which 
# they have a mean of 0 and a standard deviation of 1.

# Checking number of outliers for example for "LotFrontage","GarageArea", and "LotArea" before scaling
num_out_LotF <- find_outlier(numeric_data$LotFrontage)
num_out_LotF
length(num_out_LotF)

num_out_LotArea <- find_outlier(numeric_data$LotArea)
num_out_LotArea
length(num_out_LotArea)

num_out_GarageArea <- find_outlier(numeric_data$GarageArea)
num_out_GarageArea
length(num_out_GarageArea)


#  -----------      9.3. Scaling data   -----------

# writing a for loop in order to Loop through each column of numeric_data and scale all columns.

# Iterate over the numeric columns excluding SalePrice
for (col in colnames(numeric_data)) 
{
  numeric_data[, col] <- scale(numeric_data[, col])
}

# Checking again number of outliers for example for "LotFrontage", "GarageArea", and "LotArea" After scaling
num_out_LotF2 <- find_outlier(numeric_data$LotFrontage)
num_out_LotF2
length(num_out_LotF2)

num_out_LotArea2 <- find_outlier(numeric_data$LotArea)
num_out_LotArea2
length(num_out_LotArea2)

num_out_GarageArea2 <- find_outlier(numeric_data$GarageArea)
num_out_GarageArea2
length(num_out_GarageArea2)

# scaling did not effect the number of outliers for these features 
# So scaling does not solve outliers problem in this dataset

# -------- Checking again Normality of features---- 
# creating an empty dataframe to store the results of for loop:
normality_table2 <- data.frame(Feature = character(),
                               Statistic = numeric(),
                               P_value = numeric(),
                               stringsAsFactors = FALSE)

# Testing Shapiro-Wilk to test normality of each feature
for (feat in numerical_feat2) {
  result <- shapiro.test(numeric_data[[feat]])
  
  # Storing results in dataframe
  normality_table2 <- rbind(normality_table, data.frame(Feature = feat,
                                                        Statistic = result$statistic,
                                                        P_value = result$p.value,
                                                        stringsAsFactors = FALSE))
}

# Printing the result:
print(normality_table2)


# -----------     9.4    Checking again and deleting most skewed data after scaling:      ----------
# In this step some features that even after scaling have more skewness will be deleted.
# Since if the skewness is more than 1 then we can say data is heavily skewed. So  
# Here we considers Threshold equal to 1.
library(dplyr)
library(broom)
library(stats)

library(magrittr)
library(MASS)
library(e1071)

numerical_feat2
most_skewd_features <- sapply(numeric_data[numerical_feat2], function(x) skewness(x[!is.na(x)]))
name_most_skewd_features <- names(most_skewd_features[most_skewd_features > 1])
name_most_skewd_features

# based of result, most_skewd_features are ("BsmtFinSF2", "LowQualFinSF", "PoolArea", "MiscVal")

# But Here if we do not take log Transform, some are data will have more skewness
# because scaling just scale data in same range and do not effect Skewness 
# a lot, so we need to operate log Transform first then other operation. otherwise 
# the result will be:
# [1] "LotArea"      "MasVnrArea"   "BsmtFinSF2"   "LowQualFinSF" "WoodDeckSF"  
# [6] "PoolArea"     "MiscVal"      "AllPorchSF"   "SalePrice"


# Plotting most skewed features:
plot(numeric_data$BsmtFinSF2, numeric_data$SalePrice, type = "p", main = "Scatter Plot", xlab = "BsmtFinSF2", ylab = "SalePrice")
plot(numeric_data$LowQualFinSF, numeric_data$SalePrice, type = "p", main = "Scatter Plot", xlab = "LowQualFinSF", ylab = "SalePrice")
plot(numeric_data$PoolArea, numeric_data$SalePrice, type = "p", main = "Scatter Plot", xlab = "PoolArea", ylab = "SalePrice")
plot(numeric_data$MiscVal, numeric_data$SalePrice, type = "p", main = "Scatter Plot", xlab = "MiscVal", ylab = "SalePrice")
# result shows that, these features have a lot of zero values that effect their shape.

# Deleting high skewed data from numerical sub_dataset after normalization: 
numeric_data <- numeric_data[, !(names(numeric_data) %in% c("BsmtFinSF2", "LowQualFinSF", "PoolArea", "MiscVal"))]
dim(numeric_data)


numerical_features_name <- names(numeric_data)
numerical_features_name



#  ////////////////  10. Correlation between features    ///////////// 


# ------- ----    10.1.   First step:  finding correlation and plotting

library(caret)
# Compute the correlation matrix
cor_matrix <- cor(cbind(numeric_data, SalePrice))
cor_matrix
# Store the result in a dataframe
corr_df <- as.data.frame(cor_matrix)

# Createing and displaying heatmap of correlation matrix
heatmap(cor_matrix, col = colorRampPalette(c("blue", "white", "red"))(100))

# -------------- 10.2.  Second step:  Multicollinearity  ----------------

# drop correlated features with correlation bigger than 80%  to avoid Multicollinearity.
library(caret)
# the findCorrelation() function exist in 'caret' package which are used to find highly correlated features.
# ?findCorrelation

# Finding highly correlated features
highly_correlated <- findCorrelation(cor_matrix, cutoff = 0.8)
highly_correlated
# Result shows that there is no highly correlated features, so we do not need delete any feature

# Drop the highly correlated features from the dataset
# numeric_data <- numeric_data[, !(colnames(data) %in% highly_correlated)]



#  /////////////////     Some Operation on 'Categorical' Data     ////////////////////


#  /////////////////  11.  Plotting 'categorical' variables -before labeling  ////////////////////

cat_fea_name2 <- names(categorical_data)
cat_fea_name2

# count house by "MSZoning", barplot 
library(ggplot2)

options(repr.plot.width=5, repr.plot.height=4)
ggplot(categorical_data, aes(x = MSZoning, fill = MSZoning )) + 
  geom_bar()+ 
  scale_fill_hue(c = 80)+
  ggtitle("Figure: Distribution of MSZoning")+
  theme(plot.title = element_text(hjust = 0.5),legend.position="right", legend.background = element_rect(fill="grey90",
                                                                                                         size=0.5, linetype="solid", 
                                                                                                         colour ="black"))+
  geom_text(stat='count',aes(label=..count..),vjust=-0.25)


# count house by "SaleType", barplot 
options(repr.plot.width=5, repr.plot.height=4)
ggplot(categorical_data, aes(x = SaleType, fill = SaleType )) + 
  geom_bar()+ 
  scale_fill_hue(c = 80)+
  ggtitle("Figure : Distribution of SaleType")+
  theme(plot.title = element_text(hjust = 0.5),legend.position="right", legend.background = element_rect(fill="grey90",
                                                                                                         size=0.5, linetype="solid", 
                                                                                                         colour ="black"))+
  geom_text(stat='count',aes(label=..count..),vjust=-0.25)


# count house by "Heating", barplot 
options(repr.plot.width=5, repr.plot.height=4)
ggplot(categorical_data, aes(x = Heating, fill = Heating )) + 
  geom_bar()+ 
  scale_fill_hue(c = 80)+
  ggtitle("Figure : Distribution of Heating")+
  theme(plot.title = element_text(hjust = 0.5),legend.position="right", legend.background = element_rect(fill="grey90",
                                                                                                         size=0.5, linetype="solid", 
                                                                                                         colour ="black"))+
  geom_text(stat='count',aes(label=..count..),vjust=-0.25)


# count house by "CentralAir", barplot 
options(repr.plot.width=5, repr.plot.height=4)
ggplot(categorical_data, aes(x = CentralAir, fill = CentralAir )) + 
  geom_bar()+ 
  scale_fill_hue(c = 80)+
  ggtitle("Figure : Distribution of CentralAir")+
  theme(plot.title = element_text(hjust = 0.5),legend.position="right", legend.background = element_rect(fill="grey90",
                                                                                                         size=0.5, linetype="solid", 
                                                                                                         colour ="black"))+
  geom_text(stat='count',aes(label=..count..),vjust=-0.25)


# count house by "HouseStyle", barplot 
options(repr.plot.width=5, repr.plot.height=4)
ggplot(categorical_data, aes(x = HouseStyle, fill = HouseStyle )) + 
  geom_bar()+ 
  scale_fill_hue(c = 80)+
  ggtitle("Figure : Distribution of HouseStyle")+
  theme(plot.title = element_text(hjust = 0.5),legend.position="right", legend.background = element_rect(fill="grey90",
                                                                                                         size=0.5, linetype="solid", 
                                                                                                         colour ="black"))+
  geom_text(stat='count',aes(label=..count..),vjust=-0.25)



#  /////////////////     12.   Encoding Categorical variables -  ////////////////////

# Encoding method converts categorical variables into numerical format to be used in model.
# Two Strategy: 1. label Encoding , 2. One-hot Encoding

# -------------- 12.1.  First step:  Separating categorical features in two group  ----------------

# Here we Separate our categorical data in two groups, one features with more than four categories (factors)
# and other with less than equal four categories (factors) to avoid dimensionality problem,
# because number of data points in this dataset is small.

# Geting unique categories that exist inside each column with using "unique" function:
unique_categories <- lapply(categorical_data, unique)
print(unique_categories)

unique_categories[2]

# Finding features more than four categories (factors)
unique_catList <- lapply(unique_categories, function(x) length(x) > 4)
unique_catList

unique_catGreater4 <- names(unique_catList[unique_catList == TRUE])
unique_catGreater4
# creating sub-dataset for features More than four categories
unique_catGreater4_data <- categorical_data[unique_catGreater4]
dim(unique_catGreater4_data)


# Finding features Less than equal four categories (factors)
unique_catLess4 <- names(unique_catList[unique_catList == FALSE])
unique_catLess4
# creating sub-dataset for features Less than four categories
unique_catLess4_data <- categorical_data[unique_catLess4]
dim(unique_catLess4_data)


# ---- 12.2.  Second step: transform features with ordinal value to 'unique_catGreater4' sub-dataset

# Here some features with ordinal value such as:"ExterQual","LotShape", "KitchenQual" are deleted from
# 'unique_catLess4' sub-dataset and then are added to 'unique_catGreater4' sub-dataset to avoid dimentionality.

#  'unique_catGreater4' dataset

unique_catLess4_ordinal <- unique_catLess4_data[, (colnames(unique_catLess4_data) %in% c("ExterQual", "LotShape", "KitchenQual"))]
dim(unique_catLess4_ordinal)
unique_catLess4_ordinal

dim(unique_catLess4_data)

# Removing above features from 'unique_catLess4_data' sub-dataset:
unique_catLess4_data <- unique_catLess4_data[, !(colnames(unique_catLess4_data) %in% c("ExterQual", "LotShape", "KitchenQual"))]
dim(unique_catLess4_data)

# then we add unique_catLess4_ordinal to'unique_catGreater4_data' sub-dataset, since we want to do Label encode on them
dim(unique_catGreater4_data)
unique_catGreater4_data <- cbind(unique_catGreater4_data, unique_catLess4_ordinal )
dim(unique_catGreater4_data)

# -------------   12.3.  Third step:    Encoding     ---------

# ---------------  12.3.1.    Label Encoding ----------------

#  label encoding is applied on features with more than 4 categories and some 
# ordinal data with less than four features:

# install.packages("superml") 
library(superml)

# Implementing Label Encoding on all features with for loop:
for(i in 1:length(unique_catGreater4_data)) 
{
  factors <- factor(unique_catGreater4_data[[i]])
  unique_catGreater4_data[[i]] <- as.numeric(factors)
}

# I removed the features with categorical data which have more than 4 category from the
# original categorical_data dataset, then "replace" the new Encoding features.
categorical_data[unique_catGreater4] <- unique_catGreater4_data[unique_catGreater4] 
unique_catGreater4_data


# ---------------  12.3.2.    One-hot Encoding ----------------

# we apply one-hot encoding on features with less than equal 4 categories.

#install.packages("caret") 
library(caret)

# Create dummy variables 
dummy_data <- dummyVars(~., data = unique_catLess4_data)
encoded_data <- predict(dummy_data, newdata = unique_catLess4_data)
dim(encoded_data)
encoded_data = data.frame(encoded_data)
encoded_data

# removing original categorical features from categorical_data sub-dataset
categorical_data[unique_catLess4] <- NULL


# ---------------  12.3.3.    Concatenating Encoded data ----------------

# adding All Encoded data by combining two sub-dataset together
categorical_data <- cbind(categorical_data, encoded_data)

dim(categorical_data)
dim(numeric_data)
dim(original_cat_dataset)


# Now categorical dataset is encoded and ready for any operation.



#  /////////////////   13.  Merging numerical and categorical sub-datasets into one dataset  /////////

# Adding  all datasets together. Specially "original_cat_dataset" dataset which was separated in the first steps

all_data <- cbind(original_cat_dataset, categorical_data, numeric_data)

dim(all_data)
length(SalePrice)

# adding "SalePrice" column to dataset. 
all_data$SalePrice <- SalePrice
dim(all_data)




#   ///////////////    14. Splitting data to train and test data  ////////////////////

library(caret)
set.seed(123)  # Set a seed for reproducibility

# Splitting the data into train and test. 80 % of the sample size
train_indices <- sample(nrow(all_data), round(0.8 * nrow(all_data)))  # 70% for train
train_data <- all_data[train_indices, ]
test_data <- all_data[-train_indices, ]

# Separating the target variable(SalePrice) and features in train data
train_label <- train_data$SalePrice
train_data <- train_data[, - which(names(train_data) == "SalePrice")]
train_label
# Separating the target variable (SalePrice) and features in test data
test_label <- test_data$SalePrice
test_data <- test_data[, - which(names(test_data) == "SalePrice")]
test_data



# /////////////////      16.	Model Fitting , After deleting some Outliers    ///////////


# ----------- 16. 1.  Linear Regression      --------

library(dplyr)
library(broom)
library(stats)

# Creating a linear regression
lm_model = lm(train_label~ ., data = train_data)

summary(lm_model)

# Make predictions with test data
predictions <- predict(lm_model, newdata = test_data)

length(predictions)
length(test_label)

#  Evaluating the model
mse <- mean((predictions - test_label)^2)
rmse <- sqrt(mse)
r_squared <- cor(predictions, test_label)^2

cat("Mean Squared Error (MSE):", mse, "\n")
cat("Root Mean Squared Error (RMSE):", rmse, "\n")
cat("R-squared Value:", r_squared, "\n")


#  Visualize the predictions
# Creating a dataframe with actual and predicted values
results <- data.frame(Actual_data = test_label, Predicted_data = predictions)

# Plotting the scatter plot of actual vs predicted values
options(repr.plot.width=5, repr.plot.height=4)
ggplot(results, aes(x = Actual_data, y = Predicted_data)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(x = "Actual_data", y = "Predicted_data") +
  ggtitle("Actual vs Predicted Prices")


# residual plot an its normality test      
residuals <- test_label - predictions

# Residual plot
plot(predictions, residuals, xlab = "Predicted Values", ylab = "Residuals", main = "Residual Plot")
abline(h = 0, col = "red")

# Plot a histogram and a Q-Q plot of the residuals
hist(residuals, main = "Histogram of Residuals")
qqnorm(residuals)
qqline(residuals)


# Shapiro-Wilk test for normality
shapiro.test(residuals)

# Check for endogeneity using the Durbin-Watson test (autocorrelation)
library(lmtest)
dwtest(lm_model)




# --------------- 16.2. Lasso Regression     --------

library(glmnet)

#perform k-fold cross-validation to find optimal lambda value
x <- data.matrix(train_data)
lasso_model <- cv.glmnet(x, train_label, alpha = 1)
lasso_model

# Plot the cross-validation results
plot(lasso_model)

# find optimal lambda value based on cross-validation that minimizes test MSE
best_lambda <- lasso_model$lambda.min
best_lambda

# find coefficients of best model
best_model <- glmnet(train_data, train_label, alpha = 1, lambda = best_lambda)
coef(best_model)

#  test_data
new = data.matrix(test_data) 

# predict response value
y_pred_lasso = predict(best_model, s = best_lambda, newx = new)

# evaluation metrics
mse_lasso <- mean((y_pred_lasso - test_label)^2)
rmse_lasso <- sqrt(mse_lasso)
r_squared_lasso <- cor(y_pred_lasso, test_label)^2

cat("Mean Squared Error (MSE):", mse_lasso, "\n")
cat("Root Mean Squared Error (RMSE):", rmse_lasso, "\n")
cat("R-squared:", r_squared_lasso, "\n")





# --------------- 16.3. Ridge Regression     --------

library(glmnet)

#perform k-fold cross-validation to find optimal lambda value
x <- data.matrix(train_data)
ridge_model <- cv.glmnet(x, train_label, alpha = 0)
ridge_model
# Plot the cross-validation results
plot(ridge_model)

# find optimal lambda value based on cross-validation that minimizes test MSE
best_lambda <- ridge_model$lambda.min
best_lambda

#finding coefficients of best model
best_model <- glmnet(train_data, train_label, alpha = 0, lambda = best_lambda)
coef(best_model)

# define test data
new = data.matrix(test_data) 

# prediction target value
y_pred_ridge = predict(best_model, s = best_lambda, newx = new)

# evaluation metrics
mse_ridge <- mean((y_pred_ridge - test_label)^2)
rmse_ridge <- sqrt(mse_ridge)
r_squared_ridge <- cor(y_pred_ridge, test_label)^2

cat("Mean Squared Error (MSE):", mse_ridge, "\n")
cat("Root Mean Squared Error (RMSE):", rmse_ridge, "\n")
cat("R-squared:", r_squared_ridge, "\n")






# --------------- 16.4. Decision Tree Regression     --------


library(rpart)

regressor_dt <- rpart(formula = train_label~ ., data = train_data,  method = "anova")

# prediction
y_pred_dt = predict(regressor_dt, test_data)

printcp(regressor_dt)
library("rpart.plot")
rpart.plot(regressor_dt)


# evaluation metrics
mse_dt <- mean((y_pred_dt - test_label)^2)
mse_dt
rmse_dt <- sqrt(mse_dt)
rmse_dt
r_squared_dt <- cor(y_pred_dt, test_label)^2
r_squared_dt






# --------------- 16.5. Random Forest Regression     --------
#?randomForest

library(randomForest)
set.seed(123)
regressor_RF <- randomForest(x = train_data, y = train_label, ntree = 1000)

# prediction
y_pred_RF <-predict(regressor_RF, test_data)

# evaluation metrics
mse_RF <- mean((y_pred_RF - test_label)^2)
mse_RF
rmse_RF <- sqrt(mse_RF)
rmse_RF
r_squared_RF <- cor(y_pred_RF, test_label)^2
r_squared_RF



# --------------- 16.6. support vector Regression     --------

library(e1071)

svm_fit = svm(formula = train_label ~ .,
              data = train_data,
              type = 'eps-regression',
              kernel = "radial", gamma = 0.002)

print(svm_fit)

# Predicting a new result
y_pred = predict(svm_fit, test_data)

#  Evaluate the model
mse_svm <- mean((y_pred - test_label)^2)
mse_svm
rmse_svm <- sqrt(mse_svm)
rmse_svm
r_squared_svm <- cor(y_pred, test_label)^2
r_squared_svm


# different kernel types:

# Train the SVM regression model with a polynomial kernel
svm_model_poly <- svm(train_labels ~ ., data = train_data, kernel = "polynomial", degree = 5)
svm_model_poly <- svm(train_labels ~ ., data = train_data, kernel = "radial", degree = 2)
# Train the SVM regression model with an radial kernel
svm_model_radial <- svm(train_labels ~ ., data = train_data, kernel = "radial", gamma = 0.002)

# Train the SVM regression model with a sigmoid kernel
svm_model_sigmoid <- svm(train_labels ~ ., data = train_data, kernel = "sigmoid")

# Train the SVM regression model with a linear kernel
svm_model_sigmoid <- svm(train_labels ~ ., data = train_data, kernel = "linear")
svm_model_radial <- svm(train_labels ~ ., data = train_data, kernel = "linear", gamma = 0.002)







# --------------- 16.7. Gradient Boosting    --------

library(gbm)
library(caret)
library(dplyr)
library(ggplot2)

# Fitting the Gradient Boosting model:
# In Gradient Boosting Model, the distribution 'Gaussian' is used for for regression.
gbm_model <- gbm(
  formula = train_label ~ .,
  data = train_data,
  distribution = "gaussian", 
  n.trees = 1000,
  interaction.depth = 4,
  cv.folds = 10,
  shrinkage = 0.005,
  verbose = FALSE)


# Plot the variable importance
var_importance <- gbm.perf(gbm_model, plot.it = FALSE)
var_importance
plot(var_importance, main = "Variable Importance")

# predictions
predictions <- predict(gbm_model, newdata = test_data, n.trees = gbm_model$best.iteration)

# Evaluate the model
mse <- mean((predictions - test_label)^2)
rmse <- sqrt(mse)
rsquared <- cor(predictions, test_label)^2

cat("Mean Squared Error (MSE):", mse, "\n")
cat("Root Mean Squared Error (RMSE):", rmse, "\n")
cat("R-squared Value:", rsquared, "\n")



