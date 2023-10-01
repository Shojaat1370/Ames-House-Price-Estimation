

# //// Pre-Processing and Clustering Algorithm analysis on "Ames House Price" dataset  //////


# ////-------------------- 1.  Pre-Processing ---------------/////


# //////////     1. 1. Import the dataset and description of dataset //////////////////

data <- read.csv("C:/00 Transferable Folders/01 Unimi- Uni Milan/03 Third Trimester/02 Statistics for ML. Prof. Salini/Project/House Price/train.csv", stringsAsFactors = F)
dim <- dim(data)

# "structure" of the data
str(data)

# storing and then removing ID as they are useless. but keeping them as "labels"
data_Ids <- data.frame(data$Id)
data$Id <- NULL

# ////////////////     1. 2.    Feature Engineer and combining        /////////

# since 'MSSubClass' is originally a categorical data, so we need to convert it to
# to character then encode them in categorical sub-dataset 
# then we will do lable encoding on them.
library(car)
library(dplyr)

data <- data %>% mutate(MSSubClass=recode(MSSubClass, `20` = "SC20", `30` = "SC30", `40` = "SC40", `45` = "SC45", 
                                          `50` = "SC50", `60` = "SC60", `70` = "SC70", `75` = "SC75", 
                                          `80` = "SC80", `85` = "SC85", `90` = "SC90", `120` = "SC120", 
                                          `150` = "SC150", `160` = "SC160", `180` = "SC180", '190' = "SC190" ))


# Combinations of some features that have same meaning to to avoid dimension issue:

# 1. Creating Total number of bathrooms (TotalBath) by Combinations of four features 
data["TotalBath"] <- data["BsmtFullBath"] + (0.5 * data["BsmtHalfBath"]) + data["FullBath"] + (0.5 * data["HalfBath"])
dim(data)

# deleting above used features from dataset
data <- data[, !(names(data) %in% c("BsmtFullBath", "BsmtHalfBath","FullBath", "HalfBath"))]
dim(data)
# 
# 2. Creating Total square feet of porch area (AllPorchSF) by Combinations of four following features
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




#  ////////////////   1.3   Discovering numerical value which originally categorical //////////

# In dataset exist some variable which have numerical value, but actually they are 
# categorical data, so first, before any operation on numerical data we need to separate them
# from numerical data and add them to categorical sub-dataset after
#  we operate Encoding on categorical data.

# these variable are:
# TotalBath  # new created features. total number of Bath
# OverallQual   # Overall material and finish quality
# OverallCond    # Overall condition rating
# BedroomAbvGr       # Number of bedrooms above basement level
# TotRmsAbvGrd  # Total rooms above grade
# Fireplaces   # Number of fireplaces
# GarageCars   # Size of garage in car capacity. number of cars that can park
# MoSold       # month sold
# KitchenAbvGr   # Number of Kitchens above grade


# creating sub-dataset for these variable by using original dataset is called "data"
original_cat_dataset <- data[, c("TotalBath", "OverallQual", "OverallCond", "BedroomAbvGr", "TotRmsAbvGrd", "Fireplaces", "GarageCars", "MoSold", "KitchenAbvGr")]
dim(original_cat_dataset)
# then deleting them from original dataset then creating new dataset without them. "data2"
data2 <- data[, !(names(data) %in% c("TotalBath", "OverallQual", "OverallCond", "BedroomAbvGr", "TotRmsAbvGrd", "Fireplaces", "GarageCars", "MoSold", "KitchenAbvGr"))]
dim(data2)



# ////////////////////   1.4 . Separating Numerical features and Categorical features ///////

#     we get features that have 'numeric' values and 'categorical' values their index(column:index)
num_features <- which(sapply(data2, is.numeric))
num_features

cat_features <- which(sapply(data2, is.character))
cat_features

#      we save column names in a variable for both
num_fea_name <- names(num_features)
num_fea_name

cat_fea_name <- names(cat_features)
cat_fea_name

# create a sub-dataset for all numerical features and all categorical features separately

# creating sub-dataset for numerical features
numeric_data <- data2[, num_fea_name]

# creating sub-dataset for categorical features
categorical_data <- data2[, cat_fea_name]

str(categorical_data)
dim(categorical_data)
dim(numeric_data)
str(numeric_data)


# /////////////////////////   1.5. Missing data handling - /////////////////////////


# -------     1.5.1. First :   Not exist data ---------

# some missing data are not missing , they are NA like for FireplaceQu (Fireplace quality)
# NA means 'No Fireplace' which we need to put them to unknown or put 0 and impute manually 

# GarageFinish, GarageQual, GarageCond

# Replace values in the 'GarageFinish' column - then add to categorical data

categorical_data$GarageFinish[is.na(categorical_data$GarageFinish)] <- "None"

categorical_data$GarageQual[is.na(categorical_data$GarageQual)] <- "None"

categorical_data$GarageCond[is.na(categorical_data$GarageCond)] <- "None"

categorical_data$BsmtQual[is.na(categorical_data$BsmtQual)] <- "None"

categorical_data$BsmtCond[is.na(categorical_data$BsmtCond)] <- "None"


# encoding manually

categorical_data$GarageFinish <- recode(categorical_data$GarageFinish,
                                        'None'  = 0,
                                        'Unf'   = 1,
                                        'RFn'   = 2,
                                        'Fin'   = 3)


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


# ----- deleting above used features: ----
# deleting these features from catogorical sub-dataset , then adding to original_cat_dataset 
# which is sub-datset that contain features with integer data while originally they have categorical type.
# creating sub-dataset for these variable in order to combine with last "original_cat_dataset":

# creating new original_cat_dataset2 for these features to add last original_cat_dataset
original_cat_dataset2 <- categorical_data[, c("GarageFinish", "GarageQual", "GarageCond", "BsmtCond", "BsmtQual")]
original_cat_dataset2

# Add the new sub-dataset to the last original_cat_dataset sub-dataset
original_cat_dataset <- cbind(original_cat_dataset, original_cat_dataset2)
dim(original_cat_dataset)

# then deleting these features from catogorical sub-dataset
categorical_data <- categorical_data[, !(names(categorical_data) %in% c("GarageFinish", "GarageQual", "GarageCond", "BsmtCond", "BsmtQual"))]
dim(categorical_data)



# -------   1.5.2. Second step: removing features with more than 50 percent missing -------

# finding missing data and removing data which has more than 50 percent missing values

# Set the threshold percentage
threshold <- 50

# Calculate the percentage of missing values for each feature in both sub datasets
missing_percent_num <- colMeans(is.na(numeric_data)) * 100
missing_percent_num
missing_percent_cat <- colMeans(is.na(categorical_data)) * 100
missing_percent_cat
# This uses the colMeans function along with is.na to create a logical matrix indicating missing values
# and then computes the mean of each column. Multiplying by 100 gives the percentage. 

# Identify the features with more than the threshold percentage of missing values
num_features_to_remove <- names(missing_percent_num[missing_percent_num > threshold])
num_features_to_remove

cat_features_to_remove <- names(missing_percent_cat[missing_percent_cat > threshold])
cat_features_to_remove

# deleted features are : "Alley"  , "PoolQC" , "Fence" ,"MiscFeature", which all are categorical data.

# Remove the identified features from the dataset
numeric_data <- numeric_data[, !(names(numeric_data) %in% num_features_to_remove)]
categorical_data <- categorical_data[, !(names(categorical_data) %in% cat_features_to_remove)]

dim(categorical_data)
dim(numeric_data)


# ------------------  1.5.3. Third step :   imputing missing values -------------

# imputing value of features with less than 50 percent missing value

# Mean imputation for 'numerical' data 
for (col in colnames(numeric_data)) {
  if (any(is.na(numeric_data[, col]))) {
    col_mean <- mean(numeric_data[, col], na.rm = TRUE)
    numeric_data[is.na(numeric_data[, col]), col] <- col_mean
  }
}


# Mode imputation for 'categorical' data
for (col in colnames(categorical_data)) {
  if (any(is.na(categorical_data[, col]))) {
    col_mode <- names(table(categorical_data[, col]))[which.max(table(categorical_data[, col]))]
    categorical_data[is.na(categorical_data[, col]), col] <- col_mode
  }
}


# Check for any remaining rows with null values
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



# ---------------   1.5.4. Forth step: Label Encoding ---------------

# we apply label encoding on features with more than 4 categories to avoid very large feature set as we can
# end up with dimensional issue.

# Get unique categories in each column
unique_categories <- lapply(categorical_data, unique)
print(unique_categories)

#install.packages("superml")
library(superml)

encoded_data = categorical_data

for(i in 1:length(unique_categories)) 
{
  factors <- factor(encoded_data[[i]])
  encoded_data[[i]] <- as.numeric(factors)
}

# we need to remove the categorical ones with more than 4 category from original data, then add the new labeled.
categorical_data <- encoded_data

dim(categorical_data)


#  //////////////////   1.6:   Merge numerical and binary features into one data set  /////////

# Adding all datasets together. 

all_data <- cbind(original_cat_dataset, categorical_data, numeric_data)

dim(all_data)


#  //////////////////  1.7 . Outlier detection for numerical data /////////////////

# --------- First Step: function and using Z score -----------

set.seed(1)

# Function to detect outliers in "one-dimensional " datasets
find_anomalies <- function(data) {
  # Define a vector to accumulate anomalies
  anomalies <- c()
  
  # Set upper and lower limit to 3 standard deviations
  data_std <- sd(data)
  data_mean <- mean(data)
  anomaly_cut_off <- data_std * 3
  
  lower_limit <- data_mean - anomaly_cut_off
  upper_limit <- data_mean + anomaly_cut_off
  
  # Detect outliers
  for (outlier in data) {
    if (outlier > upper_limit || outlier < lower_limit) {
      anomalies <- c(anomalies, outlier)
    }
  }
  
  return(anomalies)
}

# finding outliers for some important features
find_anomalies(all_data$LotFrontage)
find_anomalies(all_data$LotArea)
find_anomalies(all_data$GarageArea)


# --------- First Step: Ploting -----------

# using scatter plot for numerical values in order to display the shape to find
# existence of any outliers and non-linear relationship.
# Ploting some important

library(ggplot2)

# Ploting the "Scatter Plot" for "LotFrontage", also Adding a line using the lm method (linear regression)
options(repr.plot.width=5, repr.plot.height=4)
ggplot(all_data, aes(x = all_data$LotFrontage , y = SalePrice)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  
  labs(x = "LotFrontage", y = "SalePrice") +
  ggtitle("Scatter Plot: LotFrontage vs. SalePrice")

all_data <- all_data[all_data$LotFrontage <= 200, ]


# Plot the scatter plot for "LotArea"
options(repr.plot.width=5, repr.plot.height=4)
ggplot(all_data, aes(x = all_data$LotArea , y = SalePrice)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  
  labs(x = "LotArea", y = "SalePrice") +
  ggtitle("Scatter Plot: LotArea vs. SalePrice")

all_data <- all_data[all_data$LotArea <= 100000, ]

# Plot the scatter plot for "GrLivArea"
options(repr.plot.width=5, repr.plot.height=4)
ggplot(all_data, aes(x = all_data$GrLivArea , y = SalePrice)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  
  labs(x = "GrLivArea", y = "SalePrice") +
  ggtitle("Scatter Plot: GrLivArea vs. SalePrice")

all_data <- all_data[all_data$GrLivArea <= 4000, ]


options(repr.plot.width=5, repr.plot.height=4)
ggplot(all_data, aes(x = data$MSZoning , y = data$SalePrice)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  
  labs(x = "MSZoning", y = "SalePrice") +
  ggtitle("Scatter Plot: MSZoning vs. SalePrice")

data$BedroomAbvGr
unique(data$BedroomAbvGr)
# ...................   1.7 .     Final data  ...............

train_data <- all_data



# //////////////////  part 2.   Unsupervised Learning Algorithms ///////////


# ...................    2.1.    K-Means Clustering ...............

# some libraries

library(cluster)
library(tidyverse)

library(factoextra)

# optimal number of clusters
fviz_nbclust(train_data,kmeans,method="wss")+geom_vline(xintercept=5,linetype=2)
fviz_nbclust(train_data,kmeans,method="wss")+geom_vline(xintercept = 7,linetype=2)

set.seed(123)
k <- 5
km.res <- kmeans(train_data, k, nstart = 10)

# Print the results
print(km.res$centers)  # this shows center of clusters for each features 

print(km.res$size) 

print(km.res$betweenss/km.res$totss)

# clustering of all data in two dimensions:
# visualize the clusters created by the k-means clustering model (km.res)
# using a scatter plot with additional features.
fviz_cluster(km.res, train_data, geom = "point",ellipse.type = "norm",repel = TRUE)




# assigning cluster number to each row
train_data['cluster']=as.factor(km.res$cluster)

head(train_data)

# ploting for clustering on all variables
clusplot(train_data, train_data$cluster, main='2D representation of the Cluster solution', color=TRUE, shade=TRUE, labels=2, lines=0)


# ----- Separation based on features among 5 clusters: ----

# seperation in SalePrice among 5 clusters. 
spentplot <- ggplot(train_data, aes(x=cluster,y=train_data$SalePrice,fill=cluster)) + geom_boxplot(outlier.colour="black",
                                                                                   outlier.shape=16, outlier.size=2, notch=T)
spentplot


# y = Neighborhood, x=cluster , 
spentplot = ggplot(train_data, aes(x=cluster,y=train_data$Neighborhood,fill=cluster))+geom_boxplot(outlier.colour="black",
                                                                        outlier.shape=16,outlier.size=2, notch=T)
spentplot


# y=MSZoning
numdealplot = ggplot(train_data, aes(x=cluster,y=train_data$MSZoning,fill=cluster))+geom_boxplot(outlier.colour="black",
                                                                                        outlier.shape=16,outlier.size=2, notch=T)
numdealplot

# y = OverallQual
numdealplot = ggplot(train_data, aes(x=cluster,y=train_data$OverallQual,fill=cluster))+geom_boxplot(outlier.colour="black",
                                                                                                 outlier.shape=16,outlier.size=2, notch=T)
numdealplot



# -----------Visualize the clusters based on some important features --- 

# colored points  according to the "cluster" variable. 

library(ggplot2)

# x = YearBuilt, y = SalePrice
ggplot(train_data, aes(x = YearBuilt, y = SalePrice, color = factor(cluster))) +
  geom_point() +
  labs(x = "YearBuilt", y = "SalePrice", color = "cluster") +
  theme_minimal()


fviz_cluster(km.res, train_data, stand = FALSE, geom = "point", ellipse.type = "norm", repel = TRUE, choose.vars=c("YearBuilt", "SalePrice"))


# x = LotArea, y = SalePrice
ggplot(train_data, aes(x = LotArea, y = SalePrice, color = factor(cluster))) +
  geom_point() +
  labs(x = "LotArea", y = "SalePrice", color = "cluster") +
  theme_minimal()


fviz_cluster(km.res, train_data, stand = FALSE, geom = "point", ellipse.type = "norm", repel = TRUE, choose.vars=c("LotArea", "SalePrice"))


# x = OverallCond
ggplot(train_data, aes(x = OverallCond, y = SalePrice, color = factor(cluster))) +
  geom_point() +
  labs(x = "OverallCond", y = "SalePrice", color = "cluster") +
  theme_minimal()


fviz_cluster(km.res, train_data, stand = FALSE, geom = "point", ellipse.type = "norm", repel = TRUE, choose.vars=c("OverallCond", "SalePrice"))


# x = BedroomAbvGr
ggplot(train_data, aes(x = BedroomAbvGr, y = SalePrice, color = factor(cluster))) +
  geom_point() +
  labs(x = "BedroomAbvGr", y = "SalePrice", color = "cluster") +
  theme_minimal()

fviz_cluster(km.res, train_data, stand = FALSE, geom = "point", ellipse.type = "norm", repel = TRUE, choose.vars=c("BedroomAbvGr", "SalePrice"))


# x = Neighborhood
ggplot(train_data, aes(x = Neighborhood, y = SalePrice, color = factor(cluster))) +
  geom_point() +
  labs(x = "Neighborhood", y = "SalePrice", color = "cluster") +
  theme_minimal()


fviz_cluster(km.res, train_data, stand = FALSE, geom = "point", ellipse.type = "norm", repel = TRUE, choose.vars=c("Neighborhood", "SalePrice"))


#x = GarageArea
ggplot(train_data, aes(x = GarageArea, y = SalePrice, color = factor(cluster))) +
  geom_point() +
  labs(x = "GarageArea", y = "SalePrice", color = "cluster") +
  theme_minimal()


fviz_cluster(km.res, train_data, stand = FALSE, geom = "point", ellipse.type = "norm", repel = TRUE, choose.vars=c("GarageArea", "SalePrice"))



# x = MSZoning
ggplot(train_data, aes(x = MSZoning, y = SalePrice, color = factor(cluster))) +
  geom_point() +
  labs(x = "MSZoning", y = "SalePrice", color = "cluster") +
  theme_minimal()


fviz_cluster(km.res, train_data, stand = FALSE, geom = "point", ellipse.type = "norm", repel = TRUE, choose.vars=c("MSZoning", "SalePrice"))


# Foundation: Type of foundation
ggplot(train_data, aes(x = Foundation, y = SalePrice, color = factor(cluster))) +
  geom_point() +
  labs(x = "Foundation", y = "SalePrice", color = "cluster") +
  theme_minimal()


fviz_cluster(km.res, train_data, stand = FALSE, geom = "point", ellipse.type = "norm", repel = TRUE, choose.vars=c("Foundation", "SalePrice"))





# -------------------- 2.2.     Hierarchical Clustering ----------------


# Perform hierarchical clustering
dist_mat <- dist(train_data)       # Calculate distance matrix
hclust_result <- hclust(dist_mat, method = "ward.D2")  

plot(hclust_result)
plot(hclust_result, hang = -1)

# Cut the dendrogram to obtain clusters
k <- 5  # Number of clusters
clusters <- cutree(hclust_result, k)

plot(x = hclust_result, labels =  row.names(hclust_result), cex = 0.5)
rect.hclust(tree = hclust_result, k = 5, which = 1:5, border = 1:6, cluster = clusters)





