###############################################################################
# You can use this template to draft the script for your Assessment 2 of SIT718.
###############################################################################
# save your code as "name-code.R" (where ''name'' is replaced 
# with your surname or first name).
###############################################################################
install.packages("ggplot2")
install.packages("dgof")
install.packages("e1071")
install.packages("raster")
install.packages('stringr')
install.packages('moments')
library(e1071)
library(ggplot2)
library(dgof)
library(raster)
library(stringr)
library(moments)
###############################################################################
#------------------------------------------------------------------------------
#Question 1 - Understand the Data
#------------------------------------------------------------------------------
###############################################################################
# Set working directory
setwd('D:/MDS-Deakin/SIG718 - Real World Analytics/Mid-Term Assignment')
# Read file using read.table
data.raw <- as.matrix(read.table("RedWine.txt"))
# using your student ID number for reproducible sampling with the seed function
set.seed(224918243) 
# create a subset of the data (sample)
data.subset <- data.raw[sample(1:1599, 400), c(1:6)]
df <- as.data.frame(data.subset)
# store column names in an array
data.variable.names <- c("citric acid", "chlorides"
                         , "total sulfur dioxide", "pH", "alcohol")
features <- data.variable.names
label <- "quality"
###############################################################################
# Create 5 scatterplots function (for each X variable against 
#   the variable of interest Y) 
###############################################################################
# The function takes the data frame, list of x column names & the label column
# name as parameters. The function requires y column to be the last in the 
# data frame
scatter_plots <- function(df, features, label) {
  # Get the total number of features
  numcols <- length(features)
  # Use that to determine the number of plots to be created
  # Use par(mfrow..) to create placeholders for subplots
  par(mfrow = c(2,(numcols%/%2+1)))
  # Iterate through all columns, set the labels for x / y axes
  # and plot the scatter plot for each x against y ("Quality")
  for(i in (1:numcols)) {
    plot(df[,(numcols+1)], df[,i], xlab=label, ylab=features[i])
  }
  mtext("Scatterplots for all features(X) against variable of interest(Y)"
        , side = 3, line = - 2, outer = TRUE)
}
# Call the scatter_plots function to print 5 scatterplots
scatter_plots(df, features, "quality")
###############################################################################
# Create 6 histograms for each X variable and Y
###############################################################################
histograms <- function(df, features, label) {
  # Get the total number of cols
  numcols <- ncol(df)
  if(numcols%%2==0) {n=numcols%/%2} else {n=(numcols%/%2+1)}
  # Use that to determine the number of plots to be created
  # Use par(mfrow..) to create placeholders for subplots
  par(mfrow = c(2,n))
  # Iterate through all columns, set the labels for x / y axes
  # and plot the scatter plot for each x against y ("Quality")
  for(i in (1:numcols)) {
    xlab = features[i]
    if (i==numcols) {xlab=label}
    hist(df[,i], xlab=xlab, main="")
  }
  mtext("Histograms for all features(X) including variable of interest(Y)"
        , side = 3, line = - 2, outer = TRUE)
}
histograms(df, features, "quality")
###############################################################################
# As can be inferred from the histograms, we see the following skews
# Positively Skewed : Citric Acid, Chlorides, Total Sulfur Dioxide, Alcohol
# Almost no skew : Citric Acid ( only scaling, no transformation needed )
# Extreme skew : Chlorides ( inverse / reciprocal transformation )
# Greater skew : Total Sulfur Dioxide ( log transformation )
# Moderate skew : Alcohol ( sqrt transformation )
###############################################################################
#------------------------------------------------------------------------------
# Question 2 - Transform the Data
#------------------------------------------------------------------------------
###############################################################################
# Rename column names in dataframe df 
names(df)[names(df)=="V1"] <- features[1]
names(df)[names(df)=="V2"] <- features[2]
names(df)[names(df)=="V3"] <- features[3]
names(df)[names(df)=="V4"] <- features[4]
names(df)[names(df)=="V5"] <- features[5]
names(df)[names(df)=="V6"] <- label
# Choose any four X variables and Y
I <- c("citric acid", "chlorides", "total sulfur dioxide", "alcohol", "quality") 
# obtain a 400 by 5 matrix
variables_for_transform <- df[,I]  
data.transformed <- variables_for_transform
###############################################################################
# for each variable, you need to figure out a good data transformation method, 
# such as Polynomial, log and negation transformation. The k-S test and Skewness 
# calculation may be helpful to select the transformation method
# Hiding warnings for this section of code 
###############################################################################
oldw <- getOption("warn")
options(warn = -1)
###############################################################################
# K-S test for all 4 variables (X) & print p-values from K-S test,
pnorm1 = (ks.test(variables_for_transform[,"citric acid"], "pnorm"))$p.value
pnorm2 = (ks.test(variables_for_transform[,"chlorides"], "pnorm"))$p.value
pnorm3 = (ks.test(variables_for_transform[,"total sulfur dioxide"]
                  , "pnorm"))$p.value
pnorm4 = (ks.test(variables_for_transform[,"alcohol"], "pnorm"))$p.value
###############################################################################
print(pnorm1)
print(pnorm2)
print(pnorm3)
print(pnorm4)
# As can be seen all are 0. This indicates the K-S test is not 
# able to discern if its a normal distribution, so lets check skewness
###############################################################################
# Skewness test for all 4 variables (X)
# Print skewness values from skewness test for all features
###############################################################################
skewn1 = skewness(variables_for_transform[,"citric acid"])
skewn2 = skewness(variables_for_transform[,"chlorides"])
skewn3 = skewness(variables_for_transform[,"total sulfur dioxide"])
skewn4 = skewness(variables_for_transform[,"alcohol"])
###############################################################################
print(skewn1)
print(skewn2)
print(skewn3)
print(skewn4)
# As can be seen ("chlorides", "total sulfur dioxide") high skew 
# indicating they are not in normal distribution 
###############################################################################
# Setting warnings option back to original
options(warn = oldw)
###############################################################################
# Min-Max and Z-score transformation functions 
# that can be used to scale each variable
###############################################################################
# min-max normalization
minmax <- function(x){
  (x - min(x))/(max(x)-min(x))
}
###############################################################################
# z-score standardization and scaling to unit interval
unit.z <- function(x){
  0.15*((x-mean(x))/sd(x)) + 0.5
}
###############################################################################
# Function to Plot before and after transformation values to check if 
# transformations have moved the data to a normal distribution
# Boxplot checks for presence of outliers
###############################################################################
plot_data <- function(prex, postx, xlab, title) {
  par(mfrow=c(1,4), oma=c(0,0,2,0))
  boxplot(prex, xlab=xlab, main="Original Values")
  hist(prex,xlab=xlab, main="Original Values")
  boxplot(postx, xlab=xlab, main="Transformed Values")
  hist(postx,xlab=xlab, main="Transformed Values")
  mtext(title, side = 3, outer = TRUE)
}
###############################################################################
# Transformation for "citric acid" feature
###############################################################################
# Citric Acid feature has very little skew with a skewness of 0.29 
# The distribution for Citric Acid can be considered normal 
# We can scale the values in the 0 to 1 range using z-score
data.transformed[,1] = unit.z(variables_for_transform[,1])
x1mean = mean(variables_for_transform[,1])
x1sd = sd(variables_for_transform[,1])
###############################################################################
# Transformation for "chlorides" feature
###############################################################################
# "chlorides" feature has severe skew (4.9) and we can transform
# it using a reciprocal transformation (1/x) 
# We can scale the values in the 0 to 1 range using z-score
p=-1
data.transformed[,2] <- variables_for_transform[,"chlorides"]^p
x2mean = mean(data.transformed[,2])
x2sd = sd(data.transformed[,2])
data.transformed[,2] = unit.z(data.transformed[,2])
###############################################################################
# Transformation for "total sulfur dioxide" feature
###############################################################################
# total sulfur dioxide feature is greater skew (1.75), so we can 
# use log transformation with constant to transform
# we will use z-score scaling to bring this to unit interval
data.transformed[,3] <- log(variables_for_transform[,"total sulfur dioxide"]+10)
x3mean = mean(data.transformed[,3])
x3sd = sd(data.transformed[,3])
data.transformed[,3] = unit.z(data.transformed[,3])
###############################################################################
# Transformation for "alcohol" feature
###############################################################################
# alcohol feature is moderately skewed data and we can use a simple
# square root transformation to transform this feature
# we will use z-score scaling to bring this to unit interval
p2=1/2
data.transformed[,4] <- variables_for_transform[,"alcohol"]^p2
x4mean = mean(data.transformed[,4])
x4sd = sd(data.transformed[,4])
data.transformed[,4] = unit.z(data.transformed[,4])
###############################################################################
# Transformation the y variable "quality" 
###############################################################################
# quality target variable has categorical values 
# we can use a min-max scaling to bring this to the range (0,1)
data.transformed[,5] = minmax(variables_for_transform[,5])
ymax = max(variables_for_transform[,5])
ymin = min(variables_for_transform[,5])
###############################################################################
# finally, checking the transformed feature using plots for each feature
###############################################################################
plot_data( variables_for_transform[,1], data.transformed[,1]
           , "citric acid", "Transformation for Citric Acid" )
plot_data( variables_for_transform[,2], data.transformed[,2]
           , "chlorides", "Transformation for Chlorides" )
plot_data( variables_for_transform[,3], data.transformed[,3]
           , "total sulfur dioxide", "Transformation for Total Sulfur Dioxide")
plot_data( variables_for_transform[,4], data.transformed[,4]
           , "alcohol", "Transformation for Alcohol" )
###############################################################################
# Check the skewness for each transformed feature including target (y)
###############################################################################
skewness(data.transformed[,1])
skewness(data.transformed[,2])
skewness(data.transformed[,3])
skewness(data.transformed[,4])
skewness(data.transformed[,5])
###############################################################################
# View the transformed data to check if all variables have values 
# in the range (0,1)
###############################################################################
View(data.transformed)
###############################################################################
# Save this transformed data to a text file
###############################################################################
# replace ??name?? with either your surname or first name.
write.table(data.transformed, "killedar-transformed.txt")  
###############################################################################
#------------------------------------------------------------------------------
# Question 3 - Build models and investigate
#------------------------------------------------------------------------------
###############################################################################
source("AggWaFit718.R")
# import your saved data
data.transformed_copy <- as.matrix(read.table("killedar-transformed.txt"))  
###############################################################################
# Get weights for Weighted Arithmetic Mean with fit.QAM() 
###############################################################################
fit.QAM(data.transformed_copy
        , output.1="QAM-Output.txt"
        , stats.1="QAM-Stats.txt"
        , g=AM
        , g.inv=invAM
        )
QAMStats = readLines("QAM-Stats.txt")
###############################################################################
# Get weights for Power Mean p=0.5 with fit.QAM()
###############################################################################
fit.QAM(data.transformed_copy
        , output.1="PM05-Output.txt"
        , stats.1="PM05-Stats.txt"
        , g=PM05
        , g.inv=invPM05
)
PM05Stats = readLines("PM05-Stats.txt")
###############################################################################
# Get weights for Power Mean p=2 (Quadratic Mean - QM) with fit.QAM()
###############################################################################
fit.QAM(data.transformed_copy
        , output.1="QM-Output.txt"
        , stats.1="QM-Stats.txt"
        , g=QM
        , g.inv=invQM
)
QMStats = readLines("QM-Stats.txt")
###############################################################################
# Get weights for Ordered Weighted Average with fit.OWA()
###############################################################################
fit.QAM(data.transformed_copy
        , output.1="OWA-Output.txt"
        , stats.1="OWA-Stats.txt"
)
OWAStats = readLines("OWA-Stats.txt")
###############################################################################
report = as.data.frame(cbind(QAMStats, PM05Stats, QMStats, OWAStats))
View(report)
###############################################################################
# From the report above, we can see that QM is the best model 
# has the lowest RMSE score of 0.1402 and correlation of 0.55 
# other models have lower correlation scores & higher RMSE
###############################################################################
#------------------------------------------------------------------------------
# Question 4 - Use Model for Prediction
#------------------------------------------------------------------------------
###############################################################################
# choose the same four X variables as in Q2 
###############################################################################
# selected 1, 2, 3, 5 - citric acid, chlorides, total sulfur dioxide
# and alcohol as the four X variables 
new_input <- c(1, 0.75, 40, 3.53, 8.3) 
new_input_for_transform <- new_input[c(1,2,3,5)] 
print(new_input_for_transform)
# Create a copy of input to store transformed values 
new_input.transformed <- new_input_for_transform
###############################################################################
# transforming the four variables in the same way as in question 2 
###############################################################################
# Feature 1 : Citric Acid, Z Score scaling
new_input.transformed[1] = 0.15*((new_input_for_transform[1]-x1mean)/x1sd) + 0.5
# Feature 2 : Chlorides, Reciprocal Transform + Z-Score scaling
new_input.transformed[2] <- new_input_for_transform[2]^p
new_input.transformed[2] = 0.15*((new_input.transformed[2]-x2mean)/x2sd) + 0.5
# Feature 3 : Total Sulfur Dioxide, Log Transform + Z-Score scaling
new_input.transformed[3] <- log(new_input_for_transform[3]+10)
new_input.transformed[3] = 0.15*((new_input.transformed[3]-x3mean)/x3sd) + 0.5
# Feature 4 : Alcohol, Square Root Transform + Z-Score scaling
new_input.transformed[4] <- new_input.transformed[4]^p2
new_input.transformed[4] = 0.15*((new_input.transformed[4]-x4mean)/x4sd) + 0.5
###############################################################################
# applying the transformed variables to the best model selected from 
# Q3 for Y prediction  
###############################################################################
x <- new_input.transformed
print(x)
# Weights of the best model (QM) are below, captured from the report above
w1 <- 0.184881187711602
w2 <- 0.123023090253593
w3 <- 0.00153471618549255
w4 <- 0.69056100584931
# Calculate the predicted value
y = w1*x[1] + w2*x[2] + w3*x[3] + w4*x[4]
print(y)
###############################################################################
# Reverse the transformation to convert back the predicted Y to the 
# original scale and then round it to integer
###############################################################################
ytxf = round(y*(ymax-ymin)+ymin)
print(ytxf)
###############################################################################
# References 
# Following Harvard style: 
# https://www.deakin.edu.au/students/studying/study-support/referencing/harvard
###############################################################################
# You must cite all the data sets and packages you used for this assessment.
# References : 
#
# [1] Alboukadel Kassambara (2020) DataNovia (https://www.datanovia.com)
#     Statistical Tests and Assumptions, accessed Dec 2024
#     https://www.datanovia.com/en/lessons
#       /transform-data-to-normal-distribution-in-r/
#     
# [2] Simon James (2016) An Introduction to Data Analysis using Aggregation 
#       Functions in R : Chapters (2,3,4), accessed December 2024
#
# [3] Anatomise Biostats (June 2017) (https://anatomisebiostats.com/) 
#     Transforming Skewed Data: How to choose the right transformation for your 
#     distribuition https://anatomisebiostats.com/biostatistics-blog
#        /transforming-skewed-data/
#
