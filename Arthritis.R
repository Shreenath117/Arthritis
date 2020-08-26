# Case Study Solutions : Arthritis.csv file
#---------------------------------------------------------------------------------------------------------------
# Soln. to Question 1:
# Reading the CSV file and dropping the first column
data=read.csv('\arthritis.csv')
# View the data loaded
data
# Dropping the first column which is nothing but the Serial number
data=data[2:8]
# View the dimensions (shape) of the data to be used for the analysis
dim(data)
# There are 906 rows and 7 columns

#---------------------------------------------------------------------------------------------------------------
# Soln. to Question 2:

# The key objective of the case study is to Evaluate the effectiveness of the drug auranofin versus the placebo therapy for the treatment of rheumatoid arthritis. 
# The response variable : y using the predictor variables given in the dataset like sex, age, trt , baseline & time

# The data collected in the given excel file is grouped under which data class "Longitudinal or Panel Data"

#---------------------------------------------------------------------------------------------------------------
# Soln. to Question 3:
data$y

# Here, y is a ordinal variable measured on  a five-level ordinal response scale

#-------------------------------------------------------------------------------------------------
# Soln. to Question 4:
#Plotting a scatterplot matrix

pairs(~y+sex+age+trt+baseline+time, data=data)


#-------------------------------------------------------------------------------------------------

# Soln. to Question 5:

#Summarising the dataset : 
summary(data)

# Observations :
# The mean age is 50 years with maximum being 66 of the 302 patients
# The average self assessment score given has been 3.227 out of the max 5

# Check the datatypes
str(data)
sapply(data, class)

# Convert the datatype of trt variable to factor
data$trt <- as.factor(data$trt)

str(data)
sapply(data, class)

#-------------------------------------------------------------------------------------------------
# Soln. to Question 6:

# Creating a frequency distribution table
table(data$trt)

counts <- table(data$trt)
barplot(counts, main="Treatment Group Distribution")

# We observe Treatment group 2 (Drug Group) has higher count compared to  treatment group 1 (Placebo group)


#-------------------------------------------------------------------------------------------------
# Soln. to Question 7:

hist(data$age,xlab = "Age",col = "red", xlim = c(20,70),breaks = 5)
table(data$age)

# Clearly seen, Age 55 has 66 observations which has the maximum count against all other age groups

#-------------------------------------------------------------------------------------------------
# Soln. to Question 8:

boxplot(age ~ trt, data = data, xlab = "Treatment Group",
        ylab = "Age", main = "Age vs Trt")

# Observation:
# It can be seen that the distribution of articles for both the treatment groups are quite similar wrt. age of patients,
# Also : Median age is higher for Trt 1 than Trt 2 group
# Skewness is towards the left, clearly seen from the plot above

#-------------------------------------------------------------------------------------------------

# Soln. to Question 9:

# Creating a 2 way frequency table

freq_table <- table(data$trt,data$sex)
freq_table

#-------------------------------------------------------------------------------------------------
# Soln. to Question 10:

# Draw a bar plot to the table created above: 

barplot(freq_table, main="Treatment Group Distribution",xlab="Treatment Group",legend = rownames(freq_table), beside=TRUE)

#-------------------------------------------------------------------------------------------------
# Soln. to Question 11:

boxplot(age ~ baseline,data = data, xlab = "Self Assessment Score at Baseline",
        ylab = "Age", main = "Age vs Baseline")

# Observations:
# 1) Potential Outliers for baseline 1 and 4 in the lower age group compared to others
# 2) Also : Median age is the highest for Baseline 1 followed by 2,4,5 and 3
# 3) Skewness is towards the left, clearly seen from the plot above

#-------------------------------------------------------------------------------------------------
# Soln. to Question 12:

# The response of interest is often a categorical variable, whether binary, nominal, or ordinal
# Here : the response variable 'y' is ordinal. 
# Often the categorical responses will be correlated in clusters, e.g., in repeated measure studies. 
# The observations within a cluster will often be positively correlated, i.e., the observations will tend to be more like each other than like observations from other clusters. 
# This correlation must be taken into account when analyzing clustered studies for proper inference and valid hypothesis testing

# Generalized estimating equations (GEE) modeling approach is useful for analyzing such correlated data with categorical or continuous responses

#-------------------------------------------------------------------------------------------------
# Soln. to Question 13:

# I.I.D stands for : Independent and identically distributed
# Assumption : Measured values of dependent variables or predictor variables are independent from each other.

# Values repeatedly measured in the same individual are usually more similar to each other than 
# values from different individuals : therefore not independent and hence this is a violation. 

# GEE method with a cumulative logit marginal model is proposed to solve here
# We use a technique that focuses on specifying the mean response of the outcome 
# ( a marginal approach or population average approach) to estimate regression parameters that is valid in the presence of correlated measurements

# GEE models and  Mixed effects model decompose variance to some extent and make them I.I.D
# Both GEE and GLMM are used when the assumption of independence is violated

#-------------------------------------------------------------------------------------------------
# Soln. to Question 14:

# GEE is the extension of generalized linear models (GLM) to longitudinal data. 
# Logistic regression with correlated errors : GEE
# The GEE method was developed by Liang and Zeger (1986) in order to produce regression estimates when analyzing repeated measures with non-normal response variables.
# The goal is to make inferences about the population when accounting for the within-subject correlation
# For every one-unit increase in a covariate across the population, GEE tells us how much the average response would change

# Assumptions of GEE :
# i) Measurements are independent across clusters
# ii) Measurements may be correlated within cluster

#-------------------------------------------------------------------------------------------------
# Soln. to Question 15:

# The package to be used to implement GEE in R is "multgee"

#-------------------------------------------------------------------------------------------------
# Soln. to Question 16:

# GEE estimates are the same as "Ordinary Least Squares (OLS)" if the dependent variable is normally distributed and no correlation within responses are assumed

#-------------------------------------------------------------------------------------------------
# Soln. to Question 17:

# GLMM vs GEE models
# GLMM : Generalized Linear Mixed Models  (Mixed effects models)
# GEE  : Generalized Estimating equations (Marginal models )

# Both use different approach to account for non independence of repeated measurements
# While marginal models focus on the mean outcome, mixed effect models provide a fully specified mode for multivariate distribution of the repeatedly measured outcome

#-------------------------------------------------------------------------------------------------
# Soln. to Question 18:

# If the response variable y were not repeated and Multi class ordinal level data,
# Proportional odds ordinal logistic regression model is the best model to be chosen
# Implemented in R using "polr"


#-------------------------------------------------------------------------------------------------
# Soln. to Question 19:

# This is implemented by ordLORgee since the response is a ordinal variable in the multgee package.
# install.packages("multgee")
library(multgee)

fitord <- ordLORgee(y~factor(time)+factor(trt)+factor(baseline), data=data,
                    id=id, repeated=time)
summary(fitord)

# p-value shows value <0.001 : Significant

# The effects of the 5 months follow-up indicator, of the drug auranofin (Treatment group 2), 
# and of the indicators of the baseline response level 3 or higher (3,4,5) are statistically significant

# Conclusion :
# The drug auranofin improved the self-assessment during the trial, as subjects in the treatment group have estimated cumulative odds
# 1.67 times those of subjects in the placebo group. 

intrinsic.pars(y,data,id,time,rscale="ordinal")
# The intrinsic parameters do not differ much. The 'uniform' local odds ratios
# structure might be a good approximation for the association pattern.

# An examination of the 5 × 5 marginalized contingency tables (5 levels of ordinal data)
# At each time pair indicates strong pairwise association, since the majority of observed frequencies fall 
# on the diagonal, subdiagonal, and superdiagonal, with less than 11% falling in the remaining cells.

# Varies from 0.65 to 0.91 from the intrinsic.pars function

#-------------------------------------------------------------------------------------------------
# Soln. to Question 20:

# R package lme4, function glmer for repeated analysis in GLMM ( Generalised Linear Mixed Models)

#-------------------------------------------------------------------------------------------------
