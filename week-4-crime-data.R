#Dalton Anderson
#install.packages('janitor')
library(readxl)
library(dplyr)
library(ggplot2)
library(janitor)

#1.) Load in dataset
#Import data
crime_master <- clean_names(read_excel("6304 Module 4 Assignment Data.xlsx"))

#2.) Create random sample sets by unumber

set.seed(59657076)
sample <- crime_master
crime12 = sample_n(sample, 12)

#Check Datasets

head(crime_master)
head(crime12)


#Analysis

#1.) 1.	With the data in your primary data set, 
#use R to calculate and report the correlation coefficient between the two variables.  
#Report and interpret the p values for the correlation coefficients. 

crime12cof = t.test(crime12$reported_crimes_per_million, conf.level = .95)
cor(crime12$reported_crimes_per_million, crime12$police_funding_dols_per_resident)
cor.test(crime12$reported_crimes_per_million, crime12$police_funding_dols_per_resident)
#need to write report on p value
#The p value is low, but the confidence interval has a large width. The results in my opinion are
#not inclusive enough. I need more information.

#2.) Use your primary data set to create a scatterplot of your data. 
#Show police funding on the x-axis and reported crimes per 1,000,000 on the y-axis.  
#Make certain the x-axis is scaled between 0 and 100 and the y-axis is scaled between 300 and 2000. 
#Apply appropriate axis labels to identify the variables.

#master set
ggplot(data = crime_master, mapping = aes(x = police_funding_dols_per_resident, y = reported_crimes_per_million)) + 
  geom_point(fill="slategrey", colour="darkslategrey") +
  xlab("Police Funding") +
  ylab("Reported Crimes Per 1,000,000") +
  xlim(0,100) +
  ylim(300, 2000)
#sample set
ggplot(data = crime12, mapping = aes(x = police_funding_dols_per_resident, y = reported_crimes_per_million)) + 
  geom_point(fill="slategrey", colour="darkslategrey") +
  xlab("Police Funding") +
  ylab("Reported Crimes Per 1,000,000") +
  xlim(0,100) +
  ylim(300, 2000)


#3.) With your primary data set use R to conduct a simple linear regression on the data 
#with reported crimes per 1,000,000 as the dependent variable and police funding as the independent
#variable.  As a part of this be sure to:
cor(crime12$reported_crimes_per_million, crime12$police_funding_dols_per_resident)

simlin12 = lm(crime12$reported_crimes_per_million ~ crime12$police_funding_dols_per_resident)
summary(simlin12)

#Use population lm model later on
simlin = lm(crime_master$reported_crimes_per_million ~ crime_master$police_funding_dols_per_resident)
summary(simlin)

#master
ggplot(data = crime_master, mapping = aes(x = police_funding_dols_per_resident,
                                          y = reported_crimes_per_million)) + 
  geom_point(fill="slategrey", colour="darkslategrey") +
  geom_smooth(method = "lm", se = FALSE)
xlab("Police Funding") +
  ylab("Reported Crimes Per 1,000,000") +
  xlim(0,100) +
  ylim(300, 2000)
#sample
ggplot(data = crime12, mapping = aes(x = police_funding_dols_per_resident,
                                     y = reported_crimes_per_million)) + 
  geom_point(fill="slategrey", colour="darkslategrey") +
  geom_smooth(method = "lm", se = FALSE)
xlab("Police Funding") +
  ylab("Reported Crimes Per 1,000,000") +
  xlim(0,100) +
  ylim(300, 2000)

#a.	Report the beta coefficients and associated p values and confidence intervals
#from your model.
summary(simlin12)
confint(simlin12)
#b.	Give a written interpretation of your beta coefficients.
#My sample set is slightly statistically significant. My model can only account for 47%
#of the data. My slope p value is high. My model isn't skewed, but I don't think is 
#useful for prediction. The confidence interval width is too big for me to be 
#comfortable using it.

#c.	Assess your model’s conformance with the LINE assumptions of regression.

#Linearity
plot(crime12$reported_crimes_per_million,simlin12$fitted.values,
     pch=19,main="Crime Actual v. Fitted Values")
xlim=c(300,2000), ylim=c(300,2000)
abline(0,1,col="red",lwd=3)

#Normality
qqnorm(crime12$reported_crimes_per_million,pch=19,main="Crime Normality Plot")
qqline(simlin12$fitted.values,col="red",lwd=3)
#There is a bit of curve on the right tail.

#Equality of Variances
plot(simlin12$fitted.values,scale(simlin12$residuals),
     pch=19,main="Crime Standardized Residuals",ylim = c(-4,4))
abline(0,0,col="red",lwd=3)
#A bit of heteroscedasticity.


#d.	For a given small city spending $41 per resident on police protection
#use your model to predict crime rate per 100,000 residents. 
#Include a 95% prediction interval and a written interpretation of both the prediction and 
#the accompanying interval.  If you only looked at this interval, 
#what would it potentially indicate about model fit?

ggplot(data = crime12, mapping = aes(x = police_funding_dols_per_resident,
                                     y = reported_crimes_per_million)) + 
  geom_point(fill="slategrey", colour="darkslategrey") +
  geom_smooth(method = "lm", se = FALSE)
xlab("Police Funding") +
  ylab("Reported Crimes Per 1,000,000") +
  xlim(0,100) +
  ylim(300, 2000)


#4.	New York City’s budgeted police expenditures in 2020 are $10.9 billion. 
#Its estimated population for the same year is approximately 8,550,000. 
#Give two reasons why it would be wrong to use your model to predict New York’s 
#crime rate per 100,000 residents.

small_city = data.frame( police_funding_dols_per_resident = 41 )

predict(simlin12,small_city,interval="predict")
#It looks to be about 680 crimes

#the dataset I am using is for 'small cities' New York is the largest the
#metropolitan city in the US.
summary(simlin)
#The max independent value is about 795 million for the crimes population
#New York's population and budget size makes this request out of scope of the model.









