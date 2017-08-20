#################### Library #################################################
library(ggplot2)
library(dplyr)
library(stringr)
library(MASS)
library(tidyr)
library(car)
library(lubridate)
#install.packages("DMwR")
library(DMwR)
library(scales)
library(car)
library(e1071)
library(caret)
library(cowplot)
library(caTools)


######### Read Data / EDA ######################################################
hra<-read.csv("general_data.csv")
hra.emp<-read.csv("employee_survey_data.csv")
hra.man<-read.csv("manager_survey_data.csv")
intime<-read.csv("in_time.csv")
outtime<-read.csv("out_time.csv")

#master merge
hra.merged<-merge(hra,hra.emp,by="EmployeeID", all = F)
hra.merged<-merge(hra.merged,hra.man, by="EmployeeID", all = F)
View(hra)
hra.copy<-hra.merged

#counting number of NAs
sum(is.na(hra))
sum(is.na(hra.emp))
sum(is.na(hra.man))
sum(is.na(hra.merged))
colSums(is.na(hra.merged))
which(colSums(is.na(hra.merged))>0)
##Considering for Removing all rows with NAs since its only ~2% of total number of observations.

summary(hra.merged$Attrition)
which(is.na(hra.merged)==T)
hra.merged[(which(is.na(hra.merged$NumCompaniesWorked) | 
                          is.na(hra.merged$EnvironmentSatisfaction) | 
                          is.na(hra.merged$JobSatisfaction) | 
                          is.na(hra.merged$WorkLifeBalance))),] %>%
  group_by(Attrition) %>%
  summarize(cnt=length(EmployeeID))
##Attrition yes - 14/711 ~ 2.01%
##Attrition no - 88/3699 ~ 2.50%
##This shows that omitting the rows which have NAs will not affect the distribution of data. 
hra.merged<-na.omit(hra.merged)

# check duplicated
which(duplicated(hra.merged))
#no duplicates

str(hra.merged)

################################EDA unitvariate from T ################################
# Barcharts for categorical features with stacked hra.merged information
bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="none")



plot_grid(ggplot(hra.merged, aes(x=BusinessTravel,fill=Attrition))+ geom_bar(), 
          ggplot(hra.merged, aes(x=Department,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(hra.merged, aes(x=EducationField,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(hra.merged, aes(x=JobRole,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(hra.merged, aes(x=Gender,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(hra.merged, aes(x=MaritalStatus,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")   

ggplot(hra.merged, aes(x=factor(Education),fill=Attrition))+ geom_bar()+bar_theme1



# Histogram and Boxplots for numeric variables 
box_theme<- theme(axis.line=element_blank(),axis.title=element_blank(), 
                  axis.ticks=element_blank(), axis.text=element_blank())

box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
                    axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                    legend.position="none")

plot_grid(ggplot(hra.merged, aes(Age))+ geom_histogram(binwidth = 20),
          ggplot(hra.merged, aes(x="",y=Age))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(hra.merged, aes(DistanceFromHome))+ geom_histogram(binwidth = 20),
          ggplot(hra.merged, aes(x="",y=DistanceFromHome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(hra.merged, aes(PercentSalaryHike))+ geom_histogram(binwidth = 1),
          ggplot(hra.merged, aes(x="",y=PercentSalaryHike))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(hra.merged, aes(MonthlyIncome))+ geom_histogram(binwidth = 1000),
          ggplot(hra.merged, aes(x="",y=MonthlyIncome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)
quantile(hra.merged$MonthlyIncome,seq(0,1,0.01))


plot_grid(ggplot(hra.merged, aes(YearsWithCurrManager))+ geom_histogram(binwidth = 1),
          ggplot(hra.merged, aes(x="",y=YearsWithCurrManager))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)
quantile(hra.merged$YearsWithCurrManager,seq(0,1,0.01))

plot_grid(ggplot(hra.merged, aes(YearsAtCompany))+ geom_histogram(binwidth = 1),
          ggplot(hra.merged, aes(x="",y=YearsAtCompany))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)
quantile(hra.merged$YearsAtCompany,seq(0,1,0.01))

plot_grid(ggplot(hra.merged, aes(YearsSinceLastPromotion))+ geom_histogram(binwidth = 1),
          ggplot(hra.merged, aes(x="",y=YearsSinceLastPromotion))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(hra.merged, aes(TrainingTimesLastYear))+ geom_histogram(binwidth = 0.5),
          ggplot(hra.merged, aes(x="",y=TrainingTimesLastYear))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(hra.merged, aes(TotalWorkingYears))+ geom_histogram(binwidth = 1),
          ggplot(hra.merged, aes(x="",y=TotalWorkingYears))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)
quantile(hra.merged$TotalWorkingYears,seq(0,1,0.01))

plot_grid(ggplot(hra.merged, aes(StockOptionLevel))+ geom_histogram(binwidth = 1),
          ggplot(hra.merged, aes(x="",y=StockOptionLevel))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)
quantile(hra.merged$StockOptionLevel,seq(0,1,0.01))


#Treating outliers for monthly income, TotalWorkingYears, YearsSinceLastPromotion, YearsAtCompany
#Treating them by capping the values as per quantile increament
quantile(hra.merged$MonthlyIncome,seq(0,1,0.01))
hra.merged$MonthlyIncome[which(hra.merged$MonthlyIncome>152020.0)]<-152020.0

quantile(hra.merged$TotalWorkingYears,seq(0,1,0.01))
hra.merged$TotalWorkingYears[which(hra.merged$TotalWorkingYears>25)]<-25

quantile(hra.merged$YearsSinceLastPromotion,seq(0,1,0.01))
hra.merged$YearsSinceLastPromotion[which(hra.merged$YearsSinceLastPromotion>9)]<-9

quantile(hra.merged$YearsAtCompany,seq(0,1,0.01))
hra.merged$YearsAtCompany[which(hra.merged$YearsAtCompany>22.00)]<-22.00

str(hra.merged)
################feature standardisation######################################

#normalising continuous variables
hra.merged$Age<-scale(hra.merged$Age)
hra.merged$DistanceFromHome<-scale(hra.merged$DistanceFromHome)
hra.merged$MonthlyIncome<-scale(hra.merged$MonthlyIncome)
hra.merged$NumCompaniesWorked<-scale(hra.merged$NumCompaniesWorked)
hra.merged$PercentSalaryHike<-scale(hra.merged$PercentSalaryHike)
hra.merged$TotalWorkingYears<-scale(hra.merged$TotalWorkingYears)
hra.merged$YearsAtCompany<-scale(hra.merged$YearsAtCompany)
hra.merged$YearsWithCurrManager<-scale(hra.merged$YearsWithCurrManager)
hra.merged$YearsSinceLastPromotion<-scale(hra.merged$YearsSinceLastPromotion)

# converting target variable Attrition to 0/1. yes - 1, no - 0 
hra.merged$Attrition<- ifelse(hra.merged$Attrition=="Yes",1,0)


# creating a dataframe of categorical features
hra.chr<- hra.merged[,c(4,5,7,8,10,11,12,13,25:29)]

# converting categorical attributes to factor
hra.fact<- data.frame(sapply(hra.chr, function(x) factor(x)))
str(hra.fact)

# creating dummy variables for factor attributes
dummies<- data.frame(sapply(hra.fact, 
                            function(x) data.frame(model.matrix(~x-1,data =hra.fact))[,-1]))

# female -> 0 , male -> 1 ; Performance rating 3 -> 0 , 4 -> 1

# Final dataset
hra.final<- cbind(hra.merged[,-c(1,4,5,7,8,10,11,12,13,25:29)],dummies) 
View(hra.final)
str(hra.final)

#removing StandardHours,over18 and EmployeeCount since they have only 1 level throughout and cant contribute to model
hra.final<-hra.final[,-c(4,7,9)]

#correlation matrix
cor<-round(cor(hra.final),2)

########################################################################
# splitting the data between train and test
set.seed(100)

indices = sample.split(hra.final$Attrition, SplitRatio = 0.7)

train = hra.final[indices,]

test = hra.final[!(indices),]

########################Model Building#####################################

#Initial model
model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1) #AIC 2267.8

#applying StepAIC
model_2<- stepAIC(model_1, direction="both")

#removing variables through VIF and p-value check
summary(model_2) #AIC 2243.1
sort(vif(model_2))

#BusinessTravel.xTravel_Rarely
model_3<-glm(formula = Attrition ~ Age + NumCompaniesWorked + StockOptionLevel + 
               TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
               YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
               Education.x5 + EducationField.xLife.Sciences + 
               EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
               EducationField.xTechnical.Degree + JobLevel.x2 + JobLevel.x5 + 
               JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
               JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
               MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + JobInvolvement.x3 + PerformanceRating, 
             family = "binomial", data = train)

summary(model_3) #AIC 2251.4
sort(vif(model_3))

#EducationField.xLife.Sciences
model_4<-glm(formula = Attrition ~ Age + NumCompaniesWorked + StockOptionLevel + 
               TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
               YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
               Education.x5 +
               EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
               EducationField.xTechnical.Degree + JobLevel.x2 + JobLevel.x5 + 
               JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
               JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
               MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + JobInvolvement.x3 + PerformanceRating, 
             family = "binomial", data = train)

summary(model_4) #AIC 2269.1
sort(vif(model_4))

#WorkLifeBalance.x2
model_5<-glm(formula = Attrition ~ Age + NumCompaniesWorked + StockOptionLevel + 
               TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
               YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
               Education.x5 +
               EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
               EducationField.xTechnical.Degree + JobLevel.x2 + JobLevel.x5 + 
               JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
               JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
               MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + JobInvolvement.x3 + PerformanceRating, 
             family = "binomial", data = train)

summary(model_5) #AIC 2282.8
sort(vif(model_5))

#Age
model_6<-glm(formula = Attrition ~ NumCompaniesWorked + StockOptionLevel + 
               TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
               YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
               Education.x5 +
               EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
               EducationField.xTechnical.Degree + JobLevel.x2 + JobLevel.x5 + 
               JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
               JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
               MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + JobInvolvement.x3 + PerformanceRating, 
             family = "binomial", data = train)

summary(model_6) #AIC 2292.4
sort(vif(model_6))

#EducationField.xMarketing
model_7<-glm(formula = Attrition ~ NumCompaniesWorked + StockOptionLevel + 
               TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
               YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
               Education.x5 +
               EducationField.xMedical + EducationField.xOther + 
               EducationField.xTechnical.Degree + JobLevel.x2 + JobLevel.x5 + 
               JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
               JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
               MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + JobInvolvement.x3 + PerformanceRating, 
             family = "binomial", data = train)

summary(model_7) #AIC 2291.5
sort(vif(model_7))

#StockOptionLevel
model_8<-glm(formula = Attrition ~ NumCompaniesWorked + 
               TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
               YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
               Education.x5 +
               EducationField.xMedical + EducationField.xOther + 
               EducationField.xTechnical.Degree + JobLevel.x2 + JobLevel.x5 + 
               JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
               JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
               MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + JobInvolvement.x3 + PerformanceRating, 
             family = "binomial", data = train)

summary(model_8) #AIC 2291
sort(vif(model_8))

#EducationField.xMedical
model_9<-glm(formula = Attrition ~ NumCompaniesWorked + 
               TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
               YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
               Education.x5 + EducationField.xOther + 
               EducationField.xTechnical.Degree + JobLevel.x2 + JobLevel.x5 + 
               JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
               JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
               MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + JobInvolvement.x3 + PerformanceRating, 
             family = "binomial", data = train)

summary(model_9) #AIC 2290.9
sort(vif(model_9))

#JobLevel.x2 
model_10<-glm(formula = Attrition ~ NumCompaniesWorked + 
               TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
               YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
               Education.x5 + EducationField.xOther + 
               EducationField.xTechnical.Degree + JobLevel.x5 + 
               JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
               JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
               MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + JobInvolvement.x3 + PerformanceRating, 
             family = "binomial", data = train)

summary(model_10) #AIC 2291.3
sort(vif(model_10))

#JobRole.xManager 
model_11<-glm(formula = Attrition ~ NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
                Education.x5 + EducationField.xOther + 
                EducationField.xTechnical.Degree + JobLevel.x5 + 
                JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + JobInvolvement.x3 + PerformanceRating, 
              family = "binomial", data = train)

summary(model_11) #AIC 2291.8
sort(vif(model_11))

#EducationField.xTechnical.Degree 
model_12<-glm(formula = Attrition ~ NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
                Education.x5 + EducationField.xOther + 
                JobLevel.x5 + 
                JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + JobInvolvement.x3 + PerformanceRating, 
              family = "binomial", data = train)

summary(model_12) #AIC 2292.5
sort(vif(model_12))

#EducationField.xOther  
model_13<-glm(formula = Attrition ~ NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
                Education.x5 + JobLevel.x5 + 
                JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + JobInvolvement.x3 + PerformanceRating, 
              family = "binomial", data = train)

summary(model_13) #AIC 2293.6
sort(vif(model_13))

#Education.x5  
model_14<-glm(formula = Attrition ~ NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
                JobLevel.x5 + 
                JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + JobInvolvement.x3 + PerformanceRating, 
              family = "binomial", data = train)

summary(model_14) #AIC 2295.1
sort(vif(model_14))

#WorkLifeBalance.x4   
model_15<-glm(formula = Attrition ~ NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
                JobLevel.x5 + 
                JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                JobInvolvement.x3 + PerformanceRating, 
              family = "binomial", data = train)

summary(model_15) #AIC 2295.1
sort(vif(model_15))

#JobRole.xResearch.Scientist    
model_16<-glm(formula = Attrition ~ NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
                JobLevel.x5 + 
                JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                JobRole.xSales.Executive + 
                MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                JobInvolvement.x3 + PerformanceRating, 
              family = "binomial", data = train)

summary(model_16) #AIC 2299.4
sort(vif(model_16))

#PerformanceRating
model_17<-glm(formula = Attrition ~ NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
                JobLevel.x5 + 
                JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                JobRole.xSales.Executive + 
                MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                JobInvolvement.x3, 
              family = "binomial", data = train)

summary(model_17) #AIC 2306.2
sort(vif(model_17))

#JobLevel.x5
model_18<-glm(formula = Attrition ~ NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
                JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                JobRole.xSales.Executive + 
                MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x3
                , 
              family = "binomial", data = train)

summary(model_18) #AIC 2302.1

#JobRole.xSales.Executive 
model_19<-glm(formula = Attrition ~ NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
                JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x3
              , 
              family = "binomial", data = train)

summary(model_19) #AIC 2315


#final model

final_model1<-model_19

######Model Evaluation######

## Test Data ##

#predicted probabilities of Churn 1 for test data
test_pred = predict(final_model1, type = "response", 
                    newdata = test[,-2])

# Let's see the summary 

summary(test_pred)

test$prob <- test_pred
View(test)
# Let's use the probability cutoff of 50%.

test_pred_attr <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_attr <- factor(ifelse(test$Attrition==1,"Yes","No"))

table(test_actual_attr,test_pred_attr)

#######################################################################
test_pred_attr <- factor(ifelse(test_pred >= 0.15, "Yes", "No"))

library(e1071)

test_conf <- confusionMatrix(test_pred_attr, test_actual_attr, positive = "Yes")
test_conf

#########################################################################################
# Let's Choose the cutoff value. 
# 

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_attr <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attr, test_actual_attr, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.003575 to 0.812100 for plotting and initiallizing a matrix of 100 X 3.

# Summary of test probability

summary(test_pred)

s = seq(0.001566,0.766808,length=100)
s[45]
OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]


# Let's choose a cutoff value of 0.3132 for final model

test_cutoff_attr <- factor(ifelse(test_pred >=cutoff, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_attr, test_actual_attr, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

View(test)
##################################################################################################
### KS -statistic - Test Data ######

test_cutoff_attr <- ifelse(test_cutoff_churn=="Yes",1,0)
test_actual_attr <- ifelse(test_actual_churn=="Yes",1,0)


library(ROCR)
#on testing  data
pred_object_test<- prediction(test_cutoff_attr, test_actual_attr)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)


####################################################################
# Lift & Gain Chart 

# plotting the lift chart

# Loading dplyr package 
require(dplyr)
library(dplyr)

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Churn_decile = lift(test_actual_attr, test_pred, groups = 10)

# ###################Function declaration#####################################
# 
# univariate_categorical <- function(dataset,var,var_name,Attrition){
#   
#   dataset %>% ggplot(aes(x = as.factor(var), fill=Attrition)) +
#     geom_bar(aes(y = (..count..)/sum(..count..))) +
#     geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
#     scale_y_continuous(labels = percent) +
#     labs(title = var_name, y = "Percent", x = var_name)+theme(
#       axis.text.y=element_blank(), axis.ticks=element_blank(),
#       axis.title.y=element_blank(),axis.text.x = element_text(angle = 60, hjust = 1)
#     ) 
# }
# 
# univariate_categorical(hra.merged,hra.merged$EducationField,"Education field", hra.merged$Attrition)
# 
# univariate_numerical <- function(dataset,var_name,binwidth){
#   plot_grid(ggplot(dataset, aes(var_name))+ geom_histogram(binwidth = binwidth),
#             ggplot(dataset, aes(x="",y=var_name))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
#             align = "v",ncol = 1)
# }
# univariate_numerical(hra.merged,hra.merged$MonthlyIncome,1000)
# 
#   