# Analysis of MBA SALARIES
# NAME: Apoorv Agarwal      
# EMAIL: aapoorv75@gmail.com
# COLLEGE: Indian Institute of Technology Madras     

#reading data in R
Salary.df <- read.csv(paste("SalariesData.csv",sep=" "))
attach(Salary.df)
View(Salary.df)
# Certain Parameter calculation
mean(age) # 27.36 yrs
median(sex) # most of them were male
NewSal.df <- Salary.df[Salary.df$salary!=998 & Salary.df$salary!=999,] #data frame contain only who filled survey
View(NewSal.df)
summary(NewSal.df) # mean salary : 54985  
NewSal_2 <- Salary.df[Salary.df$satis!=998,] # data frame containing only those who filled the survey might not have filled the salary
View(NewSal_2)
mean(NewSal_2$satis) # 5.56 course was quite satisfactory(according to the former students)
mean(Salary.df$gmat_tpc) # 84.19 

#Dividing data set into those who got a job and those who dont

#those who have salary data shared
job <- Salary.df[Salary.df$salary!=0 & Salary.df$salary!=998 & Salary.df$salary!=999,]
View(job)

# Informative Plots 

#box plots

# salary
boxplot(NewSal.df$salary,main="Salary Distribution", xlab="Salary ",horizontal = TRUE)
#median salary comes out to be around 80k 

#age
boxplot(Salary.df$age,main="Age", xlab="Age(in Yrs)",horizontal = TRUE)
#most of the students were around 27, with few outliers going upto 48 yrs

#Hypothesis 1 : Older student got high salary than the younger ones.

#Hypothesis Test:-
# t-test
t.test(NewSal.df$age,NewSal.df$salary)
#p-value<0.05, significant correlation, Hypothesis stands correct

#Hypothesis 2: Those who have high GMAT percentile have higher salary
#Hypothesis Test

#correlation test
cor.test(NewSal.df$gmat_qpc,NewSal.df$salary)
#high p-value, insginificant correlation, Hypothesis REJECTED

#Corrplot
library(corrplot)
library(gplots)

corrplot.mixed(corr = cor(NewSal.df[,c(1,3:8,10,12,13)],use="complete.obs"),
               upper="ellipse",tl.pos = "lt",col=colorpanel(50,"red","gray60","blue4"))
#observation : salary and the satisfaction survey are in strong correlation(in compared to others)

#Hypothesis 3: More satisfied students have higher salary.

#Correlation test
cor.test(NewSal.df$satis,NewSal.df$salary)
#cor test show significant correlation (p-value<0.05) with cor coffecient: 0.16
#Hypothesis Stands correct

#Hypothesis 4 : Those who have high s_avg, have good salaries

#Correlation test
cor.test(NewSal.df$s_avg,NewSal.df$salary)
#p-value>0.05 hypothesis rejected


#Corrplot
library(corrplot)
library(gplots)

# Mixed Correlation Plot
corrplot.mixed(corr = cor(job[,c(1,3:8,10,12,13)],use="complete.obs"),
               upper="ellipse",tl.pos = "lt",col=colorpanel(50,"red","gray60","blue4"))
#observation: salary is strongly correlated to age and work experince

#Hypothesis : Higher the age , higher the salary

#cor test
cor.test(job$salary,job$age)
#cor test shows strong correlaion in age and salary
#Hypothesis stays

#hypothesis : More work exp , higher salary
#cor test
cor.test(job$salary,job$work_yrs)
#cor test shows strong correlation in work_yrs and salary
#hypothesis stays

#some other tests 
#chi-square test 
genderSal <- job[,c(2,12)]
genderSal
chisq.test(genderSal)
#chiq square test shows no stastical significance of gender in deciding the salary

#chi-square on first language and salary
langSal <- job[,c(11,12)]
chisq.test(langSal)
#chi square test shows no stastical significance 

#chi-square on gmat performance and salary
gmatSal <- job[,c(3,12)]
chisq.test(gmatSal)
#chi-square shows stastically significant relationship

#Regression analysis
#independent variable: age, work_exp, gmat_performance ; dependent variable: salary
jobModel <- lm(job$salary~job$age+job$work_yrs+job$gmat_tot)
summary(jobModel)
# observation: together less statistical significance
#Model 1
jobModel1 <- lm(job$salary~job$age)
summary(jobModel1)
plot(job$age,job$salary)
abline(lm(job$salary~job$age))
#model1 : salary = 29962.6 + 2728.8*(age)
#Model 2
jobModel2 <- lm(job$salary~job$work_yrs)
summary(jobModel2)
plot(job$work_yrs,job$salary)
abline(lm(job$salary~job$work_yrs))
#model2 : salary = 93101 + 2699*(work_yrs)
#Model 3
jobModel3 <- lm(job$salary~job$gmat_tot)
summary(jobModel3)
#observation : Individually also, gmat_tot is statistcally insignificant
#Out of the 3 models, model1 has higher multiple R-squared value than that of model2 
#Model1 is more relaible than model2