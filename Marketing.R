setwd("G:\\MY Project\\Jigsaw\\R\\Linear regression\\Case study")                                                                                                                               
data<-read.csv("DirectMarketing.csv", header = TRUE)
library(dplyr)
library(ggplot2)
library(car)

head(data)
str(data)
dim(data)


##Do exploratory analysis##
plot(data$Age,data$AmountSpent, col="red")

#Combine the Middle and Old levels together
 data$age1<-ifelse(data$Age!="Young","Middle-Old", as.character(data$Age))
data$age1<-as.factor(data$age1)
summary(data$age1)


#again boxplot

plot(data$age1,data$AmountSpent, col="red")

#Gender
plot(data$Gender,data$AmountSpent,col="red")

#Own house
summary(data$OwnHome)
plot(data$OwnHome,data$AmountSpent,col="red")

#Married
summary(data$Married)
plot(data$Married,data$AmountSpent,col="red")

#Location
summary(data$Location)
plot(data$Location,data$AmountSpent,col="red")

#Salary
summary(data$Salary)


plot(data$Salary,data$AmountSpent)#Might be heteroescadasticity


#Children
summary(data$Children)

data$Children<-as.factor(data$Children)

plot(data$Children,data$AmountSpent,col="red")

# children with 2 & 3 are in same group

data$children1<-ifelse(data$Children==2|data$Children==3, "2-3", as.character(data$Children))
data$children1<-as.factor(data$children1)

summary(data$children1)

plot(data$children1,data$AmountSpent, col="red")

#History
summary(data$History)

#Impute Missing values
tapply(data$AmountSpent,data$History, mean)
ind<-which(is.na(data$History))
mean(data[ind,"AmountSpent"])

#Create a category called missing
data$History1<-ifelse(is.na(data$History), "Missing", as.factor(data$History))
data$History1<-as.factor(data$History1)

summary(data$History1)

data$History1<-factor(data$History1, labels = c("High", "Medium", "Low","missing"))

#Catalogues
summary(data$Catalogs)

# now making model
# define the data set once again i.e which are the columns are to be exculede.
data1<-data[,-c(1,7,8)]

attach(data1)
model1<-lm(AmountSpent~.,data=data1)

str(data1)
summary(model1)

#ittaration method so making second model

model2<-lm(AmountSpent~ Gender+Location+Salary+Catalogs+children1+History1, data = data1)

summary(model2)

#Remove insignificant variabes, since the gender and History were not significance, we have to create dummy variable
#HistoryMissing
#GenderMale

#Create dummy variables for gender

data1$male<-ifelse(data1$Gender=="Male", 1,0)
data1$Female<-ifelse(data1$Gender=="Female", 1,0)

#Create dummy variables for history

data1$Missing_d<-ifelse(data$History1=="missing",1,0)
data1$Low_d<-ifelse(data$History1=="Low",1,0)
data1$Med_d<-ifelse(data$History1=="Medium",1,0)
data1$High_d<-ifelse(data$History1=="High",1,0)

# now again making model
str(data1)

model3<-lm(AmountSpent~ Location + Salary + Catalogs +children1 + male+ Female+ Missing_d+ Med_d + Low_d+ High_d, data = data1)

summary(model3)

model4<-lm(AmountSpent~ Location + Salary + Catalogs +children1 + male+ Missing_d+ Med_d + Low_d, data = data1)

summary(model4)

model5<-lm(AmountSpent~ Location + Salary + Catalogs +children1 + male+ Med_d + Low_d, data = data1)

summary(model5)

model6<-lm(AmountSpent~ Location + Salary + Catalogs +children1 + Med_d + Low_d, data = data1)

summary(model6)

#Signs

tapply(data$AmountSpent,data$History, mean)
data1%>%filter(History1!="Medium", History1!="low")%>%summarise(Mean=mean(AmountSpent))

tapply(data1$AmountSpent,data1$Location,mean) #inline

#Assumption checks

hist(model6$residuals) # presence of +skewness
qqPlot(model6$residuals)  # quantile quantile plote states that black circle is deviating from the rated red line
#so Non normal behaviour observed

#Multicollinearity Check

vif(model6) # no issue of multicolliniarity.but issue of normality

#Constant  variance check
plot(model6$fitted.values, model6$residuals) #model suffer from heteroscadacity(high varience) i.e does not support

#Remidies: Apply log transform to y variable

model7<-lm(log(AmountSpent)~ Location + Salary + Catalogs +children1 + Med_d + Low_d, data = data1)

summary(model7)

hist(model7$residuals) # little bit improved
qqPlot(model7$residuals) # situation a bit improved

#Constant  variance check
plot(model7$fitted.values, model7$residuals) # still funnel shaped

#Multicollinearity Check

vif(model7)


#Apply square root transform

mod8<-lm(formula = sqrt(AmountSpent) ~ Location + Salary + Catalogs + children1+Med_d+Low_d, data = data1)
summary(mod8)

hist(mod8$residuals) # little bit improved
qqPlot(mod8$residuals)

#Constant  variance check
plot(mod8$fitted.values, mod8$residuals) # more funnel shaped

#Multicollinearity Check

vif(mod8)

# compare the model with actual and predicted

pred_model<-model7$fitted.values
actual<-log(data1$AmountSpent)

Final_model<-data.frame(pred_model,actual)

p<-ggplot(Final_model, aes(x=row(Final_model)[,2], y=pred_model))
p+geom_line(colour="blue")+geom_line(data = Final_model, aes(y=actual), colour="black")