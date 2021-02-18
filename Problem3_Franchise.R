install.packages("GGally")
library("readxl")
GFranchise=read_excel(file.choose())
attach(GFranchise)
str(GFranchise)
summary(GFranchise)

#check for null values
cbind(colSums(is.na(GFranchise)))   # No NUll values

# boxplots - check for outliers
boxplot(X1)
boxplot(GFranchise)
boxplot(X3)
boxplot(X2,X4,X5,X6)              # No outliers

#Creating a scatter plot between Amount of Sales and the independent variables
library(GGally)
ggpairs(GFranchise)

#Creating a Correlation matrix to identify any linear relationship
Cor.GF=cor(GFranchise)
round(Cor.GF,digits = 2)

#An additional plot to check for correlation - instead of using the matrix
install.packages("corrplot")
library(corrplot)
corrplot(Cor.GF)


## Multi-linear Regressions with all independent variables
reg1=lm(X1~.,GFranchise)
summary(reg1)
shapiro.test(reg1$residuals)

## Testing Multi-collinearity of the model
library(car)
vif(reg1)  # X3 has high VIF causing multi collinearity 

#Run Final multi-linear regression model by removing X3

reg2=lm(X1~.-X3,data = GFranchise)
summary(reg2)
vif(reg2)
shapiro.test(reg2$residuals)

plot(reg2)


#Backtracking- Actual Vs Prediction
Actual=X1
Prediction=predict(reg2)
BackTrack=data.frame(Actual,Prediction)
BackTrack
plot(Actual,col="Red")
lines(Actual,col="Red")
plot(Prediction,col="Blue")
lines(Prediction,col="Blue")
 #yhat=-39.460+16.20.444X2+0.17X4+11.53X5+13.58X5-5.31X6
# for one unit increase in X2 (Franchise size) there is increase of annual net sales by 16.2 units



anova(reg2)
confint(reg2)
