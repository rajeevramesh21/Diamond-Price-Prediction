library(ggplot2)
library(dplyr)
library(GGally)
library(scales)
library(tidyr)
library(gplots)
library(hexbin)
library(colorspace)
library(modelr)
library(leaps)
library(Hmisc)
library(xkcd)
library(glmnet)
library(ROCR)
library('tidyverse')
library('RColorBrewer')
#install.packages('shinyWidgets')
diamonds<-read.csv('diamonds.csv')
head(diamonds)
names(diamonds)
str(diamonds)
sum(is.na(diamonds))#to check whether we have any NA in our data
which(is.na(diamonds))#Location of NA values

#UNIVARIATE ANALYSIS

cor.test(diamonds$price,diamonds$carat,method = "pearson")



plot(diamonds$price)
#Such plots usually occur for exponential relationships
data("diamonds")
summary(diamonds)
dim(diamonds)

plot(diamonds$carat)
#Messy graphical view

plot(diamonds$depth) # Depth is essentially a straight line with little change. 
#Does this mean there is no correlation? Maybe we should exclude this variable. 

plot(diamonds$table)#flat model,possibly no relation to dependent variable.

plot(diamonds$cut)#Ideal cut preferred by most of the customers.best selling

plot(diamonds$color)#increase in count then decrease.Probably E,G are bestsellers.

levels(diamonds$clarity)
plot(diamonds$clarity)#Quality factor being affected due to increase in price.
#Average priced and good quality diamonds preferred.

ggplot(data=diamonds) + geom_histogram(binwidth = 500, aes(x=diamonds$price)) +
  ggtitle("Diamond Price Distribution") +
  xlab ("Diamond Price in US Dollars") +
  ylab("Frequency") +
  theme_classic()
mean(diamonds$price)#quite high mean.Cannot be used for data visualization.

#BIVARIATE ANALYSIS

#Diamond Price Distribution by Cut
ggplot(data=diamonds) + geom_histogram(binwidth = 500, aes(x=diamonds$price)) +
  ggtitle("Diamond Price Distribution by cut") +
  xlab ("Diamond Price in US Dollars") +
  ylab("Frequency") +
  theme_classic()+
  facet_wrap(~cut)
subset(diamonds,price==max(price))#18823

subset(diamonds,price==min(price))#326


#Diamond Price Distribution by Clarity
ggplot(data=diamonds) + geom_histogram(binwidth = 500, aes(x=diamonds$price)) +
  ggtitle("Diamond Price Distribution by Clarity") +
  xlab ("Diamond Price in US Dollars") +
  ylab("Frequency") +
  theme_classic()+
  facet_wrap(~clarity)

#Diamond Price Distribution by Color
ggplot(data=diamonds) + geom_histogram(binwidth = 500, aes(x=diamonds$price)) +
  ggtitle("Diamond Price Distribution by Color") +
  xlab ("Diamond Price in US Dollars") +
  ylab("Frequency") +
  theme_classic()+
  facet_wrap(~color)

#Variable Tranformation and Analysis
ggplot(data=diamonds, aes(x=carat, y=price)) +
  scale_x_continuous(lim=c(0,quantile(diamonds$carat,0.99))) +
  scale_y_continuous(lim=c(0,quantile(diamonds$price,0.99))) +
  geom_point(fill=I('#dd3333'), color= I("black"), aes(alpha=1/10),shape=21) +
  stat_smooth(method='lm')+
  theme_xkcd()#Positive Linear Relationship noticed between them
install.packages('ggpairs')
library(GGally)
set.seed(42) #Yep, inserting the "cool data science seed thing to do #42" :) Taken from https://rpubs.com/anthonycerna/diamondspredictions
diamond_samp <- diamonds[sample(1:length(diamonds$price), 25000), ] #Looking at the first 25,000 diamonds
ggpairs(diamond_samp, outlier.shape = I('.'))

#BOXPLOTS for Outliers and variation
#price vs cut
ggplot(diamonds, aes(factor(cut), price, fill=cut)) +
  geom_boxplot() + ggtitle("Diamond Price according Cut") +
  xlab("Type of Cut") + 
  ylab("Diamond Price in US Dollars")
#price vs clarity
ggplot(diamonds, aes(factor(clarity), price, fill=clarity)) +
  geom_boxplot() + ggtitle("Diamond Price according Clarity") +
  xlab("Clarity Grade") + 
  ylab("Diamond Price in US Dollars")
#price vs color
ggplot(diamonds, aes(factor(color), price, fill=color)) +
  geom_boxplot() + ggtitle("Diamond Price according Color") +
  xlab("Color") + 
  ylab("Diamond Price in US Dollars")
#Cut stratified with clarity differentiated bins
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = clarity)) +
  ggtitle("Cut Stratified with Clarity Differentiated Cut Bins") +
  xlab("Cut") +
  ylab("Count")
#correlation plot
ggcorr(diamonds[,1:10])

#Variable Transformation
library('tidyverse')
cuberoot_trans=function() trans_new('cuberoot',transform = function(x) {x^(1/3)},inverse=function(x) {x^3})
ggplot(aes(carat, price), data = diamonds) + 
  geom_point() + 
  scale_x_continuous(trans = cuberoot_trans(), limits = c(0.2, 3),
                     breaks = c(0.2, 0.5, 1, 2, 3)) + na
  scale_y_continuous(trans = log10_trans(), limits = c(350, 15000),
                     breaks = c(350, 1000, 5000, 10000, 15000)) +
  ggtitle('Price (log10) by Cube-Root of Carat')

#PRICE VS CARAT AND CLARITY
library('RColorBrewer')
ggplot(data=diamonds,aes(carat,price,color=clarity))+
  geom_point(alpha=1/2,size=1,position = 'jitter')+
  scale_color_brewer(type='div',guide_legend(title='Clarity',reverse=T,
                                             override.aes=list(alpha=1,size=2)))+
  scale_x_continuous(trans=cuberoot_trans(),limits=c(0.2,3),breaks=c(0.2,0.5,1,2,3))+
  scale_y_continuous(trans=log10_trans(),limits=c(350,15000),breaks=c(350,1000,5000,10000,15000))+
  ggtitle('Price(log10) v/s Carat(cuberoot) And Clarity')+
  facet_wrap(~clarity)

#PRICE VS CARAT AND CUT
library('RColorBrewer')
ggplot(data=diamonds,aes(carat,price,color=cut))+
  geom_jitter(alpha=0.5,size=1)+
  scale_color_brewer(palette = 'Set2',type='div',guide_legend(title='Cut',reverse=T,
                                                              override.aes=list(alpha=1,size=2)))+
  scale_x_continuous(trans=cuberoot_trans(),limits=c(0.2,3),breaks=c(0.2,0.5,1,2,3))+
  scale_y_continuous(trans=log10_trans(),limits=c(350,15000),breaks=c(350,1000,5000,10000,15000))+
  ggtitle('Price(log10) v/s Carat(cuberoot) And Cut')

#PRICE VS CARAT AND COLOR
library('RColorBrewer')
ggplot(data=diamonds,aes(carat,price,color=color))+
  geom_jitter(alpha=0.5,size=1)+
  scale_color_brewer(type='div',guide_legend(title='Color',reverse=F,
                                             override.aes=list(alpha=1,size=2)))+
  scale_x_continuous(trans=cuberoot_trans(),limits=c(0.2,3),breaks=c(0.2,0.5,1,2,3))+
  scale_y_continuous(trans=log10_trans(),limits=c(350,15000),breaks=c(350,1000,5000,10000,15000))+
  ggtitle('Price(log10) v/s Carat(cuberoot) And Color')

#Diamond color does contribute to price.
#Adjusted R squared values
reg.best <- regsubsets(price~., data = diamonds, nvmax = 19)#Price is the dependent variable
#The plot will show the Adjust R^2 when using the variables across the bottom
plot(reg.best, scale = "adjr2", main = "Adjusted R^2")

#Model Creation
#since carat weight is determined by volume in three dimensional space xyz= volume and
#hence carat weight, then we can use the following equation:
#lm(log(price) ~ carat^(1/3)).
diamondsdata <- data.frame(diamonds, header = TRUE)

#Create variables
carat <- diamonds$carat
cut <- diamonds$cut
color <- diamonds$color
clarity <- diamonds$clarity
price <- diamonds$price

lm1 <- lm(I(log(price)) ~ I(carat^(1/3)), data = diamonds)
lm2 <- update(lm1, ~ . + carat)
lm3 <- update(lm2, ~ . + cut)
lm4 <- update(lm3, ~ . + color)
lm5 <- update(lm4, ~ . + clarity)
mtable(lm1, lm2, lm3, lm4, lm5)

summary(lm5)

#EVALUATION METRICS
#First let's split our data in train and test, using say 80% train, 20% test and target/predictor variables
n_obs = dim(diamonds)[1]
n_obs

prop_split = 0.80
train_index = sample(1:n_obs, round(n_obs * prop_split))
predictors <- diamonds[c(1:4)] #specifies the predictor variables within the diamonds dataset variables 1-4 which correspond to carat, cut, color and clarity
head(predictors)

target <- diamonds$price #Selecting price as the output / target of the model
head(target)

predictors <- model.matrix(price~., predictors)
str(predictors)

head(predictors)
pred_tr = predictors[train_index,]#
pred_te = predictors[-train_index,]#
target_tr = target[train_index]#
target_te = target[-train_index]#
library("caret")
install.packages("caret")
act_pred<-data.frame(cbind(pred_te,target_te))
cor(act_pred)
set.seed(42)
lasso.diamonds <- glmnet(pred_te,target_te, alpha = 0, nlambda = 100, lambda.min.ratio = .0001)

#here we are going to train our lambda then embed it into our lasso model 
cv.diamonds.lasso <- cv.glmnet(pred_tr, target_tr, family="gaussian", alpha=1, nlambda=100, lambda.min.ratio=.0001)

coef(cv.diamonds.lasso, s=cv.diamonds.lasso$lambda.1se)
#Lambda
plot(cv.diamonds.lasso, xvar = 'lambda')#We are decreasing the error within the model. 

#Elastic net does a better job with highly correlated variables
cv.diamonds.elnet <- cv.glmnet(pred_tr, target_tr, family="gaussian", alpha=.05, nlambda=100, lambda.min.ratio=.0001)

coef(cv.diamonds.elnet, s=cv.diamonds.elnet$lambda.1se)

#RMSE_LASSO
y_hat_lasso <- predict(cv.diamonds.lasso, pred_te)
RMSE_Lasso <- sqrt(mean((target_te-y_hat_lasso)^2)) 
RMSE_Lasso
#RMSE_ELNET
y_hat_elnet <- predict(cv.diamonds.elnet, pred_te)
RMSE_elnet<- sqrt(mean((target_te-y_hat_elnet)^2)) 
RMSE_elnet

#PREDICTIONS

BlueNileDiamondLM = data.frame(carat = 1.10, cut = "Ideal",
                               color = "F", clarity="VVS2")
# data.Frame creates a data frame, we created a dataframe with one value, BlueNileDiamondLM
modelEstimate = predict(lm5, newdata = BlueNileDiamondLM,
                        interval="prediction", level = .95)
exp(modelEstimate) # this will give us the actual price because our model outputs log 10.


BlueNileDiamondLM1 = data.frame(carat = 0.90, cut = "Very Good",
                                color = "F", clarity="VS2")
# data.Frame creates a data frame, we created a dataframe with one value, BlueNileDiamondLM1
modelEstimate1 = predict(lm5, newdata = BlueNileDiamondLM1,
                         interval="prediction", level = .95)
exp(modelEstimate1) # this will give us the actual price because our model outputs log 10.


BlueNileDiamondLM1 = data.frame(carat = 0.23, cut = "Ideal",
                                color = "E", clarity="SI2")
# data.Frame creates a data frame, we created a dataframe with one value, BlueNileDiamondLM1
modelEstimate1 = predict(lm5, newdata = BlueNileDiamondLM1,
                         interval="prediction", level = .95)
exp(modelEstimate1) # this will give us the actual price because our model outputs log 10.