library("shiny")
library("shiny")
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
library(shinyWidgets)
diamonds<-read.csv('diamonds.csv')
shinyServer(
  function(input,output){
    output$mycarat<-renderText(input$Carat)
    output$myclr<-renderText(input$Color)
    output$myclarity<-renderText(input$clarityname)
    output$mycut<-renderText(input$cutname)
    output$myhist<-renderPlot({
      colm <- as.numeric(input$var)
      hist(diamonds[,colm],breaks = seq(0,max(diamonds[,colm]),l=input$bins+1),col=input$clr,main="Histogram",xlab=names(diamonds[colm]))
    }
      
    )
    #output$data<-renderTable({
     # colm<-as.numeric(input$var)
      #head(diamonds[colm])
    #})
    #output$struc<-renderPrint({
     # str(diamonds)
    #})
    #output$summ<-renderPrint({
    #  summary(diamonds)
    #})
    #output$corr<-renderPlot({
      #ggcorr(diamonds[,1:10])
    #})
    output$bivar3<-renderPlot({
      cm<-as.numeric(input$var1)
      ggplot(diamonds, aes(factor(color), price, fill=color)) +
        geom_boxplot()
    }
    )
    
    output$bivar2<-renderPlot({
      cm1<-as.numeric(input$var2)
      ggplot(data=diamonds,aes(carat,price,color=color))+
        geom_point(alpha=1/2,size=1,position = 'jitter')+
        scale_color_brewer(type='div',guide_legend(title='color ',reverse=T,
                                                   override.aes=list(alpha=1,size=2)))+
        scale_x_continuous(trans=cuberoot_trans(),limits=c(0.2,3),breaks=c(0.2,0.5,1,2,3))+
        scale_y_continuous(trans=log10_trans(),limits=c(350,15000),breaks=c(350,1000,5000,10000,15000))
    })
    output$ans<-renderTable({
      diamondsdata <- data.frame(diamonds, header = TRUE)
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
      
      n_obs = dim(diamonds)[1]
      #n_obs
      
      prop_split = 0.80
      train_index = sample(1:n_obs, round(n_obs * prop_split))
      predictors <- diamonds[c(1:4)] #specifies the predictor variables within the diamonds dataset variables 1-4 which correspond to carat, cut, color and clarity
      #head(predictors)
      
      target <- diamonds$price #Selecting price as the output / target of the model
      #head(target)
      
      predictors <- model.matrix(price~., predictors)
      #str(predictors)
      
      #head(predictors)
      pred_tr = predictors[train_index,]
      pred_te = predictors[-train_index,]
      target_tr = target[train_index]
      target_te = target[-train_index]
      
      set.seed(42)
      lasso.diamonds <- glmnet(pred_te,target_te, alpha = 0, nlambda = 100, lambda.min.ratio = .0001)
      
      #here we are going to train our lambda then embed it into our lasso model 
      cv.diamonds.lasso <- cv.glmnet(pred_tr, target_tr, family="gaussian", alpha=1, nlambda=100, lambda.min.ratio=.0001)
      
      #coef(cv.diamonds.lasso, s=cv.diamonds.lasso$lambda.1se)
      #Lambda
      #plot(cv.diamonds.lasso, xvar = 'lambda')#We are decreasing the error within the model. 
      
      #Elastic net does a better job with highly correlated variables
      cv.diamonds.elnet <- cv.glmnet(pred_tr, target_tr, family="gaussian", alpha=.05, nlambda=100, lambda.min.ratio=.0001)
      
      #coef(cv.diamonds.elnet, s=cv.diamonds.elnet$lambda.1se)
      #RMSE_LASSO
      y_hat_lasso <- predict(cv.diamonds.lasso, pred_te)
      RMSE_Lasso <- sqrt(mean((target_te-y_hat_lasso)^2)) 
      #RMSE_Lasso
      #RMSE_ELNET
      y_hat_elnet <- predict(cv.diamonds.elnet, pred_te)
      RMSE_elnet<- sqrt(mean((target_te-y_hat_elnet)^2)) 
      #RMSE_elnet
      p1<-as.numeric(input$Carat)
      p2<-input$cutname
      p3<-input$Color
      p4<-input$clarityname
      BlueNileDiamondLM = data.frame(carat =p1 , cut = p2,
                                     color = p3, clarity=p4)
      # data.Frame creates a data frame, we created a dataframe with one value, BlueNileDiamondLM
      modelEstimate = predict(lm5, newdata = BlueNileDiamondLM,
                              interval="prediction", level = .95)
      p5<-exp(modelEstimate) # this will give us the actual price because our model outputs log 10.
      p5
    })
  }
)

