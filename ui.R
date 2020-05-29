library("shiny")
library(ggplot2)
library(dplyr)
library(GGally)
library(scales)
library(tidyr)
library(colorspace)
library(modelr)
library(leaps)
library(gplots)
library(hexbin)
library(Hmisc)
library(xkcd)
library(glmnet)
library(ROCR)
library('tidyverse')
library('RColorBrewer')
library(shinyWidgets)
diamonds<-read.csv('diamonds.csv')
shinyUI(fluidPage(
  tags$h1("Diamond Price Prediction"),
  setBackgroundImage(
    src = "https://media.istockphoto.com/photos/diamond-on-the-water-picture-id639777488?k=6&m=639777488&s=612x612&w=0&h=JWUtKTN4CUTJ4dp0tPb2yXJq6Vh6s7smkZ-ZX4sgPbM="),
  #titlePanel(title = h2("Diamond Price Prediction",align="center")),
  sidebarLayout(
    sidebarPanel(("Enter information"),
                 #textInput("Carat","Enter carat"," "),
                 #selectInput("cutname","Select a cut grade",c("Good","Very Good","Ideal","Premium","Fair"),selectize=FALSE,multiple=FALSE),
                 #radioButtons("Color","Select color grade",list("D","E","F","G","H","I","J")," "),
                 #selectInput("clarityname","Select a clarity grade",c("I1","SI1","SI2","VS2","VS1","VVS2","VVS1","IF"),selectize = FALSE,multiple = FALSE),
                 #sliderInput("Slide","Select value for slider",min=1,max=2,value = 2),
                 
                 selectInput("var","Select variable for histogram",choices = c("Price"=8,"Carat"=2)),
      
                 sliderInput("bins","Select no of bins for histogram",min=5,max = 25,value=15),
                 br(),
                 radioButtons("clr","Select color of the histogram",choices=c("Green","Red","Blue","Yellow","Cyan"),selected="Green"),
                 br(),
                 selectInput("var1","Select variable for comparision with Price",choices = c("Color"=4,"Clarity"=5,"Cut"=3),selected = "Color"),
                 br(),
                 selectInput("var2","Select variable for comparision with Price and Carat",choices = c("Color"=4,"Clarity"=5,"Cut"=3),selected = "Color"),
                 br(),
                 textInput("Carat","Enter carat"," "),
                 selectInput("cutname","Select a cut grade",c("Good","Very Good","Ideal","Premium","Fair"),selectize=FALSE,multiple=FALSE),
                 radioButtons("Color","Select color grade",list("D","E","F","G","H","I","J")," "),
                 selectInput("clarityname","Select a clarity grade",c("I1","SI1","SI2","VS2","VS1","VVS2","VVS1","IF"),selectize = FALSE,multiple = FALSE),
                 br()
                
                ),
    mainPanel(h3("Select tabs"),
              #textOutput("mycarat"),
              #textOutput("myclr"),
              #textOutput("myslider"),
              #textOutput("myclarity"),
              #plotOutput("myhist"),
              tabsetPanel(type="tab",
                          tabPanel("Information",helpText(h5("->Cut quality increases Fair,Good,Very Good,Ideal,Premium.")),
                                                 helpText(h5("->This model gives 95% accurate results.")),
                                                 helpText(h5("->Carat varies from 0.2 to 5.1.")),
                                                 helpText(h5("->Price is in US Dollars."))),
                                                 #helpText("Cut quality increases Fair,Good,Very Good,Ideal,Premium")),
                          #tabPanel("Structure",verbatimTextOutput("struc")),
                          #tabPanel("Data",tableOutput("data")),
                          tabPanel("Histogram Plot",plotOutput("myhist")),
                          #tabPanel("Correlation",plotOutput("corr")),
                          tabPanel("Box Plots",plotOutput("bivar3")),
                          tabPanel("Price vs Carat and X",plotOutput("bivar2")),
                          tabPanel("Prediction",
                                                tableOutput("ans"))
                          )
              
         
    
              )
    )
)
)

  