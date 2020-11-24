#import packages
library(shiny)
library(leaflet)
library(DT)
library(dplyr)
library(readr)

#set working directory
#setwd("")

#############################################
####load data################################
business_tag <- c("McDonald's","Burger King","Five Guys","Wendy's","Shake Shack")
review_business <- read_csv("review_business.csv")
fast_food_reviews <- read_csv("fast_food_reviews.csv")
####load coefficients and tf_idf data########
coefficients <- read_csv("coefficients.csv")
review_tf_idf <- read_csv("review_tf_idf.csv")


ui = shinyUI(
  #navbarPage session
  navbarPage(title = strong("The Five Best Burger Chains in US"),
                        tab1 <- tabPanel("",
                                    #insert moving function of table     
                                         tags$head(tags$style(
                                           HTML(
                                             "
  .sidebar { height: 90vh; overflow-y: auto; }
  .dataTables_wrapper { overflow-x: scroll; }
  "
                                           )
                                         )),
                                    # sidebar style
                                         sidebarLayout(sidebarPanel(
                                           #selectInput function
                                           selectInput("restaurant","Please Select Restaurant:",business_tag),uiOutput("a"),uiOutput("b")),
                                           mainPanel(
                                             #show the result in each panel
                                             tabsetPanel(tabPanel("Data Illustrate",DTOutput("DT")),tabPanel("Distribution of Stars",plotOutput("plot1")),
                                                         tabPanel("The Plot of Word Frequency",fluidPage(sliderInput("n","Please Select Number n:",min=1,max=20,value=1),
                                                                            column(6,plotOutput("plot2",width="100%", height= "600px")),
                                                                            column(6,plotOutput("plot3",width="100%", height= "600px")))),
                                                         tabPanel("Burger Chains Map",
                                                                  leafletOutput("fast_food_map", width="100%", height= "700px")
                                                         ))
                                           )))))

