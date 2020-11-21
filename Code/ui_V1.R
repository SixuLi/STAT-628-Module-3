#import packages
library(shiny)
library(shinydashboard)
library(leafletCN)
library(leaflet)
library(openxlsx)
library(DT)
library(dplyr)
library(ggradar)
library(reshape)
library(recharts)
library(plyr)
library(readr)

#set working directory
#setwd("")

# import data
business <- read_csv("business_city.csv")
# set restaurant name
business_tag <- c("McDonald's","Burger King","Five Guys","Wendy's","Shake Shack")
# choose business name 
business <- business[business$name%in%business_tag,]
# choose variable
business <- business %>%
  select(business_id, name, address, city, state, postal_code,
         stars, review_count, is_open, categories,longitude,latitude)
# The result of business data
review_business <- business

#######Find origin text#######################
mystopwords <- tibble(word = c("bk", "mcdonald", "thru",
                               "morning", "breakfast",
                               "burgerking", "soul",
                               "xt", "snow", "passenger",
                               "q", "concrete", "satisfrie", "cle",
                               "nice", "burger", "love", "perfect", "lot",
                               "super", "fine", "pleasant", "food", "worst", "thi",
                               "king", "bad", "eat", "told", "reivew", "regular",
                               "add", "talk", "heard", "suck", "lunch", "shame",
                               "guy", "wendy"))
#import data
fast_food_reviews <- read_csv("review_city.csv") 
#choose business id
fast_food_reviews <- fast_food_reviews[fast_food_reviews$business_id%in%business$business_id,] 
#join business name
fast_food_reviews <- inner_join(fast_food_reviews,business)
#calculate review count
fast_food_reviews_count <- fast_food_reviews%>%group_by(business_id)%>%dplyr::summarise(review_count=sum(review_count))
#join the review count
review_business <- left_join(review_business,fast_food_reviews_count)



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
                                                         tabPanel("The Plot of Word Frequency",sliderInput("n","Please Select Number n:",min=1,max=20,value=1),plotOutput("plot2")),
                                                         tabPanel("Burger Chains Map",
                                                                  leafletOutput("fast_food_map", width="100%", height= "700px")
                                                         ))
                                           )))))


