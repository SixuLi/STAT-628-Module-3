#rm(list=ls())
# import package
library(DT)
library(leaflet)
library(shiny)
library(dplyr)
library(readr)
library(tidytext)
library(ggplot2)
library(tidyr)
library(forcats)

#set working directory
#setwd("")

# import data
#business <- read_csv("business_city.csv")
# set restaurant name
business_tag <- c("McDonald's","Burger King","Five Guys","Wendy's","Shake Shack")
# choose business name 
#business <- business[business$name%in%business_tag,]
# choose variable
#business <- business %>%
#  select(business_id, name, address, city, state, postal_code,
#         stars, review_count, is_open, categories,longitude,latitude)
# The business result data
#review_business <- business

#######Find origin text#######################
#mystopwords <- tibble(word = c("bk", "mcdonald", "thru",
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
#fast_food_reviews <- read_csv("review_city.csv") 
#choose business id
#fast_food_reviews <- fast_food_reviews[fast_food_reviews$business_id%in%business$business_id,] 
#join business name
#fast_food_reviews <- inner_join(fast_food_reviews,business)
#calculate review count
#fast_food_reviews_count <- fast_food_reviews%>%group_by(business_id)%>%dplyr::summarise(review_count=sum(review_count))
#join the review count
#review_business <- left_join(review_business,fast_food_reviews_count)
##################################################
##########Load Data#############
##################################################
business_tag <- c("McDonald's","Burger King","Five Guys","Wendy's","Shake Shack")
review_business <- read_csv("review_business.csv")
fast_food_reviews <- read_csv("fast_food_reviews.csv")
####load coefficients and tf_idf data########
coefficients <- read_csv("coefficients.csv")
review_tf_idf <- read_csv("review_tf_idf.csv")
server = function(input, output, session) {
  
  #choose one of the five restaurant
  Pass <- reactive({
    review_business[review_business$name==input$restaurant,]
  })
  #choose the state
  output$a <- renderUI({
    selectInput("state","Please Select State:",c("ALL",Pass()$state))
  })
  #choose the city
  output$b <- renderUI({
    selectInput("city","Please Select City:",c("ALL",Pass()[Pass()$state==input$state,]$city))
  })
  
  #output the table 
  output$DT <- renderDT({
    if(input$state=="ALL"&input$city=="ALL"){
      Pass()%>%select(name,address,stars)
     } else if (input$state!="ALL"&input$city=="ALL"){
       Pass()%>%filter(state==input$state)%>%select(name,address,stars)
     }   else {
      Pass()%>%filter(city==input$city&state==input$state)%>%select(name,address,stars)
    }
  })
  
  
  #output the stars distribution
  output$plot1 <- renderPlot({
    if(input$state=="ALL"){
      pass <- Pass()%>%select(business_id,name,stars)
    } else if (input$state!="ALL"&input$city=="ALL"){
        pass <- Pass()%>%filter(state==input$state)%>%select(business_id,name,stars)
      }     else {
      pass <- Pass()%>%filter(city==input$city&state==input$state)%>%select(business_id,name,stars)
    }
        ggplot(data=pass,aes(x=stars,fill="red"))+geom_histogram()
  })
  #output the single word plot
  #output Top 20 positive and negative words
  output$plot2 <- renderPlot({
    # Obtain the coefficients corresponding to specific restaurant
    restaurant_coefficients <- inner_join(coefficients,review_tf_idf %>% filter(name==input$restaurant)) %>% mutate(score=coef*tf)
    # Top 30 positive and negative words
    plot(restaurant_coefficients %>%
           top_n(30, abs(score)) %>%
           mutate(word = reorder(word, score)) %>%
           head(input$n) %>%
           ggplot(aes(word, score, fill = score > 0)) +
           geom_col(show.legend = FALSE) +
           labs(y = "score", x = "word", title = input$restaurant) +
           theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
           coord_flip())
  })
  
  
  
  #output the bigram plot
  output$plot3 <- renderPlot({
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
    (reviews_bigram_tf_idf <- fast_food_reviews %>%
       unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
       separate(bigram, c("word1", "word2"), sep = " ") %>%
       filter(!word1 %in% stop_words$word,
              !word2 %in% stop_words$word,
              !word1 %in% mystopwords$word,
              !word2 %in% mystopwords$word) %>%
       unite(bigram, word1, word2, sep = " ") %>%
       dplyr::count(name, bigram) %>%
       bind_tf_idf(bigram, name, n) %>%
       arrange(desc(tf_idf))
    )
    
    d <- reviews_bigram_tf_idf %>%
      group_by(name) %>%
      slice_max(tf_idf, n=input$n) %>%
      ungroup() 
    
    #draw the plot
    ggplot(data=d%>% filter(name==input$restaurant),aes(tf_idf, fct_reorder(bigram, tf_idf), fill = name)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~name, ncol = 2, scales = "free") +
      labs(x = "tf-idf", y = NULL)
  })
  #suggestion  
  output$d <- renderUI({
    switch(input$restaurant,
           "McDonald's"= h5("Suggestion of McDonald's: Please improve service speed and enhence work efficiency. 
                            Top Tags: Quarter Pounder,Happy Meal,Egg MCMuffin"),
           "Burger King"= h5("Suggestion of Burger King: Please keep food warm without overcook and avoid sending wrong meal and wrong tempertaure. 
                             Top Tags: Double Whopper, Pretzel Bun, Onion Rings "),
           "Five Guys"= h5("Suggestion of Five Guys: Please improve quality of bacon and notice the refund service. 
                           Top Tags: Guys Burgers, Cajun Fries"),
           "Wendy's"= h5("Suggestion of  Wendy's: Please improve service speed and attitude. PLease provide fresh chicken. Top Tags:Dave Thomas, Spicy Chicken"),
           "Shake Shack"= h5("Suggestion of Shake Shack: Please to increase the quantity demand for fries and speed up; 
                             Top Tags: Shake Shack, Strawberry Shake, Chessy Fries "))
  })

  #draw the map
  output$fast_food_map <- renderLeaflet({
    
    color <- c("red","blue","green","purple","cyan")
    if(input$state=="ALL"){
      pass <- Pass()
    } else {
      pass <- Pass()%>%filter(state==input$state)
    }  
    Pass<- pass%>%dplyr::mutate(stars=round(stars))%>%na.omit()%>%data.frame()
    pal <- colorFactor(as.character(color), domain = Pass$stars)
    
    leaflet(Pass) %>% 
      addTiles() %>%addLegend(position = "bottomleft",pal=pal,values = Pass$stars)%>%
      addCircles(as.numeric(Pass$longitude), as.numeric(Pass$latitude),color = pal(Pass$stars), radius=Pass$review_count*50,popup = paste("Address:",Pass$address,". ","Review Count:",Pass$review_count),fillOpacity = 1,stroke = FALSE)
    
    
  })
  
  
}

shinyApp(ui= ui, server=server)

