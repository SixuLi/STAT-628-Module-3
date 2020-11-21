#rm(list=ls())
# import package
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
# The business result data
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
  
  url1 <- a('--',"14 Ways to Burn Fat Fast!", href="https://www.healthline.com/nutrition/best-ways-to-burn-fat")
  output$reduce <- renderUI({
    tagList( url1)
  })
  
  url2 <- a("How to Take Accurate Girth Measurements?", href="http://business.fit/how-take-accurate-girth-measurements/")
  output$measure <- renderUI({
    tagList('--',url2)
  })
  
  url3 = a("How to Measure a Bicep?",hrep = 'https://www.livestrong.com/article/491858-how-to-measure-a-bicep/')
  output$m_bicep <- renderUI({tagList('--',url3)})
  
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
  #output the bigram plot
  output$plot2 <- renderPlot({
    
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
    
    d <- reviews_bigram_tf_idf %>% filter(name==input$restaurant)%>%
      group_by(name) %>%
      slice_max(tf_idf, n=input$n) %>%
      ungroup() 
    
    #draw the plot
    ggplot(data=d,aes(tf_idf, fct_reorder(bigram, tf_idf), fill = name)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~name, ncol = 2, scales = "free") +
      labs(x = "tf-idf", y = NULL)
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

