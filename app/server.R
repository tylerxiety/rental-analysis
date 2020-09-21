
# This is the server logic for STP viz web app.

library(shiny)
library(ggplot2)
library(dplyr)
library(readr)

# setwd('C:/Users/tyler/repos/rshinybnb')
# read data
df_data <- read_csv('app_data/listing20_01to06.csv')


function(input, output){
  
  # list by type
  filered_list <- reactive({
      
    rows <- (input$prop_type == 'All' | df_data$PROPERTY_TYPE == input$prop_type)&
      (input$room_type == 'All' | df_data$ROOM_TYPE == input$room_type) & 
      (input$list_sub == 'All' | df_data$NEIGHBOURHOOD == input$list_sub)
    
    df_l_type <- df_data[rows,,drop=FALSE]
    
    if (input$new_list){
      df_l_new <- df_l_type %>% filter(HOST_MONTHS==0)
    } else {
      df_l_new <- df_l_type
    }
    
    if (input$lux_list){
      df_l_lux <- df_l_new %>% filter(LUXURY_FLAG==1)
    } else {
      df_l_lux <- df_l_new
    }
    
  })
  # output 1.1
  output$bar_list_month <- renderPlot({
    # bar, listing by month
    ggplot(filered_list() %>%
             count(YEAR_MONTH), aes(x=YEAR_MONTH,n,group = 1))+
      geom_col(width = 0.4,fill = 'pink')+
      # geom_line()+
      # geom_point()+
      labs(
        title = 'Number of Short-term Rental Properties by Month',
        subtitle = 'The number of properties generally decreased in the first 6 months of this year',
        x='Month',
        y='Number of properties',
        caption = 'Data from Airbnb'
      )+
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=12,face="bold"),
            title=element_text(size=14,face="bold"))
  })
  
  # host by type
  filered_host <- reactive({
    rows <- input$host_sub == 'All' | df_data$NEIGHBOURHOOD == input$host_sub
    df_h_sub <- df_data[rows,,drop=FALSE]
    
    if (input$host_type=='All') {
      df_h_type <- df_h_sub
    } else if (input$host_type=='Lux host') {
      df_h_type <- df_h_sub %>% filter(LUXURY_FLAG==1)
    } else if (input$host_type=='Super host') {
      df_h_type <- df_h_sub %>% filter(HOST_IS_SUPERHOST==1)
    } else {
      df_h_type <- df_h_sub %>% filter(HOST_IS_SUPERHOST==0)
    }
    
    if (input$mult_host == 'All') {
      df_h_num <- df_h_type
    } else if (input$mult_host == 'Single listing'){
      df_h_num <- df_h_type %>% filter(HOST_LISTINGS_COUNT==1)
    } else {
      df_h_num <- df_h_type %>% filter(HOST_LISTINGS_COUNT > 1)  
    }
    
    if (input$new_host){
      df_h_num %>% filter(HOST_MONTHS==0)
    } else {
      df_h_num
    }
    
  })
  
  # output 1.2
  output$bar_host_month <- renderPlot({
    # bar, host by month
    ggplot(filered_host() %>%
             group_by(YEAR_MONTH) %>%
             summarise(host_c = n_distinct(HOST_ID)), aes(x=YEAR_MONTH,y=host_c,group = 1))+
      geom_col(width = 0.4,fill='lightblue')+
      # geom_line()+
      # geom_point()+
      labs(
        title = 'Number of Hosts by Month',
        subtitle = 'The number of hosts generally decreased since the beginning of this year',
        x='Month',
        y='Number of hosts'
      )+
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=12,face="bold"),
            title=element_text(size=14,face="bold"))
  })
  
  # price by type
  filered_price <- reactive({
    
    rows <- (input$p_prop_type == 'All' | df_data$PROPERTY_TYPE == input$p_prop_type)&
      (input$p_room_type == 'All' | df_data$ROOM_TYPE == input$p_room_type)&
      (input$price_sub == 'All' | df_data$NEIGHBOURHOOD == input$price_sub)
    
    df_p_type <- df_data[rows,,drop=FALSE]
    
    # if (input$price_type=='All') {
    #   df_p_type
    # } else if (input$price_type=='Lux host') {
    #   df_p_type %>% filter(LUXURY_FLAG==1)
    # } else {
    #   df_p_type %>% filter(LUXURY_FLAG==0)
    # }
    
  })
  # output 1.3
  output$line_price_month <- renderPlot({
    #line, price by month
    df_p_mean <- filered_price() %>%
      group_by(YEAR_MONTH) %>%
      summarise(avg_price_per_room = round(mean(PRICE_PER_ROOM,na.rm = TRUE)))
    ggplot(df_p_mean, aes(x=YEAR_MONTH,y=avg_price_per_room,group=1))+
      geom_line()+
      geom_label(aes(label=avg_price_per_room))+
      labs(
        title = 'Average Price by Month',
        subtitle = 'The average price generally decreased since the beginning of this year',
        x='Month',
        y='Average price per room* (A$/room)',
        caption = 'Note: price per room = nightly price / number of bedrooms.'
      )+
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=12,face="bold"),
            title=element_text(size=14,face="bold"))
      
  })
}