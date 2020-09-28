
# This is the server logic for STP viz web app.

library(shiny)
library(ggplot2)
library(dplyr)
library(readr)

# setwd('C:/Users/tyler/repos/rshinybnb')
# read data
df_data <- read_csv('app_data/listing20_01to06.csv')


function(input, output){
  
  # filter listings by property type, room type and suburb
  filered_list <- reactive({
      
    rows <- (input$list_ptype == 'All' | df_data$PROPERTY_TYPE == input$list_ptype)&
      (input$list_rtype == 'All' | df_data$ROOM_TYPE == input$list_rtype) & 
      (input$list_sub == 'All' | df_data$NEIGHBOURHOOD == input$list_sub)
    
    df_list_type <- df_data[rows,,drop=FALSE]
    
    if (input$list_new){
      df_list_new <- df_list_type %>% filter(HOST_MONTHS==0)
    } else {
      df_list_new <- df_list_type
    }
    
    if (input$list_lux){
      df_list_lux <- df_list_new %>% filter(LUXURY_FLAG==1)
    } else {
      df_list_lux <- df_list_new
    }
    
  })
  
  # output 1.1, bar, number of listings by month
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
            title=element_text(size=14,face="bold"))+
      geom_label(aes(label=format(n,big.mark=",")))
  })
  
  # filter hosts by suburb, host type, multiple or single listings,
  filered_host <- reactive({
    rows <- input$host_sub == 'All' | df_data$NEIGHBOURHOOD == input$host_sub
    df_host_sub <- df_data[rows,,drop=FALSE]
    
    if (input$host_type=='All') {
      df_host_type <- df_host_sub
    } else if (input$host_type=='Lux host') {
      df_host_type <- df_host_sub %>% filter(LUXURY_FLAG==1)
    } else if (input$host_type=='Super host') {
      df_host_type <- df_host_sub %>% filter(HOST_IS_SUPERHOST==1)
    } else {
      df_host_type <- df_host_sub %>% filter(HOST_IS_SUPERHOST==0)
    }
    
    if (input$mult_host == 'All') {
      df_host_num <- df_host_type
    } else if (input$mult_host == 'Single listing'){
      df_host_num <- df_host_type %>% filter(HOST_LISTINGS_COUNT==1)
    } else {
      df_host_num <- df_host_type %>% filter(HOST_LISTINGS_COUNT > 1)  
    }
    
    if (input$new_host){
      df_host_num %>% filter(HOST_MONTHS==0)
    } else {
      df_host_num
    }
    
  })
  
  # output 1.2, bar, number of hosts by month
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
            title=element_text(size=14,face="bold"))+
      geom_label(aes(label=format(host_c,big.mark=",")))
  })
  
  # filter price by property type, room type, suburb
  filered_price <- reactive({
    
    rows <- (input$price_ptype == 'All' | df_data$PROPERTY_TYPE == input$price_ptype)&
      (input$price_rtype == 'All' | df_data$ROOM_TYPE == input$price_rtype)&
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
  
  # output 1.3, line, average price per room by month
  output$line_price_month <- renderPlot({
    #line, price by month
    df_p_mean <- filered_price() %>%
      group_by(YEAR_MONTH) %>%
      summarise(avg_price_per_room = round(mean(PRICE_PER_ROOM,na.rm = TRUE)))
    ggplot(df_p_mean, aes(x=YEAR_MONTH,y=avg_price_per_room,group=1))+
      geom_line()+
      geom_label(aes(label=avg_price_per_room))+
      labs(
        title = 'Average Price (A$/Rooms) by Month',
        subtitle = 'The average price generally decreased since the beginning of this year',
        x='Month',
        y='Average price per room* (A$/room)',
        caption = 'Note: price per room = nightly price / number of bedrooms.'
      )+
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=12,face="bold"),
            title=element_text(size=14,face="bold"))
      
  })
  
  # filter supply by property type, room type and suburb
  filered_sup <- reactive({
    
    rows <- (input$sup_ptype == 'All' | df_data$PROPERTY_TYPE == input$sup_ptype)&
      (input$sup_rtype == 'All' | df_data$ROOM_TYPE == input$sup_rtype) & 
      (input$sup_sub == 'All' | df_data$NEIGHBOURHOOD == input$sup_sub)
    
    df_sup_type <- df_data[rows,,drop=FALSE]
    
    if (input$sup_new){
      df_sup_new <- df_sup_type %>% filter(HOST_MONTHS==0)
    } else {
      df_sup_new <- df_sup_type
    }
    
    if (input$sup_lux){
      df_sup_lux <- df_sup_new %>% filter(LUXURY_FLAG==1)
    } else {
      df_sup_lux <- df_sup_new
    }
    
  })
  
  # output 1.4, bar,supply by month
  output$bar_sup_month <- renderPlot({
    
    df_sup <- filered_sup() %>% 
      group_by(YEAR_MONTH) %>% 
      summarise(
                supply = sum(AVAILABILITY_30)
                )

    ggplot(df_sup, aes(x=YEAR_MONTH, y= supply))+
      geom_col(width = 0.4,fill = 'lightgreen')+
      labs(
        title = 'Number of Nights Available by Month',
        subtitle = 'The supply (number of nights available) generally decreased since March',
        x='Month',
        y='Number of nights available'
        
      )+
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=12,face="bold"),
            title=element_text(size=14,face="bold"))+
      geom_label(aes(label=format(supply,big.mark=",")))
  })
}
