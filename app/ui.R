
# This is the ui file for STP viz web app.

library(shiny)
library(dplyr)
library(readr)

df_data <- read_csv('app_data/listing20_01to06.csv')


fluidPage(
  title = 'Short-term Rental Properties',
  tabsetPanel(
    
    #Panel 1
    tabPanel(
      title = 'Overview',
      tags$h3('Short-term Rental Properties in Melbourne (Greater), Victoria'),
      # row 1
      wellPanel(fluidRow(
        column(2,tags$h3('Properties'),
               # input 1.1.1, select suburb
               selectInput(
                 inputId = 'list_sub',
                 label = 'Select a city/area',
                 choices=c('All', 'Melbourne'='Melbourne',df_data$NEIGHBOURHOOD %>% unique() %>% sort()),
                 selectize = TRUE),
               
               # input 1.1.2, select property type
               selectInput(
                 inputId = 'list_ptype',
                 label='Select a property type',
                 choices=c('All',
                          'Apartment'='apartment',
                          'House' = 'house',
                          'Hotel' = 'serviced',
                          'Other' = 'other'),
                 multiple=FALSE,
                 selectize = TRUE
                 ),
                              
               # input 1.1.3, select room type
               selectInput(
                 inputId = 'list_rtype',
                 label='Select a room type',
                 choices=c('All',
                          'Entire apt/home'='entire home/apt',
                          'Private room' = 'private room',
                          'Shared room' = 'shared room'),
                multiple=FALSE,
                selectize = TRUE
                ),
               
               #input 1.1.4, select new list or not
               checkboxInput(
                 inputId = 'list_new',
                 label = 'Show new listings only'
               ),
               
               #input 1.1.5, select Luxe list or not
               checkboxInput(
                 inputId = 'list_lux',
                 label = 'Show Luxe properties* only'
               ),
               tags$h5('*Luxe properties are high-end properties approved by Airbnb.')
              ),
        # output 1.1
        column(10,plotOutput("bar_list_month"))
        )
         ),
      
      # row 2
      wellPanel(fluidRow(
        column(2,tags$h3('Hosts'),
               # input 1.2.1, select suburb
               selectInput(
                 inputId = 'host_sub',
                 label = 'Select a city/area',
                 choices=c('All', 'Melbourne'='Melbourne',df_data$NEIGHBOURHOOD %>% unique() %>% sort()),
                 selectize = TRUE),

               # input 1.2.2, host type
               selectInput(
                 inputId = 'host_type',
                 label='Select a host type',
                 choices=c('All',
                           'LUX host',
                           'Super host',
                           'Normal host'),
                 multiple=FALSE,
                 selectize = TRUE
                 ),

               # input 1.2.3, host with multiple listings
               selectInput(
                 inputId = 'mult_host',
                 label='Select by number of listings',
                 choices=c('All',
                           'Hosts with one listing',
                           'Hosts with multiple listings'),
                 multiple=FALSE,
                 selectize = TRUE
                 ),

               #input 1.2.4, new host or not
               checkboxInput(
                 inputId = 'new_host',
                 label = 'Show new hosts only'
               )
        ),
        # output 1.2
        column(10,plotOutput("bar_host_month"))
      )),
      
      # row 3
      wellPanel(
        fluidRow(
          column(2, tags$h3('Prices'),
                 # input 1.3.1, select suburb
                 selectInput(
                   inputId = 'price_sub',
                   label = 'Select a city/area',
                   choices=c('All', 'Melbourne'='Melbourne',df_data$NEIGHBOURHOOD %>% unique() %>% sort()),
                   selectize = TRUE),
                 
                 #input 1.3.2, price by property type
                 selectInput(
                   inputId = 'price_ptype',
                   label='Select a property type',
                   choices=c('All',
                             'Apartment'='apartment',
                             'House' = 'house',
                             'Hotel' = 'serviced',
                             'Other' = 'other'),
                   multiple=FALSE,
                   selectize = TRUE
                 ),
                 # input 1.3.3, price by room type
                 selectInput(
                   inputId = 'price_rtype',
                   label='Select a room type',
                   choices=c('All',
                             'Entire apt/house'='entire home/apt',
                             'Private room' = 'private room',
                             'Shared room' = 'shared room'),
                   multiple=FALSE,
                   selectize = TRUE
                 ),
                 # todo: look into why avg price didn't change 
                 # between LUX and normal
                 # # input 1.3.4, price by listing price
                 # selectInput(
                 #   inputId = 'price_type',
                 #   label='Select a listing type',
                 #   choices=c('All',
                 #             'LUX listing',
                 #             'Normal listing'),
                 #   multiple=FALSE,
                 #   selectize = TRUE
                 # )
                 ),
          column(10, plotOutput('line_price_month'))
        )
      ),
      
      # row 4 
      wellPanel(
        fluidRow(
          column(2, tags$h3('Supply'),
                 # input 1.4.1, select suburb
                 selectInput(
                   inputId = 'sup_sub',
                   label = 'Select a city/area',
                   choices=c('All', 'Melbourne'='Melbourne',df_data$NEIGHBOURHOOD %>% unique() %>% sort()),
                   selectize = TRUE),
                 
                 #input 1.4.2, supply by property type
                 selectInput(
                   inputId = 'sup_ptype',
                   label='Select a property type',
                   choices=c('All',
                             'Apartment'='apartment',
                             'House' = 'house',
                             'Hotel' = 'serviced',
                             'Other' = 'other'),
                   multiple=FALSE,
                   selectize = TRUE
                 ),
                 # input 1.4.3, supply by room type
                 selectInput(
                   inputId = 'sup_rtype',
                   label='Select a room type',
                   choices=c('All',
                             'Entire apt/house'='entire home/apt',
                             'Private room' = 'private room',
                             'Shared room' = 'shared room'),
                   multiple=FALSE,
                   selectize = TRUE
                 ),
                 
                 #input 1.4.4, select new list or not
                 checkboxInput(
                   inputId = 'sup_new',
                   label = 'Show new listings only'
                 ),
                 
                 #input 1.4.5, select Luxe list or not
                 checkboxInput(
                   inputId = 'sup_lux',
                   label = 'Show Luxe properties* only')
 

          ),
          column(10, plotOutput('bar_sup_month'))
        )
                     
     )
    ),
      # panel 2
      tabPanel(title = 'By Suburb',
               tags$h3('Coming soon ..'))
          )
)