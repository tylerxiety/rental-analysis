library(tidyverse)
library(treemapify)
library(readr)

df_data <- read_csv('data/listing20_01to06.csv')
names(df_data)
summary(df_data)


# bar, estimated supply and booked
df_sd <- df_data %>% 
  group_by(YEAR_MONTH) %>% 
  summarise(n=n(),
            supply = sum(AVAILABILITY_30),
            demand = sum(BOOKED))

ggplot(df_sd, aes(x=YEAR_MONTH, y= supply))+
  geom_col(stat= "identity")+
  geom_col(aes(x=YEAR_MONTH,y=demand),stat= "identity", width = 0.1,col = "green",fill="green")

# treemap, neighb
df_n <- df_data %>% group_by(NEIGHBOURHOOD) %>%
  summarise(count=n())

ggplot(data=df_n)+
  geom_treemap(aes(area=count,fill=NEIGHBOURHOOD))

# bar room type
ggplot(data=df_data)+
  geom_bar(mapping = aes(x=ROOM_TYPE, y=stat(prop),group=1),fill = 'green')

# bar room type
ggplot(data = df_data) + 
  geom_bar(mapping = aes(x = ROOM_TYPE, fill = HOST_IS_SUPERHOST))


# ggplot(df, aes(longitude, latitude)) +
#   geom_polygon(fill = "white", colour = "black")




# bar and Coxcomb for host_response_time
bar_res <- ggplot(df_data) + 
  geom_bar(
    mapping = aes(x = HOST_RESPONSE_TIME, fill = HOST_RESPONSE_TIME), 
    show.legend = FALSE,
    width = 1
  ) + 
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)

bar_res + coord_flip()
bar_res + coord_polar()




# bar, average accept rate by HOST_RESPONSE_RATE
df_rt <- df_data %>%
  group_by(HOST_RESPONSE_TIME) %>%
  summarise(av_rate = mean(HOST_RESPONSE_RATE, na.rm = TRUE))
  
ggplot(df_rt)+
  geom_col(aes(x=HOST_RESPONSE_TIME,y=av_rate))
  

# avg price by accommodates
df_a <- filter(df_data,TXN_PRICE<1000) %>%
  group_by(ACCOMMODATES) %>%
  summarise(
    avp = mean(TXN_PRICE)
  )
ggplot(df_a,aes(x=ACCOMMODATES, y=avp))+
  geom_point()+
  geom_smooth(se = FALSE)+
  labs(
    title = 'Avg price generally increases with the number of people accommodates',
    subtitle = 'Some subtitles',
    caption = 'Data from Airbnb'
    )


# boxplot of price and accom
ggplot(filter(df_data,TXN_PRICE<1000),aes(x=factor(ACCOMMODATES), y=TXN_PRICE,group=ACCOMMODATES))+
  geom_boxplot(outlier.shape=NA)+
  labs(
    title = 'Price variaty generally increases with the listing capacity',
    subtitle = 'Some subtitles',
    caption = 'Data from Airbnb'
  )


# scatter plot,price and accom and room type

dfk2 <- df_data[1:2000,]

best_in_type <- dfk2 %>%
  group_by(ROOM_TYPE) %>%
  filter(row_number(desc(TXN_PRICE)) == 1)

ggplot(dfk2,aes(x=ACCOMMODATES, y=TXN_PRICE))+
  geom_point(aes(color=ROOM_TYPE))+
  geom_smooth(se = FALSE)+
  geom_text(aes(label=ID),data = best_in_type)
  labs(
    title = 'Price enerally increases with the listing capacity',
    subtitle = 'Some subtitles',
    caption = 'Data from Airbnb',
    x='Property capcaity',
    y='Price (A$)',
    color = 'Room type'
  )

  
  
# scatter plot,price and accom and room type

dfa <- filter(dfm,property_type == 'Apartment',price<1000)[1:1000,]

ap_best_in_type <- dfa %>%
  group_by(cancellation_policy) %>%
  filter(row_number(desc(price)) == 1)

ggplot(dfa,aes(x=number_of_reviews, y=price))+
  geom_point(aes(color=cancellation_policy, shape=cancellation_policy))+
  geom_smooth(se = FALSE)+
  #geom_text(aes(label=accommodates),data = best_in_type)+
  ggrepel::geom_label_repel(aes(label = cancellation_policy), 
                            data = ap_best_in_type,
                            size = 4,
                            label.size = 0,
                            segment.color = NA
                            )+
  labs(
    title = 'Price generally decrease with the number of reviews',
    subtitle = 'Some subtitles',
    caption = 'Data from Airbnb',
    x='Number of reviews',
    y='Price (A$)'
  )+
  theme(legend.position = "bottom")+
  scale_y_continuous(breaks = seq(100,800,by=100))+
  guides(colour = guide_legend(nrow = 2, override.aes = list(size = 4)))


entire <- dfm %>% filter(room_type=='Entire home/apt',price<1000)
private <- dfm %>% filter(room_type=='Private room',price<1000)
shared <- dfm %>% filter(room_type=='Shared room',price<1000)

ggplot(entire,aes(x=number_of_reviews, y=price))+
  geom_point(aes(color=cancellation_policy, shape=cancellation_policy))

ggplot(private,aes(x=number_of_reviews, y=price))+
  geom_point(aes(color=cancellation_policy, shape=cancellation_policy))

ggplot(shared,aes(x=number_of_reviews, y=price))+
  geom_point(aes(color=cancellation_policy, shape=cancellation_policy))


# line, listing by month
ggplot(df_data%>%count(YEAR_MONTH), aes(x=YEAR_MONTH,n,group = 1))+
  geom_line()+
  geom_point()+
  labs(
    title = 'Number of Listings by Month',
    subtitle = 'The number of listings was decreasing since this year',
    x='Month',
    y='Number of listings'
    
  )
  

ggplot(df_data %>%
         filter(HOST_MONTHS==0) %>%
         count(YEAR_MONTH), aes(x=YEAR_MONTH,n,group = 1))+
  geom_line()+
  geom_point()


ggplot(df_data %>%
         group_by(YEAR_MONTH) %>%
         summarise(uni_host = n_distinct(HOST_ID)), 
       aes(x=YEAR_MONTH,y=uni_host,group = 1))+
  geom_col(width = 0.6)
  # geom_line()+
  # geom_point()

df_data %>%
  group_by(YEAR_MONTH) %>%
  filter(LUXURY_FLAG==1) %>%
  summarise(uni_host = n_distinct(ID))


# line, price per room by month

df_data$BEDROOMS[is.na(df_data$BEDROOMS)] <- 1
df_data$BEDROOMS[df_data$BEDROOMS==0] <- 1
df_data$PRICE_PER_ROOM <- df_data$TXN_PRICE/df_data$BEDROOMS

df_p <- df_data %>%
  group_by(YEAR_MONTH) %>%
  summarise(avg_price_per_room =round(mean(PRICE_PER_ROOM,na.rm = TRUE)))
ggplot(df_p, aes(x=YEAR_MONTH,y=avg_price_per_room,group=1))+
  geom_line()+
  geom_label(aes(label=avg_price_per_room))+
  labs(
    title = 'Number of Listings by Month',
    subtitle = 'The number of listings was decreasing since this year',
    x='Month',
    y='Number of listings'
  )+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12,face="bold"),
        title=element_text(size=14))




