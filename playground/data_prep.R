# This file saves data prerprocessing steps

# load library
library(readr)

# read data
df_data <- read.csv('data/listing20_01to06.csv')

# create column PRICE_PER_ROOM
df_data$BEDROOMS[is.na(df_data$BEDROOMS)] <- 1
df_data$BEDROOMS[df_data$BEDROOMS==0] <- 1  #todo: check if 0 makes sense
df_data$PRICE_PER_ROOM <- df_data$TXN_PRICE/df_data$BEDROOMS


# save data
write_csv(df_data,'app/app_data/listing20_01to06.csv')
