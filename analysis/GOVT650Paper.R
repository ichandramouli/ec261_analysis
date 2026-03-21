library(tidyverse)
library(here)
#setwd('C:/Users/ianch/Desktop/Rstuff/GOVT650')

EU261_data <- read_csv(here('data/EUSeats2003_2005.csv'))
canada_data <- read_csv(here('data/CanadaSeats_2017_2024.csv'))

#add a city pair column so we can group by it
EU261_data <- EU261_data |> 
  mutate(city_pair=str_c(usg_apt,fg_apt,sep=''))

canada_data <- canada_data |> 
  mutate(city_pair=str_c(usg_apt,fg_apt,sep=''))

#EU261 took effect on 2/17/2005
#also want to add a EU261 in effect column. Probably create a binary variable to do that.
#okay i think what we need to do here is:
EU261_data <- EU261_data |> 
  mutate(EU261_active=ifelse(((Year>2005)|(Year==2005 & Month>2)),'Yes','No'))

#lets just do the summary first to see what we're look
EU261_data |> 
  group_by(city_pair) |> 
  summarise(total_seats=sum(Total)) |> 
  arrange(desc(total_seats)) |> 
  head(20)

#it doesn't look like there is a lot of overlap here. 
#ggplot(data=EU261_top20_only,mapping=aes(x=EU261_active,y=Total)) + geom_boxplot()

#before EU261
EU261_before <- EU261_data |> 
  filter(EU261_active=='No')

EU_Carriers <- c('BA','AF','VS','FI','LH','IB','KL','EI','SK','AZ','OS','LO','LX',
                 'OA','LT','TP','MP','AY','BD','DE','OK','MA','BY','S4','ZB','RO',
                 'OR')  

EU261_before |> 
  filter(Year==2003 & carrier %in% EU_Carriers) |> 
  group_by(city_pair) |> 
  summarise(total_seats=sum(Total)) |> 
  arrange(desc(total_seats)) |> 
  head(20)


#after EU 261
EU261_after <- EU261_data |> 
  filter(EU261_active=='Yes')

EU261_after |> 
  group_by(city_pair) |> 
  summarise(total_seats=sum(Total)) |> 
  arrange(desc(total_seats)) |> 
  head(20)

before_top20_citypair <- c('JFKLHR', 'LAXLHR', 'MCOLGW', 'BOSLHR', 'JFKCDG','SFOLHR',
                           'EWRLHR', 'IADLHR', 'MIALHR', 'MIAMAD', 'JFKFRA', 'ORDLHR',
                           'LAXFRA', 'LAXCDG','ORDFRA', 'PHLLHR', 'IADFRA', 'IADCDG',
                           'JFKAMS', 'ATLCDG')

EU261_top20_only <- EU261_data |> 
  filter(city_pair %in% before_top20_citypair & carrier %in% EU_Carriers)

EU261_top20_only <- EU261_top20_only |> 
  mutate(date=as.Date(str_c(Year,Month,'01',sep="-")))

EU261_Top20_counts <- EU261_top20_only |> 
  select(date,Year,city_pair,EU261_active,Total, Month,carrier) |> 
  group_by(city_pair,date, Year, Month) |> 
  summarise(seat_cts=sum(Total),
            carrier_ct=n_distinct(carrier)) |> 
  ungroup()

EU261_small_top20_cnts <- EU261_Top20_counts |> 
  select(Year,Month,city_pair, seat_cts,date,carrier_ct) |> 
  mutate(cp_month=str_c(city_pair,Month,sep="_"))

EU261_smaller <- EU261_small_top20_cnts |> 
  select(city_pair,Month,cp_month,Year,seat_cts,date,carrier_ct) |> 
  group_by(cp_month) |> 
  mutate(prev_seatct=lag(seat_cts,n=1),
         perc_chg=(log(seat_cts)-log(prev_seatct))*100,
         EC261_active=ifelse((Year>2005)|(Year==2005 & Month>2),1,0))

EU_ols <- lm(perc_chg~EC261_active+carrier_ct,data=EU261_smaller)
summary(EU_ols)

ggplot(data=EU261_Top20_counts, mapping=aes(x=date,y=seat_cts,color=city_pair)) + geom_line() + labs(
  title='Seat counts on Top 20 US-Europe Markets (2003-2007)',
  x='Date',
  y='Monthly Aggregated Seat Counts',
  color='City Pair'
) + geom_vline(xintercept=as.Date('2005-02-17'))



ggplot(data=EU261_top20_only,mapping=aes(x=EU261_active,y=Total)) + geom_boxplot()

EU261_before |> 
  filter(carriergroup==0) |> 
  group_by(city_pair) |> 
  summarise(total_seats=sum(Total)) |> 
  arrange(desc(total_seats)) |> 
  head(20)

EU261_after |> 
  filter(carriergroup==0) |> 
  group_by(city_pair) |> 
  summarise(total_seats=sum(Total)) |> 
  arrange(desc(total_seats)) |> 
  head(20) 

EU261_tracking <- EU261_data |> 
  filter(city_pair %in% before_top20_citypair)
#ok we got this pivot to do what we want (we can compare 1 month from 1 year to the next. But now we need
#some way to get the EU261 yes or no variable into the mix. maybe lag the dummy variables. this is shaping
#up to be a time series situation)
test <- EU261_tracking |> 
  select(city_pair,Month,Total,carrier,Year) |> 
  pivot_wider(names_from=Year,values_from=Total) |> 
  rename(seats2003='2003',
         seats2004='2004',
         seats2005='2005',
         seats2006='2006',
         seats2007='2007')

#lets make a stacked bar chart to see the comparison here

#wac from 906 to 961 is canada



canada_data |> 
  filter(Year==2017) |> 
  group_by(city_pair) |> 
  summarise(total_seats=sum(Total)) |> 
  arrange(desc(total_seats)) |> 
  head(20)

canada_before_top20_citypair <- c('LGAYYZ', 'ORDYYZ', 'LAXYYZ', 'MCOYYZ', 'LGAYUL',
                                  'ATLYYZ', 'LASYYZ', 'FLLYYZ', 'BOSYYZ', 'SFOYYZ',
                                  'MIAYYZ', 'EWRYTZ', 'FLLYUL', 'IAHYYC', 'EWRYYZ',
                                  'ORDYUL', 'DFWYYZ', 'LASYYC', 'LAXYYC', 'DTWYYZ')

canada_top20_only <- canada_data |> 
  filter(city_pair %in% canada_before_top20_citypair) |> 
  mutate(date=as.Date(str_c(Year,Month,'01',sep="-")))
  
  

canada_Top20_counts <- canada_top20_only |> 
  select(date,city_pair,Total,Year,Month,carrier) |> 
  group_by(city_pair,date,Year,Month) |> 
  summarise(seat_cts=sum(Total),
            carrier_ct=n_distinct(carrier)) |> 
  ungroup()

canada_small_top20_cnts <- canada_Top20_counts |> 
  select(Year,Month,city_pair, seat_cts,date,carrier_ct) |> 
  mutate(cp_month=str_c(city_pair,Month,sep="_"))

canada_smaller <- canada_small_top20_cnts |> 
  select(city_pair,Month,cp_month,Year,seat_cts,date,carrier_ct) |> 
  group_by(cp_month) |> 
  mutate(prev_seatct=lag(seat_cts,n=1),
         perc_chg=(log(seat_cts)-log(prev_seatct))*100,
         APPR_active=ifelse((Year>2019)|(Year==2019 & Month>12),1,0))




ggplot(data=canada_Top20_counts, mapping=aes(x=date,y=seat_cts,color=city_pair)) + geom_line() + labs(
  title='Seat counts on Top 20 US-Canada Markets (2017-2024)',
  x='Date',
  y='Monthly Aggregated Seat Counts',
  color='City Pair'
) + geom_vline(xintercept=as.Date('2019-12-01'))

ggplot(data=canada_smaller,mapping=aes(x=date,y=perc_chg,color=city_pair)) + geom_line() +
  labs(title='APPR - YOY Percentage Change in Seats Over Time (2017-2024)',
       x='Date',
       y='Percentage Change',
       color='City Pair') +
  geom_vline(xintercept=as.Date('2019-12-01'))

ggplot(data=EU261_smaller,mapping=aes(x=date,y=perc_chg,color=city_pair)) + geom_line() +
  labs(title='EC261 - YOY Percentage Change in Seats over Time (2003-2007)',
       x='Date',
       y='Percentage Change',
       color='City Pair') +
  geom_vline(xintercept=as.Date('2005-02-18'))

canada_ols <- lm(perc_chg~APPR_active+carrier_ct,data=canada_smaller |> filter(Year<=2019 | Year>=2024))
summary(canada_ols)



ggplot(data=canada_smaller |> filter(Year<2020),mapping=aes(x=date,y=perc_chg,color=city_pair)) + geom_line() +
  labs(title='APPR - YOY Percentage Change in Seats Over Time (2017-2019)',
       x='Date',
       y='Percentage Change',
       color='City Pair') +
  geom_vline(xintercept=as.Date('2019-12-01'))

ggplot(data=canada_smaller |> filter(Year>2022),mapping=aes(x=date,y=perc_chg,color=city_pair)) + geom_line() +
  labs(title='APPR - YOY Percentage Change in Seats Over Time (2022-2024)',
       x='Date',
       y='Percentage Change',
       color='City Pair') +
  geom_vline(xintercept=as.Date('2019-12-01'))