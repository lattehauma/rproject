library(nycflights13)
library(ggplot2)
library(dplyr)
library(lubridate)
df<-flights
df2<-flights
head(df)
str(df)

t1<-flights %>%
  group_by(month) %>%
  summarize(cancelled=sum(is.na(dep_time)),
            total = n(),
            proportion = cancelled/total)

ggplot(t1, aes(x=month, y=proportion))+
  geom_line()

###Q2

flights %>% count(tailnum, sort=TRUE)
t2<-flights %>%
  filter(tailnum =='N725MQ') %>%
  mutate(date = ymd(sprintf('%04d%02d%02d', year, month, day))) %>%
  mutate(week = week(date)) %>%
  group_by(week) %>%
  summarize(f_num=n())
  
ggplot(t2, aes(x=week,y=f_num))+
  geom_line()
