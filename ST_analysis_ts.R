library(tidyverse)
library(plotly)
library(ggplot2)
library(DT) 
library(stringr)

dat <- readxl::read_xlsx("/Users/AudreyChen/Desktop/goodyear/15-18\ \ Retailer\ ST\ Database\ by\ SKU\ 180607\ -colin.xlsx",sheet = 2)

dat <- dat %>%
  select(Year,month, `Distributor SAP`,`Retail No`,`Description(SKU)`,`Final S/T`,Pattern)

colnames(dat)=c("year","month","dist","retail","description","ST","Pattern")

dat <- dat %>%filter(Pattern!="R176")
dat <- dat %>%filter(Pattern!="R166")

head(dat)

new_desc <- rep(NA,length(dat$description))
dat<-cbind(dat,new_desc)

dat1 <- dat %>% 
  select(year,month, dist,retail,description,ST,Pattern) %>%
  filter(str_detect(description,"^(9R)")) 
dat1 <- dat1 %>%
  mutate(new_des= str_sub(dat1$description,1,6))

dat2 <- dat %>% 
  select(year,month, dist,retail,description,ST,Pattern) %>%
  filter(str_detect(description,"^(\\d{2}R)")) 
dat2 <- dat2 %>%
  mutate(new_des= str_sub(dat2$description,1,7))

dat3 <- dat %>% 
  select(year,month, dist,retail,description,ST,Pattern) %>%
  filter(str_detect(description,"^(\\d{3}\\/)")) 
dat3 <- dat3 %>%
  mutate(new_des= str_sub(dat3$description,1,11))

dat4 <- dat %>% 
  select(year,month, dist,retail,description,ST,Pattern) %>%
  filter(str_detect(description,"^(\\d{2}.00)")) 
dat4 <- dat4 %>%
  mutate(new_des= str_sub(dat4$description,1,8))

dat_final <- rbind(dat1,dat2,dat3,dat4)
dim(dat_final) == dim(dat)

dat_final$series[dat_final$Pattern=='S100'] = "flat"
dat_final$series[dat_final$Pattern=='S100HFE'] = "flat"
dat_final$series[dat_final$Pattern=='S700'] = "mix"
dat_final$series[dat_final$Pattern=="OMNITRACMSSII"] = "mix"
dat_final$series[dat_final$Pattern=="S800"] = "city"
dat_final$series[str_detect(dat_final$Pattern,"URBAN")==TRUE] = "city"
dat_final$series[is.na(dat_final$series)==TRUE] = "hwy"

dat_flat <- dat_final %>% filter(series=="flat")
dat_hwy <- dat_final %>% filter(series=="hwy") %>% filter(Pattern!="MARATHONLHSLR8")

dat_hwy_sub <- dat_hwy %>%
  select(year,month,ST,new_des)%>%
  group_by(year,month,new_des) %>%
  summarise(total = sum(ST))
#datatable(dat_hwy_sub, caption = 'Table: Total ST for Series of Hwy')

#################################################################################
ts_dat_11 <- dat_hwy %>%
  filter(new_des==c("11R22.5")) %>%
  group_by(year,month,new_des) %>%
  summarise(total = sum(ST))

ts_dat_11_copy <- ts_dat_11
dim(ts_dat_11_copy)
ts_dat_11[1:24,]<-ts_dat_11_copy[1:24,]
ts_dat_11[26:41,]<-ts_dat_11_copy[25:40,]
colnames(ts_dat_11)<-c("year", "month", "new_des", "total")
ts_dat_11[25,1]<-2017
ts_dat_11[25,2]<-1
ts_dat_11[25,3]<-"11R22.5"
ts_dat_11$total <- na.interp(ts_dat_11$total)
ts_dat_11$time <- rep(NA,length(ts_dat_11$month))
ts_dat_11$time <- seq(as.Date("2015/1/1"), as.Date("2018/5/1"),by="month")

ts_dat_12 <- dat_hwy %>%
  filter(new_des==c("12R22.5")) %>%
  group_by(year,month,new_des) %>%
  summarise(total = sum(ST))
ts_dat_12$time <- rep(NA,length(ts_dat_12$month))
ts_dat_12$time <- seq(as.Date("2015/1/1"), as.Date("2018/5/1"),by="month")
ts_dat_12[37,]
ts_dat_295 <- dat_hwy %>%
  filter(new_des==c("295/80R22.5")) %>%
  group_by(year,month,new_des) %>%
  summarise(total = sum(ST))
ts_dat_295$time <- rep(NA,length(ts_dat_295$month))
ts_dat_295$time <- seq(as.Date("2015/1/1"), as.Date("2018/5/1"),by="month")

ts_dat_425 <- dat_hwy %>%
  filter(new_des==c("425/65R22.5")) %>%
  group_by(year,month,new_des) %>%
  summarise(total = sum(ST))
ts_dat_425$time <- rep(NA,length(ts_dat_425$month))
ts_dat_425$time <- seq(as.Date("2015/1/1"), as.Date("2018/5/1"),by="month")
ts_dat_11<-data.frame(ts_dat_11)
ts_dat_295<-data.frame(ts_dat_295)
ts_dat_425<-data.frame(ts_dat_425)

ts_dat_295$total <- as.numeric(ts_dat_295$total)
ts_dat_425$total <- as.numeric(ts_dat_425$total)
ts_dat_11$total <- as.numeric(ts_dat_11$total)
ts_dat <- rbind(ts_dat_11[,3:5],ts_dat_295[,3:5],ts_dat_425[,3:5])
myData<- data.frame(Time=ts_dat$time,Total_ST=ts_dat$total,Model=ts_dat$new_des)

ggplot(data=myData,mapping = aes(x=Time,y=Total_ST,shape=Model,color=Model)) + 
  geom_point() +
  geom_line() + 
  facet_grid(facets = Model~.)

ggplot(data=ts_dat_12,mapping = aes(x=time,y=total,color="purple")) + 
  geom_point() + geom_line() + ggtitle("Total Sell-through of 12R22.5")

ts_flat_12 <- dat_flat %>%
  filter(new_des==c("12R22.5")) %>%
  group_by(year,month,new_des) %>%
  summarise(total = sum(ST))
datatable(ts_flat_12)
ts_flat_12$total
datatable(ts_flat_12)

tot <- c(15,25,197,106,248,59,60,834,228,30,406,48,10,414,267,30,49,59,40,15,366,154,68,
         271,100,30,100,173,160,50,94,88,1012,16,NA,-66,33,9,83)

tot <- na.interp(tot)

tot <- c(15,25,197,106,248,59,60,834,228,30,406,48,10,414,267,30,49,59,40,15,366,154,68,
         271,100,30,100,173,160,50,94,88,1012,16)
length(tot)
time <- seq(as.Date("2015/1/1"), as.Date("2018/3/1"),by="month")
ts_flat_12_new<- data.frame(time,total=tot)
head(ts_flat_12_new)
ggplot(data=ts_flat_12_new,mapping = aes(x=Time,y=Total_ST,color="purple")) + 
  geom_point() + geom_line() + ggtitle("Total Sell-through of 12R22.5")

# datatable(ts_flat_11)
# ts_flat_11$time <- rep(NA,length(ts_dat_11$month))
# ts_dat_11$time <- seq(as.Date("2015/1/1"), as.Date("2018/5/1"),by="month")

ts_flat_295 <- dat_flat %>%
  filter(new_des==c("295/80R22.5")) %>%
  group_by(year,month,new_des) %>%
  summarise(total = sum(ST))
View(ts_flat_295)
ts_flat_295$time <- rep(NA,length(ts_flat_295$month))
ts_flat_295$time <- seq(as.Date("2015/1/1"), as.Date("2018/5/1"),by="month")

ggplot(data=ts_flat_295,mapping = aes(x=time,y=total,color="purple")) + 
  geom_point() + geom_line() + ggtitle("Total Sell-through of 295/80R22.5")

##########################################
#?????????
dat_d <- dat %>% 
  select(year,month,ST,Pattern) %>%
  filter(str_detect(Pattern,"^D")) %>%
  group_by(year,month) %>%
  summarise(total = sum(ST))

dat_d$time <- rep(NA,41)
dat_d$time <- seq(as.Date("2015/1/1"), as.Date("2018/5/1"),by="month")

ggplot(data=dat_d,mapping = aes(x=time,y=total,color="purple")) + 
  geom_point() + geom_line() + ggtitle("Total Sell-through of Driving Wheels")

colnames(dat)=c("year","month","dist","retail","description","ST","Pattern")

dat_d2 <- dat %>% 
  select(year,month,dist,retail,ST,Pattern) %>%
  filter(str_detect(Pattern,"^D")) %>%
  group_by(year,month) %>%
  summarize(num_dist = n()) 

dat_d2$time <- rep(NA,41)
dat_d2$time <- seq(as.Date("2015/1/1"), as.Date("2018/5/1"),by="month")


plot(dat_d2$num_dist) 
# ????????????????????????
plot(dat_d$total/dat_d2$num_dist)

dat_d$avg <- rep(NA,41)
dat_d$avg <- dat_d$total/dat_d2$num_dist

dat_15 <- dat %>% filter(year==2015)
dat_16 <- dat %>% filter(year==2016)
dat_17 <- dat %>% filter(year==2017)
dat_18 <- dat %>% filter(year==2018)

length(unique(dat_18$dist))
length(unique(dat_18$retail))

par(mfrow = c(1, 1))

plot1<- dat_15 %>% group_by(month) %>% summarise(total = sum(ST))
plot2<- dat_16 %>% group_by(month) %>% summarise(total = sum(ST))
plot3<- dat_17 %>% group_by(month) %>% summarise(total = sum(ST))
plot4<- dat_18 %>% group_by(month) %>% summarise(total = sum(ST))

plot(plot1$month,plot1$total,type="l",xlab = "Month",ylab = "Total S/T",main = "2015",col="red")
plot(plot2$month,plot2$total,type="l",xlab = "Month",ylab = "Total S/T",main = "2016",col="red")
plot(plot3$month,plot3$total,type="l",xlab = "Month",ylab = "Total S/T",main = "2017",col="red")
plot(plot4$month,plot4$total,type="l",xlab = "Month",ylab = "Total S/T",main = "2018",col="red")


dat_d2_15 <- dat_15 %>% 
  select(year,month,dist,retail,ST,Pattern) %>%
  filter(str_detect(Pattern,"^D")) %>%
  group_by(year,month) %>%
  summarise(num_dist = n())

dat_d2_16 <- dat_16 %>% 
  select(year,month,dist,retail,ST,Pattern) %>%
  filter(str_detect(Pattern,"^D")) %>%
  group_by(year,month) %>%
  summarise(num_dist = n())

dat_d2_17 <- dat_17 %>% 
  select(year,month,dist,retail,ST,Pattern) %>%
  filter(str_detect(Pattern,"^D")) %>%
  group_by(year,month) %>%
  summarise(num_dist = n())

dat_d2_18 <- dat_18 %>% 
  select(year,month,dist,retail,ST,Pattern) %>%
  filter(str_detect(Pattern,"^D")) %>%
  group_by(year,month) %>%
  summarise(num_dist = n())

dd <- dat %>% 
  select(year,month,dist,retail,ST,Pattern) %>%
  group_by(year,month) %>%
  summarise(total = sum(ST))

dat_d$year=as.factor(dat_d$year)
dat_d$month=as.factor(dat_d$month)
ggplot(data=dat_d,mapping = aes(x=month,y=total)) + 
  geom_point() +
  geom_line() 
