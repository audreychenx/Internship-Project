library(tidyverse)
big_c <- readxl::read_xlsx("/Users/AudreyChen/Desktop/goodyear/OneDrive_2_2018-5-30/Bus/??????2017??????????????????????????????-???????????????.xlsx")

big_cc=big_c
dim(big_cc)[1]
head(big_cc[,34])

big_cc$total <- NA

head(big_cc[,34])  #?????????
head(big_cc[,32])  #?????????

class(big_cc[,34])  #?????????
head(big_cc[,32])  #?????????


View(big_cc[,33])

j = 1
while (j <= dim(big_cc)[1]){
  if (is.na(big_cc[j,33])==TRUE)
    big_cc[j,33] = "blank"
  j = j+1
}

spec <- unique(big_cc[,33])
spec$count <- rep(0,307)
dim(spec)
View(spec)

x <- big_cc[,33]
v <- as.data.frame(t(table(x)))
View(v)

pat1<- "^(10.00-20)"

dat <- big_cc[,33]
colnames(dat) = "model"

dat1 <- dat %>%
  select(model) %>% 
  filter(str_detect(model,"^(10.00-20)")) %>%
  mutate(model_repl = "10.00-20")

head(dat1)
dim(dat1)


dat2 <- dat %>%
  select(model) %>% 
  filter(str_detect(model,"^(10.00R20)")) %>%
  mutate(model_repl = "10.00R20")

head(dat2)
dim(dat2)


dat3 <- dat %>%
  select(model) %>% 
  filter(str_detect(model,"^(10R)|^(10.00R22.5)")) %>%
  mutate(model_repl = "10R22.5")

head(dat3)
dim(dat3)

dat4 <- dat %>%
  select(model) %>% 
  filter(str_detect(model,"^(11.00-20)|^(11.002)")) %>%
  mutate(model_repl = "11.00-20")

head(dat4)
dim(dat4)

dat5 <- dat %>%
  select(model) %>% 
  filter(str_detect(model,"^(11.00R22.5)|^(11R22.5)|^(11R22.2)|^(11R/22.5)|^(11R 22.5)|^(11 R22.5)")) %>%
  mutate(model_repl = "11R22.5")

head(dat5)
dim(dat5)

dat6 <- dat %>%
  select(model) %>% 
  filter(str_detect(model,"^(12.00R20)")) %>%
  mutate(model_repl = "12.00R20")


pattern <- readxl::read_xlsx("/Users/AudreyChen/Desktop/??????2017??????????????????????????????-???????????????.xlsx", sheet = 1)

head(pattern)
colnames(pattern)=c("model","count","model_repl")

new_dat <- dat %>%
  select(model) %>% 
  left_join(pattern, by = "model")

View(new_dat)
unique(new_dat$model_repl)

big_cc <- cbind(big_cc,new_dat$model_repl)
head(big_cc)
getwd()
write.csv(big_cc, file = "new_dat.csv")
