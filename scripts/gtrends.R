####################################################################
##DOWNLOADING GOOGLE TRENDS R ----------
##Author: Janaina Serrano, McGill University. janaina.serrano@mail.mcgill.ca
####################################################################

#all birds
all_birds <- read.csv("data_global/all_birds/all_birds_amniote_gl.csv")
sp_list1_49 <- all_birds$English.name[c(1:49)]

library("gtrendsR")
#testing
Common_Babbler <- gtrends(keyword = "Common Babbler", time = "all")
names(Common_Babbler)
write.csv(Common_Babbler$interest_over_time, "gtrends/Common_Babbler.csv")
#onlyInterest
res <- gtrends("nhl", geo = c("CA", "US"))
plot(res)


##### loading data -------------------------------------------------

# Install and load the readr gtrendsR & purrr packages
library(readr)
library(gtrendsR)
library(purrr)
library(dplyr)
library(readxl)

googleTrendsData <- function (keywords) {
  country <- ("")
  time <- ("all")
  channel <- 'web'
  
  trends <- gtrends(keywords,
                    gprop = channel,
                    geo = country,
                    time = time)
  
  results <- trends$interest_over_time
}
output <- data.frame()
for (i in c(1:length(sp_list1_49))) {
  try({
    output_new = map_dfr(.x = sp_list1_49[i],
                         .f = googleTrendsData) %>%
      data.frame()
    output <- rbind(output, output_new)
  })
  # export dataframe as csv file in the working directory
  write.csv(output, 'all_all1_49.csv')
}


#### preparing data --------------------
a <- read.csv("gtrends/all_birds_gtrends/all_all_1_3000.csv")
b <- read.csv("gtrends/all_birds_gtrends/all_all_3001_4784.csv")
c <- read.csv("gtrends/all_birds_gtrends/all_all_4875_5591.csv")
d <- read.csv("gtrends/all_birds_gtrends/all_all_5593_6928.csv")
e <- read.csv("gtrends/all_birds_gtrends/all_all_6929_8932.csv")
f <- read.csv("gtrends/all_birds_gtrends/all_all_8933_10534.csv")
g <- read.csv("gtrends/all_birds_gtrends/all_all_10535_10855.csv")
h <- read.csv("gtrends/all_birds_gtrends/raw/all_all1_49.csv")

ab <-rbind(a, b)
cat("Combined by rows: ", "\n")
abc <-rbind(ab, c)
abcd <- rbind(abc, d)
abcde <- rbind(abcd, e)
abcdef <- rbind(abcde, f)
abcdefg <- rbind(abcdef, g)

write.csv(abcdefg, "gtrends/all_birds_gtrends/birds_trends_raw.csv")

trends_raw <- read.csv("gtrends/all_birds_gtrends/birds_trends_raw.csv")


install.packages("janitor")
library("janitor")
hits_table <- janitor::tabyl(trends_raw, keyword, hits)
nrow(hits_table)

# https://www.casualinferences.com/posts/how-to-plot-google-trends-in-r/
library(ggplot2)
# search2<-gtrends(keyword = c("extreme skiing","swimming"), time= "today 12-m", geo = "CA")
time_trend <- e15k %>%
  dplyr::mutate(s=ifelse(hits=="<1",0.5,as.numeric(hits)),
                date=as.Date(date))

ggplot(e15k, aes(x=date, y=hits, colour=keyword)) +
  geom_line()

plot<-ggplot(data=sep_dates1, aes(x=year, y=as.numeric(hits), colour=keyword)) +
  geom_smooth(method="loess",span=0.4, se=FALSE) +
  geom_vline(xintercept = as.numeric(as.Date("2020-03-17"))) +
  theme_bw() +
  scale_y_continuous(breaks = NULL) 
plot
e15k <- trends_raw[1:15000,]

plot(e15k$keyword ~ e15k$hits)

sum_e15k <- e15k %>%
  group_by(keyword) %>%
  mutate(totalhits = sum(hits))

sumtrends_raw <- trends_raw %>%
  group_by(keyword) %>%
  mutate(totalhits = sum(hits))

sumtrends_raw <- trends_raw %>%
  group_by(keyword) %>%
  mutate(totalhits = sum(hits))


total_hits_table <- janitor::tabyl(sum_e15k, keyword, totalhits)

total_hits_only <- sum_e15k %>%
  select(keyword, totalhits)

library(tidyr)

total_hits_only <- total_hits_table %>% 
  pivot_longer(names_from = keyword, values_from = totalhits)

total_hits_only <- total_hits_table %>% 
  pivot_longer(
    cols = starts_with("1005"), 
    names_to = "keyword", 
    values_to = "total_hits",
    values_drop_na = TRUE
  )

barplot(sum_e15k$totalhits ~ sum_e15k$keyword)

my_bar <- barplot(data$average , border=F , names.arg=data$name , 
                  las=2 , 
                  col=c(rgb(0.3,0.1,0.4,0.6) , rgb(0.3,0.5,0.4,0.6) , rgb(0.3,0.9,0.4,0.6) ,  rgb(0.3,0.9,0.4,0.6)) , 
                  ylim=c(0,13) , 
                  main="" )

library(lubridate)
library(tidyverse)
library(dplyr)


which(is.na(trends_raw$date))

sep_dates = trends_raw %>% 
  mutate(date = mdy(date)) %>% 
  mutate_at(vars(date), funs(month, day, year))

sep_dates1 <- subset(sep_dates, select = -c(X.1) )

sep_dates2 <- sep_dates1 %>% 
  group_by(keyword) %>%
  summarise(count_hits = n(hits))

library(janitor)
e15k_transposed<- tabyl(e15k, keyword, hits)

sep_dates1 %>% 
  dplyr::group_by(keyword) %>% 
  dplyr::summarize(nhits = count(hits),     
                   no = n())
str(sep_dates1)

sep_dates1$hits <- as.numeric(sep_dates1$hits)

write.csv(sep_dates1, "gtrends/trends_raw_dates1.csv")
sep_dates1 <- read.csv("gtrends/trends_raw_dates1.csv")
sep_dates1 <- subset(sep_dates1, select = -c(X.1) )

sum_n <- sep_dates1 %>%
  group_by(keyword, year) %>%
  summarise(count = sum(hits))   

write.csv(sum_n, "gtrends/sum_hits_year.csv")


mean_n <- sep_dates1 %>%
  group_by(keyword) %>%
  summarise(count = mean(hits))   

write.csv(mean_n, "gtrends/mean_hits.csv")


sum_n15 <- sum_n[1:15000,]


plot_smooth_40<-ggplot(sum_n15, aes(x=year, y=count, colour=keyword)) +
  geom_smooth(method="loess",span=0.4, se=FALSE) +
  labs(title="smoothing factor = 0.4")+ 
  theme(legend.position = "none")
plot_smooth_40

#many NAs - check dates again. example: Woodlark didn't have NAs in raw data

trends_raw_dmy <- trends_raw %>%
  mutate(date_onset = lubridate::dmy(date))

Sales1 <- trends_raw %>%
  mutate(orderdate = dmy(`date`))


#there are some dates with different formatting - / / vs --

birds_trends_raw_new <- read.csv("gtrends/all_birds_gtrends/birds_trends_raw_new.csv")
trends_raw_dmy <- birds_trends_raw_new %>%
  mutate(date_onset = lubridate::dmy(date))

trends_raw_dmy <- subset(trends_raw_dmy, select = -c(X.1) )

trends_raw_dmy1 <- trends_raw_dmy %>%
mutate_at(vars(date_onset), funs(month, day, year))

trends_raw_dmy1 <- subset(trends_raw_dmy1, select = -c(date) )
write.csv(trends_raw_dmy1, "gtrends/all_birds_gtrends/birds_trends_dmy.csv")

h_dmy1 <- h_dmy1 %>%
mutate_at(vars(date_onset), funs(month, day, year))

h_dmy1 <- h_dmy %>%
  mutate(date_onset = date)

h_dmy1 <- subset(h_dmy1, select = -c(date) )
colSums(is.na(h_dmy))


N <- 1768

all_world <- trends_raw_dmy1[-(1:N), , drop = FALSE]

#join h_dmy1 with all_world
# all_world_ok <-rbind(h_dmy1, all_world) 
# cat("Combined by rows: ", "\n")
h_dmy1$date_onset <- as.Date(h_dmy1$date_onset)

binded_df = bind_rows(h_dmy1,all_world)
binded_df

binded_df$hits <- as.numeric(binded_df$hits)

colSums(is.na(binded_df))

binded_df <- na.omit(binded_df)
write.csv(binded_df, "gtrends/all_birds_gtrends/birds_trends_world.csv")
#drop Mao

subset_mao <- subset(binded_df, binded_df$keyword != "Mao") 
write.csv(subset_mao, "gtrends/all_birds_gtrends/subset_mao.csv")

sum_n <- binded_df %>%
  group_by(keyword, year) %>%
  summarise(count = sum(hits))   
colSums(is.na(sum_n))

write.csv(sum_n, "gtrends/all_birds_gtrends/sum_hits_year.csv")

mean_n <- binded_df %>%
  group_by(keyword) %>%
  summarise(count = mean(hits))   
colSums(is.na(mean_n))

write.csv(mean_n, "gtrends/all_birds_gtrends/mean_hits.csv")
mean_hits <- read.csv("gtrends/all_birds_gtrends/mean_hits.csv")

ggplot(mean_n, aes(x = keyword, y = count)) + geom_point()

ggplot(sum_n, aes(x = keyword, y = count)) + geom_point()


#drop Mao?

mean_n <- subset_mao %>%
  group_by(keyword) %>%
  summarise(count = mean(hits))   


sum_n <- subset_mao %>%
  group_by(keyword, year) %>%
  summarise(count = sum(hits))   

ggplot(mean_n, aes(x = keyword, y = count)) + geom_point()

######## binding hits to all_birds-----------

read.csv("gtrends/all_birds_gtrends/birds_trends_world.csv")

mean_hits <- read.csv("gtrends/all_birds_gtrends/mean_hits.csv")

all_birds <- read.csv("data_global/data_analysis/only_threatened/analysis/data_genl_clutch.csv")

mean_hits_english <- mean_hits %>%
  dplyr::rename(English.name = keyword) 
head(mean_hits_english)


all_birds_hits_test <- all_birds %>%
  inner_join(., mean_hits_english, by = "English.name")

all_birds_hits_test <- subset(all_birds_hits_test, select = -c(X) )

all_birds_hits_test <- all_birds_hits_test %>%
  dplyr::rename(hits = count)

write.csv(all_birds_hits_test, "gtrends/all_birds_gtrends/all_birds_hits.csv")
#######rename to threatened_birds_hits
all_birds_hits <- read.csv("gtrends/all_birds_gtrends/all_birds_hits.csv")


model_hits <- glm(threatened ~ masscentr + rangecentr + Migration + agriculture + climate_change + invasive + resource_use + hits, data=all_birds_hits_test, family=binomial(logit), na.action = na.omit)

summary(model_hits)

hist(all_birds_hits$hits)

#just 838 birds with threats and hits

#USING ALL BIRDS
all_birds <- read.csv("data_global/all_birds/all_birds_amniote_gl.csv")
#NEED TO GROUP THREATENED (VU, EN, CR) VS NON-THREATENED (LC, NT)
all_birds$result.category<-c(DD=0,LC=1,NT=2,VU=3,EN=4,CR=5,EW=6,EX=7)[all_birds$result.category]
all_birds$category.n
nrow(all_birds) #10858

all_birds <- all_birds %>%
  dplyr::rename(category.n = result.category)

dd <- filter(all_birds, category.n==0) #data deficient species
dd
write.csv(dd, file = "data_global/dd_sp.csv")
ex <- filter(all_birds, category.n==6) #extinct occurrences (multiple lines same species)
write.csv(ex, file = "data_global/ex_sp.csv")
ew <- filter(all_birds, category.n==7)
write.csv(ew, file = "data_global/ew_sp.csv")
no_dd <- filter(all_birds, category.n != 0) #
nrow(no_dd)
no_dd_ex_ew <- filter(no_dd, !category.n %in% c(6, 7))
no_dd_ex_ew$category.n
#data-deficient, threatened vs non-threatened (DD=0,LC=1,NT=2,VU=3,EN=4,CR=5)
# dd_others
# dd_others <- filter(all_birds, !category.n %in% c(6, 7))
# species_cat <- janitor::tabyl(dd_others, species, category.n)
# species_cat
no_dd_ex_ew <- no_dd_ex_ew %>% 
  mutate(threatened = case_when(category.n <= 2 ~ "non-threatened", 
                                category.n >= 3 ~ "threatened")) 
no_dd_ex_ew$threatened<-c("non-threatened" = 0,"threatened" = 1)[no_dd_ex_ew$threatened]
write.csv(no_dd_ex_ew, "data_global/data_analysis/only_threatened/analysis/all_birds_01.csv")

##adding hits to all birds with threatened (01)

no_dd_ex_ew_hits <- no_dd_ex_ew %>%
  inner_join(., mean_hits_english, by = "English.name")

no_dd_ex_ew_hits <- subset(no_dd_ex_ew_hits, select = -c(X) )

no_dd_ex_ew_hits <- no_dd_ex_ew_hits %>%
  dplyr::rename(hits = count)

write.csv(no_dd_ex_ew_hits, "gtrends/all_birds_gtrends/all01_birds_hits.csv")
all01_birds_hits <- read.csv("gtrends/all_birds_gtrends/all01_birds_hits.csv")


model_hits <- glm(threatened ~ masscentr + rangecentr + Migration + agriculture + climate_change + invasive + resource_use + hits, data=all01_birds_hits, family=binomial(logit), na.action = na.omit)

summary(model_hits) #2129 birds that may or may not have threats associated with them

hist(all01_birds_hits$hits)
plot(all01_birds_hits$hits, all01_birds_hits$threatened)
model_hits <- glm(threatened ~ hits, data=all01_birds_hits, family=binomial(logit), na.action = na.omit)

summary(model_hits)

##############download spanish and french-----------------
all_birds <- read.csv("data_global/all_birds/all_birds_amniote_gl.csv")
#french <- read.csv("gtrends/french.csv")
#spanish <- read.csv("gtrends/spanish.csv")
#french9347_9676 <- french$French.name[c(9347:9676)]
english9859_10858 <- all_birds$English.name[c(9859:10858)]
# Install and load the readr gtrendsR & purrr packages
library(readr)
library("gtrendsR")
library(purrr)
library(dplyr)
library(readxl)

googleTrendsData <- function (keywords) {
  country <- ("")
  time <- ("all")
  channel <- 'web'
  
  trends <- gtrends(keywords,
                    gprop = channel,
                    geo = country,
                    time = time)
  
  results <- trends$interest_over_time
}
output <- data.frame()
for (i in c(1:length(english9859_10858))) {
  try({
    output_new = map_dfr(.x = english9859_10858[i],
                         .f = googleTrendsData) %>%
      data.frame()
    output <- rbind(output, output_new)
  })
  # export dataframe as csv file in the working directory
  write.csv(output, 'gtrends/english/english9859_10858.csv')
}

########preparing data of relative numbers of hits
library(lubridate)
library(tidyverse)
library(dplyr)

# 1 - English
eng_1_1000 <- read.csv("gtrends/english/english1_1000.csv")

#how to do relative number? try 1: sum of number of hits by species divided by number of species * number of months?
eng_1_1000_ymd <- eng_1_1000 %>%
  mutate(date_onset = lubridate::ymd(date))

eng_1_1000_ymd <- eng_1_1000_ymd %>%
  mutate_at(vars(date_onset), funs(month, day, year))
as.numeric(eng_1_1000_ymd$hits)

sum_n_keyword <- eng_1_1000_ymd %>%
  group_by(keyword) %>%
  summarise(count = sum(as.numeric(hits)))   

colSums(is.na(sum_n))

mean_n <- eng_1_1000_ymd %>%
  group_by(keyword) %>%
  summarise(count = mean(as.numeric(hits)))  

write.csv(sum_n_keyword, 'gtrends/english/1_1000_sum_n_keyword.csv')
 
write.csv(sum_n, 'gtrends/english/1_1000_sum_n.csv')
