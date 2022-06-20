##models and data visualization for birds and extinction risk
##Author: Janaina Serrano
##Date: 2022-03-02

##Let's visualize the data

##world data
#selected_traits <- read.csv("selected_traits.csv")
#traits <- read.csv("traits_birds.csv")

setwd("C:/Users/janas/OneDrive/Documentos/R")
library("dplyr")
library("lme4")
library("ggplot2")
library("tidyverse")

#https://stats.stackexchange.com/questions/82532/logit-link-glm-summary-interpretation
###try
test_sp <- read.csv(file = "C:/Users/janas/OneDrive/Documentos/R/Birds/IUCN_data/31-100sp.csv")
names(test_sp)
## or by using DPLYR::RENAME
test_sp <- test_sp %>%
  dplyr::rename(Scientific.name = sp)
names(test_sp)

traits_test <- test_sp %>%
  inner_join(., birds_ca, by = "Scientific.name")
#m <- glmer(y ~ trt * sex + (1|id), data = d, family = binomial)
#summary(m, corr = FALSE)

traits_test

##Corkal:
birds_ca <- read.csv(file = "C:/Users/janas/OneDrive/Documentos/R/Birds/IUCN_data/AC_traits_data.csv")
names(birds_ca)

birds_ca$Global.IUCN.Red.List.Category <- gsub("LC", 1, birds_ca$Global.IUCN.Red.List.Category) 
birds_ca$Global.IUCN.Red.List.Category <- gsub("NT", 2, birds_ca$Global.IUCN.Red.List.Category) 
birds_ca$Global.IUCN.Red.List.Category <- gsub("VU", 3, birds_ca$Global.IUCN.Red.List.Category) 
birds_ca$Global.IUCN.Red.List.Category <- gsub("EN", 4, birds_ca$Global.IUCN.Red.List.Category) 
birds_ca$Global.IUCN.Red.List.Category <- gsub("CR", 5, birds_ca$Global.IUCN.Red.List.Category) 


#ggplot(data = birds_ca, 
 #      mapping = aes(x = result.category, y = Range.Size, 
 #                    colour = result.category)) + 
  #geom_point(size = 2, position = position_jitter(w = 0.2, h = 0.2)) + 
  #facet_wrap(~ species, nrow = 2, scales = "free") + 
  #theme(legend.position = "none")
names(traits_test)
boxplot(Mean_Body_Mass_.g.~Global.IUCN.Red.List.Category,data=traits_test)
boxplot(Mean_Body_Mass_.g. ~ title, data = traits_test)


model1 <- lm(Mean_Body_Mass_.g. ~ Global.IUCN.Red.List.Category, data=traits_test)
summary(model1)

model2 <- lm(traits_test$Mean_Body_Mass_.g. ~ traits_test$title)
summary(model2)



traits_test$Global.IUCN.Red.List.Category <- gsub("LC", 1, traits_test$Global.IUCN.Red.List.Category) 
traits_test$Global.IUCN.Red.List.Category <- gsub("NT", 2, traits_test$Global.IUCN.Red.List.Category) 
traits_test$Global.IUCN.Red.List.Category <- gsub("VU", 3, traits_test$Global.IUCN.Red.List.Category) 
traits_test$Global.IUCN.Red.List.Category <- gsub("EN", 4, traits_test$Global.IUCN.Red.List.Category) 
traits_test$Global.IUCN.Red.List.Category <- gsub("CR", 5, traits_test$Global.IUCN.Red.List.Category) 

model2 <- lm(Mass ~ result.category, data=selected_traits)
summary(model2)

model1log <- lm(log10(Range.Size) ~ result.category, data=selected_traits)
summary(model1log)

model2log <- lm(log10(Mean_Body_Mass_.g.) ~ Global.IUCN.Red.List.Category, data=traits_test)
summary(model2log)
plot(log10(Mean_Body_Mass_.g.) ~ Global.IUCN.Red.List.Category, data=traits_test)
boxplot(Migration ~ result.category, data=selected_traits)

boxplot(log10(Mass) ~ result.category, data=selected_traits)
boxplot(log10(Range.Size) ~ result.category, data=selected_traits)

ggplot(data.frame(selected_traits), aes(x=result.category)) +
  geom_bar()

plot(log10(Range.Size)~log10(Mass),data=selected_traits)

ggplot(mpg, 
       aes(x = class, 
           fill = drv)) + 
  geom_bar(position = position_dodge(preserve = "single"))

##All canadian species with threats

can_birds_threats <- read.csv(file = "data_canada/can_birds_threats.csv")

can_birds_threats <- can_birds_threats %>%
  dplyr::rename(Scientific.name = sp)

traits_threats <- can_birds_threats %>%
  inner_join(., birds_ca, by = "Scientific.name")
traits_threats

can_traits_threats <- write.csv(traits_threats, file = "data_canada/can_traits_threats.csv")

traits_threats_wide_code <- traits_threats %>% 
pivot_wider(names_from = Scientific.name, values_from = code)
wide_code <- write.csv(traits_threats_wide_code, file = "C:/Users/janas/OneDrive/Documentos/R/Birds/IUCN_data/wide_code.csv")

can_traits_threats <- read.csv(file = "data_canada/can_traits_threats.csv")

###data global
birds_codes <-read.csv("data_global/data_analysis/birds_codes.csv",h=T)
model <- lm(Mean_Body_Mass_.g. ~ Global.IUCN.Red.List.Category, data=traits_test)


ggplot(birds_codes, aes(factor(threatened), y = Range.Size, fill=factor(threatened))) +
  geom_boxplot() +
  facet_wrap(~variable, scales="free_y")

m <- glm(threatened ~ Range.Size + Mass, data = birds_codes, family = binomial)
m
summary(m)
plot(m)


