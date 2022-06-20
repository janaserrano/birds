####################################################################
##Bird traits (AVONET) & threats(IUCN)
##Author: Janaina Serrano, McGill University. janaina.serrano@mail.mcgill.ca
####################################################################
##models dirty
library(dplyr)
library(devtools)
library(lme4) # for multilevel models
library(tidyverse) # for data manipulation and plots
library(sjstats) #for calculating intra-class correlation (ICC)
library(effects) #for plotting parameter effects
library(jtools)#for transforming model summaries
library(MASS)
library(arm)

# all birds
traits_birds_cat <- read.csv("C:/Users/janas/OneDrive/Documentos/R/Birds/IUCN_data/data_global/data_analysis/threats_binary_cat.csv", stringsAsFactors = TRUE)

data<- read.csv("data_global/data_analysis/threats_binary_cat.csv")

glimpse(traits_birds_cat)
nrow(traits_birds_cat)
traits_birds_cat$Migration <- factor(traits_birds_cat$Migration)
traits_birds_cat$threatened <- factor(traits_birds_cat$threatened)
traits_birds_cat$agriculture <- factor(traits_birds_cat$agriculture)
traits_birds_cat$agriculture <- factor(traits_birds_cat$agriculture)

my_fun <- function(x) { 
  factor(x)
}
my_fun
data_apply <- apply(data[ , c(43:55)], 2, my_fun) 
data_new <- data                                                         # Replicate original data
data_new[ , colnames(data_new) %in% colnames(data_apply)] <- data_apply  # Replace specific columns
data_new 
str(data_new)

levels(traits_birds_cat$threatened)
str(traits_birds_cat)
table(traits_birds_cat$threatened) #0=9429 1=1423 for threats only:    0    1 2030 1427
hist(traits_birds_cat$threatened)
table(traits_birds_cat$agriculture) #0=8674 1=2178 for threats only:    0    1 1276 2181 
table(traits_birds_cat$climate_change) #9636 1216  for threats only:  0    1 2241 1216 
table(traits_birds_cat$resource_use) #8529 2323 for threats only:    0    1 1130 2327 
table(traits_birds_cat$invasive) #9851 1001 for threats only:   0    1 2451 1006 
table(traits_birds_cat$pollution) #10360   492 for threats only:    0    1 2965  492

#histograms to explore data
hist(log(traits_birds_cat$Mass))
hist(log(traits_birds_cat$Range.Size))
hist(traits_birds_cat$Migration)
hist(traits_birds_cat$Wing.Length)

#boxplots

boxplot(log(Mass)~threatened, data=traits_birds_cat)
mass_t <- t.test(Mass ~ threatened, data = traits_birds_cat, var.equal = TRUE)
mass_t
boxplot(log(Range.Size)~threatened, data=traits_birds_cat)
range_t <- t.test(Range.Size ~ threatened, data = traits_birds_cat, var.equal = TRUE)
range_t
boxplot(Wing.Length~threatened, data=traits_birds_cat)
wing_t <- t.test(Wing.Length ~ threatened, data = traits_birds_cat, var.equal = TRUE)
wing_t
plot(Migration~threatened, data=traits_birds_cat)

ggplot(traits_birds_cat, 
       aes(x = threatened, 
           fill = Migration)) + 
  geom_bar(position = "dodge")


##mass x agriculture
traits_birds_cat %>%
  ggplot(aes(x = Mass, y = agriculture)) +
  geom_point(alpha = .1, position = "jitter")+
  geom_smooth(method = "glm", se = F, 
              method.args = list(family = "binomial")) +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = c(0, 1)) +
  scale_y_continuous(breaks = c(0, 1))

#plot Mass
p <- traits_birds_cat %>%
  ggplot(aes(x = Mass, y = threatened, color = as.factor(Habitat), fill=Habitat)) +
  geom_point(alpha = .1, position = "jitter")+
  geom_smooth(method = "glm", se = F, 
              method.args = list(family = "binomial")) +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = c(0, 1)) +
  scale_y_continuous(breaks = c(0, 1))
p + theme(legend.position = "bottom")


#plot range
p <- traits_birds_cat %>%
  ggplot(aes(x = Range.Size, y = threatened, color = as.factor(Habitat), fill=Habitat)) +
  geom_point(alpha = .1, position = "jitter")+
  geom_smooth(method = "glm", se = F, 
              method.args = list(family = "binomial")) +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = c(0, 1)) +
  scale_y_continuous(breaks = c(0, 1))
p + theme(legend.position = "bottom")


#plot migration
  ggplot(subset(traits_birds_cat, !is.na(Habitat)), aes(x = Migration, y = threatened, color = as.factor(Habitat), fill=Habitat)) +
  geom_point(alpha = .1, position = "jitter")+
  geom_smooth(method = "glm", se = F, 
              method.args = list(family = "binomial")) +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = c(0, 1)) +
  scale_y_continuous(breaks = c(0, 1)) + theme(legend.position = "bottom")
barplot(traits_birds_cat$Migration)
x <- table(traits_birds_cat$Migration)
barplot(x)

test1 <- lm(traits_birds_cat$Mass ~ traits_birds_cat$Wing.Length)
plot(test1)
abline(test1)
summary(test1)

#For all birds
wing_migration <- lm(traits_birds_cat$Wing.Length ~ traits_birds_cat$Migration)

summary(wing_migration)
##glm
naiveglm <- glm(threatened ~ log(Mass), data=traits_birds_cat, family=binomial)
naiveglm_range <- glm(threatened ~ log(Range.Size), data=traits_birds_cat, family=binomial)
summary(naiveglm) #AIC AIC: 8120.2
summary(naiveglm_range)#AIC 6650.5

#mass, range and migration vs threatened
#migration as factor
traits_birds_cat$Migration <- factor(traits_birds_cat$Migration)

#this is for all birds so not valid anymore
#all_var <- glm(threatened ~ log(Mass) + log(Range.Size) + Migration, data=traits_birds_cat, family=binomial)
summary(all_var) #5956.9
summ(all_var)
plot(allEffects(all_var))
plot(all_var, which = 5)


library(psych)

#if include habitat for all species (even not threatened)
five_var <- glm(threatened ~ log(Mass) + log(Range.Size) + Migration + Wing.Length + Habitat, data=traits_birds_cat, family=binomial)
summary(five_var)
plot(allEffects(five_var)) #5726.8

selected_threats <- traits_birds_cat %>%
  select(c(43:55))
selected_threats

selected_traits <- traits_birds_cat %>%
  select(Mass, Range.Size, Migration, Wing.Length)
pairs.panels(selected_traits)

#including wing.length
#four_threatened <- glm(threatened ~ log(Mass) + log(Range.Size) + Migration + Wing.Length, data=traits_birds_cat, family=binomial(link='logit'))
#summary(four_threatened) #AIC: 5895.7
#plot(allEffects(four_threatened))

#plot
#traits_birds_cat %>%
#  ggplot(aes(x = Mass, y = threatened)) +
#  geom_point(alpha = .1, position = "jitter")+
#  geom_smooth(method = "glm", se = F, 
#              method.args = list(family = "binomial")) +
#  theme(legend.position = "none") +
#  scale_x_continuous(breaks = c(0, 1)) +
#  scale_y_continuous(breaks = c(0, 1))

#traits_birds_cat %>%
 # ggplot(aes(x = Range.Size, y = threatened)) +
#  geom_point(alpha = .1, position = "jitter")+
 # geom_smooth(method = "glm", se = F, 
#              method.args = list(family = "binomial")) +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = c(0, 1)) +
  scale_y_continuous(breaks = c(0, 1))

traits_birds_cat %>%
  ggplot(aes(x = Migration, y = threatened)) +
  geom_point(alpha = .1, position = "jitter")+
  geom_smooth(method = "glm", se = F, 
              method.args = list(family = "binomial")) +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = c(0, 1)) +
  scale_y_continuous(breaks = c(0, 1))


#glmer
#intercept only model
Model_Multi_Intercept <- glmer(formula = threatened ~ 1 + (1|Habitat),
                               family = binomial,
                               data = traits_birds_cat,
                               control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5)))

summary(Model_Multi_Intercept)
sjstats::icc(Model_Multi_Intercept)

##mass
Model_Mass <- glmer(formula = threatened ~ log(Mass) + (1|Habitat),
                               family = binomial,
                               data = traits_birds_cat,
                               control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5)))

summary(Model_Mass) #AIC   7884.5   
sjstats::icc(Model_Mass) #  Adjusted ICC: 0.111

summ(Model_Mass, exp = T)
plot(allEffects(Model_Mass))
##Mass + Range size
Model_Mass_Range <- glmer(formula = threatened ~ log(Mass) +log(Range.Size) + (1|Habitat),
                    family = binomial,
                    data = traits_birds_cat,
                    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5)))

summary(Model_Mass_Range) #AIC 5821.7
summ(Model_Mass_Range, exp = T) #MODEL FIT: AIC = 5821.73, BIC = 5850.86 Pseudo-R² (fixed effects) = 0.38 Pseudo-R² (total) = 0.44 


plot(allEffects(Model_Mass_Range))


##Range Size
Model_Range <- glmer(formula = threatened ~ log(Range.Size) + (1|Habitat),
                    family = binomial,
                    data = traits_birds_cat,
                    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5)))

summary(Model_Range)   ##AIC 6229.2 MODEL FIT: AIC = 6229.23, BIC = 6251.07 Pseudo-R² (fixed effects) = 0.29 Pseudo-R² (total) = 0.42 
summ(Model_Range, exp = T) 
sjstats::icc(Model_Range) 

#Range+ Mass random slope
#Model_Multi_Full <- glmer(REPEAT ~ SEX + PPED + MSESC + (1 + SEX + PPED|SCHOOLID),
#                          family = binomial(logit),
#                          data = ThaiEdu_Center,
#                          control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5)))

Model_Full <- glmer(threatened ~ log(Mass) + log(Range.Size) + (1 + log(Mass) + log(Range.Size)|Habitat),
                          family = binomial,
                          data = traits_birds_cat,
                          control = glmerControl(optimizer = "bobyqa"))
summary(Model_Full)
summ(Model_Full, exp = T)
plot(allEffects(Model_Full)) # AIC = 5705.48, BIC = 5771.00 Pseudo-R² (fixed effects) = 0.37 Pseudo-R² (total) = 0.49  

Model_Full_range <- glmer(threatened ~ log(Mass) + log(Range.Size) + (1 + log(Range.Size)|Habitat),
                    family = binomial,
                    data = traits_birds_cat,
                    control = glmerControl(optimizer = "bobyqa"))
summ(Model_Full_range, exp =T) #AIC = 5724.54, BIC = 5768.22 Pseudo-R² (fixed effects) = 0.37 Pseudo-R² (total) = 0.45 


Model_Full_mass <- glmer(threatened ~ log(Mass) + log(Range.Size) + (1 + log(Mass)|Habitat),
                          family = binomial,
                          data = traits_birds_cat,
                          control = glmerControl(optimizer = "bobyqa"))

summ(Model_Full_mass, exp =T) # MODEL FIT: AIC = 5794.81, BIC = 5838.49 Pseudo-R² (fixed effects) = 0.33 Pseudo-R² (total) = 0.50 

#full model #not full
Model_Multi_Full <- glmer(threatened ~ log(Mass) + (1 + log(Mass) + log(Mass)|Habitat),
                          family = binomial,
                          data = traits_birds_cat,
                          control = glmerControl(optimizer = "bobyqa"))
summary(Model_Multi_Full)
summ(Model_Multi_Full, exp = T)
plot(allEffects(Model_Multi_Full))

#let's fit a less-than-full model that leaves out the random slope term of `SEX`
Model_Multi_Full_No_Mass <- glmer(threatened ~ log(Mass) + (1 + log(Mass)|Habitat),
                                 family = binomial,
                                 data = traits_birds_cat,
                                 control = glmerControl(optimizer = "bobyqa"))
Model_Multi_Full_No_Mass

Model_Multi_Full_No_Mass1 <- glmer(threatened ~ log(Mass) + (1|Habitat),
                                     family = binomial,
                                     data = traits_birds_cat,
                                     control = glmerControl(optimizer = "bobyqa"))
summary(Model_Multi_Full_No_Mass1) #AIC  7884.5

anova(Model_Full_range, Model_Full, test="Chisq")

Model_Multi_Mass_Range <- glmer(threatened ~ log(Mass) + log(Range.Size) + (1|Habitat),
                                   family = binomial,
                                   data = traits_birds_cat,
                                   control = glmerControl(optimizer = "bobyqa"))
Model_Multi_Mass_Range #AIC 5821.733

##ADD migration
Model_Full_migration <- glmer(threatened ~ log(Mass) + log(Range.Size) + Migration + (1 + log(Mass) + log(Range.Size) + Migration|Habitat),
                    family = binomial,
                    na.action=na.exclude,
                    data = traits_birds_cat,
                    control = glmerControl(optimizer = "bobyqa"))
#didnt change excluding NAs
summ(Model_Full_migration, exp = T) #BEST AIC = 5685.45, BIC = 5787.37 Pseudo-R² (fixed effects) = 0.37 Pseudo-R² (total) = 0.51 

plot(allEffects(Model_Full_migration))

#For agriculture

#glm
all_var_agr <- glm(agriculture ~ log(Mass) + log(Range.Size) + Migration, data=traits_birds_cat, family=binomial)

summary(all_var_agr)
plot(allEffects(all_var_agr)) #9054

four_var_agr  <- glm(agriculture ~ log(Mass) + log(Range.Size) + Migration + Wing.Length, data=traits_birds_cat, family=binomial)
summary(four_var_agr)
plot(allEffects(four_var_agr)) #9015.5

test2 <- glm(threatened ~ agriculture, data=traits_birds_cat, family=binomial)
summary(test2)
plot(allEffects(test2))

ggplot(subset(traits_birds_cat, !is.na(Habitat)), aes(x = Range.Size, y = agriculture, color = as.factor(Habitat), fill=Habitat)) +
  geom_point(alpha = .1, position = "jitter")+
  geom_smooth(method = "glm", se = F, 
              method.args = list(family = "binomial")) +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = c(0, 1)) +
  scale_y_continuous(breaks = c(0, 1)) +
  theme(legend.position = "bottom")


ggplot(subset(traits_birds_cat, !is.na(Habitat)), aes(x = Mass, y = agriculture, color = as.factor(Habitat), fill=Habitat)) +
  geom_point(alpha = .1, position = "jitter")+
  geom_smooth(method = "glm", se = F, 
              method.args = list(family = "binomial")) +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = c(0, 1)) +
  scale_y_continuous(breaks = c(0, 1)) +
  theme(legend.position = "bottom")

ggplot(subset(traits_birds_cat, !is.na(Habitat)), aes(x = Migration, y = agriculture, color = as.factor(Habitat), fill=Habitat)) +
  geom_point(alpha = .1, position = "jitter")+
  geom_smooth(method = "glm", se = F, 
              method.args = list(family = "binomial")) +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = c(0, 1)) +
  scale_y_continuous(breaks = c(0, 1)) +
  theme(legend.position = "bottom")

#models
Model_Intercept <- glmer(formula = agriculture ~ 1 + (1|Habitat),
                               family = binomial,
                               data = traits_birds_cat,
                               control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5)))

summary(Model_Intercept)
sjstats::icc(Model_Intercept) #ICC: 0.078


Model_Intercept_hd <- glmer(formula = agriculture ~ 1 + (1|Habitat.Density),
                         family = binomial,
                         data = traits_birds_cat,
                         control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5)))
summary(Model_Intercept_hd)
summ(Model_Intercept_ls, exp = T)
sjstats::icc(Model_Intercept_ls) 

Model_Full_migration_agri <- glmer(agriculture ~ log(Mass) + log(Range.Size) + Migration + (1 + log(Mass) + log(Range.Size) + Migration|Habitat),
                                   family = binomial,
                                   data = traits_birds_cat,
                                   control = glmerControl(optimizer = "bobyqa"))
summ(Model_Full_migration_agri, exp = T) #MODEL FIT:  AIC = 8833.14, BIC = 8935.06 Pseudo-R² (fixed effects) = 0.21 Pseudo-R² (total) = 0.30 
plot(allEffects(Model_Full_migration_agri))

Model_Mass_range_agri <- glmer(agriculture ~ log(Mass) + log(Range.Size) + (1 + log(Mass) + log(Range.Size)|Habitat),
                               family = binomial,
                               data = traits_birds_cat,
                               control = glmerControl(optimizer = "bobyqa"))
summ(Model_Mass_range_agri, exp = T) #MODEL FIT: AIC = 8862.31, BIC = 8927.84 Pseudo-R² (fixed effects) = 0.20 Pseudo-R² (total) = 0.30

Model_Mass <- glmer(agriculture ~ log(Mass) + (1 + log(Mass)|Habitat),
                               family = binomial,
                               data = traits_birds_cat,
                               control = glmerControl(optimizer = "bobyqa"))

summ(Model_Mass, exp = T) #MODEL FIT:   AIC = 10251.99, BIC = 10288.41 Pseudo-R² (fixed effects) = 0.04 Pseudo-R² (total) = 0.15 

Model_Range <- glmer(agriculture ~ log(Range.Size) + (1 + log(Range.Size)|Habitat),
                    family = binomial,
                    data = traits_birds_cat,
                    control = glmerControl(optimizer = "bobyqa"))

summ(Model_Range, exp = T) #MODEL FIT: AIC = 9326.02, BIC = 9362.42 Pseudo-R² (fixed effects) = 0.13 Pseudo-R² (total) = 0.23

Model_Range_hab <- glmer(agriculture ~ log(Range.Size) + (1|Habitat),
                     family = binomial,
                     data = traits_birds_cat,
                     control = glmerControl(optimizer = "bobyqa"))

summ(Model_Range_hab, exp = T) #MODEL FIT: AIC = 9343.63, BIC = 9365.47 Pseudo-R² (fixed effects) = 0.17 Pseudo-R² (total) = 0.25 

#try to use threats to predict extinction risk (threatened sp or iucn categories)

#biological resource use: 5 - includes hunting and logging: split?

all_var_res <- glm(resource_use ~ log(Mass) + log(Range.Size) + Migration, data=traits_birds_cat, family=binomial)
summary(all_var_res) #8829.7
plot(allEffects(all_var_res))
#including wing.length
four_res <- glm(resource_use ~ log(Mass) + log(Range.Size) + Migration + Wing.Length, data=traits_birds_cat, family=binomial)
summary(four_res) #AIC: 8779.8
plot(allEffects(four_res))

four_res_unlog <- glm(resource_use ~ Mass + Range.Size + Migration + Wing.Length, data=traits_birds_cat, family=binomial)
summary(four_res_unlog) 
plot(allEffects(four_res_unlog))


#climate change
cc_threat <- glm(threatened ~ climate_change, data=traits_birds_cat, family=binomial)
summary(cc_threat)
plot(allEffects(cc_threat))

all_var_cc <- glm(climate_change ~ log(Mass) + log(Range.Size) + Migration, data=traits_birds_cat, family=binomial)
summary(all_var_cc) #6364.1
plot(allEffects(all_var_cc))
#including wing.length
four_cc <- glm(climate_change ~ log(Mass) + log(Range.Size) + Migration + Wing.Length, data=traits_birds_cat, family=binomial)
summary(four_cc) #AIC: 6352.1
plot(allEffects(four_cc))
four_cc_unlog <- glm(climate_change ~ Mass + Range.Size + Migration + Wing.Length, data=traits_birds_cat, family=binomial)
summary(four_cc_unlog) #AIC: 7296.9
plot(allEffects(four_cc_unlog))

#invasive species
three_var_inv <- glm(invasive ~ log(Mass) + log(Range.Size) + Migration, data=traits_birds_cat, family=binomial)
summary(three_var_inv) #5013.2
plot(allEffects(three_var_inv))
#including wing.length
four_inv <- glm(invasive ~ log(Mass) + log(Range.Size) + Migration + Wing.Length, data=traits_birds_cat, family=binomial)
summary(four_inv) #AIC: 5014.2
plot(allEffects(four_inv))
#unlogged
four_inv <- glm(invasive ~ Mass + Range.Size + Migration + Wing.Length, data=traits_birds_cat, family=binomial)
summary(four_inv) #6126.4
plot(allEffects(four_inv))

three_var_inv <- glm(invasive ~ Mass + Range.Size + Migration, data=traits_birds_cat, family=binomial)
summary(three_var_inv) #6258.7
plot(allEffects(three_var_inv))

#other types of plots


traits_birds_cat$result.category <- factor(traits_birds_cat$result.category)

ggplot(traits_birds_cat, aes(log(Mass), as.numeric(threatened)-1, color=result.category)) +
  stat_smooth(method="glm", method.args=list(family="binomial"), formula=y~x,
              alpha=0.2, size=2, aes(fill=result.category)) +
  geom_point(position=position_jitter(height=0.03, width=0)) +
  xlab("Mass") + ylab("Pr (threatened)")

glimpse(traits_birds_cat)
selected_threats


#model pollution



#nodd:
traits_birds_cat_nodd <- read.csv("data_global/data_analysis/threats_binary_cat_nodd.csv")

traits_birds_cat_nodd$Migration <- factor(traits_birds_cat_nodd$Migration)

all_var_nodd <- glm(threatened ~ log(Mass) + log(Range.Size) + Migration, data=traits_birds_cat_nodd, family=binomial)
summary(all_var_nodd) #5927.8
plot(allEffects(all_var_nodd))

four_var_nodd <- glm(threatened ~ log(Mass) + log(Range.Size) + Migration + Wing.Length, data=traits_birds_cat_nodd, family=binomial)
summary(four_var_nodd) #5865.8
plot(allEffects(four_var_nodd))


###################### 2022-04-21 only for birds with threats -------------------------------------

#only birds with threats
only_w_threats <- read.csv("C:/Users/janas/OneDrive/Documentos/R/Birds/IUCN_data/data_global/data_analysis/only_threatened/only_threatened.csv")

##glm
naiveglm <- glm(threatened ~ log(Mass), data=only_w_threats, family=binomial)
naiveglm_range <- glm(threatened ~ log(Range.Size), data=only_w_threats, family=binomial)
summary(naiveglm) #AIC 4680.1
summary(naiveglm_range)#AIC 4204.1
plot(allEffects(naiveglm))
plot(allEffects(naiveglm_range))

#mass, range and migration vs threatened
#migration as factor
only_w_threats$Migration <- factor(only_w_threats$Migration)

three_var <- glm(threatened ~ log(Mass) + log(Range.Size) + Migration, data=only_w_threats, family=binomial)
summary(three_var) #4066.7
plot(allEffects(three_var))
plot(three_var, which = 5)

install.packages("psych")
library(psych)

selected_traits <- only_w_threats %>%
  select(Mass, Range.Size, Migration, Wing.Length)
pairs.panels(selected_traits)

#including wing.length
four_threatened <- glm(threatened ~ log(Mass) + log(Range.Size) + Migration + Wing.Length, data=only_w_threats, family=binomial(link='logit'))
summary(four_threatened) #AIC: 4042.9
summ(four_threatened)
plot(allEffects(four_threatened))

jtools::plot_summs(three_var, four_threatened, scale = TRUE, legend.title = "Threatened")

#including habitat
five_threatened <- glm(threatened ~ log(Mass) + log(Range.Size) + Migration + Wing.Length + Habitat, data=only_w_threats, family=binomial(link='logit'))
summary(five_threatened) #3949.8
plot(allEffects(five_threatened))

jtools::plot_summs(three_var_pol, four_pol, five_threatened, scale = TRUE, legend.title = "Threatened + habitat")

only_w_threats %>%
  ggplot(aes(x = Mass, y = threatened)) +
  geom_point(alpha = .1, position = "jitter")+
  geom_smooth(method = "glm", se = F, 
              method.args = list(family = "binomial")) +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = c(0, 1)) +
  scale_y_continuous(breaks = c(0, 1))

only_w_threats %>%
  ggplot(aes(x = Range.Size, y = threatened)) +
  geom_point(alpha = .1, position = "jitter")+
  geom_smooth(method = "glm", se = F, 
              method.args = list(family = "binomial")) +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = c(0, 1)) +
  scale_y_continuous(breaks = c(0, 1))

#For agriculture

#glm
three_var_agr <- glm(agriculture ~ log(Mass) + log(Range.Size) + Migration, data=only_w_threats, family=binomial)

summary(three_var_agr)
plot(allEffects(three_var_agr)) #4186.8 mass and range size not significant

four_var_agr  <- glm(agriculture ~ log(Mass) + log(Range.Size) + Migration + Wing.Length, data=only_w_threats, family=binomial)
summary(four_var_agr)
plot(allEffects(four_var_agr)) #4186.2 only migration significant

jtools::plot_summs(three_var_agr, four_var_agr, scale = TRUE, legend.title = "Agriculture")

test2 <- glm(threatened ~ agriculture, data=only_w_threats, family=binomial)
summary(test2)
plot(allEffects(test2))

ggplot(subset(only_w_threats, !is.na(Habitat)), aes(x = Range.Size, y = agriculture, color = as.factor(Habitat), fill=Habitat)) +
  geom_point(alpha = .1, position = "jitter")+
  geom_smooth(method = "glm", se = F, 
              method.args = list(family = "binomial")) +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = c(0, 1)) +
  scale_y_continuous(breaks = c(0, 1)) +
  theme(legend.position = "bottom")

#model with habitat
five_var_agr  <- glm(agriculture ~ log(Mass) + log(Range.Size) + Migration + Wing.Length + Habitat, data=only_w_threats, family=binomial)
summary(five_var_agr)
plot(allEffects(five_var_agr)) #3832.4 - does it make sense?
jtools::plot_summs(three_var_agr, four_var_agr, five_var_agr, scale = TRUE, legend.title = "Agriculture")
table(only_w_threats$Habitat) #Coastal         Desert         Forest      Grassland Human Modified 
#79             12           1975            241             14 
#Marine       Riverine           Rock      Shrubland        Wetland 
#259             35             24            276            268 
#Woodland 
#213 

ggplot(subset(only_w_threats, !is.na(Habitat)), aes(x = Mass, y = agriculture, color = as.factor(Habitat), fill=Habitat)) +
  geom_point(alpha = .1, position = "jitter")+
  geom_smooth(method = "glm", se = F, 
              method.args = list(family = "binomial")) +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = c(0, 1)) +
  scale_y_continuous(breaks = c(0, 1)) +
  theme(legend.position = "bottom")

ggplot(subset(only_w_threats, !is.na(Habitat)), aes(x = Migration, y = agriculture, color = as.factor(Habitat), fill=Habitat)) +
  geom_point(alpha = .1, position = "jitter")+
  geom_smooth(method = "glm", se = F, 
              method.args = list(family = "binomial")) +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = c(0, 1)) +
  scale_y_continuous(breaks = c(0, 1)) +
  theme(legend.position = "bottom")

#biological resource use: 5 - includes hunting and logging: split?

three_var_res <- glm(resource_use ~ log(Mass) + log(Range.Size) + Migration, data=only_w_threats, family=binomial)
summary(three_var_res) #3964.9
plot(allEffects(three_var_res))
#including wing.length
four_res <- glm(resource_use ~ log(Mass) + log(Range.Size) + Migration + Wing.Length, data=only_w_threats, family=binomial)
summary(four_res) #AIC: 3956.9
plot(allEffects(four_res))
jtools::plot_summs(three_var_res, four_res, scale = TRUE, legend.title = "Resource use")

five_res <- glm(resource_use ~ log(Mass) + log(Range.Size) + Migration + Wing.Length + Habitat, data=only_w_threats, family=binomial)
summary(five_res) #AIC: 3700.1
plot(allEffects(five_res))


#climate change
cc_threat <- glm(threatened ~ climate_change, data=only_w_threats, family=binomial)
summary(cc_threat)
plot(allEffects(cc_threat))

three_var_cc <- glm(climate_change ~ log(Mass) + log(Range.Size) + Migration, data=only_w_threats, family=binomial)
summary(three_var_cc) #4259.9
plot(allEffects(three_var_cc))

#including wing.length
four_cc <- glm(climate_change ~ log(Mass) + log(Range.Size) + Migration + Wing.Length, data=only_w_threats, family=binomial)
summary(four_cc) #AIC: 4261.4
plot(allEffects(four_cc)) 

jtools::plot_summs(three_var_cc, four_cc, scale = TRUE, legend.title = "Climate change")

#invasive species
three_var_inv <- glm(invasive ~ log(Mass) + log(Range.Size) + Migration, data=only_w_threats, family=binomial)
summary(three_var_inv) #3661.3
plot(allEffects(three_var_inv))
#including wing.length
four_inv <- glm(invasive ~ log(Mass) + log(Range.Size) + Migration + Wing.Length, data=only_w_threats, family=binomial)
summary(four_inv) #AIC: 3661.2
plot(allEffects(four_inv))

jtools::plot_summs(three_var_inv, four_inv, scale = TRUE, legend.title = "Invasive species")


#pollution
three_var_pol <- glm(pollution ~ log(Mass) + log(Range.Size) + Migration, data=only_w_threats, family=binomial)
summary(three_var_pol) #2307.7
plot(allEffects(three_var_pol))
#including wing.length
four_pol <- glm(pollution ~ log(Mass) + log(Range.Size) + Migration + Wing.Length, data=only_w_threats, family=binomial)
summary(four_pol) #AIC: 2308.6
plot(allEffects(four_pol))
summ(four_pol)

##PLOTTING CI
#(requires the officer and flextable packages):
library(officer)
library(flextable)
library(huxtable)
library(sandwich)

jtools::plot_summs(three_var_pol, four_pol, scale = TRUE, legend.title = "Pollution")
plot_coefs(three_var_pol, four_pol, scale = TRUE, inner_ci_level = .95)

plot_summs(four_pol, scale = TRUE, plot.distributions = TRUE, inner_ci_level = .95)
plot_summs(three_var_pol, scale = TRUE, plot.distributions = TRUE, inner_ci_level = .95)

export_summs(four_pol, three_var_pol, scale = TRUE, to.file = "docx", file.name = "test.docx")

####center and scale variables
#arm::standardize
four_threatened1 <- arm::standardize(four_threatened, unchanged="log(Mass)", "log(Range.Size)", "Migration")

summ(four_threatened1)
## why so many Observations deleted? 200 (3257 missing obs. deleted)
plot(allEffects(four_threatened1))
plot_summs(all_var1, scale = TRUE, plot.distributions = TRUE, inner_ci_level = .95)


######## center the logged variables but not scale them: mass and range size -  just subtract out the mean
only_w_threats <- read.csv("C:/Users/janas/OneDrive/Documentos/R/Birds/IUCN_data/data_global/data_analysis/only_threatened/only_threatened.csv")

centered <- only_w_threats %>% 
  mutate(logmass = log(Mass))
avgmass <- mean(centered1$logmass)
centered1<- centered %>% 
  mutate(logrange = log(Range.Size))


#centered1$result.category<-c(DD=0,LC=1,NT=2,VU=3,EN=4,CR=5,EW=6,EX=7)[all_df$result.category]

###need to exclude dd species 
no_dd <- filter(centered1, result.category != 0)
##exclude negative ranges
no_neg <- filter(no_dd, logrange > 0)

avgrange<- mean(centered1$logrange)

write.csv(no_neg, "C:/Users/janas/OneDrive/Documentos/R/Birds/IUCN_data/data_global/data_analysis/only_threatened/no_neg_dd.csv")
write.csv(no_dd, "C:/Users/janas/OneDrive/Documentos/R/Birds/IUCN_data/data_global/data_analysis/only_threatened/no_dd.csv")

no_dd_is.na <- no_dd[!is.na(no_dd$Range.Size), ]                 # Omit NA by column via is.na
no_dd_is.na  

write.csv(no_dd_is.na, "C:/Users/janas/OneDrive/Documentos/R/Birds/IUCN_data/data_global/data_analysis/only_threatened/no_dd_na.csv")

no_dd_na <- read.csv("C:/Users/janas/OneDrive/Documentos/R/Birds/IUCN_data/data_global/data_analysis/only_threatened/no_dd_na.csv")

centered_log <- no_dd_na %>% 
  mutate(rangecentr = logrange-avgrange) %>% 
  mutate(masscentr = logmass-avgmass) 

centered_scaled <- centered_log %>%
  mutate(winglength_scaled = Wing.Length - avgwinglength)

centered_scaled_2 <- centered_scaled %>%
  mutate(STDwinglength_2 = 2*(STDwinglength))
#for the non-logged numeric, substract out the mean and then divide by 2*sd
centered_scaled_3 <- centered_scaled_2 %>%
  mutate(winglength_ok = winglength_scaled/STDwinglength_2)

write.csv(centered_scaled_3, "C:/Users/janas/OneDrive/Documentos/R/Birds/IUCN_data/data_global/data_analysis/only_threatened/centered_scaled.csv")

###ok to run
centered_scaled <- read.csv("C:/Users/janas/OneDrive/Documentos/R/Birds/IUCN_data/data_global/data_analysis/only_threatened/centered_scaled.csv")


##named this file models_dirty and made a new models_clean