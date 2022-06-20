####################################################################
##Bird traits (AVONET) & threats(IUCN)
##Author: Janaina Serrano, McGill University. janaina.serrano@mail.mcgill.ca
##Date: 2022-04-26
####################################################################
##models clean (using centered variables, no dd, common names)

library(lme4) # for multilevel models
library(tidyverse) # for data manipulation and plots
library(effects) #for plotting parameter effects
library(jtools) #for transforming model summaries
#plotting CI (requires the officer and flextable packages):
library(officer)
library(flextable)
library(psych) #plotting the correlation traits

###ok to run - mass and range logged and centered (-mean), wing length (-mean *2std)
data_common_names <- read.csv("C:/Users/janas/OneDrive/Documentos/R/Birds/IUCN_data/data_global/data_analysis/only_threatened/analysis/data_common_names.csv")

#mass, range and migration vs threatened

three_var <- glm(threatened ~ masscentr + rangecentr + Migration, data=data_common_names, family=binomial)
summ(three_var) #4026.94
plot(allEffects(three_var))


#including winglength_ok
four_threatened <- glm(threatened ~ masscentr + rangecentr + Migration + winglength_ok, data=data_common_names, family=binomial(link='logit'))
summ(four_threatened) #AIC: 4002.3
plot(allEffects(four_threatened))

jtools::plot_summs(three_var, four_threatened, scale = TRUE, legend.title = "Threatened")

#including habitat
five_threatened <- glm(threatened ~ masscentr + rangecentr + Migration + winglength_ok + Habitat, data=data_common_names, family=binomial(link='logit'))
summ(five_threatened) #3911.68
plot(allEffects(five_threatened))

jtools::plot_summs(three_var, four_threatened, five_threatened, scale = TRUE, legend.title = "Threatened + habitat")


####For agriculture

#glm
three_var_agr <- glm(agriculture ~ masscentr + rangecentr + Migration, data=data_common_names, family=binomial)

summ(three_var_agr)
plot(allEffects(three_var_agr)) #4179.6 mass and range size not significant

four_var_agr  <- glm(agriculture ~ masscentr + rangecentr + Migration + winglength_ok, data=data_common_names, family=binomial)
summ(four_var_agr)
plot(allEffects(four_var_agr)) #4178 range and wingl non siginificant

jtools::plot_summs(three_var_agr, four_var_agr, scale = TRUE, legend.title = "Agriculture")

test2 <- glm(threatened ~ agriculture, data=data_common_names, family=binomial)
summary(test2)
plot(allEffects(test2))

ggplot(subset(data_common_names, !is.na(Habitat)), aes(x = Range.Size, y = agriculture, color = as.factor(Habitat), fill=Habitat)) +
  geom_point(alpha = .1, position = "jitter")+
  geom_smooth(method = "glm", se = F, 
              method.args = list(family = "binomial")) +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = c(0, 1)) +
  scale_y_continuous(breaks = c(0, 1)) +
  theme(legend.position = "bottom")

#model with habitat
five_var_agr  <- glm(agriculture ~ masscentr + rangecentr + Migration + winglength_ok + Habitat, data=data_common_names, family=binomial)
summ(five_var_agr)
plot(allEffects(five_var_agr)) #3786.33 - does it make sense?
jtools::plot_summs(three_var_agr, four_var_agr, five_var_agr, scale = TRUE, legend.title = "Agriculture")

#biological resource use: 5 - includes hunting and logging: split?

three_var_res <- glm(resource_use ~ masscentr + rangecentr + Migration, data=data_common_names, family=binomial)
summ(three_var_res) #3940.87
plot(allEffects(three_var_res))
#including winglength_ok
four_res <- glm(resource_use ~ masscentr + rangecentr + Migration + winglength_ok, data=data_common_names, family=binomial)
summ(four_res) #AIC: 3933.69
plot(allEffects(four_res))
jtools::plot_summs(three_var_res, four_res, scale = TRUE, legend.title = "Resource use")

five_res <- glm(resource_use ~ masscentr + rangecentr + Migration + winglength_ok + Habitat, data=data_common_names, family=binomial)
summary(five_res) #AIC: 3700.1
plot(allEffects(five_res))


#climate change
three_var_cc <- glm(climate_change ~ masscentr + rangecentr + Migration, data=data_common_names, family=binomial)
summ(three_var_cc) #4219.94
plot(allEffects(three_var_cc))

#including winglength_ok
four_cc <- glm(climate_change ~ masscentr + rangecentr + Migration + winglength_ok, data=data_common_names, family=binomial)
summ(four_cc) #AIC: 4221.5
plot(allEffects(four_cc)) 
#three var has lower AIC

jtools::plot_summs(three_var_cc, four_cc, scale = TRUE, legend.title = "Climate change")

#invasive species
three_var_inv <- glm(invasive ~ masscentr + rangecentr + Migration, data=data_common_names, family=binomial)
summ(three_var_inv) #3697.25
plot(allEffects(three_var_inv))
#including winglength_ok
four_inv <- glm(invasive ~ masscentr + rangecentr + Migration + winglength_ok, data=data_common_names, family=binomial)
summ(four_inv) #AIC: 3697.73
plot(allEffects(four_inv))

jtools::plot_summs(three_var_inv, four_inv, scale = TRUE, legend.title = "Invasive species")


#pollution
three_var_pol <- glm(pollution ~ masscentr + rangecentr + Migration, data=data_common_names, family=binomial)
summ(three_var_pol) #2299.14
plot(allEffects(three_var_pol))
#including winglength_ok
four_pol <- glm(pollution ~ masscentr + rangecentr + Migration + winglength_ok, data=data_common_names, family=binomial)
summary(four_pol) #AIC: 2300.2
plot(allEffects(four_pol))
summ(four_pol)
jtools::plot_summs(three_var_pol, four_pol, scale = TRUE, legend.title = "Pollution")

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
