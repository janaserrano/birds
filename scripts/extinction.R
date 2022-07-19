####################################################################
##Extinction risk, Bird traits (AVONET) & threats(IUCN)
##Author: Janaina Serrano, McGill University. janaina.serrano@mail.mcgill.ca
##Date: 2022-04-26
####################################################################
library(lme4) # for multilevel models
library(tidyverse) # for data manipulation and plots
library(effects) #for plotting parameter effects
library(jtools) #for transforming model summaries
#plotting CI (requires the officer and flextable packages):
library(officer)
library(flextable)
library(psych) #plotting the correlation traits
library(sjPlot) #plotting marginal effects https://cran.r-project.org/web/packages/sjPlot/vignettes/plot_interactions.html 
library(ggplot2)
library(MuMIn)
library(margins)

data <- read.csv("data_global/data_analysis/only_threatened/analysis/data_genl_clutch.csv")

##model: IUCN ext risk ~ traits * threats * habitat (only 14 human modified)
#maybe use habitat density?
#Laura: use open habitat-natural versus open habitat - cities
table(data$Habitat.Density)
hist(data$gen.length)
hist(data$clutch.size)

lm1 <- glm(threatened ~ masscentr + rangecentr + Migration + wing.lcentr + clutch.size + gen.length, data=data, family=binomial)
summ(lm1)
plot(allEffects(lm1))

jtools::plot_summs(lm1, scale = TRUE, legend.title = "Threatened")

dev.off()

lm2 <- glm(threatened ~ masscentr + rangecentr + Migration + wing.lcentr + clutch.size + gen.length + Habitat.Density, data=data, family=binomial)
summ(lm21)
jtools::plot_summs(lm21, scale = TRUE, legend.title = "Threatened")
lm21 <- glm(threatened ~ masscentr + rangecentr + wing.lcentr + gen.length + Habitat.Density, data=data, family=binomial)

lm3 <- glm(threatened ~ masscentr + rangecentr + Migration + wing.lcentr + clutchcentr + genlcentr + Habitat.Density + agriculture + climate_change + invasive + resource_use , data=data, family=binomial)
summ(lm3) #best Pseudo-R² (Cragg-Uhler) = 0.37 Pseudo-R² (McFadden) = 0.24 AIC = 1732.88, BIC = 1792.86 

#without climate change
lm31 <- glm(threatened ~ masscentr + rangecentr + Migration + wing.lcentr + clutch.size + gen.length + Habitat.Density + agriculture + invasive + resource_use , data=data, family=binomial)
summ(lm31) #same as lm3

#everything w interactions
lm4 <- glm(threatened ~ masscentr * rangecentr * Migration * wing.lcentr * clutch.size * gen.length * Habitat.Density * agriculture * climate_change * invasive * pollution * resource_use, data=data, family=binomial)
summ(lm4) #did not converge

lm5 <- glm(threatened ~ masscentr * rangecentr * Migration * wing.lcentr * clutch.size * gen.length, data=data, family=binomial)
lm6 <- glm(threatened ~ agriculture * climate_change * invasive * pollution * resource_use, data=data, family=binomial)

summ(lm6)
plot(allEffects(lm3))

jtools::plot_summs(lm3, scale = TRUE, legend.title = "Threatened")


#all threats
lm7 <- glm(threatened ~ agriculture + climate_change + invasive + pollution + resource_use + residential + energy + transportation + intrusion + nat_sys_modif + geological + other, data=data, family=binomial)
summ(lm7)
jtools::plot_summs(lm7, scale = TRUE, legend.title = "Threatened")
#selected threats: maybe I am just using agriculture, invasives and resource use
#excluding mass, clutch size, habitat density and migration

lm8 <- glm(threatened ~ rangecentr + wing.lcentr + gen.length + agriculture + invasive + resource_use , data=data, family=binomial)
summ(lm8) #AIC higher, R2 lower than lm6

#+ residential
lm9 <- glm(threatened ~ rangecentr + wing.lcentr + gen.length + agriculture + invasive + resource_use + residential , data=data, family=binomial)
summ(lm9)

#all threats with traits (excluding mass, migration, clutch size and habitat)
lm10 <- glm(threatened ~ rangecentr + wing.lcentr + gen.length + agriculture + climate_change + invasive + pollution + resource_use + residential + energy + transportation + intrusion + nat_sys_modif + geological + other, data=data, family=binomial)
summ(lm10)
jtools::plot_summs(lm10, scale = TRUE, legend.title = "Threatened")

#test all traits without threats
all_birds <- read.csv("data_global/all_birds/all_birds_amniote_gl.csv")

transform(all_birds, rangecentr = as.numeric(rangecentr))
str(all_birds)
str(data)

# http://www.science.smith.edu/~jcrouser/SDS293/labs/lab4-r.html
glm_probs = data.frame(probs = predict(lm3, 
                                       newdata = all_birds, 
                                       type="response"))

library(ROCR)
ROCRpred <- prediction(glm_probs, data$threatened)
ROCRperf <- performance(ROCRpred, 'tpr', 'fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2, 1.7))
#didnt work, can I plot IUCN risk x P y ?

#something went on with the genl
vars_selected <- data %>%
  select(masscentr, rangecentr, Migration, clutchcentr, Habitat.Density, agriculture, climate_change, invasive, resource_use)

lm10 <- glm(threatened ~ masscentr * rangecentr * Migration * clutchcentr * Habitat.Density * agriculture * climate_change * invasive, resource_use, data=data, family=binomial)

#####CHECK:
#####clutch not enough data (will have to model taxonomically)

#result category should have only ex
library(janitor)
category <- janitor::tabyl(data, species, result.category)
category
table(data$result.category)
#test combinations
#marginal effects??

selected_var <- data %>%
  select(masscentr, rangecentr, Migration, genlcentr, Habitat.Density, agriculture, climate_change, invasive, resource_use)
selected_var_na <- na.omit(selected_var)
cor(selected_var_na) #0.71 between mass and genlength

#lm11 <- glm(threatened ~ masscentr + rangecentr + Migration + genlcentr + Habitat.Density + agriculture + climate_change + invasive + resource_use , data=data, family=binomial)
#summ(lm11)
#take out gen length because its correlated with mass but AIC drops
lm111 <- glm(threatened ~ masscentr + rangecentr + Migration + Habitat.Density + agriculture + climate_change + invasive + resource_use , data=data, family=binomial)
summ(lm111)
jtools::plot_summs(lm111, scale = TRUE, legend.title = "Threatened")

#lm12 <- glm(threatened ~ masscentr + rangecentr + Migration + genlcentr + agriculture + climate_change + invasive + resource_use , data=data, family=binomial)
#summ(lm12) 
#lets take away habitat, stick with lm11 (full_add_model.png)
#include 
lm13 <- glm(threatened ~ masscentr + rangecentr + Migration + agriculture + climate_change + invasive + resource_use + masscentr:resource_use, data=data, family=binomial)
summ(lm13) #significant interaction: masscentr:resource
jtools::plot_summs(lm13, scale = TRUE, legend.title = "Threatened")

lm14 <- glm(threatened ~ masscentr + rangecentr + Migration + agriculture + climate_change + invasive + resource_use + agriculture:invasive, data=data, family=binomial)
summ(lm14) #significant interaction: agriculture:invasive
jtools::plot_summs(lm14, scale = TRUE, legend.title = "Threatened")

#other interactions
lm15 <- glm(threatened ~ masscentr + rangecentr + Migration + agriculture + climate_change + invasive + resource_use + agriculture:resource_use, data=data, family=binomial)
summ(lm15)
 
# lm_multiple <- glm(threatened ~ masscentr + rangecentr + Migration + agriculture + climate_change + invasive + resource_use + invasive:agriculture:resource_use + resource_use:masscentr, data=data, family=binomial)
# summ(lm_multiple) #not as good even doing diff combinations

lm16 <- glm(threatened ~ masscentr + rangecentr + Migration + agriculture + climate_change + invasive + resource_use + climate_change:resource_use, data=data, family=binomial)
summ(lm16) #climate_change:resource_use is also significant but adding to the other doesnt work

lm17 <- glm(threatened ~ masscentr + rangecentr + Migration + agriculture + climate_change + invasive + resource_use + resource_use:invasive, data=data, family=binomial)
summ(lm17) #invasive:resource_use significant, added there and lowered AIC

#see interaction among traits
lm18 <- glm(threatened ~ masscentr + rangecentr + Migration + agriculture + climate_change + invasive + resource_use + masscentr:rangecentr , data=data, family=binomial)
summ(lm18) #masscentr:rangecentr significant but didnt add 

lm19 <- glm(threatened ~ masscentr + rangecentr + Migration + agriculture + climate_change + invasive + resource_use + rangecentr:Migration , data=data, family=binomial)
summ(lm19)

#combine lm13, lm14, lm15, lm17 ?? best so far 3614.39
lm_combined <- glm(threatened ~ masscentr + rangecentr + Migration + agriculture + climate_change + invasive + resource_use + masscentr:resource_use + agriculture:invasive + agriculture:resource_use + invasive:resource_use, data=data, family=binomial)
summ(lm_combined) #looks better
jtools::plot_summs(lm_combined, scale = TRUE, plot.distributions = TRUE, legend.title = "Threatened")
plot_coefs(lm_combined, scale = TRUE, plot.distributions = TRUE, legend.title = "Threatened", groups)
plot_summs(lm_combined,
           coefs = c("mass" = "masscentr", "range size" = "rangecentr", "Migration" = "migration", "agriculture", "climate change" = "climate_change", "resource use" = "resource_use", "invasive species" = "invasive",
                     "mass:resource use" = "masscentr:resource_use", "agriculture:invasive", "agriculture:resource use" = "agriculture:resource_use", "invasive:resource use" = "invasive:resource_use" ))
hist(data$rangecentr)
summary(data$rangecentr)

theme_set(theme_sjplot())

plot_model(lm_combined, type = "pred", terms = "masscentr [all]")
plot_model(lm_combined, type = "pred", terms = "rangecentr [all]")
plot_model(lm_combined, type = "pred", terms = "Migration")
plot_model(lm_combined, type = "pred", terms = "resource_use")
plot_model(lm_combined, type = "pred", terms = "agriculture")
plot_model(lm_combined, type = "pred", terms = "invasive")
plot_model(lm_combined, type = "pred", terms = "climate_change")

plot_model(lm_combined, type = "pred", terms = c("masscentr", "Migration", "rangecentr"))

plot_model(lm_combined, type = "pred", terms = c("agriculture", "rangecentr", "Migration"))
plot_model(lm_combined, type = "pred", terms = c("agriculture", "resource_use", "invasive"))
plot_model(lm_combined, type = "pred", terms = c("masscentr", "resource_use"))

plot_model(lm_combined, type = "pred", terms = c("masscentr", "agriculture"))

fig1 <- plot_model(lm_combined, type = "int", terms = c("invasive", "resource_use"))

fig2 <- plot_model(lm_combined, type = "int", terms = c("agriculture", "resource_use"))
fig2
plot_model(lm_combined, type = "int")

export_summs(lm_combined)
install.packages("margins")
library(margins)
margins <- margins(lm_combined)
export_summs(margins)


selected_var <- data %>%
  select(threatened, species, masscentr, rangecentr, Migration, agriculture, climate_change, invasive, resource_use)
nrow(selected_var)
#1 MIGRATION VALUE

model_na_ex <- na.exclude(selected_var)
nrow(model_na_ex)
write.csv(model_na_ex, "model_na_ex.csv")
model_na_ex <- read.csv("model_na_ex.csv")

#dredge to automatically select best model
#https://terpconnect.umd.edu/~egurarie/teaching/Biol709/Topic3/Lab11_GLMandModelSelection.html


lm_global <- glm(threatened ~ masscentr + rangecentr + Migration + agriculture + climate_change + invasive + resource_use, data=model_na_ex, family=binomial(logit), na.action = na.fail)

maineffects.glm <- glm(threatened ~ ., family=binomial(logit), data=model_na_ex, na.action = na.fail)

mega.model.comparison <- dredge(maineffects.glm)

# dd <- dredge(fm1)
# subset(dd, delta < 4)

head(mega.model.comparison)
library(MASS)
best.main.glm <- stepAIC(maineffects.glm)
anova(maineffects.glm, test="Chi")

interactions.glm <- glm(threatened ~ (.)^2, data=model_na_ex, family=binomial(logit), na.action = na.fail)
best.interaction.glm <- stepAIC(interactions.glm, trace = 0)
anova(best.interaction.glm, test = "Chi")
dd <- dredge(interactions.glm)

head(best.interaction.glm)
plot(dd)
#and need to plot using non transformed variables

summ(best.interaction.glm)

jtools::plot_summs(best.interaction.glm, scale = TRUE, plot.distributions = TRUE, legend.title = "Threatened")

######## using raw/logged variables -----
data <- data %>%
  mutate(logmass = log(Mass))

data <- data %>%
  mutate(logrange = log(Range.Size))

summ(fit.originalvar)

fit.originalvar_log <- glm(formula = threatened ~ logmass + logrange + Migration + 
      agriculture + climate_change + invasive + resource_use + 
      logmass:logrange + logmass:Migration + logmass:resource_use + 
      logrange:invasive + logrange:resource_use + Migration:climate_change + 
      agriculture:climate_change + agriculture:invasive + agriculture:resource_use + 
      invasive:resource_use, family = binomial(logit), data = data)
summ(fit.originalvar_log)

jtools::plot_summs(fit.originalvar, scale = TRUE, plot.distributions = TRUE, legend.title = "Threatened")

plot_model(fit.originalvar, type = "pred", terms = "Mass")
plot_model(fit.originalvar, type = "pred", terms = "Range.Size")
plot_model(fit.originalvar, type = "pred", terms = "Migration")
plot_model(fit.originalvar, type = "pred", terms = "resource_use")
plot_model(fit.originalvar, type = "pred", terms = "agriculture")
plot_model(fit.originalvar, type = "pred", terms = "invasive")
plot_model(fit.originalvar, type = "pred", terms = "climate_change")

plot_model(fit.originalvar, type = "pred", terms = c("Mass", "Migration", "Range.Size"))
#use logged mass

plot_model(fit.originalvar, type = "pred", terms = c("agriculture", "rangecentr", "Migration"))
plot_model(fit.originalvar, type = "pred", terms = c("agriculture", "resource_use", "invasive"))
plot_model(fit.originalvar, type = "pred", terms = c("masscentr", "resource_use"))

plot_model(fit.originalvar, type = "pred", terms = c("masscentr", "agriculture"))


####log

jtools::plot_summs(fit.originalvar_log, scale = TRUE, plot.distributions = TRUE, legend.title = "Threatened")
sjplot_pal(pal = "breakfast club")
p <- plot_model(fit.originalvar_log, type = "pred", terms = "logmass")
p + sjplot_pal(pal = "breakfast club")
sjPlot::plot_model(fit.originalvar, type = "pred", terms = "Range.Size [all]", pal = "breakfast club", show.data = TRUE)
plot_model(fit.originalvar, type = "pred", terms = "Migration")
plot_model(fit.originalvar, type = "pred", terms = "resource_use")
plot_model(fit.originalvar, type = "pred", terms = "agriculture")
plot_model(fit.originalvar_log, type = "pred", terms = "invasive")
plot_model(fit.originalvar_log, type = "pred", terms = "climate_change")

plot_model(fit.originalvar, type = "int", terms = c("Range.Size", "Mass"))

plot_model(fit.originalvar_log, type = "int", terms = c("logmass", "logrange"))
summary(fit.originalvar)

plot_model(fit.originalvar_log, type = "pred", terms = c("agriculture", "rangecentr", "Migration"))
plot_model(fit.originalvar_log, type = "pred", terms = c("agriculture", "resource_use", "invasive"))
plot_model(fit.originalvar_log, type = "pred", terms = c("masscentr", "resource_use"))

plot_model(fit.originalvar_log, type = "pred", terms = c("masscentr", "agriculture"))

##other type of plots 

show_sjplot_pals()


###########with hits data-------------

selected_var <- all01_birds_hits %>%
  select(threatened, species, masscentr, rangecentr, Migration, agriculture, climate_change, invasive, resource_use, hits)
nrow(selected_var)
#1 MIGRATION VALUE

model_na_ex <- na.exclude(selected_var)
nrow(model_na_ex)
write.csv(model_na_ex, "model_na_ex.csv")
model_na_ex <- read.csv("model_na_ex.csv")

#dredge to automatically select best model
#https://terpconnect.umd.edu/~egurarie/teaching/Biol709/Topic3/Lab11_GLMandModelSelection.html


lm_global <- glm(threatened ~ masscentr + rangecentr + Migration + agriculture + climate_change + invasive + resource_use + hits, data=model_na_ex, family=binomial(logit), na.action = na.fail)

maineffects.glm <- glm(threatened ~ ., family=binomial(logit), data=model_na_ex, na.action = na.fail)

mega.model.comparison <- dredge(maineffects.glm)

#all birds including dd without ew and ex

birds <- read.csv("data_global/data_analysis/analysis/all_birds_01_dd.csv")

#quantile graphs
install.packages("quantreg")
install.packages("caret")

# Loading the packages
library(quantreg)
library(dplyr)
library(ggplot2)
library(caret)

plot(birds$rangecentr ~ birds$masscentr)

# Model: Quantile Regression
Quan_fit <- rq(rangecentr ~ masscentr, data = birds)
Quan_fit

# Summary of Model
summary(Quan_fit)

# Plot
plot(rangecentr ~ masscentr, data = birds, pch = 16, main = "Plot")
abline(lm(rangecentr ~ masscentr, data = birds), col = "red", lty = 2)
abline(rq(rangecentr ~ masscentr, data = birds), col = "blue", lty = 2)

ratiodatalog <- birds %>%
  mutate(proprangelog = log(Range.Size)/log(Mass))

model_ratio <- glm(threatened ~ proprangelog, data=ratiodatalog, family=binomial)

summary(model_ratio)

plot(threatened ~ proprangelog, data = ratiodatalog, pch = 5, main = "Plot")
plot(ratiodatalog$proprangelog)
hist(ratiodata$proprangeraw)


library(effects)
library(jtools)
plot(allEffects(model_ratio))

jtools::plot_summs(model_ratio, scale = TRUE, legend.title = "Threatened")
summ(model_ratio)
