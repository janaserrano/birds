### Abbie Gail Jones
##Missforest

library(missForest)
type = “opt” #input either “opt” for optimized parameters, or “base” for base package ones.
setwd(“~/Documents/University Work/Masters : PhD/Project/Data/TRY”)
ID_list <- readRDS(“~/Documents/University Work/Masters : PhD/Project/Data/Combined Species Lists/final_parsedlistmodel_1970_eva10.rds”)
try <- readRDS(“newtry.rds”)
try_formiss <- try[, -c(1)]
cat <- c(“dispersal_syndrome”, “leaf_compoundness”, “Raunkiaer_lifeform”, “lifespan”, “growth_form”)
try_formiss[,cat] <- lapply(try_formiss[,cat], as.factor)
if (type == “opt”)
{
  misstry <- missForest(xmis = try_formiss, maxiter = 100, ntree = 100, mtry = floor(sqrt(ncol(try_formiss))), variablewise = TRUE, decreasing = FALSE, replace = TRUE, classwt = NULL, cutoff = NULL, strata = NULL, parallelize = c(‘no’), verbose = TRUE)
}
if (type == “base”)
{
  misstry <- missForest(xmis = try_formiss, verbose = TRUE)
}
filled <- misstry$ximp
filled$species <- try$species
filled$speciesID <- ID_list$ID[match(filled2$species, ID_list$acceptedSpecies)]
filled <- filled[, c((ncol(try_formiss)+1), (ncol(try_formiss)+2), 1:ncol(try_formiss))]
#Add in uncleaned gbif observations.
filled$n_obs <- ID_list$numberOfOccurrences[match(filled$species, ID_list$acceptedSpecies)]
filled$n_obs[which(is.na(filled$n_obs))] <- 0
saveRDS(filled, file = “traits_filled_misstry.rds”)