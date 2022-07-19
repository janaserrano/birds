####################################################################
##downloading bird observations from GBIF
##Author: Janaina Serrano, McGill University. janaina.serrano@mail.mcgill.ca
##Date: 2022-07-13
####################################################################
# https://github.com/ropensci/gbifdb
# https://docs.ropensci.org/gbifdb/
# install.packages("devtools")
#devtools::install_github("ropensci/gbifdb")

library(gbifdb)
library(dplyr)

#To begin working with GBIF data directly without downloading the data first, simply establish a remote connection using

gbif <- gbif_remote()

#We can now perform most dplyr operations:
  
chordata <- gbif %>%
  filter(phylum == "Chordata", year > 1990) %>%
  count(class, year) %>%
  collect()

aves_family <- gbif %>%
  filter(class == "Aves", year >= 2004) %>%
  count(family, year) %>%
  collect()

write.csv(aves_family, "data_global/gbif/aves_family.csv")

aves_species <- gbif %>%
  filter(class == "Aves", year >= 2004) %>%
  count(species, year) %>%
  collect()
#started 3:12

#maybe try this?

growth <- gbif %>%
  filter(phylum == "Chordata", year > 1990) %>%
  count(class, year) %>% arrange(year)

write.csv(aves_species, "data_global/gbif/aves_species.csv")
aves_species <- read.csv("data_global/gbif/aves_species.csv")

aves_sum <- aves_species %>%                                        
  group_by(species) %>%                         
  summarise_at(vars(n),             
               list(count = sum))

aves_mean_sum_na <- aves_species %>%                                        
  group_by(species) %>%                         
  summarise_at(vars(n),             
               list(mean = mean, sum = sum))

write.csv(aves_mean_sum, "data_global/gbif/aves_mean_sum.csv")

agg_sum<-aggregate(aves_species$n, by=list(species=aves_species$species), FUN=sum)
agg_mean<-aggregate(aves_species$n, by=list(species=aves_species$species), FUN=mean)


library(ggplot2)
install.packages("forcats")
library(forcats)
# GBIF: the global bird information facility?
aves_species %>%
  collect() %>%
  mutate(species = fct_lump_n(class, 6)) %>%
  ggplot(aes(year, n, fill=species)) + geom_col() +
  ggtitle("GBIF observations of birds by species")

growth <- gbif %>%
  filter(phylum == "Chordata", year > 1990) %>%
  count(class, year) %>% arrange(year)

# growth %>%
#   collect() %>%
#   mutate(class = fct_lump_n(class, 6)) %>%
#   ggplot(aes(year, n, fill=class)) + geom_col() +
#   ggtitle("GBIF observations of vertebrates by class")

aves_family %>%
  collect() %>%
  ggplot(aes(year, n, fill=family)) + geom_col() +
  ggtitle("GBIF observations of birds by family")


ggplot(aves_family, aes(fill=family, y=n, x=year)) + 
  geom_bar(position="dodge", stat="identity")


#Anatidae
ggplot(aves_family, aes(fill="Machaerirhynchidae", y=n, x=year)) + 
  geom_bar(position="dodge", stat="identity")

