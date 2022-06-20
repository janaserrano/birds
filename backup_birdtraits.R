####################################################################
##Bird traits relate to threats
##Author: Janaina Serrano, McGill University. janaina.serrano@mail.mcgill.ca
####################################################################

#required packages
library("rredlist")
library("tidyverse")
library("purrr")
library("dplyr")

#load traits from Tobias et al. 2022
BirdLife <-read.csv("AVONET1_BirdLife.csv",h=T)

#loading IUCN data for feb2022
iucn_data <- read.csv("birds.csv",h=T)

iucn_selected <- iucn_data %>%
  select(result.taxonid, species = result.scientific_name, result.category)

iucn_selected

names(BirdLife)
birdlife_selected <- BirdLife %>%
  dplyr::rename(species = Species1)
head(birdlife_selected)


traits_birds <- birdlife_selected %>%
  inner_join(., iucn_selected, by = "species")
names(iucn_selected)

write.csv(traits_birds, file = "C:/Users/janas/OneDrive/Documentos/R/Birds/IUCN_data/traits_birds.csv")

traits <- read.csv("C:/Users/janas/OneDrive/Documentos/R/Birds/IUCN_data/traits_birds.csv")
names(traits)

selected_traits <- traits %>%
  select(species, result.category, Range.Size, Mass, Habitat, Migration)

write.csv(selected_traits, file= "C:/Users/janas/OneDrive/Documentos/R/Birds/IUCN_data/selected_traits.csv")
selected_traits <- read.csv("C:/Users/janas/OneDrive/Documentos/R/Birds/IUCN_data/selected_traits.csv")
head(selected_traits)

##let's use threatened vs nonthreatened

selected_traits %>%
  mutate(threatened = ifelse(result.category = LC, NT, "nonthreatened",
                                 ifelse(result.category = CR, EN, "medium", NA)))



##Red List package
library("rredlist")
# set your api key as an environmental variable so you do not upload
Sys.setenv(IUCN_KEY = "b15c9ee9773c61097356e025c43e685b73d301d287373a7470117f8429939210")
# now call the variable that you set
Sys.getenv("IUCN_KEY")
apikey <- Sys.getenv("IUCN_KEY")

test <- rl_threats("Gorilla gorilla", key = apikey)

test

#extracting bird species
birds <- rl_comp_groups('birds', key = apikey)
birds

write.csv(birds, file = "R/Tobias2022/IUCN_data/birds.csv")
birds <- read.csv(file = "birds.csv")


###########################################
#Original: make a data frame of the species you want to iterate over
df <- tribble(
  ~species_names, 
  "Pan paniscus",
  "Gorilla gorilla",
  "Chiroxiphia linearis",
  "Lemur catta"
) %>% # now apply the rl_search function to each species using purrr::map()
  mutate(iucn_pull = map(species_names, rl_threats, key = apikey)) 


#doesnt work
write.csv(df, file =  "df.csv")

##Error in utils::write.table(df, file = "df.csv", col.names = NA, sep = ",",  : 
#unimplemented type 'list' in 'EncodeElement'
#Error in (function (..., na.last = TRUE, decreasing = FALSE, method = c("auto",  : 
#unimplemented type 'list' in 'orderVector1'
#Error: VECTOR_ELT() can only be applied to a 'list', not a 'closure'

#from https://stackoverflow.com/questions/14521492/weird-behaviour-by-ordering-a-data-frame
df2 <- as.data.frame(lapply(df, unlist))

write.csv(df2, file = "df.csv")

#still not what I want because there are only two columns of data

#this saves only for one species
write.csv((df[[2]][[1]][["result"]]), "df21result.csv")

#convert into wide format?
#for canadian species
birds_ca <- read.csv(file = "AC_traits_data.csv")
#DOM: sp = tibble(Species_names = birds$result.scientific_name[c(1:10)])
sp = tibble(Species_names = birds_ca$Scientific.name)

ca_threats <- sp %>% # now apply the rl_search function to each species using purrr::map()
mutate(iucn_pull = map(Species_names, rl_threats, key = apikey)) 


#for all species
#sp = tibble(Species_names = birds$result.scientific_name)

#df <- sp %>% # now apply the rl_search function to each species using purrr::map()
  #mutate(iucn_pull = map(Species_names, rl_threats, key = apikey)) 



#try this way: 1 LC 2 NT 3 VU 4 EN 5 CR

#################################################

df_table
# make a data frame of the species you want to iterate over
df_threats <- tribble(~species_names = book_test$scientificName
) %>% # now apply the rl_threats function to each species using purrr::map()
  
  mutate(iucn_pull = map(species_names, rl_threats, key = apikey)) 


# if you look at the df object the iucn_pull column is a list with a lot of information that we are not interested in. So we can use purr::pluck to get the elements we want!
df_threats
# we will need to look at the list format to see what elements of the api we are interested in are named. In this case we want to go into the "result" list and pull out the value from "category." 

api_clean <- df %>% 
  mutate(category = map_chr(iucn_pull, pluck, "result", "category")) %>% 
  select(species_names, category)

api_clean

book_test <- read.csv(file="book_test.csv")


#this didnt work because for each species we have many lines
api_threats <- df_threats %>% 
  mutate(title = map_chr(iucn_pull, pluck, "result", "title")) %>% 
  select(species_names, title)



api_clean

####DOM SAVED ME


# getting threats for Aburria aburri
test <- rl_threats('Aburria aburri', key = apikey)

birds <- read.csv("birds.csv")
names(birds)

##DOM's original ###########################################
#sp_list   <-  book_test$species_names
sp_list <- birds$result.scientific_name[c(1:10)]

df  <-  c()
for (sp in sp_list){
  x  <-  rl_threats(sp, key = apikey)$result
  if (length(x)!=0){
    x$sp <- sp
    df <- rbind(df, x)
  }
}
#############################################################
#first hundred species
sp_list500 <- birds$result.scientific_name[c(101:500)]


df  <-  c()
for (sp in sp_list500){
  x  <-  rl_threats(sp, key = apikey)$result
  if (length(x)!=0){
    x$sp <- sp
    df <- rbind(df, x)
  }
}
 
 
 df

 
##1000 
 sp_list1000 <- birds$result.scientific_name[c(501:1000)]
 
 df2  <-  c()
 for (sp in sp_list1000){
   x  <-  rl_threats(sp, key = apikey)$result
   if (length(x)!=0){
     x$sp <- sp
     df2 <- rbind(df2, x)
   }
 }
 
 #saving a file for 1000 threats 
 
 write.csv(df2,"data_global/501-1000.csv")

#1001-5000

sp_list5000 <- birds$result.scientific_name[c(1001:5000)]

df3  <-  c()
for (sp in sp_list5000){
  x  <-  rl_threats(sp, key = apikey)$result
  if (length(x)!=0){
    x$sp <- sp
    df3 <- rbind(df3, x)
  }
}

#saving a file for 1001-5000
write.csv(df3,"data_global/1001-5000.csv")

#5001-10000

sp_list10000 <- birds$result.scientific_name[c(5001:10000)]

df4  <-  c()
for (sp in sp_list10000){
  x  <-  rl_threats(sp, key = apikey)$result
  if (length(x)!=0){
    x$sp <- sp
    df4 <- rbind(df4, x)
  }
}
# stopped at 8727 
sp_list10000 <- birds$result.scientific_name[c(8727:10000)]

df5  <-  c()
for (sp in sp_list10000){
  x  <-  rl_threats(sp, key = apikey)$result
  if (length(x)!=0){
    x$sp <- sp
    df5 <- rbind(df5, x)
  }
}

#saving a file for 5001-10000
write.csv(df5,"data_global/8727-10000.csv")
head(df5)
#10001-11162

sp_list11162 <- birds$result.scientific_name[c(10001:11162)]

df6  <-  c()
for (sp in sp_list11162){
  x  <-  rl_threats(sp, key = apikey)$result
  if (length(x)!=0){
    x$sp <- sp
    df6 <- rbind(df6, x)
  }
}

#saving a file for 5001-10000
write.csv(df6,"data_global/10000-11162.csv")
head(df6)

getwd()
#loading file
Aaburri <- read.csv("R/Tobias2022/IUCN_data/Aaburri.csv")
#selecting species name, threat code, title and severity
Aaburri_threats1 <- Aaburri %>%
  select(name, result.code, result.title,  result.severity)
#joining threat category
joined <- Aaburri_threats1 %>% filter(name == 'Aburria aburri') %>% left_join(., book_test, by = "name")

#now do this for all of them?

test <- rl_threats('Gorilla gorilla', key = apikey)
test


lapply(book_test,1,sum)

rl_threats('Abroscopus schisticeps', key = apikey)

rl_comp_groups(group = NULL, key = NULL, parse = TRUE, ...)
rl_countries(key = apikey)

rl_threats('22715451', key = apikey)
