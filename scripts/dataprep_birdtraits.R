####################################################################
##Bird traits (AVONET) & threats(IUCN)
##Author: Janaina Serrano, McGill University. janaina.serrano@mail.mcgill.ca
####################################################################

#required packages
library("rredlist")
library("tidyverse")
library("janitor")

#load traits from Tobias et al. 2022
BirdLife <-read.csv("AVONET1_BirdLife.csv",h=T)
BirdLife
#loading IUCN data for feb2022
iucn_data <- read.csv("birds.csv",h=T)
iucn_data
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
############################################################# extract threats
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
write.csv(df5,"data_global/5001-10000.csv")
# stopped at 8727 

#sp_list10000 <- birds$result.scientific_name[c(8728:10000)]

#df5  <-  c()
#for (sp in sp_list10000){
#  x  <-  rl_threats(sp, key = apikey)$result
#  if (length(x)!=0){
#    x$sp <- sp
#    df5 <- rbind(df5, x)
#  }
#}

#saving a file for 5001-10000
#write.csv(df5,"data_global/8728-10000.csv")
#head(df5)
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
write.csv(df6,"data_global/10001-11162.csv")
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


#combining 7 output tables

one <- read.csv("data_global/0-100.csv")

two <- read.csv("data_global/101-500.csv")
three <- read.csv("data_global/501-1000.csv")

four <- read.csv("data_global/1001-5000.csv")

five <- read.csv("data_global/5001-8727.csv")

six <- read.csv("data_global/8728-10000.csv")

seven <- read.csv("data_global/10001-11162.csv")

#binding one and two

cat("First Data Frame: ", "\n")
df1
cat("Second Data Frame: ", "\n")
df2

combinedDf <- rbind(one, two)
cat("Combined by rows: ", "\n")
combinedDf

combined123 <- rbind(combinedDf, three)
cat("Combined by rows: ", "\n")
combined123

combined1234 <-rbind(combined123, four)
cat("Combined by rows: ", "\n")
combined1234

combined12345 <-rbind(combined1234, five)
cat("Combined by rows: ", "\n")
combined12345

combined123456 <- rbind(combined12345, six)
cat("Combined by rows: ", "\n")
combined123456

combined1234567 <- rbind(combined123456, seven)
cat("Combined by rows: ", "\n")
combined1234567

write.csv(combined1234567, file = "data_global/birds_threats.csv")


#need to merge threats with traits
traits_global <- read.csv(file = "C:/Users/janas/OneDrive/Documentos/R/Birds/IUCN_data/data_global/traits_birds.csv")
threats_global <- read.csv(file = "C:/Users/janas/OneDrive/Documentos/R/Birds/IUCN_data/data_global/birds_threats.csv")

threats_global <- threats_global %>%
  dplyr::rename(species = sp)

traits_threats_global <- threats_global %>%
  inner_join(., traits_global, by = "species")
traits_threats_global

write.csv(traits_threats_global, "data_global/traits_threats_global.csv")

##arranging the table #didnt work
traits_threats_wide_code <- traits_threats %>% 
  pivot_wider(names_from = Scientific.name, values_from = code)

wide_code <- write.csv(traits_threats_wide_code, file = "C:/Users/janas/OneDrive/Documentos/R/Birds/IUCN_data/wide_code.csv")

wide_code_can <- read.csv("C:/Users/janas/OneDrive/Documentos/R/Birds/IUCN_data/wide_code.csv")


#read table
traits_threats_global <-read.csv("C:/Users/janas/OneDrive/Documentos/R/Birds/IUCN_data/data_global/traits_threats_global.csv",h=T)
table(traits_threats_global$species)

species_code <- tabyl(all_df, species, code)
species_code
write.csv(species_code, file = "species_code_test.csv")
species_code_test <-read.csv("C:/Users/janas/OneDrive/Documentos/R/Birds/IUCN_data/species_code_test.csv",h=T)
nrow(species_code_test) #3457 rows = species with threats
names(species_code_test)
species_code
species_category <- tabyl(traits_threats_global, species, result.category)
species_category


#now need to get species that are not threatened
##but we are only using them for threatened vs non threatened, not for specific threats
iucn_data <- read.csv("birds.csv",h=T)
iucn_data

birds <- iucn_data %>%
  dplyr::rename(species = result.scientific_name)
head(birds)


all_birds <- birds %>%
  inner_join(., traits_threats_global, by = "species")
names(all_birds)

##nao deu certo
install.packages("compare")
library("compare")
comparedf(iucn_data,traits_threats_global)

require(dplyr) 
anti_join(iucn_data,traits_threats_global)
anti_join(traits_threats_global,iucn_data)


traits_birds <-read.csv("data_global/traits_birds.csv",h=T)

no_threats <- traits_threats_global %>%
  select(X, code, title,	timing,	scope,	severity,	score,	invasive,	species)


no_threats = subset(traits_threats_global, select = -c(Sequence, Family1,	Order1,	Avibase.ID1,	Total.individuals,	Female,	Male,	Unknown,	Complete.measures,	Beak.Length_Culmen,	Beak.Length_Nares,	Beak.Width,	Beak.Depth,	Tarsus.Length,	Wing.Length,	Kipps.Distance,	Secondary1,	Hand.Wing.Index,	Tail.Length,	Mass,	Mass.Source,	Mass.Refs.Other,	Inference,	Traits.inferred,	Reference.species,	Habitat,	Habitat.Density,	Migration,	Trophic.Level,	Trophic.Niche,	Primary.Lifestyle,	Min.Latitude,	Max.Latitude,	Centroid.Latitude,	Centroid.Longitude,	Range.Size,	result.taxonid) )
no_threats
head(no_threats)
names(no_threats)
names(traits_birds)

#so o nome traits_birds e comparar com threats

all_traits <- traits_birds %>%
  select(species)

threatened_sp <- traits_threats_global  %>%
  select(species)

non_threatened <- anti_join(all_traits,threatened_sp)

write.csv(non_threatened, "non_threatened.csv")
traits_non_threatened <- semi_join(traits_birds, non_threatened, by="species")
names(traits_non_threatened)
write.csv(traits_non_threatened, "data_global/traits_non_threatened.csv")
traits_non_threatened <- read.csv("data_global/traits_non_threatened.csv")
nrow(traits_non_threatened)


non_threatened_cats <- janitor::tabyl(traits_non_threatened, species, result.category)
non_threatened_cats
table(traits_non_threatened$result.category)


# 7401 no threats -   DD   EN   LC   NT   VU 14    1 7380    5    1 
# threats for 3457
# traits for 10,858.

write.csv(names, "names_w_threats.csv")
getwd()
#Now add rows
all_df <- bind_rows(traits_threats_global, traits_non_threatened)      
names(all_df)

write.csv(all_df, "data_global/all_birds_df.csv")

all_df <- read.csv("data_global/all_birds_df.csv", h=T)
print(all_df$result.category)


#NEED TO GROUP THREATENED (VU, EN, CR) VS NON-THREATENED (LC, NT)
all_df$result.category<-c(DD=0,LC=1,NT=2,VU=3,EN=4,CR=5,EW=6,EX=7)[all_df$result.category]
all_df$category.n
nrow(all_df) #34644

all_df <- all_df %>%
  dplyr::rename(category.n = result.category)

dd <- filter(all_df, category.n==0) #data deficient species
dd
write.csv(dd, file = "data_global/dd_sp.csv")
ex <- filter(all_df, category.n==6) #extinct occurrences (multiple lines same species)
write.csv(ex, file = "data_global/ex_sp.csv")
ew <- filter(all_df, category.n==7)
write.csv(ew, file = "data_global/ew_sp.csv")
no_dd <- filter(all_df, category.n != 0) #
nrow(no_dd)
no_dd_ex_ew <- filter(no_dd, !category.n %in% c(6, 7))
no_dd_ex_ew$category.n
#data-deficient, threatened vs non-threatened (DD=0,LC=1,NT=2,VU=3,EN=4,CR=5)
dd_others
dd_others <- filter(all_df, !category.n %in% c(6, 7))
species_cat <- janitor::tabyl(dd_others, species, category.n)
species_cat
dd_others_th <- dd_others %>% 
  mutate(threatened = case_when(category.n <= 2 ~ "non-threatened", 
                                category.n >= 3 ~ "threatened")) 
dd_others_th$threatened<-c("non-threatened" = 0,"threatened" = 1)[dd_others_th$threatened]
species_threatened <- janitor::tabyl(dd_others_th, species, threatened)

dd_others_th
write.csv(dd_others_th, file = "data_global/dd_others_th.csv")

#only threatened vs non-threatened (LC=1,NT=2,VU=3,EN=4,CR=5)
species_cat <- janitor::tabyl(no_dd_ex_ew, species, category.n)
species_cat



threat_st<- no_dd_ex_ew %>% 
  mutate(threatened = case_when(category.n <= 2 ~ "non-threatened", 
                                category.n >= 3 ~ "threatened")) 
threat_st$threatened
species_threatened <- janitor::tabyl(threat_st, species, threatened)
species_threatened
#so far so good

threat_st$threatened<-c("non-threatened" = 0,"threatened" = 1)[threat_st$threatened]
species_threatened <- janitor::tabyl(threat_st, species, threatened)
species_threatened
head(threat_st$threatened)

write.csv(threat_st, file = "data_global/01threatened.csv")
#(LC=1,NT=2,VU=3,EN=4,CR=5) couldnt do

traits_threats_global <-read.csv("data_global/traits_threats_global.csv",h=T)

sp_cats <- traits_threats_global  %>%
  select(species,result.category)

selected_cats <- sp_cats %>%
  inner_join(., threat_st, by = "species")

nrow(selected_cats)

th_nth_dd <- rbind(threat_st)
cat("Combined by rows: ", "\n")

######now setting up the categories#####couldn't
birds <- read.csv("data_global/01threatened.csv")
str(birds)
library(janitor)
species_codes <- tabyl(birds, species, code)
species_codes
nrow(species_codes) #10802 species without dd, ex, ew
write.csv(species_codes, file = "data_global/sp_codes.csv")

#original:
traits_threats_global <-read.csv("data_global/traits_threats_global.csv",h=T)
species_orig <- tabyl(traits_threats_global, species, code)
species_orig #just species with threats
nrow(species_orig) #3457

#dd
dd_others <- read.csv("data_global/dd_others_th.csv")
species_dd <- tabyl(dd_others, species, code)
species_dd #just species with threats

dd <- read.csv("data_global/dd_sp.csv")
#example: Acrocephalus orinus has threats but is DD so I should look at dd and others
species_title_dd <- tabyl(dd_others, species, title)
species_title_dd 
#just species with threats
write.csv(species_title_dd, file = "data_global/sp_title.csv")

cat_dd_others<- dd_others %>% 
 # mutate(threat_code = case_when(code < 2 ~ "1", 
 #                                code >= 2 & code < 3 ~ "2" 
 #                                code >= 3 & code < 4 ~ "3"
 #                                code >= 4 & code < 5 ~ "4"
 #                                 code >= 5 & code < 6 ~ "5"
 #                               code >= 6 & code < 7 ~ "6" ) %>% View()

         
  GetThreatCodesPhases <-function()
  {
    threat.codes.1 = list(
      'Habitat conversion' = c(1,2,5.3),
      'Residential' = c(1),
      'Agriculture' = c(2),
      'Logging' = c(5.3),
      'Hunting' = c(5.1),
      'Invasives'= c(8.1),
      'Pollution' = c(9), 
      'Climate change' = c(11),
      'AnyThreat' = c(1,2,5.3,5.1,8.1,9,11)
    )
    #Already run from Phase 1
    
    threat.codes.2 = list( 
      'Fire'= c(7.1),
      'Human disturbance'  = c(6)
    )
    
    
    
    list(phase1 = threat.codes.1,
         phase2 = threat.codes.2)
  }

threat_st<- no_dd_ex_ew %>% 
  mutate(threatened = case_when(category.n == 2, ~ "non-threatened", 
                                category.n >= 3 ~ "threatened")) 
all_df
all_df$result.category<-c(1=1, 1.1=1, 1.2=1)[all_df$result.category]
all_df$result.category<-c(DD=0,LC=1,NT=2,VU=3,EN=4,CR=5,EW=6,EX=7)[all_df$result.category]
species_code <- tabyl(dd_others, species, code)
species_code 

data[] <- lapply(all_df, gsub, pattern = "2.1.1", replacement = "2", fixed = TRUE)
######now setting up the categories#####couldn't
install.packages("stringr")
library("stringr")
dd_others[c("code1", "code2", "code3")] <- str_split_fixed(dd_others$code, '\\.', 3)

dd_others
write.csv(dd_others,"data_global/data_analysis/birds_codes.csv")

birds_codes <-read.csv("data_global/data_analysis/birds_codes.csv",h=T)
library("janitor")
species_code1 <- tabyl(birds_codes, species, code1)
species_code1
threats_mass <- tabyl(birds_codes, code1, Mass)
threats_mass

selected_c <- birds_codes %>%
  select(species, threatened, code1, category.n)

selected_traits <-birds_codes %>%
  select(species, Range.Size, Mass, Primary.Lifestyle, Migration, Habitat)

selected <- selected_c %>%
  inner_join(., selected_traits, by = "species")


agri <-  selected %>% mutate_if(is.numeric, code1 != 2)

#code agriculture (2)

species_code1 <- tabyl(birds_codes, species, code1)
species_code1


species_code1$agriculture <-factor(ifelse(species_code1$`2`<1,0,1))
species_code1$invasive <-factor(ifelse(species_code1$`8`<1,0,1))
species_code1$residential <-factor(ifelse(species_code1$`1`<1,0,1))
species_code1$energy <-factor(ifelse(species_code1$`3`<1,0,1))
species_code1$transportation <-factor(ifelse(species_code1$`4`<1,0,1))
species_code1$resource_use <-factor(ifelse(species_code1$`5`<1,0,1))
species_code1$instrusion <-factor(ifelse(species_code1$`6`<1,0,1))
species_code1$nat_sys_modif <-factor(ifelse(species_code1$`7`<1,0,1))
species_code1$pollution <-factor(ifelse(species_code1$`9`<1,0,1))
species_code1$geological <-factor(ifelse(species_code1$`10`<1,0,1))
species_code1$climate_change <-factor(ifelse(species_code1$`11`<1,0,1))
species_code1$other <-factor(ifelse(species_code1$`12`<1,0,1))
species_code1

write.csv(threats_binary1,"data_global/data_analysis/threats_binary.csv")
threats_binary1 <- species_code1[ -c(2:14) ]

threats_binary <-read.csv("data_global/data_analysis/threats_binary.csv")
threats_binary

traits_birds <- read.csv("data_global/traits_birds.csv")

traits_threats_cat <- traits_birds %>%
  inner_join(., threats_binary, by = "species")
names(traits_threats_cat)


traits_threats_cat$result.category<-c(DD=0,LC=1,NT=2,VU=3,EN=4,CR=5,EW=6,EX=7)[traits_threats_cat$result.category]
cats <- tabyl(traits_threats_cat, species, result.category)
cats

th <- traits_threats_cat %>% 
  mutate(threatened = case_when(result.category <= 2 ~ "non-threatened", 
                                result.category >= 3 ~ "threatened")) 
th
th$threatened<-c("non-threatened" = 0,"threatened" = 1)[th$threatened]
th
species_threatened <- janitor::tabyl(th, species, threatened)
species_threatened

write.csv(th,"data_global/data_analysis/threats_binary_cat.csv")

traits_birds_cat <- read.csv("data_global/data_analysis/threats_binary_cat.csv")
traits_birds_cat


#nodd:
traits_birds_cat_nodd <- read.csv("data_global/data_analysis/threats_binary_cat_nodd.csv")

#2022-04-21 just the ones that have threats traits_global and species_code_test
#species_code_test and traits_global

sp_threats = subset(threats_global, select = c(code, sp) )
sp_threats

sp_threats <- sp_threats  %>%
  dplyr::rename(species = sp)

w_threats_only <- sp_threats %>%
  inner_join(., traits_global, by = "species")
names(w_threats_only)

w_threats_only = subset(w_threats_only, select = -c(code) )

library("stringr")
w_threats_only[c("code1", "code2", "code3")] <- str_split_fixed(w_threats_only$code, '\\.', 3)
names(w_threats_only)
nrow(w_threats_only)
species_threats <- tabyl(w_threats_only,species,code1)
species_threats
nrow(species_threats)
#add to traits_birds
only_threatstraits <- traits_global %>%
  inner_join(., species_threats, by = "species")
names(only_threatstraits)
nrow(only_threatstraits) #yeahhhhhh

#changing category
only_threatstraits$result.category<-c(DD=0,LC=1,NT=2,VU=3,EN=4,CR=5,EW=6,EX=7)[only_threatstraits$result.category]

only_threatstraits <- only_threatstraits %>% 
  mutate(threatened = case_when(result.category <= 2 ~ "non-threatened", 
                                result.category >= 3 ~ "threatened")) 
only_threatstraits$threatened<-c("non-threatened" = 0,"threatened" = 1)[only_threatstraits$threatened]

table(only_threatstraits$threatened) #interesting    0    1 2030 1427

#now rename threats
only_threatstraits$agriculture <-factor(ifelse(only_threatstraits$`2`<1,0,1))
only_threatstraits$invasive <-factor(ifelse(only_threatstraits$`8`<1,0,1))
only_threatstraits$residential <-factor(ifelse(only_threatstraits$`1`<1,0,1))
only_threatstraits$energy <-factor(ifelse(only_threatstraits$`3`<1,0,1))
only_threatstraits$transportation <-factor(ifelse(only_threatstraits$`4`<1,0,1))
only_threatstraits$resource_use <-factor(ifelse(only_threatstraits$`5`<1,0,1))
only_threatstraits$instrusion <-factor(ifelse(only_threatstraits$`6`<1,0,1))
only_threatstraits$nat_sys_modif <-factor(ifelse(only_threatstraits$`7`<1,0,1))
only_threatstraits$pollution <-factor(ifelse(only_threatstraits$`9`<1,0,1))
only_threatstraits$geological <-factor(ifelse(only_threatstraits$`10`<1,0,1))
only_threatstraits$climate_change <-factor(ifelse(only_threatstraits$`11`<1,0,1))
only_threatstraits$other <-factor(ifelse(only_threatstraits$`12`<1,0,1))
names(only_threatstraits)
nrow(only_threatstraits)
only_threatstraits <- only_threatstraits[ -c(41:52) ]
only_threatstraits <- only_threatstraits[ -c(1) ]

write.csv(only_threatstraits, "C:/Users/janas/OneDrive/Documentos/R/Birds/IUCN_data/data_global/data_analysis/only_threatened/only_threatened.csv")

##############need to get species common names #didnt work
# https://bioinformatics.stackexchange.com/questions/578/how-to-convert-species-names-into-common-names
install.packages("taxize")
library(taxize)

#only_w_threats
sp_list <- only_w_threats$species[c(1:50)]

#species <- c('Helianthus annuus', 'Mycobacterium bovis', 'Rattus rattus', 'XX', 'Mus musculus')

options(ENTREZ_KEY="efbcb84043b6358e6ffc01354a4e8dd83a08")

uids <- get_uid(sp_list)
#efbcb84043b6358e6ffc01354a4e8dd83a08

# keep only uids which you have in the database
uids.found <- as.uid(uids[!is.na(uids)])
#Too many requests 
#Sys.sleep
# keep only species names  corresponding to your ids
species.found <- species[!is.na(uids)]

common.names <- sci2comm(uids.found, db = 'ncbi')

names(common.names) <- species.found

species.founddf <- as.data.frame(species.found)
#trying to save 
write.csv(species.found$Value, "commonnames.csv")

attributes(common.names)$'Accipiter badius'
common.names[["Accipiter gentilis"]]
#[1] "Northern goshawk"

df2  <-  c()
for (species in sp_list){
  common.names <- sci2comm(uids.found, db = 'ncbi')
  if (length(common.names)!=0){
    common.names$sp <- sp
    df2 <- rbind(df2, common.names)
  }
}

common.names[[10]]

common.names[[]]

df3 <- do.call(rbind.data.frame, common.names)

df <- data.frame(matrix(unlist(common.names), nrow=length(common.names), byrow=TRUE))

#try to save list https://stackoverflow.com/questions/27594541/export-a-list-into-a-csv-or-txt-file-in-r
lapply(mylist, function(x) write.table( data.frame(x), 'test.csv'  , append= T, sep=',' ))

readLines(file.path(dir, "Platanaceae.csv"), n = 5)

## or pipe the src to sql_collect
install.packages("taxizedb")
library("taxizedb")
common.names %>% sql_collect("select * from hierarchy limit 5")
dataframe <- as.data.frame(common.names)

#maybe this one https://ropensci.github.io/taxize/
out <- lapply(list("Helianthus debilis", "Astragalus aduncus"), function(x) ubio_namebank(searchName = x, sci = 1, vern = 0))
head(out[[2]][, -c(2, 3)])  


#saving list as df
capture.output(common.names, file = "test_cn.csv")
tib <- tibble(norm_col = 1:3,
              list_col = list(1:5, 6:10, 11:15))

tib %>% mutate(list_col = map_chr(list_col, ~ capture.output(dput(.)))) %>%
  write_csv("output.csv")

tib_saved <- read_csv("output.csv") %>%
  mutate(list_col = map(list_col, . %>% (rlang::parse_expr)() %>% eval()))
#> Parsed with column specification:
#> cols(
#>   norm_col = col_integer(),
#>   list_col = col_character()
#> )

all_equal(tib %>% unnest(),
          tib_saved %>% unnest())

sapply(only_w_threats,function(x) sum(is.na(x)))

library("taxadb")
cnames <- filter_name(name = "Pied Tamarin", provider = "col")
td_create("col")
filter_common


# https://github.com/ropensci/taxadb/blob/master/R/filter_common.R

filter_common("Pied Tamarin")


#using birdlife table!!!
###ok to run - mass and range logged and centered (-mean), wing length (-mean / 2std)
centered_scaled <- read.csv("C:/Users/janas/OneDrive/Documentos/R/Birds/IUCN_data/data_global/data_analysis/only_threatened/centered_scaled.csv")
birdlife_query <- read.csv("C:/Users/janas/OneDrive/Documentos/R/Birds/IUCN_data/data_global/data_analysis/common_names/birdlife_query.csv")

birdlife_selected <- birdlife_query %>%
  select(species = Scientific.name, English.name)

data_common_names <- birdlife_selected %>%
  inner_join(., centered_scaled, by = "species")
nrow(centered_scaled)
nrow(data_common_names)
###same number of species!! 3381

write.csv(data_common_names,"C:/Users/janas/OneDrive/Documentos/R/Birds/IUCN_data/data_global/data_analysis/only_threatened/data_common_names.csv")
data_common_names <- read.csv("data_global/data_analysis/only_threatened/analysis/data_common_names.csv")



################## add clutch size and generation length to common_names ------------------
data_common_names <- read.csv("data_global/data_analysis/only_threatened/analysis/data_common_names.csv")

#generation length: Bird et al. 2020: https://doi.org/10.1111/cobi.13486 
#clutch size: amniote Myhrvold et al. 2015; https://doi.org/10.1890/15-0846R.1 
gl_birds <- read.csv("GL_bird2020/cobi13486-sup-0004-tables4.csv") #genlength
amniote <- read.csv("amniote/Data_Files/Amniote_Database_Aug_2015.csv") #clutchsize

gl_birds <- gl_birds %>%
  dplyr::rename(species = Scientific.name)

amniote_name <- transform(amniote, Scientific.name=paste(genus, species))

amniote_name <- amniote_name %>%
  dplyr::rename(species_lat = species)
amniote_name <- amniote_name %>%
  dplyr::rename(species = Scientific.name)

amniote_selected <- amniote_name %>%
  select(species, litter_or_clutch_size_n)

data_common_names_amnio <- data_common_names %>%
  left_join(., amniote_selected , by = "species")

print(data_common_names_amnio)

gl_birds_selected <- gl_birds %>%
  select(species, GenLength)

data_common_names_amnio_gl <- data_common_names_amnio %>%
  left_join(., gl_birds_selected , by = "species")
install.packages("naniar")
library("naniar")

data_NA <- data_common_names_amnio_gl %>% replace_with_na(replace = list(litter_or_clutch_size_n = -999))

table(data_NA$litter_or_clutch_size_n)

data_NA_clutch <- data_NA %>%
  dplyr::rename(clutch.size = litter_or_clutch_size_n)
data_NA_clutch <- data_NA_clutch %>%
  dplyr::rename(gen.length = GenLength)

data <- data %>%
  dplyr::rename(wing.lcentr = wing.length)

data_NA_clutch <- data_NA_clutch[ -c(1) ]
data_NA_clutch <- data_NA_clutch[ -c(3) ]

data_NA_clutch <- data_NA_clutch %>%
  dplyr::rename(intrusion = instrusion)

write.csv(data, "data_global/data_analysis/only_threatened/analysis/data_genl_clutch.csv")

#prep habitat - human altered or not

data_common_names_amnio_gl$Habitat <-c(Coastal=0, Desert=0, Forest=0, Grassland=0, "Human Modified"=1,Marine=0, Riverine=0, Rock=0, Shrubland=0, Wetland=0, Woodland=0)[data_common_names_amnio_gl$Habitat]
table(data_common_names$Habitat) #nope

library(janitor)
habitat <- janitor::tabyl(data_common_names, species, Habitat)
habitat

#get table to predict glm
#so add threats binary, clutch, gen length to all sp, then center and scale mass and range for all
traits_global <- read.csv(file = "C:/Users/janas/OneDrive/Documentos/R/Birds/IUCN_data/data_global/traits_birds.csv")
threats_global <- read.csv(file = "C:/Users/janas/OneDrive/Documentos/R/Birds/IUCN_data/data_global/data_analysis/threats_binary.csv")

traits_threats <- traits_global %>%
  left_join(., threats_global , by = "species")

#common names
birdlife_selected

names_traits_threats <- traits_threats %>%
  left_join(., birdlife_selected, by = "species")

###mass and range logged and centered (-mean), wing length (-mean / 2std)

names_traits_threats <- names_traits_threats %>%
  mutate(logmass = log(Mass))

names_traits_threats <- names_traits_threats %>%
  mutate(logrange = log(Range.Size))

write.csv(names_traits_threats, "data_global/all_birds/names_traits_threats.csv")

#have to add clutch size and gen  length and center them

all_data <- read.csv("data_global/all_birds/names_traits_threats.csv")
gl_birds <- read.csv("GL_bird2020/cobi13486-sup-0004-tables4.csv") #genlength
amniote <- read.csv("amniote/Data_Files/Amniote_Database_Aug_2015.csv") #clutchsize

gl_birds <- gl_birds %>%
  dplyr::rename(species = Scientific.name)

amniote_name <- transform(amniote, Scientific.name=paste(genus, species))

amniote_name <- amniote_name %>%
  dplyr::rename(species_lat = species)
amniote_name <- amniote_name %>%
  dplyr::rename(species = Scientific.name)

amniote_selected <- amniote_name %>%
  select(species, litter_or_clutch_size_n)

all_data_amniote <- all_data %>%
  left_join(., amniote_selected , by = "species")

gl_birds_selected <- gl_birds %>%
  select(species, GenLength)

all_data_amniote_gl <- all_data_amniote %>%
  left_join(., gl_birds_selected , by = "species")
install.packages("naniar")
library("naniar")

all_data_amniote_gl_NA <- all_data_amniote_gl %>% replace_with_na(replace = list(litter_or_clutch_size_n = -999))

table(all_data_amniote_gl$litter_or_clutch_size_n)

all_data_amniote_gl_NA <- all_data_amniote_gl_NA %>%
  dplyr::rename(clutch.size = litter_or_clutch_size_n)
all_data_amniote_gl_NA <- all_data_amniote_gl_NA %>%
  dplyr::rename(gen.length = GenLength)

write.csv(all_data_amniote_gl_NA, "data_global/all_birds/all_birds_amniote_gl.csv")


