library(tidyverse)


getwd()


Tree_Data <- read.csv("TS3_Raw_Tree_Data.csv")
str_match(Tree_Data$City, "^([:alpha:]+), ([:upper:]+)$")
Tree_Data[,c("City", "State")] = str_match(Tree_Data$City, "^([:alpha:]+), ([:upper:]+)$")[,2:3]
ggplot(data = Tree_Data, aes(x= State) )+
  geom_bar()

NC_SC_Tree <- Tree_Data %>% 
  filter(str_detect(State,"[NS]C")) #checks for NC or SC. 
ggplot(data = NC_SC_Tree, aes(x= City) )+
  geom_bar()

str_match(NC_SC_Tree$ScientificName, "^([:alpha:]+) ([:alpha:]+[:punct:]*)$") 
#there are a few of the scientific names that end in a "." so this adds optional punct
NC_SC_Tree[,c("Genus", "Species")] = 
  str_match(NC_SC_Tree$ScientificName, "^([:alpha:]+) ([:alpha:]+[:punct:]*)$")[,2:3]

NC_SC_Tree %>% 
  group_by(Genus) %>% #groups by genus
  summarise(average_diameter = mean(AvgCdia..m.)) %>% #finds the mean diameter
  ungroup %>% 
  slice_max(average_diameter, n = 1) #takes the top average diameter

  
Age_Comparison <- NC_SC_Tree %>% 
  group_by(Genus) %>% #groups by genus
  summarise(average_diameter = mean(AvgCdia..m.), #finds the mean diameter
            average_age = mean(Age)) %>% 
  ungroup

ggplot(Age_Comparison, aes(x = average_age, y = average_diameter, colour = Genus)) +
  geom_point()


Species_Tree <- Tree_Data
Species_Tree[,c("Genus", "Species")] = 
  str_match(Species_Tree$ScientificName, "^([:alpha:]+)[:space:]*[xX]*[:space:]([:alpha:]+[:punct:]*)")[,2:3]

Species_Tree <- Tree_Data
Species_Tree[,c("Genus", "Species")] = 
  str_match(Species_Tree$ScientificName, "^([:alpha:]+)[:space:]*[xX]*[:space:]([:alpha:]+[:punct:]*)")[,2:3]
#checks if there is an x or X between the genus and species. The first space and the x are optional but the second space is not (making it basically the same as the one used in Question 3)
#because the species grabber no longer has the $ at the end it doesn't pick up the cultivars or variety information. 
#it just works

Unique_Species <- Species_Tree %>% 
  group_by(Genus) %>% 
  summarise(Unique_Species_Count = n_distinct(Species)) %>% 
  ungroup

ggplot(Unique_Species, aes(y = Genus, x = Unique_Species_Count)) +
  geom_col()+
  theme(axis.text.y = element_text(size = 5))

                