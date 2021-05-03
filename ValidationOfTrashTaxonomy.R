library(dplyr)
library(stringdist)
library(stringi)
library(tidyr)
library(data.table)
library(data.tree)

#Working Directory ----
setwd("G:/My Drive/GrayLab/Projects/Plastics/ActiveProjects/TrashTaxonomy/WinTrashTaxonomy/Data/Raw Data")


#Functions ----
cleantext <- function(x) {
  tolower(gsub("[[:space:]]", "", x))
}

removeslash <- function(x){
  gsub("/", " OR ", x)
}

converttodataframenetwork <- function(x){
 df <-  x %>% 
    mutate_all(removeslash) %>%
    mutate(key = "trash") %>%
    dplyr::relocate(key) %>%
    unite(pathString, sep = "/", na.rm = T) 
 
    ToDataFrameNetwork(FromDataFrameTable(df)) %>%
    mutate(to = gsub(".+/", "", to))%>%
    mutate(from = gsub(".+/", "", from)) %>%
    group_by(to) %>%
    summarise(count = n()) %>%
    filter(count != 1)
  
}

#Datasets----
ItemHierarchy <- mutate_all(read.csv("ITEMSHierarchyLower.csv"), cleantext)
MaterialHierarchy <- mutate_all(read.csv("MaterialsHierarchyLower.csv"), cleantext) 
ItemsAlias <- mutate_all(read.csv("PrimeItems.csv"), cleantext) 
MaterialsAlias <- mutate_all(read.csv("PrimeMaterials.csv"), cleantext) 
Misaligned <- mutate_all(read.csv("PrimeUnclassifiable.csv"), cleantext)
RawMaterialItem <- mutate_all(read.csv("RawMaterialItem.csv"), cleantext)
MaterialItem <- RawMaterialItem


#Check that all hierarchy terms have exactly one parent ----
converttodataframenetwork(ItemHierarchy) #This tells us when a child has more than one parent. we want all children to have exactly one parent. 
converttodataframenetwork(MaterialHierarchy)

#Duplicates ----
#*ItemsHierarchy ----
duplicateditemhierarchy <-ItemHierarchy %>%
  dplyr::group_by(across()) %>%
  dplyr::summarise(sum = n()) %>%
  ungroup() %>%
  filter(sum > 1)

duplicateditemhierarchy #If anything appears here then those are rows that have duplicated levels.

#*MaterialsHierarchy----
duplicatedmaterialhierarchy <-MaterialHierarchy %>%
  group_by(across()) %>%
  summarise(sum = n()) %>%
  ungroup() %>%
  filter(sum > 1)

duplicatedmaterialhierarchy #If anything appears here then those are rows that have duplicated levels.

#*Duplicated rows in MaterialItems----

duplicatedmaterialitem <- MaterialItem %>%
  group_by(Material, Item, Organization) %>%
  summarise(sum = n()) %>%
  ungroup() %>%
  filter(sum > 1)

duplicatedmaterialitem #If anything appears here then those are rows that have duplicated levels.

#*Duplicated Misaligned Categories-----
duplicatedmisaligned <- unname(unlist(Misaligned))
tablemisaligned <- table(duplicatedmisaligned[!is.na(duplicatedmisaligned) & duplicatedmisaligned != ""])
tablemisaligned[tablemisaligned > 1] #If there are any duplicates or triplicates this will return them to the console


#Relationship Validation ----
#*Items----
  
ItemAliasUnique <- unname(unlist(ItemsAlias$Item))
ItemAll <- unique(unname(unlist(ItemsAlias)))
ItemAliasUnique <- unique(ItemAliasUnique[!is.na(ItemAliasUnique) & ItemAliasUnique != ""])
SurveyItems <- unique(unname(unlist(MaterialItem$Item)))
ItemAliasOnly <- unique(unname(unlist(ItemsAlias$Alias)))

ItemHierarchyUnique <- unname(unlist(ItemHierarchy))
ItemHierarchyUnique <- unique(ItemHierarchyUnique[!is.na(ItemHierarchyUnique) & ItemHierarchyUnique != ""])

#ItemAliasUnique[!ItemAliasUnique %in% ItemHierarchyUnique] #Words that are in the hierarchy but are not in the Alias
#ItemHierarchyUnique[!ItemHierarchyUnique %in% ItemAliasUnique] #Words that are in the alias but are not in the hierarchy

#This is all we actually want to test.
ItemAliasOnly[!ItemAliasOnly %in% SurveyItems]


#*Materials ----

MaterialAliasUnique <- unname(unlist(MaterialsAlias$Material))
MaterialAll <- unique(unname(unlist(MaterialsAlias)))
MaterialAliasUnique <- unique(MaterialAliasUnique[!is.na(MaterialAliasUnique) & MaterialAliasUnique != ""])
SurveyMaterials <- unique(unname(unlist(MaterialItem$Material)))
MaterialAliasOnly <- unname(unlist(MaterialsAlias$Alias))

MaterialHierarchyUnique <- unname(unlist(MaterialHierarchy))
MaterialHierarchyUnique <- unique(MaterialHierarchyUnique[!is.na(MaterialHierarchyUnique) & MaterialHierarchyUnique != ""])

MaterialAliasUnique[!MaterialAliasUnique %in% MaterialHierarchyUnique] #Words that are in the hierarchy but are not in the Alias
MaterialHierarchyUnique[!MaterialHierarchyUnique %in% MaterialAliasUnique] #Words that are in the alias but are not in the hierarchy
MaterialAll[!MaterialAll %in% SurveyMaterials]
MaterialAliasOnly[!MaterialAliasOnly %in% SurveyMaterials]

#*Misaligned words and material-item relation ----
#Make sure that Materials-Item values are accounted for in alias table or misaligned tables
MisalignedWords <- unique(unname(unlist(Misaligned)))
SurveyItems <- unique(unname(unlist(MaterialItem$Item)))
materialsanditems <- c(SurveyItems, SurveyMaterials)

SurveyItems[!SurveyItems %in% c(unique(ItemAll), unique(MisalignedWords))]# Words that are in the material-items list but not in the items alias or the misaligned words. 
SurveyItems[!SurveyItems %in% c(unique(ItemAliasOnly), unique(MisalignedWords))]# Words that are in the material-items list but not in the items alias or the misaligned words. 
#Difference between above tells me if the word is in the first column but not the second for the alias sheet. 

SurveyMaterials[!SurveyMaterials %in% c(unique(MaterialAll), unique(MisalignedWords))]# Words that are in the material-items list but not in the Materials alias or the misaligned words. 
SurveyMaterials[!SurveyMaterials %in% c(unique(MaterialAliasOnly), unique(MisalignedWords))]# Words that are in the material-items list but not in the Materials alias or the misaligned words. 

MisalignedWords[!MisalignedWords %in% unique(materialsanditems)]
MisalignedWords[MisalignedWords %in% c(unique(ItemAll), unique(MaterialAll))]# Words in misaligned that are already accounted for in Item or MaterialsAlias

