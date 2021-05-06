#Libraries ----

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(ggpol)
library(ggExtra)
library(data.table)
library(factoextra)
library(tidyverse) 
library(cluster)
library(tidyverse)
library(ggrepel)
library(gridExtra)
library(cluster) 
library(ggfortify)
library(corrplot)
library(FactoMineR)
library(Factoshiny)
library(ggpubr)
theme_set(theme_pubr())

#Functions ----
cleantext <- function(x) {
  x <- tolower(gsub("[[:space:]]", "", x))
  ifelse(x == "", NA, x)
}

theme_gray<- function (base_size = 11, base_family = "Arial") 
{
  half_line <- base_size/2
  theme(
    line = element_line(colour = "black", size = rel(1.5), 
                        linetype = 1, lineend = "butt"), 
    rect = element_rect(fill = NA, colour = "black",
                        size = 0.5, linetype = 1),
    text = element_text(family = base_family, face = "plain",
                        colour = "black", size = base_size,
                        lineheight = 0.9,  hjust = 0.5,
                        vjust = 0.5, angle = 0, 
                        margin = margin(), debug = FALSE), 
    
    axis.line = element_blank(), 
    axis.text = element_text(family= "Arial", size = rel(1.5), colour = "grey10"),
    axis.text.x = element_text(margin = margin(t = half_line/2), 
                               vjust = 1), 
    axis.text.y = element_text(margin = margin(r = half_line/2),
                               hjust = 1),
    axis.ticks = element_line(colour = "black", size=1), 
    axis.ticks.length = unit(half_line*0.75, "pt"), 
    axis.title = element_text(family="Arial",size = rel(1.5), colour = "black"),
    axis.title.x = element_text(margin = margin(t = half_line*5,
                                                b = half_line)),
    axis.title.y = element_text(angle = 90, 
                                margin = margin(r = half_line*5,
                                                l = half_line)),
    
    legend.background = element_rect(colour = NA), 
    legend.key = element_rect(colour = NA),
    legend.key.size = unit(2, "lines"), 
    legend.key.height = NULL,
    legend.key.width = NULL, 
    legend.text = element_text(family = "Arial", size = rel(1)),
    legend.text.align = NULL,
    legend.title = element_text(family = "Arial", size = rel(1)), 
    legend.title.align = NULL, 
    legend.position = "right", 
    legend.direction = NULL,
    legend.justification = "center", 
    legend.box = NULL, 
    
    panel.background = element_rect(fill=NA,colour = "black", size = 2), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.spacing = unit(half_line, "pt"), panel.margin.x = NULL, 
    panel.spacing.y = NULL, panel.ontop = FALSE, 
    
    #Facet Labels
    strip.background = element_blank(),
    strip.text = element_text(family = "Arial",face="bold",colour = "black", size = rel(1.5)),
    strip.text.x = element_text(margin = margin(t = half_line,
                                                b = half_line)), 
    strip.text.y = element_text(angle = 0, 
                                margin = margin(l = half_line, 
                                                r = half_line)),
    strip.switch.pad.grid = unit(5, "lines"),
    strip.switch.pad.wrap = unit(5, "lines"), 
    
    
    plot.background = element_rect(colour = rgb(119,136,153, max = 255)), 
    plot.title = element_text(size = rel(1.5), 
                              margin = margin(b = half_line * 1.2)),
    plot.margin = margin(4*half_line, 4*half_line, 4*half_line, 4*half_line),
    complete = TRUE)
}

#Datasets ----
MaterialsAlias <- read.csv("data/PrimeMaterials.csv")
ItemsAlias <- read.csv("data/PrimeItems.csv")
GroupRelationships <- read.csv("data/RawMaterialItem.csv")
SurveySheetInfo <- read.csv("data/SurveySheetInformation.csv")
ItemsHierarchy <- read.csv("data/ITEMSHierarchyLower.csv")
MaterialsHierarchy <- read.csv("data/MaterialsHierarchyLower.csv")

#Analysis ----
#*Data cleanup ----
ItemsAliasClean <- ItemsAlias %>%
  mutate_all(cleantext) %>%
  distinct() %>%
  mutate(RowId = 1:nrow(.))


MaterialsAliasClean <- MaterialsAlias %>%
  mutate_all(cleantext) %>%
  distinct()%>%
  mutate(RowIdMaterials = 1:nrow(.))

ItemsHierarchyClean <- mutate_all(ItemsHierarchy, cleantext)
  
MaterialHierarchyClean <- mutate_all(MaterialsHierarchy, cleantext)

SurveySheetInfoClean <- mutate_all(SurveySheetInfo, cleantext) %>%
  rename(organization = Survey.Sheet.Name)

GroupRelationshipsClean <- mutate_all(GroupRelationships, cleantext) %>%
  mutate(rowid = 1:nrow(.))

GroupRelationshipsCleanjoined <- GroupRelationshipsClean %>%
  rename(Alias = Item) %>%
  mutate(Alias = ifelse(Alias == "", NA, Alias)) %>%
  left_join(select(ItemsAliasClean, Item, Alias) %>% rename(ItemKey = Item), na_matches = "never") %>%
  select(-Alias) %>%
  rename(Alias = Material) %>%
  mutate(Alias = ifelse(Alias == "", NA, Alias)) %>%
  left_join(select(MaterialsAliasClean,  Material, Alias) %>% rename(MaterialKey = Material), na_matches = "never") %>%
  distinct() %>%
  select(-Alias)

Combo <- as.data.frame(t(combn(unique(GroupRelationshipsCleanjoined$Organization), 2)), stringsAsFactors = F)

ComboComparability <- GroupRelationshipsCleanjoined %>%
  filter(!is.na(ItemKey) & !is.na(rowid)) %>%
  group_by(ItemKey, Organization) %>%
  summarise(count = n()) %>%
  mutate(count = 1) %>%
  ungroup() 

#*Processing the comparability metric for items ----
#This takes a little while.
for(row in 1:nrow(Combo)) {
  totalcomboright <- filter(ComboComparability, Organization == Combo[row,2]) 
  totalcomboleft <- filter(ComboComparability, Organization == Combo[row,1]) 
  
  
  joinedsum <- filter(ComboComparability, Organization %in% c(Combo[row,1], Combo[row,2])) %>%
    group_by(ItemKey) %>%
    summarise(sum = sum(count)) %>%
    filter(sum == 2) %>%
    mutate(count = 1)
  
  totalcombo <- filter(ComboComparability, Organization %in% c(Combo[row,1], Combo[row,2])) 
  joinedsumcombo <- filter(ComboComparability, Organization %in% c(Combo[row,1], Combo[row,2])) %>%
    group_by(ItemKey) %>%
    summarise(sum = sum(count)) %>%
    filter(sum == 2)
  
  Combo[row, "ComparabilityItems"] <- sum(joinedsumcombo$sum)/sum(totalcombo$count)
  Combo[row, "ComparabilityItemsRight"] <- sum(joinedsum$count)/sum(totalcomboright$count)
  Combo[row, "ComparabilityItemsLeft"] <- sum(joinedsum$count)/sum(totalcomboleft$count)
  
}

#*Processing the comparability metric for materials----

ComboComparabilityMaterials <- GroupRelationshipsCleanjoined %>%
  filter(!is.na(MaterialKey) & !is.na(rowid)) %>%
  group_by(MaterialKey, Organization) %>%
  summarise(count = n()) %>%
  mutate(count = 1) %>%
  ungroup() 

#This is a little slow
for(row in 1:nrow(Combo)) {
  totalcombo <- filter(ComboComparabilityMaterials, Organization %in% c(Combo[row,1], Combo[row,2])) 
  
  joinedsumcombo <- filter(ComboComparabilityMaterials, Organization %in% c(Combo[row,1], Combo[row,2])) %>%
    group_by(MaterialKey) %>%
    summarise(sum = sum(count)) %>%
    filter(sum == 2)
  
  totalcomboright <- filter(ComboComparabilityMaterials, Organization == Combo[row,2]) 
  totalcomboleft <- filter(ComboComparabilityMaterials, Organization == Combo[row,1]) 
  
  
  joinedsum <- filter(ComboComparabilityMaterials, Organization %in% c(Combo[row,1], Combo[row,2])) %>%
    group_by(MaterialKey) %>%
    summarise(sum = sum(count)) %>%
    filter(sum == 2) %>%
    mutate(count = 1)
  
  Combo[row, "ComparabilityMaterials"] <- sum(joinedsumcombo$sum)/sum(totalcombo$count)
  Combo[row, "ComparabilityMaterialsRight"] <- sum(joinedsum$count)/sum(totalcomboright$count)
  Combo[row, "ComparabilityMaterialsLeft"] <- sum(joinedsum$count)/sum(totalcomboleft$count)
}

AverageForOrganizations <- Combo %>%
  gather(Column, Organization, -ComparabilityItemsLeft, -ComparabilityItemsRight, -ComparabilityMaterials, -ComparabilityMaterialsRight, -ComparabilityMaterialsLeft, -ComparabilityItems) %>%
  mutate(ComparabilityItemsEncompassingOthers = ifelse(Column == "V1", ComparabilityItemsRight, ComparabilityItemsLeft)) %>%
  mutate(ComparabilityMaterialsEncompassingOthers = ifelse(Column == "V1", ComparabilityMaterialsRight, ComparabilityMaterialsLeft)) %>%
  select(Organization, ComparabilityItems, ComparabilityItemsEncompassingOthers, ComparabilityMaterials, ComparabilityMaterialsEncompassingOthers)

AverageForOrganizations[is.na(AverageForOrganizations)]<-0 #Change NA values to 0

#Mean of all the comparability statistics we ran for each organization----
AverageForOrganizationsTotal <- AverageForOrganizations %>%
  group_by(Organization) %>%
  summarize_all(mean)

#Comparability Metric Plot ----
ggplot(AverageForOrganizationsTotal, aes(x = ComparabilityItemsEncompassingOthers, y = ComparabilityMaterialsEncompassingOthers)) + 
  #geom_bin2d(bins = 10)  + 
  geom_point(color = "black", size = 3, alpha = 0.5)  +
  geom_segment(aes(x = 0.6, y = 0.7, xend = 1, yend = 1),
               arrow = arrow(length = unit(0.5, "cm"))) + 
  labs(x = "Items Comparability (mean decimal %)", y = "Materials Comparability (mean decimal %)") + 
  scale_x_continuous(breaks = c(seq(0,1, by = 0.1)), limits = c(0,1)) + scale_y_continuous(breaks = c(seq(0,1, by = 0.1)), limits = c(0,1))+
  scale_fill_viridis_c(breaks = c(1:10)) + theme_gray() + coord_equal() + 
  geom_label_repel(label.size = 0.01, size = 3,  data = AverageForOrganizationsTotal[sample(1:nrow(AverageForOrganizationsTotal), 5, replace = F),], aes(label = Organization)) 

#Stats of the comparability metric ----

#Mean comparability for items and material types ----
mean(AverageForOrganizations$ComparabilityMaterialsEncompassingOthers, na.rm = T)
mean(AverageForOrganizations$ComparabilityItemsEncompassingOthers, na.rm = T)

#Number of comparisons which are 100 percent compatible ----
length(AverageForOrganizations$ComparabilityMaterialsEncompassingOthers[AverageForOrganizations$ComparabilityMaterialsEncompassingOthers == 1])
length(AverageForOrganizations$ComparabilityItemsEncompassingOthers[AverageForOrganizations$ComparabilityItemsEncompassingOthers == 1])

#Number of comparisons ----
nrow(AverageForOrganizations)

#Number of comparisons which are 0 percent compatible ----
length(AverageForOrganizations$ComparabilityMaterialsEncompassingOthers[AverageForOrganizations$ComparabilityMaterialsEncompassingOthers == 0])
length(AverageForOrganizations$ComparabilityItemsEncompassingOthers[AverageForOrganizations$ComparabilityItemsEncompassingOthers == 0])

#Table of the number of comparisons for each organization, should be the same for each organization.
table(AverageForOrganizations$Organization)

#Number of organizations compared ----
unique(AverageForOrganizations$Organization)

#Number of organizations with greater than 20% average comparability metric for items and material types ----
AverageForOrganizationsTotal %>%
  filter(ComparabilityItemsEncompassingOthers > 0.2 & ComparabilityMaterialsEncompassingOthers > 0.2)

#Number of organizations with greater than 40% average comparability metric for items and material types ----
AverageForOrganizationsTotal %>%
  filter(ComparabilityItemsEncompassingOthers > 0.4 & ComparabilityMaterialsEncompassingOthers > 0.4)

ggplot() + 
  stat_ecdf(data = AverageForOrganizations, aes(x = ComparabilityItemsEncompassingOthers), color = "red") +
  stat_ecdf(data = AverageForOrganizations, aes(x = ComparabilityMaterialsEncompassingOthers), color = "black") + 
  stat_ecdf(data = AverageForOrganizationsTotal, aes(x = ComparabilityItemsEncompassingOthers), color = "blue") +
  stat_ecdf(data = AverageForOrganizationsTotal, aes(x = ComparabilityMaterialsEncompassingOthers), color = "green") 
  
  

##Stats of surveys----
unique(GroupRelationshipsClean$Material) #Total number of Material types
unique(GroupRelationshipsClean$Item) #Total number of Item types
unique(ItemsAliasClean$Alias)[unique(ItemsAliasClean$Alias) %in% unique(GroupRelationshipsClean$Item)] #Number of item types used in the analysis
unique(ItemsAliasClean$Item) # total number of alias keys
unique(MaterialsAliasClean$Alias)[unique(MaterialsAliasClean$Alias)%in% unique(GroupRelationshipsClean$Material)] #Number of material types used in the analysis.
unique(MaterialsAliasClean$Material) #total number of alias keys for materials
unique(ComboComparability$Organization) #total number of organizations with item types
unique(ComboComparabilityMaterials$Organization) #total number of organizations with material types.

#MCA Analysis----
onehotitems <- ComboComparability %>%
  distinct() %>%
  spread(ItemKey, count) %>%
  inner_join(SurveySheetInfoClean %>% rename(OrgType = Organization) %>% rename(Organization = organization), by= "Organization") %>%
  as.data.frame() 

onehotitems[is.na(onehotitems)] <- 0

colnames(onehotitems)

onehotitems <- mutate_all(onehotitems, as.factor)

StatsItems <- onehotitems %>%
  group_by(Ecosystem, Substrate, OrgType) %>%
  summarise(count = n())

onehotmaterials <- ComboComparabilityMaterials %>%
  distinct() %>%
  spread(MaterialKey, count) %>%
  inner_join(SurveySheetInfoClean %>% rename(OrgType = Organization) %>% rename(Organization = organization), by = "Organization") %>%
  as.data.frame()

onehotmaterials[is.na(onehotmaterials)] <- 0

colnames(onehotmaterials)

onehotmaterials <- mutate_all(onehotmaterials, as.factor)

row.names(onehotmaterials) <- onehotmaterials$Organization

onehotmaterials <- onehotmaterials %>%
  select(-Location, -URL, -Trash.Survey, -Organization)

row.names(onehotitems) <- onehotitems$Organization

onehotitems <- onehotitems %>%
  select(-Location, -URL, -Trash.Survey, -Organization)

#write.csv(onehotmaterials, "materialsmcasetup.csv")
#write.csv(onehotitems, "itemsmcasetup.csv")

#result <- Factoshiny(onehotmaterials)
res.MCA<-MCA(onehotmaterials,quali.sup=c(26,27,28),graph=FALSE)
plot.MCA(res.MCA, choix='var', label = c("quali.sup"), autoLab = "y")

#MCA Plots for material types ----
a <- plotellipses(res.MCA,keepvar=26, label = "none") + theme_pubr() + theme(legend.position = c(0.5,0.5)) + labs(title = NULL) + labs_pubr()  + coord_cartesian(ylim = c(-1, 2.5))+ scale_color_viridis_d(option = "C")+ scale_fill_viridis_d(option = "C")#+ theme_gray() #+ scale_fill_viridis_d(option = "A")
b <- plotellipses(res.MCA,keepvar=27, label = "none") + theme_pubr() + theme(legend.position = c(0.5,0.5)) + labs(title = NULL) + labs_pubr() + coord_cartesian(ylim = c(-1, 2.5))#+ scale_color_viridis_d(option = "B")
c <- plotellipses(res.MCA,keepvar=28, label = "none") + theme_pubr() + theme(legend.position = c(0.5,0.5)) + labs(title = NULL) + labs_pubr() + coord_cartesian(ylim = c(-1, 2.5))#+ scale_color_viridis_d(option = "C")

ggarrange(a,b,c, labels = c("A", "B", "C"), ncol = 3, nrow = 1)

dimdesc(res.MCA)

#Material Survey stats for the table ----
materialstats <- as.data.frame(summary(onehotmaterials)) #Here are the stats for the surveys

#result <- Factoshiny(onehotitems)
res.MCA<-MCA(onehotitems,ncp=13,quali.sup=c(418, 419,420),graph=FALSE)
res.HCPC<-HCPC(res.MCA,nb.clust=10,consol=FALSE,graph=FALSE)

#MCA plots for item types. ----
d <- plotellipses(res.MCA,keepvar=418, label = "none") + theme_pubr()+ theme(legend.position = c(0.5,0.5)) + labs(title = NULL) + labs_pubr() + coord_cartesian(xlim = c(-1, 3.5))
e <- plotellipses(res.MCA,keepvar=419, label = "none") + theme_pubr()+ theme(legend.position = c(0.5,0.5)) + labs(title = NULL) + labs_pubr()+ coord_cartesian(xlim = c(-1, 3.5))
f <- plotellipses(res.MCA,keepvar=420, label = "none") + theme_pubr()+ theme(legend.position = c(0.5,0.5)) + labs(title = NULL) + labs_pubr()+ coord_cartesian(xlim = c(-1, 3.5))
ggarrange(d,e,f, labels = c("A", "B", "C"), ncol = 3, nrow = 1)


dimdesc(res.MCA)
summary(res.HCPC)

#Item Survey stats for the table ----
itemstats <- as.data.frame(summary(onehotitems))

#http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/114-mca-multiple-correspondence-analysis-in-r-essentials/
